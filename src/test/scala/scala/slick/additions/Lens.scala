package scala.slick.additions


object States {
  def init[S]: State[S, S] = State[S, S](s => (s, s))

  def modify[S](f: S => S) = init[S] flatMap (s => State(_ => (f(s), ())))

  def put[S](s: S) = State[S, Unit](_ => (s, ()))

  def gets[S, A](f: S => A): State[S, A] =
    for (s <- init) yield f(s) // init map f

  case class State[S, +A](f: S => (S, A)) {
    def apply(s: S): (S, A) = f(s)

    def map[B](f: A => B): State[S, B] = State[S, B](apply(_) match {
      case (s, a) => (s, f(a))
    })

    def flatMap[B](f: A => State[S, B]): State[S, B] = State(apply(_) match {
      case (s, a) => f(a)(s)
    })

    def !(s: S): A = apply(s)._2

    def ~>(s: S): S = apply(s)._1

    def withs(f: S => S): State[S, A] = State(f andThen (apply(_)))
  }

  case class Lens[A, B](get: A => B, set: (A, B) => A) extends Immutable {
    /** A Lens[A,B] can be used as a function from A => B, or implicitly via Lens.asState as a State[A,B] action */
    def apply(whole: A): B = get(whole)

    /** Modify the value viewed through the lens */
    def mod(a: A, f: B => B): A = set(a, f(get(a)))

    /** modp[C] = modf[PartialApply1Of2[Tuple,C]#Flip], but is more convenient to think about
     * 
     * @return a pair. _1 is the result of setting a with f(get(a))._1 --
     *                 so it gets the value through this lens and passes it to f, and gets the return's _1
     *                 _2 is f's result's _2
     * @param f a function from a field value to (another field value, and some other value) 
     *  */
    def modp[C](a: A, f: B => (B, C)): (A, C) = {
      val (b, c) = f(get(a))
      (set(a, b), c)
    }

    /** Lenses can be composed */
    def compose[C](that: Lens[C, A]) = Lens[C, B](
      c => get(that.get(c)),
      (c, b) => that.mod(c, set(_, b))
    )
    def andThen[C](that: Lens[B, C]) = that compose this

    /** You can apply an isomorphism to the value viewed through the lens to obtain a new lens. */
    def xmap[C](f: B => C)(g: C => B) = Lens[A, C](
      a => f(get(a)),
      (a, c) => set(a, g(c))
    )

    /** Two lenses that view a value of the same type can be joined */
    def |||[C](that: Lens[C, B]) = Lens[Either[A, C], B](
      {
        case Left(a)  => get(a)
        case Right(b) => that.get(b)
      },
      {
        case (Left(a), b)  => Left (set(a, b))
        case (Right(c), b) => Right(that.set(c, b))
      }
    )

    /** Two disjoint lenses can be paired */
    def ***[C, D](that: Lens[C, D]) = Lens[(A, C), (B, D)](
      ac => (get(ac._1), that.get(ac._2)),
      (ac, bd) => (set(ac._1, bd._1), that.set(ac._2, bd._2))
    )

    implicit def toState: State[A, B] = State[A, B](a => (a, get(a)))

    /** We can contravariantly map the state of a state monad through a lens */
    def lifts[C](s: State[B, C]): State[A, C] = State[A, C](a => modp(a, s.apply))

    /** modify the state, and return a derived value as a state monadic action. */
    def modps[C](f: B => (B, C)): State[A, C] = lifts(State(f))
    
    /** modify the portion of the state viewed through the lens and return its new value */
    def mods[C](f: B => B): State[A, B] = State[A, B](a => modp(a, { b => val b2 = f(b); (b2, b2) }))

    /** modify the portion of the state viewed through the lens, but do not return its new value */
    def mods_[C](f: B => B): State[A, Unit] = State[A, Unit](a => (mod(a, f), Unit))

    /** Set the value viewed through the lens to a given value */
    def :=(b: B): State[A, Unit] = State[A, Unit](a => (set(a, b), Unit))

    /** flatMapping a lens yields a state action to avoid ambiguity */
    def flatMap[C](f: B => State[A, C]): State[A, C] = State[A, C](a => f(get(a))(a))

    /** Mapping a lens yields a state action to avoid ambiguity */
    def map[C](f: B => C): State[A, C] = State[A, C](a => (a, f(get(a))))
  }

  object Lens {
    /** The identity lens for a given object */
    def self[A] = Lens[A, A](a => a, (_, a) => a)

    /** The trivial lens that can retrieve Unit from anything */
    def trivial[A] = Lens[A, Unit](_ => Unit, (a, _) => a)

    /** A lens that discards the choice of Right or Left from Either */
    def codiag[A]: Lens[Either[A, A], A] = self[A] ||| self[A]

    /** Access the first field of a tuple */
    def fst[A, B] = Lens[(A, B), A](_._1, (ab, a) => (a, ab._2))

    /** Access the second field of a tuple */
    def snd[A, B] = Lens[(A, B), B](_._2, (ab, b) => (ab._1, b))
  }

}