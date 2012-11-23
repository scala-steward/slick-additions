scalaVersion := "2.10.0-RC2"

scalaBinaryVersion <<= scalaVersion

libraryDependencies += "com.typesafe" % "slick" % "0.11.2" cross CrossVersion.full

name := "slick-additions"

organization := "nafg"

version := "0.1.1"

