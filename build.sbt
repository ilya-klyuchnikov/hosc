name := "hosc"

scalaVersion := "2.13.5"

scalacOptions ++= Seq("-deprecation", "-feature")

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"

libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"
