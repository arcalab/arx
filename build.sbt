name := "reactiveDsl"

version := "0.2"

//scalaVersion := "2.12.8"

// to compile within ReoLive with crossVersions
scalaVersion := "3.0.0-M1"
libraryDependencies ++= Seq(
  ("org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2").withDottyCompat(scalaVersion.value), //.cross(CrossVersion.for2_13Use3),
  ("org.scalatest" %% "scalatest" % "3.2.9" % "test").withDottyCompat(scalaVersion.value), //.cross(CrossVersion.for2_13Use3),
  ("org.typelevel" %% "cats-parse" % "0.3.4").withDottyCompat(scalaVersion.value)
)

// to compile alone
//scalaVersion := "3.0.2"
//libraryDependencies ++= Seq(
//  ("org.scala-lang.modules" % "scala-parser-combinators_3" % "2.1.0"),
//  ("org.scalatest" % "scalatest_3" % "3.2.9" % "test"),
//  "org.typelevel" %% "cats-parse" % "0.3.4"
//)


//// failed experiments
//libraryDependencies ++= {
//  CrossVersion.partialVersion(scalaVersion.value) match {
//    case Some((2, n)) if n >= 13 => List(
//        "org.scala-lang.modules" % "scala-parser-combinators_2.13" % "1.1.2",
//        "org.scalatest" % "scalatest_2.13" % "3.2.9" % "test"
//      )
//    case _ => List(
//        ("org.scala-lang.modules" % "scala-parser-combinators_3" % "2.1.0"),
//        ("org.scalatest" % "scalatest_3" % "3.2.9" % "test")
//      )
//  }
//}
