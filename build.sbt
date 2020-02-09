lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.github.spinalhdl",
      scalaVersion := "2.11.6",
      version      := "1.0.0"
    )),
    libraryDependencies ++= Seq(
      "com.github.spinalhdl" % "spinalhdl-core_2.11" % "1.3.1",
      "com.github.spinalhdl" % "spinalhdl-lib_2.11" % "1.3.1",
      "org.scalactic" %% "scalactic" % "3.0.5",
      "org.scalatest" % "scalatest_2.11" % "2.2.1",
      "org.yaml" % "snakeyaml" % "1.8"
    ),
    fork := true,
    name := "spinalProjects",
    scalaSource in Compile := baseDirectory.value / "hardware" / "scala",
    scalaSource in Test    := baseDirectory.value / "test" / "scala"
  )

lazy val trunk = RootProject(file("."))

