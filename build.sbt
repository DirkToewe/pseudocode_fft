

lazy val root = (project in file("."))
  .settings(
    name := "pseudocode_fft",
    version := "0.0.0-SNAPSHOT",
    scalaVersion := "3.3.0",
    scalacOptions ++= Seq(
      "-deprecation",
      "-source:future",
    ),
    fork := true,
    javaOptions ++= Seq(
      "-ea",
      "-Xmx16g",
    ),

    testFrameworks += new TestFramework("utest.runner.Framework"),

    libraryDependencies ++= Seq(
      // TEST DEPENDENCIES
      "com.lihaoyi"            %% "utest"                      % "0.8.1"  % "test",
      "org.scalacheck"         %% "scalacheck"                 % "1.17.0" % "test",
      "org.typelevel"          %% "spire"                      % "0.18.0"
    ),

    test / parallelExecution  := true,
    test / testForkedParallel := true,
    test / javaOptions ++= Seq(
      "-Xmx16g"
    ),
  )
