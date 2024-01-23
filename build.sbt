lazy val mset =
  project
    .in(file("."))
    .settings( scalaVersion := "3.3.1"
             , name         := "fpa-mset"
             , version      := "0.1.0"
             , libraryDependencies ++= Seq( "org.typelevel"  %% "cats-core"   % "2.10.0"
                                          , "org.typelevel"  %% "cats-effect" % "3.5.2"
                                          , "org.scalacheck" %% "scalacheck"  % "1.17.0"
                                          , "org.typelevel"  %% "squants"     % "1.8.3"
                                          , "org.scalatest"  %% "scalatest"   % "3.2.16" % "test" )
             )

scalacOptions ++= Seq( "-encoding", "utf8"
                     , "-feature"
                     , "-language:implicitConversions"
                     , "-language:existentials"
                     , "-unchecked"
                     , "-Werror"
                     , "-deprecation" )

Compile / run / fork        := true
Compile / run / javaOptions += "-Xmx4G"
