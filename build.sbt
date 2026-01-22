lazy val mset =
  project
    .in(file("."))
    .settings( scalaVersion := "3.8.1"
             , name         := "fpa-mset"
             , version      := "0.1.0"
             , libraryDependencies ++= Seq( "org.typelevel"  %% "cats-core"   % "2.13.0"
                                          , "org.typelevel"  %% "cats-effect" % "3.6.3"
                                          , "org.scalacheck" %% "scalacheck"  % "1.19.0"
                                          , "org.typelevel"  %% "squants"     % "1.8.3"
                                          , "org.scalatest"  %% "scalatest"   % "3.2.19" % "test" )
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
