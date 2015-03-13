name := "reasonably"

version := "1.0"

scalaVersion := "2.11.6"

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)

libraryDependencies ++= Seq(
  "org.scalaz"              %% "scalaz-core"      % "7.1.1",
  "net.databinder.dispatch" %% "dispatch-core"    % "0.11.2")
