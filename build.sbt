name := "discord-irc-gw"
version := "0.1"
scalaVersion := "2.12.1"

fork := true
scalacOptions ++= Seq("-deprecation", "-feature", "-Yinfer-argument-types", "-Ypartial-unification", "-Xlint", "-opt:_", "-opt-warnings:_")

libraryDependencies ++= Seq(
  "net.dv8tion" % "JDA" % "3.0.BETA_75" exclude ("net.java.dev.jna", "jna"),
  "com.github.scopt" %% "scopt" % "3.5.0",
  "com.lihaoyi" %% "fastparse" % "0.4.2"
)
resolvers += "jcenter" at "http://jcenter.bintray.com"

enablePlugins(JavaAppPackaging)
mainClass in Compile := Some("discordircgw.Main")
javaOptions in Universal ++= Seq("-J-Xmx14m")
