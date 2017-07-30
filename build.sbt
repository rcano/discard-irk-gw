name := "discord-irc-gw"
version := "0.1"
scalaVersion := "2.12.3"

fork := true
scalacOptions ++= Seq("-deprecation", "-feature", "-Yinfer-argument-types", "-Ypartial-unification", "-Xlint", "-opt:_", "-opt-warnings:_")

libraryDependencies ++= Seq(
  "net.dv8tion" % "JDA" % "3.2.0_241" exclude ("net.java.dev.jna", "jna"),
  "com.github.scopt" %% "scopt" % "3.6.0",
  "com.lihaoyi" %% "fastparse" % "0.4.3"
)
resolvers += "jcenter" at "http://jcenter.bintray.com"

enablePlugins(JavaAppPackaging)
mainClass in Compile := Some("discordircgw.Main")
javaOptions in Universal ++= Seq("-J-Xmx40m")
