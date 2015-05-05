import sbt.Defaults.sbtPluginExtra

// Put in ~/.sbt/0.13/plugins/plugins.sbt to have plugins available in every sbt project
// mkdir -p ~/.sbt/0.13/plugins && ln -s `pwd`/plugins.sbt $HOME/.sbt/0.13/plugins/plugins.sbt

// optionally add new sbt maven resolver based on sbt version
libraryDependencies ++= {
  if (sbtVersion.value == "0.13.8") {
    Seq(sbtPluginExtra(ModuleID("org.scala-sbt", "sbt-maven-resolver", sbtVersion.value), sbtBinaryVersion.value, scalaBinaryVersion.value))
  } else Seq.empty
}

addSbtPlugin("com.timushev.sbt" % "sbt-updates" % "0.1.8")

addSbtPlugin("net.virtual-void" % "sbt-dependency-graph" % "0.7.5")

addSbtPlugin("com.gilt" % "sbt-dependency-graph-sugar" % "0.7.5")


