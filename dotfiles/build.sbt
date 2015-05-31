//
//ln -s `pwd`/build.sbt $HOME/.sbt/0.13/build.sbt

//scalaVersion := "2.11.6"

//libraryDependencies ++= Seq( "org.sameersingh.scalaplot" % "scalaplot" % "0.0.3")

//resolvers += "bintray/paulp" at "https://dl.bintray.com/paulp/maven"
//addCompilerPlugin("org.improving" %% "sxr" % "1.0.1")
//scalacOptions += { "-P:sxr:base-directory:" + (sourceDirectories in Compile).value.mkString(":") }

//resolvers += Resolver.url("Typesafe Releases", url("http://repo.typesafe.com/typesafe/ivy-releases"))(Resolver.ivyStylePatterns)
//addCompilerPlugin("org.scala-sbt.sxr" %% "sxr" % "0.3.0")
//scalacOptions <+= scalaSource in Compile map { "-P:sxr:base-directory:" + _.getAbsolutePath }

/*
resolvers += "bintray/paulp" at "https://dl.bintray.com/paulp/maven"
addCompilerPlugin("org.improving" %% "sxr" % "1.0.1")
scalacOptions in Compile <+= scalaSource in Compile map {
  "-P:sxr:base-directory:" + _.getAbsolutePath
}
*/

/*
Seq(Compile, Test, Runtime, Provided, Optional).map { config =>
  println(s"setting in config $config")
  dependencyGraphOutputFormat in config := "svg"
}
*/

initialCommands += s"""
  import java.nio.file._
  def writeFile(name: String, contents: String) = {
    Files.write(Paths.get(name), contents.getBytes)
  }
"""

