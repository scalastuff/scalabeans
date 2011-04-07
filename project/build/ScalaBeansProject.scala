import sbt._
import de.element34.sbteclipsify._

class ScalaBeansProject(info:ProjectInfo) extends DefaultProject(info) with Eclipsify {

  // repositories
  val protostuffRepo = "Protostuff Repository" at "http://protostuff.googlecode.com/svn/repos/maven2/"
    
  // dependencies
  val paranamer = "com.thoughtworks.paranamer" % "paranamer" % "2.3" withSources()
  val protostuffApi = "com.dyuproject.protostuff" % "protostuff-api" % "1.0.0" withSources()
  val protostuffCore = "com.dyuproject.protostuff" % "protostuff-core" % "1.0.0" withSources()
  val protostuffRuntime = "com.dyuproject.protostuff" % "protostuff-runtime" % "1.0.0" withSources()
  val protostuffJson = "com.dyuproject.protostuff" % "protostuff-json" % "1.0.0" withSources()
  val junit = "junit" % "junit" % "4.8" % "test" withSources()

	// documentation
	override def documentOptions = List(LinkSource)
  
  // publishing
  override def managedStyle = ManagedStyle.Maven
  val publishTo = Resolver.file("maven-local", Path.userHome / ".m2scalastuff" /  "repository" asFile)
}
