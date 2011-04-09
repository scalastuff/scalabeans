import sbt._
import de.element34.sbteclipsify._
  import xml.{Elem,Node,NodeSeq,Text}

class ScalaBeansProject(info:ProjectInfo) extends DefaultProject(info) with Eclipsify {

  // repositories
  val protostuffRepo = "Protostuff Repository" at "http://protostuff.googlecode.com/svn/repos/maven2/"
    
  // dependencies
  val paranamer = "com.thoughtworks.paranamer" % "paranamer" % "2.3" withSources()
  val protostuffApi = "com.dyuproject.protostuff" % "protostuff-api" % "1.0.0" withSources()
  val protostuffCore = "com.dyuproject.protostuff" % "protostuff-core" % "1.0.0" withSources() 
  val junit = "junit" % "junit" % "4.8" % "test" withSources()

	// documentation
	override def documentOptions = List(LinkSource)
	
	// publish sources	 
  override def packageSrcJar= defaultJarPath("-sources.jar")
  val sourceArtifact = Artifact.sources(artifactID)
  override def packageToPublishActions = super.packageToPublishActions ++ Seq(packageSrc)
  
  // publishing
  override def managedStyle = ManagedStyle.Maven
  val publishTo = Resolver.file("maven-local", "mavenrepo" /  "releases" asFile)
  
  override def pomPostProcess(node : Node) : Node = node match {
    case e : Elem =>
 		  val children = if ((e \ "groupId").text == "com.dyuproject.protostuff") e.child ++ <optional>true</optional>
 		  else if ((e \ "groupId").text == "org.scala-lang") e.child.filter(_.label!="scope") ++ <scope>provided</scope>
 		  else e.child.map(pomPostProcess)
    	Elem(e.prefix, e.label, e.attributes, e.scope, children:_*)
    case xml => xml
  }
}
