/*
 * Copyright (c) 2011 ScalaStuff.org (joint venture of Alexander Dvorkovyy and Ruud Diterwich)
 *
 *    Licensed under the Apache License, Version 2.0 (the "License");
 *    you may not use this file except in compliance with the License.
 *    You may obtain a copy of the License at
 *
 *        http://www.apache.org/licenses/LICENSE-2.0
 *
 *    Unless required by applicable law or agreed to in writing, software
 *    distributed under the License is distributed on an "AS IS" BASIS,
 *    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *    See the License for the specific language governing permissions and
 *    limitations under the License.
 */

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

  override def pomExtra =
    <licenses>
      <license>
        <name>Apache License 2.0</name>
        <url>http://www.apache.org/licenses/LICENSE-2.0.txt</url>
        <distribution>repo</distribution>
      </license>
    </licenses>

  override def pomPostProcess(node : Node) : Node = node match {
    case e : Elem =>
 		  val children = if ((e \ "groupId").text == "com.dyuproject.protostuff") e.child ++ <optional>true</optional>
 		  else if ((e \ "groupId").text == "org.scala-lang") e.child.filter(_.label!="scope") ++ <scope>provided</scope>
 		  else e.child.map(pomPostProcess)
    	Elem(e.prefix, e.label, e.attributes, e.scope, children:_*)
    case xml => xml
  }
}
