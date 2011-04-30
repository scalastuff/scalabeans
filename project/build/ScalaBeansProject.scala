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
  val protostuffJson = "com.dyuproject.protostuff" % "protostuff-json" % "1.0.0" withSources() 
  val protostuffXml = "com.dyuproject.protostuff" % "protostuff-xml" % "1.0.0" withSources() 
  val junit = "junit" % "junit" % "4.8" % "test" withSources()
  val woodstox = "org.codehaus.woodstox" % "woodstox-core-asl" % "4.1.1"

	// publish sources
  override def packageDocsJar = defaultJarPath("-javadoc.jar")
  override def packageSrcJar= defaultJarPath("-sources.jar")
  val sourceArtifact = Artifact.sources(artifactID)
  val docsArtifact = Artifact.javadoc(artifactID)
  override def packageToPublishActions = super.packageToPublishActions ++ Seq(packageSrc, packageDocs)
  
  // publishing
  override def managedStyle = ManagedStyle.Maven
  val publishTo = Resolver.file("maven-local", new java.io.File(Path.userHome+"/.m2/repository"))

  override def pomExtra =
    <name>ScalaBeans</name> ++
    <description>Reflection toolkit for Scala</description> ++
    <url>http://scalastuff.org</url> ++
    <licenses>
      <license>
        <name>The Apache Software License, Version 2.0</name>
        <url>http://www.apache.org/licenses/LICENSE-2.0.txt</url>
        <distribution>repo</distribution>
      </license>
    </licenses> ++
    <scm>
      <connection>scm:git:git@github.com:scalastuff/scalabeans.git</connection>
      <url>https://github.com/scalastuff/scalabeans</url>
    </scm> ++
    <developers>
      <developer>
        <id>rditerwich</id>
        <name>Ruud Diterwich</name>
      </developer>
      <developer>
        <id>advorkovyy</id>
        <name>Alexander Dvorkovyy</name>
      </developer>
    </developers>

  override def pomIncludeRepository(repo: MavenRepository) = false

  override def pomPostProcess(node : Node) : Node = node match {
    case e : Elem =>
      val children = if ((e \ "groupId").text == "com.dyuproject.protostuff") e.child ++ <optional>true</optional>
      else if ((e \ "groupId").text == "org.scala-lang") e.child.filter(_.label!="scope") ++ <scope>provided</scope>
      else e.child.map(pomPostProcess)
      Elem(e.prefix, e.label, e.attributes, e.scope, children:_*)
    case xml => xml
  }
}
