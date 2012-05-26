Introduction
============

ScalaBeans is a reflection library for Scala. Its aims are similar to JavaBeans, but scala-specific features have been added:

* Recognizes scala properties
* Can handle immutable and case classes
* Scala collections are supported, including builders
* Preserve generic type information
* Type pattern matching (including generic type arguments)
* High performance serialization to/from protobuf, json, xml

Further information:

- Documentation: [http://scalabeans.googlecode.com/](http://scalabeans.googlecode.com/)
- User group:    [http://groups.google.com/group/scalastuff](groups.google.com/group/scalastuff)
- Issues:        [http://code.google.com/p/scalabeans/issues/list](http://code.google.com/p/scalabeans/issues/list)

Getting started
===============
	
If you use SBT, add following lines to your project:	

    val scalaStuffRepo = "Sonatype OSS releases" at "http://oss.sonatype.org/content/repositories/releases"
    val scalabeans = "org.scalastuff" % "scalabeans" % "0.2"


If you use Maven, add following lines to your pom.xml:

    <dependencies>
    ...
      <dependency>
        <groupId>org.scalastuff</groupId>
        <artifactId>scalabeans</artifactId>
        <version>0.2</version>
      </dependency>
    </dependencies>
    
For Scala 2.8.x use scalabeans_2.8.1 artifactId