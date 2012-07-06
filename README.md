Introduction
============

ScalaBeans is a reflection library for Scala. Reflection is used to build immutable Metamodel object for a given type.
Metamodel contains metainformation about underlying data structure (like bean property names, types, bean constructors,
collection builders etc) and can be used to introspect it. 

Metamodel can be recursively rewritten using provided rules to alter visible metamodel (underlying object stays the same). 

Metamodel can also be used to construct a Format or a Converter for given data structure.

Metamodel features:
* Recognizes scala properties
* Can handle immutable and case classes
* Scala collections are supported, including builders
* Generic type information is preserved
* Type pattern matching (including generic type arguments)

Important rewrites (see API for complete list):
* rename bean properties
* add/hide bean properties
* inject bean factory methods
* type conversion

Supported Formats:
* Protobuf, protostuff (using protostuff)
* JSON (using jackson)
* planned: smile

Supported Converters (planned):
* DbObject (mongodb)
* JsValue (playframework)
* Jodatime (can be injected into other Formats and Converters using Metamodel rewrites)


Further information:

- Documentation: [http://scalabeans.googlecode.com/](http://scalabeans.googlecode.com/)
- User group:    [http://groups.google.com/group/scalastuff](groups.google.com/group/scalastuff)
- Issues:        [http://code.google.com/p/scalabeans/issues/list](http://code.google.com/p/scalabeans/issues/list)

Getting started
===============

If you use Maven, add following lines to your pom.xml:

    <dependencies>
    ...
      <dependency>
        <groupId>org.scalastuff</groupId>
        <artifactId>scalabeans</artifactId>
        <version>0.9-SNAPSHOT</version>
      </dependency>
    </dependencies>

    <repositories>
    ...

      <repository>
        <id>Sonatype OSS snapshots</id>
        <url>http://oss.sonatype.org/content/repositories/snapshots</url>    
      </repository>
    </repositories>