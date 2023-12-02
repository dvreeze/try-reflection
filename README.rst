==============
Try-reflection
==============

Even when inspecting Scala source code with meta-programming libraries like scalameta_, it may make sense to
use mere Java reflection for dependencies on the classpath. This project toys with that idea and Java reflection.

Java reflection offers no introspection into method implementations etc., but it requires no more than a classpath to
make use of it.

.. _scalameta: https://scalameta.org/
