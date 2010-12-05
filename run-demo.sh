#!/bin/sh

scalac -d out src/PickledVisualizer.scala
scala -classpath out PickledVisualizer
