#!/bin/sh

scalac -d out src/PickledVisualizer.scala
scala -classpath out:$1 PickledVisualizer $2
