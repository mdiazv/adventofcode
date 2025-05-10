#!/usr/bin/env bash
#
export JAVA_PROGRAM_ARGS=`echo "$@"`
mvn -q exec:java@Advent -Dexec.args="$JAVA_PROGRAM_ARGS"