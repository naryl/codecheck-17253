#!/bin/bash

ROOT=$(cd $(dirname $0) && pwd)

### Java ###
java -jar $(ls $ROOT/java/fw/target/exam2a-framework-*.jar) "$@"

### CL ###
#$ROOT/cl/exam2a-fw "$@"
