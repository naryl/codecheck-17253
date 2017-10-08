#!/bin/bash

ROOT=$(cd $(dirname $0) && pwd)

### Java ###
java -jar $(ls $ROOT/java/ai/target/exam2a-ai-*.jar) "$@"

### CL ###
#$ROOT/cl/exam2a-ai "$@"
