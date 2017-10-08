#!/bin/bash

ROOT=$(cd $(dirname $0) && pwd)

### Java ###
cd $ROOT/java/fw
mvn package -Dmaven.test.skip=true
cd $ROOT/java/ai
mvn package -Dmaven.test.skip=true

### CL ###
#cd $ROOT/cl
#sbcl --eval '(load "src/base.lisp")' --eval '(load "src/fw.lisp")' --eval '(codecheck-fw:build-main)'
#sbcl --eval '(load "src/base.lisp")' --eval '(load "src/ai.lisp")' --eval '(codecheck-ai:build-main)'
