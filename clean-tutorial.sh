#!/bin/bash

cd dist/doc
for d in `find . -type d | grep /` ; do pushd $d ; make clean ; popd ; done
