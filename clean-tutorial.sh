#!/bin/bash

cd docs/tutorial/
for d in `find . -type d | grep /` ; do pushd $d ; make clean ; popd ; done
