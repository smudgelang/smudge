#!/bin/bash

VERSION=`$3 --version | cut -f 3 -d ' ' | head -n 1`
cd $1
tar -czf smudge-$VERSION-$4.tgz $2
