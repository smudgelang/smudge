#!/bin/bash

VERSION=`$1 --version | cut -f 3 -d ' '`
tar -czf dist/smudge-$VERSION.tgz dist/release
