#!/bin/bash

VERSION=`$1 --version | cut -f 3 -d ' ' | head -n 1`
tar -czf dist/smudge-$VERSION-$2.tgz dist/release
