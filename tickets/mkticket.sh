#!/bin/bash

prev=`ls closed open | sort -n | tail -n 1 | cut -f 1 -d ' '`
next=$(($prev + 1))
fn="$next - $1"
echo $fn
touch "open/$fn"
${FCEDIT:-${VISUAL:-${EDITOR:-vi}}} "open/$fn"
hg add "open/$fn"
