#!/bin/bash

prev=`ls closed open | sort -n | tail -n 1`
next=$(($prev + 1))
title=$1
fn="open/$next"
touch $fn
echo Title: $title > $fn
echo >> $fn
${FCEDIT:-${VISUAL:-${EDITOR:-vi}}} $fn
hg add $fn
