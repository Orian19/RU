#!/bin/bash
ls -l $1 | grep "^d" | tr -s " " | cut -f9 -d" " | tr "\n" " " | sed 's/ $/\n/'
