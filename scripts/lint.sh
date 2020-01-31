#!/usr/bin/env bash

weeder . --build
hlint .
brittany --check-mode **/*.hs && echo "brittany OK" || echo "brittany failed" && exit 1
