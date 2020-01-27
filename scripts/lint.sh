#!/usr/bin/env bash

weeder . --build
hlint .
brittany --check-mode **/*.hs
