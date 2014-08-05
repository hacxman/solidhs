#!/bin/bash
cabal build && cabal test && cat dist/test/*.log
