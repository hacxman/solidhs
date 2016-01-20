#!/bin/bash
CA=~/.cabal/bin/cabal
$CA build && $CA test && cat dist/test/*.log
