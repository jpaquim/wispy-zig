#!/usr/bin/env bash

set -ex

git submodule update --init

BINARYEN=$PWD/binaryen-zig
FIZZY=$PWD/fizzy-zig

pushd $BINARYEN
  git submodule update --init
  ./build_binaryen.sh
popd

pushd $FIZZY
  git submodule update --init
  ./build_fizzy.sh
popd
