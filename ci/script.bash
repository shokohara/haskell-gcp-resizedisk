#!/bin/bash
export PATH=$HOME/.local/bin:$PATH
script_dir=$(cd $(dirname ${BASH_SOURCE:-$0}); pwd)/..
echo $script_dir
stack --no-terminal docker pull
stack --no-terminal --install-ghc clean
stack --no-terminal --install-ghc test --no-haddock-deps
mkdir -p resizedisk_linux_amd64
cp `stack path --docker --local-install-root`/bin/* $_
tar -zcvf $_.tar.gz $_

