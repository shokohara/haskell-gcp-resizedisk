#!/bin/sh
sudo apt-get update
sudo apt-get install -y haskell-stack git
git clone --depth=1 https://github.com/shokohara/haskell-gcp-resizedisk.git
cd haskell-gcp-resizedisk
stack --install-ghc install
echo "$(stack path --local-bin)/resizedisk"
