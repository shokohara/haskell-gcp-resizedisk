#!/bin/sh -eux
sudo apt-get update
sudo apt-get install -y git

curl -sSL https://get.haskellstack.org/ | sh

git clone --depth=1 https://github.com/shokohara/haskell-gcp-resizedisk.git
cd haskell-gcp-resizedisk
stack --install-ghc install
echo "$(stack path --local-bin)/resizedisk"
