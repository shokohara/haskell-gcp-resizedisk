[![Build Status](https://travis-ci.org/shokohara/haskell-gcp-resizedisk.svg?branch=master)](https://travis-ci.org/shokohara/haskell-gcp-resizedisk)

# haskell-gcp-resizedisk

```
curl -fsSL "$(curl -s https://api.github.com/repos/shokohara/haskell-gcp-diskresize/releases/latest | jq --raw-output '.assets[0] | .browser_download_url')"| tar xz
```

```
curl -fsSL "https:$(curl -sL https://api.github.com/repos/shokohara/haskell-gcp-resizedisk/releases/latest | egrep '/.*/.*/.*tar.gz' -o`)" | tar xz
```
