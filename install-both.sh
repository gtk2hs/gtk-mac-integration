#!/bin/sh -ex

cabal clean
mv gtk-mac-integration.cabal-renamed gtk-mac-integration.cabal || true
mv gtk3-mac-integration.cabal gtk3-mac-integration.cabal-renamed || true
cabal-src-install "$@"

cabal clean
mv gtk3-mac-integration.cabal-renamed gtk3-mac-integration.cabal || true
mv gtk-mac-integration.cabal gtk-mac-integration.cabal-renamed || true
cabal-src-install "$@"

