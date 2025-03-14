#!/usr/bin/env bash
set -xeu

pname=tls-sslkeylogfile

version=$(perl -ne '/^version:\s+([^\s]+)$/ && print "$1\n"' ${pname}.cabal)
git fetch origin

# Ensure we are on origin/master
[[ $(git rev-parse origin/main) == $(git rev-parse HEAD) ]] || (echo "not up to date"; exit 1)

# Verify Nix
nix flake check -Lv

cabal sdist

sdist="dist-newstyle/sdist/${pname}-${version}.tar.gz"
tar tf "$sdist"

echo -e "\n\nWould you like to release this sdist as $version? Enter the version number if so."
read resp

[[ "$resp" == "$version" ]]

cabal upload "$sdist"
git tag "v${version}"
git push origin "v${version}"
