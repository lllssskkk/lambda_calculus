#!/bin/bash -eu

cabal build

# Get the path to the binary
BINARY_PATH=$(cabal list-bin LetPoly-hs)

# Check that the binary path is valid
if [ ! -x "$BINARY_PATH" ]; then
  echo "Error: '$BINARY_PATH' is not executable"
  exit 1
fi

# Remove any existing file/symlink and recreate the symlink
rm -f  LetPoly-hs
ln -sf "$BINARY_PATH" LetPoly-hs
