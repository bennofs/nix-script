#!/usr/bin/env nix-script
#!> haskell
#! haskell | text lens optparse-applicative
#! shell | nix nix-prefetch-scripts

import Control.Lens

main :: IO ()
main = do
  putStrLn "It works!"
