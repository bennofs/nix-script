#!/usr/bin/env nix-script
#!>zsh
#! nix | zsh

function a { echo "this is zsh!" }
a
echo "your args: $@"