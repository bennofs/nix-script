nix-bang
========

The Nix-bang script allows you to define dependencies at the top of scripts which will then be used to create a nix-shell environment when run. This project is only useful if you're using the nix package manager.

Installation
------------

Clone the repository and run:

```
$ nix-env -if.
```

This will install the `nix-bang` and `nix-bangi` files into your user profile.

Usage
-----

To use `nix-bang`, you need to add a header to your file. Here is an example for a haskell file:

```haskell
#!nix-bang
#!> haskell
#! haskell | text lens optparse-applicative
#! shell   | nix nix-prefetch-scripts
```

The first line just tells the shell to use `nix-bang` when executing the script. The next line is then read by `nix-bang` to determine the language used for running the script. In this case, we tell `nix-bang` that we want haskell, so it will use `runhaskell` to execute the script.

The next lines the specify dependencies of the script. The first entry on each line is the language of the following dependencies. This is required so that language-specific names can be converted to the correct nix attribute names. You should have one line per language. In our case, we say that we want to use the `text`, `lens` and `optparse-applicative` haskell packages. We also want that `nix` and `nix-prefetch-scripts` are available in $PATH (the `shell` language doesn't apply any renaming to their dependencies and just passes them through unmodified).

Contributing
------------

If you want to add support for another language, or just have a good improvment you'd like to implement, feel free to fork the repository and then submit a pull request. You can find me on irc.freenode.org as bennofs in the #nixos channel if you have questions.
