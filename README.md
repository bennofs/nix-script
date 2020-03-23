nix-script
==========

Nix-script allows you to define dependencies at the top of scripts which will then be used to create a nix-shell environment when run. This project is only useful if you're using the nix package manager.

Features:

  * Run scripts in an isolated environment with only the required dependencies
  * Load scripts in an interpreter in that environment

Installation
-------------

Clone the repository and run:

```
$ nix-env -if.
```

This will install the `nix-script` and `nix-scripti` files into your user profile.

Usage
-----

To use `nix-script`, you need to add a header to your file. Here is an example for a haskell file `example.hs`:

```haskell
#!/usr/bin/env nix-script
#!> haskell
#! env     | EDITOR
#! haskell | text lens optparse-applicative
#! shell   | nix nix-prefetch-scripts

{- ... code using text, lens and optparse-applicative -}

main :: IO ()
main = {- ... -}
```

The first line just tells the shell to use `nix-script` when executing the script. The next line is then read by `nix-script` to determine the language used for running the script. In this case, we tell `nix-script` that we want haskell, so it will use `runhaskell` to execute the script.

The next lines the specify dependencies of the script. The first entry on each line is the language of the following dependencies. This is required so that language-specific names can be converted to the correct nix attribute names. You should have one line per language. In our case, we say that we want to use the `text`, `lens` and `optparse-applicative` haskell packages. We also want that `nix` and `nix-prefetch-scripts` are available in $PATH (the `shell` language doesn't apply any renaming to their dependencies and just passes them through unmodified).

The lines starting with `env` specify additional environment variables to be kept in the environment where the script will run. In this case the variable `EDITOR`.

We can now mark the script executable and run it:

```
$ chmod +x ./example.hs
$ ./example.hs # This works even if you don't have ghc, text, lens or optparse-applicative installed
```

We can also load the script in `ghci`:

```
$ nix-scripti ./example.hs
```

or open a nix-shell with the requested packages:

```
$ nix-scripts ./example.hs
```

### Supported languges

|  Identifiers         | Language             |
|:--------------------:|:---------------------|
| `haskell`            | Haskell              |
| `python2`, `python3` | Python               |
| `perl`               | Perl 5               |
| `javascript`         | JavaScript (node.js) |
| `shell`              | Shell script (bash)  |

Anything else will be treated as "passthough", meaning the rest of
the line will be intpreted as nixpkgs attributes as-is.

Contributing
------------

If you want to add support for another language, or just have a good improvment you'd like to implement, feel free to fork the repository and then submit a pull request. You can find me on irc.freenode.org as bennofs in the #nixos channel if you have questions.
