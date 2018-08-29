# Haskell Demo

This is an example Haskell project using Nix to setup a development environment.

This uses Nix and Cabal without Stack. This is because when using Nix, you don't need Stack, as Nix provides harmonious snapshots of Haskell packages. However Stack users can still develop on this project, they just have to generate an appropriate `stack.yaml` from the Cabal file.

The first step is that we have to acquire `cabal2nix`, which we use to generate a `cabal.nix` file from the `package.yaml`. Note that the usage of `package.yaml` means we are using the [hpack format](https://github.com/sol/hpack). This format is transformed to a cabal file via the `hpack` command.

```sh
# using --hpack ensures that we always use package.yaml
cabal2nix --hpack . >./cabal.nix
```

The above command is also executed at the beginning of the `shellHook`.

This `cabal.nix` will be imported by the `default.nix` to be used as the core derivation. Unlike other `*2nix` tools, this still retains package sharing, because the generated `cabal.nix` expects the current package's dependencies to be passed down from a higher level package set.

Run `nix-shell`, and once you're inside, you can use:

```
# check the GHC version
ghc --version
# show all the packages that is registed with GHC
ghc-pkg list
# launch the repl
ghci
```

The `cabal-install` package installs the `cabal` command. This command and associated protocols is what defines what a Haskell package is. So even if you are using Stack, you are still creating Cabal packages. Note that `cabal` configuration file is a `~/.cabal/config`.

To use `cabal`, you need to generate the cabal file from the `package.yaml`. You can do this by running `hpack`. However this is also executed as part of the `shellHook`.

At this point, you need to run `cabal configure`. This will create a `dist` directory that will contain any build artifacts. This is also executed as part of the `shellHook`.

It's important to read the guide for Cabal as this is information relevant to the Haskell ecosystem: https://www.haskell.org/cabal/users-guide/developing-packages.html

The most important commands are:

```
# this will launch GHCI for a given target
cabal repl
# this will build the executable and library and put them into ./dist
cabal build
# this will run the executable (you can pass the name of the executable)
cabal run
# this will run the tests
cabal test
# deletes ./dist
cabal clean
# this will install the executable into the ~/.cabal/bin
cabal install
```

---

Because Haskell is a compiled language, most building tools are `nativeBuildInputs`. However for the `shell.nix` this distinction doesn't matter, because it just puts you into an environment that has all the dependencies.

Note that if you want to create a quick and dirty `nix-shell` with GHC and a few packages, just use:

```nix
haskell.packages.ghc822.ghcWithPackages (pkgs: [ pkgs.aeson pkgs.dlist ])
```
