# Haskell Demo

This is an example Haskell project using Nix to setup a development environment.

This uses Nix and Cabal without Stack. This is because when using Nix, you don't need Stack, as Nix provides harmonious snapshots of Haskell packages. However Stack users can still develop on this project, they just have to generate an appropriate `stack.yaml` from the Cabal file.

The first step is that we have to acquire `cabal2nix`, which we use to generate a `cabal.nix` file from the `package.yaml`. Note that the usage of `package.yaml` means we are using the [hpack format](https://github.com/sol/hpack). This format is transformed to a cabal file via the `hpack` command.

```sh
nix-shell -p cabal2nix
# using --hpack ensures that we always use package.yaml
cabal2nix --hpack . >./cabal.nix
```

The above command is also executed at the beginning of the `shellHook`.

This `cabal.nix` will be imported by the `default.nix` to be used as the core derivation. Unlike other `*2nix` tools, this still retains package sharing, because the generated `cabal.nix` expects the current package's dependencies to be passed down from a higher level package set.

If this is the first time you've ran `cabal`, then run `cabal update` to get the latest package list in `~/.cabal`.

## Installation

If on Nix, you can install just by using:

```sh
nix-env -f ./default.nix -i
```

If you are not, then use `cabal`:

```sh
# you need to first generate the cabal file
hpack
cabal install
```

It installs the executable into `~/.cabal/bin`.

## Developing

Run `nix-shell`, and once you're inside, you can use:

```sh
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

```sh
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

Once you have finished developing, you can build the package using:

```sh
nix-build
```

## Using the `package.yaml`

Any module that is meant to be consumed as a library needs to be listed in the `exposed-modules`. Any module that is not listed there are considered to be private modules.

The `package.yaml` is also where you list new Haskell packages. For example if you want to get the `algebraic-graphs` library, you basically use:

```yaml
dependencies:
- base >= 4.7 && < 5
- algebraic-graphs >= 0.2 && < 0.3
```

Then you use `cabal2nix` again and you re-enter the shell.

Note that Haskell dependency constraints and versions when using `cabal2nix` is not determined by your `package.yaml`, but instead by the Nixpkgs hash located in `pkgs.nix`.

Remember that Haskell package versions conventionally use `Major.Major.Minor.Patch`. For more information see: https://pvp.haskell.org/

## Using GHCi (or `cabal repl` or `stack ghci`)

The `cabal repl` only works against the build targets specified in the `package.yaml`. You have to specify the target name:

```sh
# targets the library
cabal repl haskell-demo
# targets the executable (which depends on the library)
cabal repl haskell-demo-exe
# targets the tests (which depends on the library)
cabal repl haskell-demo-test
```

However you need to understand how modules work in GHCi to use the REPL well. The documentation here explains the necessary commands: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/ghci.html#what-s-really-in-scope-at-the-prompt

Basically remember these:

```
:browse
:show modules
:show imports
:module ModuleName
```

---

Because Haskell is a compiled language, most building tools are `nativeBuildInputs`. However for the `shell.nix` this distinction doesn't matter, because it just puts you into an environment that has all the dependencies.

Note that if you want to create a quick and dirty `nix-shell` with GHC and a few packages, just use:

```sh
nix-shell -p 'ghc.ghcWithPackages (pkgs: [ pkgs.aeson pkgs.dlist ])'
# or if you want to specify a version
nix-shell -p 'haskell.packages.ghc843.ghcWithPackages (pkgs: [ pkgs.aeson pkgs.dlist ])'
```
