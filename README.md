# Haskell Demo

[![pipeline status](https://gitlab.com/MatrixAI/open-source/Haskell-Demo/badges/master/pipeline.svg)](https://gitlab.com/MatrixAI/open-source/Haskell-Demo/commits/master)

This is an example Haskell project using Nix to setup a development environment.

This uses Nix and Cabal without Stack. This is because when using Nix, you don't need Stack, as Nix provides harmonious snapshots of Haskell packages.

Stack users can still develop on this project, they just have to generate an appropriate `stack.yaml` from the `haskell-demo.cabal` file.

The first step is that we have to acquire `cabal2nix`, which we use to generate a `cabal.nix` file from the `package.yaml`.

We also write a custom `default.nix` then imports the `cabal.nix` and adds extra custom build steps like encoding environment variables.

Both the `cabal.nix` and `default.nix` remain as Haskell `callPackage` derivations.

This means the function parameter names may conflict with non-Haskell package names.

If you get a conflict, make sure to do explicit overrides when using `callPackage`.

Note that the usage of `package.yaml` means we are using the [hpack format](https://github.com/sol/hpack). This format is transformed to a cabal file via the `hpack` command.

```sh
nix-shell -p cabal2nix
# using --hpack ensures that we always use package.yaml
cabal2nix --hpack . >./cabal.nix
```

The above command is also executed at the beginning of the `shellHook`.

If this is the first time you've ran `cabal`, then run `cabal update` to get the latest package list in `~/.cabal`.

## Installation

Building the package:

```
nix-build -E '(import ./pkgs.nix).haskellPackages.callPackage ./default.nix {}'
```

Building the releases:

```
nix-build --attr application
nix-build --attr applicationStrict
nix-build --attr docker
```

Install into Nix user profile:

```
nix-env -f ./release.nix --install --attr application
```

Install into Docker:

```
docker load --input "$(nix-build ./release.nix --attr docker)"
```

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

At this point, you need to run `cabal v2-configure`. This will create a `dist` directory that will contain any build artifacts. This is also executed as part of the `shellHook`.

It's important to read the guide for Cabal as this is information relevant to the Haskell ecosystem: https://www.haskell.org/cabal/users-guide/developing-packages.html

The most important commands are:

```sh
# this will launch GHCI for a given target
cabal v2-repl
# this will build the executable and library and put them into ./dist
cabal v2-build
# this will run the executable (you can pass the name of the executable)
cabal v2-run
# this will run the tests
cabal v2-test
# deletes ./dist
cabal v2-clean
# this will install the executable into the ~/.cabal/bin
cabal v2-install
```

Once you have finished developing, you can build the package using:

```sh
nix-build
```

Note that if you want to create a quick and dirty `nix-shell` with GHC and a few packages, just use:

```sh
nix-shell -p 'ghc.ghcWithPackages (pkgs: [ pkgs.aeson pkgs.dlist ])'
# or if you want to specify a version
nix-shell -p 'haskell.packages.ghc865.ghcWithPackages (pkgs: [ pkgs.aeson pkgs.dlist ])'
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

## Non-Haskell Dependencies

For non-Haskell dependencies that are CLI executables, add them to:

```yaml
system-build-tools:
- hello
```

They will be available during `nix-build` and `nix-shell`.

For them to be available for the output, you must use `makeWrapper`.

For non-Haskell dependencies that are linkable libraries, add them to:

```yaml
extra-libraries:
- mnl
- z
```

The name of these libraries is the suffix of the `gcc` linking option:

* `-lmnl` giving you `mnl`
* `-lz` giving you `z` for the zlib library.

They will be will available to `nix-build` and `nix-shell`. The `cabal configure` will automatically find them and link them during compilation.

These non-Haskell dependencies must be explicitly named when using `callPackage` so that they refer to the C libraries and not Haskell packages of the same name.

This is done in both `shell.nix` and `release.nix`.

For example:

```nix
haskellPackages.callPackage ./cabal.nix { hello = pkgs.hello; mnl = pkgs.libmnl; z = pkgs.zlib; }
```

## Using GHCi (or `cabal repl` or `stack ghci`)

The `cabal v2-repl` only works against the build targets specified in the `package.yaml`. You have to specify the target name:

```sh
# targets the library
cabal v2-repl haskell-demo
# targets the executable (which depends on the library)
cabal v2-repl haskell-demo-exe
# targets the tests (which depends on the library)
cabal v2-repl haskell-demo-test
```

However you need to understand how modules work in GHCi to use the REPL well. The documentation here explains the necessary commands: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/ghci.html#what-s-really-in-scope-at-the-prompt

Basically remember these:

```
:browse
:show modules
:show imports
:module ModuleName
```

The `cabal v2-repl target` command can be too heavy weight especially if you're modifying just 1 module in the entire codebase. And it can be problematic when you are breaking type signatures.

So instead use `ghci ModuleA/ModuleB` to load up just that module and its dependencies.

## FFI

This project also demonstrates how to use the FFI. C source files are located in `csrc`, while the C headers are in `include`.

The relevant attributes of `package.yaml` are `c-sources`, `include-dirs` and `install-includes`.

The `c-sources` be a list of C files that need to be compiled into objects that are linked during compilation.

The `include-dirs` is a list of directories containing C headers to be included. In this case, we have only pointed to `include` because we are only using standard library headers and our own headers. But you can also point to system directories using absolute paths.

The `install-includes` will ensure that these headers (relative to the include-dirs) are also exported to any downstream package that depends on this package. So they can make use of those same headers, if they were also writing their own C code.

Finally you just need to write code like `FFI.hs`, and everything just works normally.

## Quality Assurance

### Hlint

Use `hlint` to lint your Haskell code which can suggest better ways of writing Haskell expressions.

```sh
hlint lint ./src ./app ./test
```

Hlint will give suggestions that aren't always relevant. In order to ignore these suggestions, they must be recorded like:

```sh
hlint lint ./src ./app ./test --default > ./.hlint.yaml
```

### Brittany

Use `brittany` to automatically format your code.

```sh
find ./src ./app ./test -type f -name '*.hs' -print0 | xargs -0 -I{} sh -c 'brittany --check-mode "{}" || echo "{}"; exit 1'
```

In order to apply the formatting to each file:

```sh
brittany --write-mode inplace ./src/Demo.hs ./app/library/Main.hs
```

Then use `git diff` to find the actual difference.

## PostgreSQL Integration

There is an orphaned branch [postgres](https://github.com/MatrixAI/Haskell-Demo/tree/postgres) that shows how to integrate PostgreSQL into a Haskell project.

It contains a `scripts` directory that contains executable scripts that initializes and starts and runs a local database. It also contains scripts that allow migration.

```
database-clean
database-destroy
database-init
database-migrate
database-start
database-status
database-stop
```

We bring in `postgresql` and `flyway` as additional dependencies into `shell.nix`.

Then the migration files are put into `migrations` directory.

See flyway documentation and postgresql documentation for more details.
