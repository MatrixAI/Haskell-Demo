GraphQL Demo
===============

This is a demo of using GraphQL with a Haskell API backend. In this case we use Yesod. Or yesod wai to make it all work. We may not be able to use yesod if it doesn't cleanly integrate. Alternatively something lighterweight like soctty would be better.

Note the use of transient haskell as well to do weird things.

Note the use of warp with WAI.

So we are using shell.nix to create the development environment. We don't use the stack nix integration, since we just want stack to create the necessary GHC environment. In fact does it even make sense to use haskellPackages.ghc at all? I don't think so, since we are using stack to essentially build GHC for ourselves. We just need to try it.

Ok this nix-shell brought in stack is not working. I think we need to rethink this as a sort of nix shell bringing in stack but stack still uses nix maybe?

What the hell is `stack --nix build`.

So there's a perception that you use `stack` inside the user profile and use its integration in into nix outside the nix shell, but that's dumb, I want to use stack inside a nix shell and make sure it works. So why doesn't it work!

Before it uses `haskell.lib.buildStackProject`. And it gets stack and ghc inside. 

So why doesn't work!?

Hence all the commands like `stack --nix build`. Which is dumb cause.

Ok so I can now use `stack new` finally by upgrading my commit hash to the latest nixpkgs version completely. This one uses haskell.lib.stack.buildStackProject. However I want to try with mkDerivation instead.

Note that even while using `stack` inside nix-shell, it still looks for `~/.stack`. And it stores built artifacts there. This allows one to share stack things between different projects. But with `nix-shell` this sharing might be a bad idea, especially with multiple stack version being used. Instead I should isolate each stack individually, cause different stack versions may hurt each other. This is because stack tries to create a situation where it's the one handling the different GHC versions and stuff, but I just want an environment with stack. The only way this would make sense is if stack were something you install in your dev environment. That way you only have 1 stack for your entire user profile.

Ok `stack new` always creates a new project in a subdirectory. I want to create stack new in the current directory.

Also `stack init` is used for existing projects. So it doesn't use a template. Instead we write everything ourselves. But it only works if you have an existing cabal file or package.yaml.

Suppose you could use `STACK_ROOT` to point to a hidden directory for it to work.

The `stack new` doesn't setup the compiler and its GHC packages. Instead it just sets up the LTS package index. This just means an index of packages that work together.

`STACK_ROOT` by default is `~/.stack`, and `STACK_WORK` is by default `.stack-work`. So the work is actually within your normal directory. Ok I get it now.

I think for now we shall experiment with a global `STACK_ROOT`, it shouldn't be a problem for most cases. Since stack should be compatible from one area to another, and the point is that it shares compiler versions, things that may be shared between the different environments.

However that still doesn't solve the stack new.

`stack init` or `stack new` will try to default to the current Haskell LTS present on the stackage snapshots. With nix if you inherit ghc, you'd need stack to use system ghc. But if you really want stack, then you would use stack's selection of GHC, hence you wouldn't specify GHC in your shell.nix. However this is still pretty weird. Since now the stack version inside can actually use a totally different GHC version. But that's only on stack init or stack new, since you would have specified the GHC version via your LTS resolver in your stack.yaml file.

For initialisation, one can instead pass `--prefer-lts` or `--prefer-nightly`, which will choose the latest LTS or nightly versions, or one could directly use `--resolver` to pick the specific snapshot name.

So I think I should use init by first creating a cabal file.

In some cases, you may still need `cabal-install`, since not all packages are inside stackage, some are still left inside hackage. That's what you need `cabal-install` for.

Oh... you can't actually build GHC inside nix-shell. You have to have ghc inherited. And as long as that's on path, stack will work fine.

Then you have to run commands with `stack --nix setup` and things like that.

Note the usage of `buildStackProject` wraps `mkDerivation` and adds all these features: https://github.com/NixOS/nixpkgs/blob/master/pkgs/development/haskell-modules/generic-stack-builder.nix

I get it, we just need a shellHook. Now this prevents us needing to always run everything under nix in our stack.yaml.

Since you are using `buildStackProject`, there's no need to even have `stack` put into the buildInputs, since it already has it. It also says it has ghc already, so it appears we don't even need to inherit ghc I think. I think it should work without inheriting ghc. Although `cabal-install` should still need to be brought in explicitly if necessary.

Finally I have the minimal working environment. Ok finally `buildStackProject` should have made sure that `stack` is always running with `--nix` anyway. So this ensures that stack only uses the ghc that nix is providing and that's it. Boom, now let's actually do the `stack init`. But we need to an example thing first.
