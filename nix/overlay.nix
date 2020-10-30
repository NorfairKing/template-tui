# This file is a nixpkgs overlay.
# Here we add our packages to whichever version of nixpkgs it is being laid over.
final: previous:
with final.lib;
with final.haskell.lib;
{
  # This attribute contains all packages in this repository.
  foobarPackages =
    let
      foobarPkg =
        name:
          dontHaddock (
            doBenchmark (
              addBuildDepend (
                failOnAllWarnings (
                  disableLibraryProfiling (
                    # I turn off library profiling because it slows down the build.
                    final.haskellPackages.callCabal2nix name (final.gitignoreSource (../. + "/${name}")) {}
                  )
                )
              ) (final.haskellPackages.autoexporter)
            )
          );
    in
      {
        "foobar-tui" = foobarPkg "foobar-tui";
      };

  # This attribute puts them all together into one.
  foobarRelease =
    final.symlinkJoin {
      name = "foobar-release";
      paths = attrValues final.foobarPackages;
    };

  foobarCasts =
    let
      mkCastDerivation = import (
        builtins.fetchGit {
          url = "https://github.com/NorfairKing/autorecorder";
          rev = "da5bf9d61108a4a89addc8203b1579a364ce8c01";
          ref = "master";
        } + "/nix/cast.nix"
      ) { pkgs = final // final.foobarPackages; };
    in
      {
        foobar-basics-cast = mkCastDerivation {
          name = "foobar-basics-cast";
          src = ../casts/basics.yaml;
          debug = true;
        };
      };


  # This is where we specify specific haskell package versions.
  # These need to match the `extra-deps` part of `stack.yaml` for reproducibility.
  haskellPackages =
    previous.haskellPackages.override (
      old:
        {
          overrides =
            final.lib.composeExtensions (
              old.overrides or (
                _:
                _:
                  {}
              )
            ) (
              self: super:
                final.foobarPackages
            );
        }
    );
}
