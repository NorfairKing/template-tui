# This file is a nixpkgs overlay.
# Here we add our packages to whichever version of nixpkgs it is being laid over.
final: previous:
with final.lib;
with final.haskell.lib;
let
  # This is where we define our Haskell packages.
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
  # This attribute contains all packages in this repository.
  foobarPackages = {
    "foobar-tui" = foobarPkg "foobar-tui";
  };

  # This attribute puts them all together into one.
  foobarRelease =
    final.symlinkJoin {
      name = "foobar-release";
      paths = attrValues final.foobarPackages;
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
