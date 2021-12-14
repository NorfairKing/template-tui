# This file is a nixpkgs overlay.
# Here we add our packages to whichever version of nixpkgs it is being laid over.
final: previous:
with final.lib;
with final.haskell.lib;
{
  # This attribute contains all packages in this repository.
  fooBarPackages =
    let
      fooBarPkg =
        name:
        addBuildDepend
          (
            buildStrictly (
              disableLibraryProfiling (
                # I turn off library profiling because it slows down the build.
                final.haskellPackages.callCabal2nixWithOptions name (final.gitignoreSource (../. + "/${name}")) "--no-hpack" { }
              )
            )
          )
          (final.haskellPackages.autoexporter);
      fooBarPkgWithComp =
        exeName: name:
        generateOptparseApplicativeCompletion exeName (fooBarPkg name);
      fooBarPkgWithOwnComp = name: fooBarPkgWithComp name name;
    in
    {
      "foo-bar-tui" = fooBarPkgWithOwnComp "foo-bar-tui";
    };

  # This attribute puts them all together into one.
  fooBarRelease =
    final.symlinkJoin {
      name = "fooBar-release";
      paths = builtins.map justStaticExecutables (attrValues final.fooBarPackages);
    };

  fooBarCasts =
    let
      mkCastDerivation = import
        (sources.autorecorder + "/nix/cast.nix"
        )
        { pkgs = final // final.fooBarPackages; };
    in
    {
      fooBar-basics-cast = mkCastDerivation {
        name = "foo-bar-basics-cast";
        src = ../casts/basics.yaml;
        debug = false;
      };
    };


  # This is where we specify specific haskell package versions.
  # These need to match the `extra-deps` part of `stack.yaml` for reproducibility.
  haskellPackages =
    previous.haskellPackages.override (
      old:
      {
        overrides =
          final.lib.composeExtensions
            (
              old.overrides or (
                _:
                _:
                { }
              )
            )
            (
              self: super:
                final.fooBarPackages
            );
      }
    );
}
