final: prev:
with final.lib;
with final.haskell.lib;
{
  fooBarRelease =
    final.symlinkJoin {
      name = "foo-bar-release";
      paths = final.lib.attrValues final.fooBarReleasePackages;
    };

  fooBarReleasePackages =
    let
      enableStatic = pkg:
        overrideCabal pkg
          (old: {
            configureFlags = (old.configureFlags or [ ]) ++ optionals final.stdenv.hostPlatform.isMusl [
              "--ghc-option=-optl=-static"
              # Static
              "--extra-lib-dirs=${final.gmp6.override { withStatic = true; }}/lib"
              "--extra-lib-dirs=${final.libffi.overrideAttrs (old: { dontDisableStatic = true; })}/lib"
              "--extra-lib-dirs=${final.ncurses.override { enableStatic = true; }}/lib" # for -ltinfo
            ];
            enableSharedExecutables = !final.stdenv.hostPlatform.isMusl;
            enableSharedLibraries = !final.stdenv.hostPlatform.isMusl;
          });
    in
    builtins.mapAttrs
      (_: pkg: justStaticExecutables (enableStatic pkg))
      final.haskellPackages.fooBarPackages;

  haskellPackages = prev.haskellPackages.override (old: {
    overrides = final.lib.composeExtensions (old.overrides or (_: _: { }))
      (
        self: super:
          let
            fooBarPkg = name:
              buildFromSdist (overrideCabal (self.callPackage (../${name}/default.nix) { }) (old: {
                configureFlags = (old.configureFlags or [ ]) ++ [
                  # Optimisations
                  "--ghc-options=-O2"
                  # Extra warnings
                  "--ghc-options=-Wall"
                  "--ghc-options=-Wincomplete-uni-patterns"
                  "--ghc-options=-Wincomplete-record-updates"
                  "--ghc-options=-Wpartial-fields"
                  "--ghc-options=-Widentities"
                  "--ghc-options=-Wredundant-constraints"
                  "--ghc-options=-Wcpp-undef"
                  "--ghc-options=-Werror"
                ];
                doBenchmark = true;
                doHaddock = false;
                doCoverage = false;
                doHoogle = false;
                doCheck = false; # Only for coverage
                hyperlinkSource = false;
                enableLibraryProfiling = false;
                enableExecutableProfiling = false;
                buildDepends = (old.buildDepends or [ ]) ++ (with final; [
                  haskellPackages.autoexporter
                ]);
                # Ugly hack because we can't just add flags to the 'test' invocation.
                # Show test output as we go, instead of all at once afterwards.
                testTarget = (old.testTarget or "") + " --show-details=direct";
              }));
            fooBarPkgWithComp =
              exeName: name:
              self.generateOptparseApplicativeCompletions [ exeName ] (fooBarPkg name);
            fooBarPkgWithOwnComp = name: fooBarPkgWithComp name name;

            fooBarPackages = {
              foo-bar-tui = fooBarPkg "foo-bar-tui";
            };
          in
          {
            inherit fooBarPackages;
          } // fooBarPackages
      );
  });
}
