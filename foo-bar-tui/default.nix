{ mkDerivation, async, autodocodec, autodocodec-yaml, autoexporter
, base, brick, cursor, directory, envparse, lib, mtl
, optparse-applicative, path, path-io, text, vty, yaml
}:
mkDerivation {
  pname = "foo-bar-tui";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    async autodocodec autodocodec-yaml base brick cursor directory
    envparse mtl optparse-applicative path path-io text vty yaml
  ];
  libraryToolDepends = [ autoexporter ];
  executableHaskellDepends = [ base ];
  license = lib.licenses.unfree;
  hydraPlatforms = lib.platforms.none;
  mainProgram = "foo-bar-tui";
}
