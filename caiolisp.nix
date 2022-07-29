{ mkDerivation, base, containers, haskeline, hspec, lib, megaparsec
, mtl
}:
mkDerivation {
  pname = "caiolisp";
  version = "0.0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base containers megaparsec mtl ];
  executableHaskellDepends = [
    base containers haskeline megaparsec mtl
  ];
  testHaskellDepends = [ base containers hspec megaparsec mtl ];
  homepage = "https://github.com/caiorulli/caiolisp#readme";
  license = lib.licenses.gpl3Plus;
}
