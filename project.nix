{ mkDerivation, array, base, bytestring, containers, deepseq
, http-client, http-conduit, monad-par, parallel, repa, stdenv
, time
}:
mkDerivation {
  pname = "pcph";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    array base bytestring containers deepseq http-client http-conduit
    monad-par parallel repa time
  ];
  executableHaskellDepends = [
    array base bytestring containers deepseq http-client http-conduit
    monad-par parallel repa time
  ];
  license = stdenv.lib.licenses.mit;
}
