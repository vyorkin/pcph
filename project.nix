{ mkDerivation, array, base, deepseq, parallel, stdenv, time, threadscope }:

mkDerivation {
  pname = "pcph";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ array base deepseq parallel time ];
  executableHaskellDepends = [ array base deepseq parallel time threadscope ];
  license = stdenv.lib.licenses.mit;
}
