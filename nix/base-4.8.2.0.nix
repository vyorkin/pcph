{ mkDerivation, ghc-prim, rts, stdenv }:
mkDerivation {
  pname = "base";
  version = "4.8.2.0";
  sha256 = "f2bc9eb2773f74c231a25f32dc3b47b704cccc6b9064b6e1140dded364fafe8c";
  libraryHaskellDepends = [ ghc-prim rts ];
  doCheck = false;
  description = "Basic libraries";
  license = stdenv.lib.licenses.bsd3;
}
