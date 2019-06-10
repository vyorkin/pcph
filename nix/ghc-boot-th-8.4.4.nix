{ mkDerivation, base, stdenv }:
mkDerivation {
  pname = "ghc-boot-th";
  version = "8.4.4";
  sha256 = "b4f68ed6c7f984d36257a949c75e0b74efa5be62d41f05a14e7aafba71034313";
  libraryHaskellDepends = [ base ];
  doCheck = false;
  description = "Shared functionality between GHC and the @template-haskell@ library";
  license = stdenv.lib.licenses.bsd3;
}
