{ mkDerivation, base, stdenv }:
mkDerivation {
  pname = "ansi-terminal";
  version = "0.6.3";
  sha256 = "da3c4eab720a1db0accd2b533666cd9756aa478d33d0b9fc10f015c9102c3ad5";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base ];
  executableHaskellDepends = [ base ];
  homepage = "https://github.com/feuerbach/ansi-terminal";
  description = "Simple ANSI terminal support, with Windows compatibility";
  license = stdenv.lib.licenses.bsd3;
}
