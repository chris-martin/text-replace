{ mkDerivation, base, filepath, stdenv, time, unix }:
mkDerivation {
  pname = "directory";
  version = "1.2.6.2";
  sha256 = "1q0kljhbj4fz30f0phyklfdkar0hgfal6iz7lyaq7714r90h91jc";
  revision = "1";
  editedCabalFile = "1433kfvy2d8x1bvp2r8snkniijgpzqxsvcd2g0h0dg29p9imbyd5";
  libraryHaskellDepends = [ base filepath time unix ];
  testHaskellDepends = [ base filepath time unix ];
  description = "Platform-agnostic library for filesystem operations";
  license = stdenv.lib.licenses.bsd3;
}
