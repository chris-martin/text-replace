{ mkDerivation, base, deepseq, directory, filepath, stdenv, unix }:
mkDerivation {
  pname = "process";
  version = "1.4.2.0";
  sha256 = "1v1bav5isqxq9fc4lw714x94qbfsjbm2nn12kjp69r1ql8jaaaqw";
  libraryHaskellDepends = [ base deepseq directory filepath unix ];
  testHaskellDepends = [ base ];
  description = "Process libraries";
  license = stdenv.lib.licenses.bsd3;
}
