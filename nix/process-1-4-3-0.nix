{ mkDerivation, base, bytestring, deepseq, directory, filepath
, stdenv, unix
}:
mkDerivation {
  pname = "process";
  version = "1.4.3.0";
  sha256 = "5473f4d20a19c3ba448ace7d4d01ec821ad531574c23934fd3c55627f5a7f0eb";
  libraryHaskellDepends = [ base deepseq directory filepath unix ];
  testHaskellDepends = [ base bytestring directory ];
  description = "Process libraries";
  license = stdenv.lib.licenses.bsd3;
}
