{ mkDerivation, base, base-prelude, HTF, parsec, stdenv
, template-haskell, text
}:
mkDerivation {
  pname = "neat-interpolation";
  version = "0.3.1.1";
  sha256 = "051a3ee1f9389e2ff1d05dc8f78fc3aae0e50be90ddee1868561ecac095469c9";
  libraryHaskellDepends = [
    base base-prelude parsec template-haskell text
  ];
  testHaskellDepends = [ base-prelude HTF ];
  homepage = "https://github.com/nikita-volkov/neat-interpolation";
  description = "A quasiquoter for neat and simple multiline text interpolation";
  license = stdenv.lib.licenses.mit;
}
