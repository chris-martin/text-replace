{ mkDerivation, base, bytestring, HUnit, mtl, stdenv
, test-framework, test-framework-hunit, text
}:
mkDerivation {
  pname = "parsec";
  version = "3.1.11";
  sha256 = "0vk7q9j2128q191zf1sg0ylj9s9djwayqk9747k0a5fin4f2b1vg";
  libraryHaskellDepends = [ base bytestring mtl text ];
  testHaskellDepends = [
    base HUnit test-framework test-framework-hunit
  ];
  homepage = "https://github.com/aslatter/parsec";
  description = "Monadic parser combinators";
  license = stdenv.lib.licenses.bsd3;
}
