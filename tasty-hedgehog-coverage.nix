{ mkDerivation, base, containers, hedgehog, mtl, stdenv, tagged
, tasty, tasty-expected-failure, tasty-hedgehog, text
, wl-pprint-annotated
}:
mkDerivation {
  pname = "tasty-hedgehog-coverage";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base containers hedgehog mtl tagged tasty tasty-hedgehog text
    wl-pprint-annotated
  ];
  testHaskellDepends = [
    base hedgehog tasty tasty-expected-failure
  ];
  license = stdenv.lib.licenses.bsd3;
}
