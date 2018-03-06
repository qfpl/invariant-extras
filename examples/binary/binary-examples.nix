{ mkDerivation, base, binary, bytestring, common, contravariant
, invariant-extras, stdenv
}:
mkDerivation {
  pname = "binary-examples";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base binary bytestring common contravariant invariant-extras
  ];
  license = stdenv.lib.licenses.bsd3;
}
