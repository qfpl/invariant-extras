{ mkDerivation, base, contravariant, invariant, semigroupoids
, stdenv, transformers
}:
mkDerivation {
  pname = "invariant-extras";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base contravariant invariant semigroupoids transformers
  ];
  license = stdenv.lib.licenses.bsd3;
}
