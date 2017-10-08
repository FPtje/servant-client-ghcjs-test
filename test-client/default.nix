{ mkDerivation, aeson, base, base-compat, bytestring, containers
, deepseq, hspec, http-api-data, http-client, http-media
, http-types, HUnit, mtl, network, QuickCheck, servant
, servant-client-core, servant-client-ghcjs, stdenv, text
, transformers, transformers-compat
}:
mkDerivation {
  pname = "test-client";
  version = "0.11";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base base-compat bytestring containers deepseq hspec
    http-api-data http-client http-media http-types HUnit mtl network
    QuickCheck servant servant-client-core servant-client-ghcjs text
    transformers transformers-compat
  ];
  homepage = "http://haskell-servant.readthedocs.org/";
  description = "Client part of the test";
  license = stdenv.lib.licenses.bsd3;
}
