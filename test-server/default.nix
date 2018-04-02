{ mkDerivation, aeson, base, base-compat, base64-bytestring
, bytestring, containers, deepseq, directory, exceptions, hspec
, hspec-wai, http-api-data, http-types, HUnit, mtl, network, parsec
, QuickCheck, resourcet, safe, servant, servant-server
, should-not-typecheck, stdenv, string-conversions, temporary, text
, transformers, transformers-compat, wai, wai-extra, warp
}:
mkDerivation {
  pname = "test-server";
  version = "0.13";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base base-compat base64-bytestring bytestring containers
    deepseq directory exceptions hspec hspec-wai http-api-data
    http-types HUnit mtl network parsec QuickCheck resourcet safe
    servant servant-server should-not-typecheck string-conversions
    temporary text transformers transformers-compat wai wai-extra warp
  ];
  homepage = "http://haskell-servant.readthedocs.org/";
  description = "Server for testing servant-client-ghcjs";
  license = stdenv.lib.licenses.bsd3;
}
