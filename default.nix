{pkgs ? import <nixpkgs> {}, stdenv ? pkgs.stdenv}:
let
  nixpkgs = import (pkgs.fetchgit {
    url = "https://github.com/NixOS/nixpkgs-channels.git";
    rev = "694529e45d92fe3ca28c7aa78f57ee4b11d4bb73";
    sha256 = "07h6jl0mqy5g5xq5wnicd1add3n2cri8601aw957y46mfbm6j3n5";
  }) {};

  ghc = nixpkgs.haskellPackages;
  ghcjs = nixpkgs.haskell.packages.ghcjsHEAD;

  # Obvious TODO is not linking to a relative path. Servant doesn't have nix
  # files by default though (you have to run
  # servant/scripts/generate-nix-files.sh), so it's not trivial
  servant-ghc = ghc.callPackage ../servant/servant {};
  servant-server = ghc.callPackage ../servant/servant-server { servant = servant-ghc; };

  servant-ghcjs = ghcjs.callPackage ../servant/servant {};
  servant-client-core = ghcjs.callPackage ../servant/servant-client-core { servant = servant-ghcjs; };
  servant-client-ghcjs = ghcjs.callPackage ../servant/servant-client-ghcjs { inherit servant-client-core; };
in
{
  test-server = ghc.callPackage ./test-server { servant = servant-ghc; inherit servant-server;};
  test-client = ghcjs.callPackage ./test-client { servant = servant-ghcjs; inherit servant-client-core servant-client-ghcjs; };
}
