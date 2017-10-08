# Quick, dirty, manual copy paste hack to test servant-client-ghcjs

Made for this PR:

https://github.com/haskell-servant/servant/pull/818

## How to run:

- Make sure nix is installed
- Create folders `somefolder/servant` and `somefolder/servant-client-ghcjs-test`
- Clone servant into `somefolder/servant` (preferably a commit with `servant-client-ghcjs` in it of course)
- Run `somefolder/servant/scripts/generate-nix-files.sh`
- Run `cabal2nix servant/servant-client-ghcjs --compiler ghcjs > somefolder/servant/servant-client-ghcjs/default.nix`
- Clone this repo into `somefolder/servant-client-ghcjs-test`
- `cd somefolder/servant-client-ghcjs-test`
- $(nix-build -A test-server)/bin/test-server $(nix-build -A test-client)/bin/test-client.jsexe
