VERSION="0.4.0.0"
GHC_VERSION="9.6.2"

cabal new-build \
    --with-compiler javascript-unknown-ghcjs-ghc-$GHC_VERSION \
    --with-hc-pkg javascript-unknown-ghcjs-ghc-pkg-$GHC_VERSION 

npx google-closure-compiler \
    --compilation_level=SIMPLE \
    --env=CUSTOM \
    --module_resolution=NODE \
    --jscomp_off=checkVars \
    --externs=dist-newstyle/build/javascript-ghcjs/ghc-$GHC_VERSION/spedn-$VERSION/x/spedn/build/spedn/spedn.jsexe/all.js.externs \
    --js=dist-newstyle/build/javascript-ghcjs/ghc-$GHC_VERSION/spedn-$VERSION/x/spedn/build/spedn/spedn.jsexe/all.js \
    --js_output_file=dist-newstyle/build/javascript-ghcjs/ghc-$GHC_VERSION/spedn-$VERSION/x/spedn/build/spedn/spedn.jsexe/all.min.js

rm -f spedn
echo "#!/usr/bin/env node" >> spedn
cat dist-newstyle/build/javascript-ghcjs/ghc-$GHC_VERSION/spedn-$VERSION/x/spedn/build/spedn/spedn.jsexe/all.min.js >> spedn
chmod +x spedn
