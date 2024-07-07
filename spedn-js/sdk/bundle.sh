VERSION="0.4.0.0"
GHC_VERSION="9.6.2"


cabal new-build \
    --with-compiler javascript-unknown-ghcjs-ghc-$GHC_VERSION \
    --with-hc-pkg javascript-unknown-ghcjs-ghc-pkg-$GHC_VERSION 

rm -rf dist
mkdir -p dist

npx google-closure-compiler \
    --compilation_level=SIMPLE \
    --env=CUSTOM \
    --module_resolution=NODE \
    --jscomp_off=checkVars \
    --output_wrapper="(function(global){%output%})(exports)" \
    --assume_function_wrapper \
    --externs=dist-newstyle/build/javascript-ghcjs/ghc-$GHC_VERSION/spedn-$VERSION/x/spedn/build/spedn/spedn.jsexe/all.js.externs \
    --js=dist-newstyle/build/javascript-ghcjs/ghc-$GHC_VERSION/spedn-$VERSION/x/spedn/build/spedn/spedn.jsexe/rts.js \
    --js=dist-newstyle/build/javascript-ghcjs/ghc-$GHC_VERSION/spedn-$VERSION/x/spedn/build/spedn/spedn.jsexe/lib.js \
    --js=dist-newstyle/build/javascript-ghcjs/ghc-$GHC_VERSION/spedn-$VERSION/x/spedn/build/spedn/spedn.jsexe/out.js \
    --js=src/interop.js \
    --js_output_file=dist/compiler_service.js
