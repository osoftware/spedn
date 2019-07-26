cabal new-build --ghcjs

rm -rf dist
mkdir -p dist

npx google-closure-compiler \
    --compilation_level=SIMPLE \
    --env=CUSTOM \
    --module_resolution=NODE \
    --jscomp_off=checkVars \
    --output_wrapper="(function(global){%output%})(exports)" \
    --assume_function_wrapper \
    --externs=dist-newstyle/build/x86_64-linux/ghcjs-8.6.0.1/spedn-0.1.0/x/spedn/build/spedn/spedn.jsexe/all.js.externs \
    --js=dist-newstyle/build/x86_64-linux/ghcjs-8.6.0.1/spedn-0.1.0/x/spedn/build/spedn/spedn.jsexe/rts.js \
    --js=dist-newstyle/build/x86_64-linux/ghcjs-8.6.0.1/spedn-0.1.0/x/spedn/build/spedn/spedn.jsexe/lib.js \
    --js=dist-newstyle/build/x86_64-linux/ghcjs-8.6.0.1/spedn-0.1.0/x/spedn/build/spedn/spedn.jsexe/out.js \
    --js=src/interop.js \
    --js_output_file=dist/compiler_service.js

