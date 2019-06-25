rm -rf dist
mkdir -p dist
cabal new-build --ghcjs
echo "(function(global){" >> dist/compiler_service.js
cat dist-newstyle/build/x86_64-linux/ghcjs-8.6.0.1/spedn-0.0.0/x/spedn/build/spedn/spedn.jsexe/rts.js >> dist/compiler_service.js
cat dist-newstyle/build/x86_64-linux/ghcjs-8.6.0.1/spedn-0.0.0/x/spedn/build/spedn/spedn.jsexe/lib.js >> dist/compiler_service.js
cat dist-newstyle/build/x86_64-linux/ghcjs-8.6.0.1/spedn-0.0.0/x/spedn/build/spedn/spedn.jsexe/out.js >> dist/compiler_service.js
cat src/interop.js >> dist/compiler_service.js
echo "})(exports);" >> dist/compiler_service.js
