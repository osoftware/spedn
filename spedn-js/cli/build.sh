cabal new-build --ghcjs

npx google-closure-compiler \
    --compilation_level=SIMPLE \
    --env=CUSTOM \
    --module_resolution=NODE \
    --jscomp_off=checkVars \
    --externs=dist-newstyle/build/x86_64-linux/ghcjs-8.6.0.1/spedn-0.1.3/x/spedn/build/spedn/spedn.jsexe/all.js.externs \
    --js=dist-newstyle/build/x86_64-linux/ghcjs-8.6.0.1/spedn-0.1.3/x/spedn/build/spedn/spedn.jsexe/all.js \
    --js_output_file=dist-newstyle/build/x86_64-linux/ghcjs-8.6.0.1/spedn-0.1.3/x/spedn/build/spedn/spedn.jsexe/all.min.js

rm spedn.js
echo "#!/usr/bin/env node" >> spedn.js
cat dist-newstyle/build/x86_64-linux/ghcjs-8.6.0.1/spedn-0.1.3/x/spedn/build/spedn/spedn.jsexe/all.min.js >> spedn.js
chmod +x spedn.js