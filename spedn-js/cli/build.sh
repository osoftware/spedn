cd ../../spedn
cabal new-build --ghcjs
cd ../spedn-js/cli
echo "#!/usr/bin/env node" >> spedn.js
cat ../../spedn/dist-newstyle/build/x86_64-linux/ghcjs-8.6.0.1/spedn-0.0.0/x/spedn/build/spedn/spedn.jsexe/all.js >> spedn.js
chmod +x spedn.js
