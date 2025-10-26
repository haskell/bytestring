# small script to hackily work around the dependency cycle
# 'bytestring -> [plugin] -> ghc -> bytestring' that prevents
# the testsuite from using plugins, by renaming the library
# to 'bytestring-plugins-hack'

sed -E '
  /Name:|build-depends:/s/bytestring/bytestring-plugins-hack/ ;
  s/if false && impl\(pluginTestsHack\)/if true/' \
    bytestring.cabal > bytestring-plugins-hack.cabal

mv bytestring.cabal bytestring.cabal.__MOVED_DURING_PLUGIN_TESTS__
cabal test --test-show-details=direct "$@"
mv bytestring.cabal.__MOVED_DURING_PLUGIN_TESTS__ bytestring.cabal
rm bytestring-plugins-hack.cabal
