{ mkDerivation, aeson, base, brittany, bytestring, containers
, cryptonite, directory, filepath, hedgehog, hedgehog-corpus, hlint
, hpack, htoml, http-api-data, http-client, http-media, http-types
, jose, JuicyPixels, lens, mmorph, monad-logger, mtl, network-uri
, servant, servant-auth, servant-auth-client, servant-auth-server
, servant-client, servant-multipart, servant-server, sqlite-simple
, stdenv, template-haskell, text, time, unordered-containers, uuid
, wai, wai-cors, warp, weeder, python3
}:
let
  cleanSource' = with stdenv.lib; builtins.filterSource (p: t: cleanSourceFilter p t && baseNameOf p != ".stack-work");
in
mkDerivation {
  pname = "servant-photo-gallery";
  version = "0.1.0.0";
  src = cleanSource' ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring cryptonite directory filepath htoml
    http-api-data http-media http-types jose JuicyPixels lens
    monad-logger mtl network-uri servant servant-auth
    servant-auth-server servant-multipart servant-server sqlite-simple
    text time unordered-containers uuid wai wai-cors warp
  ];
  libraryToolDepends = [ brittany hlint hpack weeder ];
  executableHaskellDepends = [
    aeson base bytestring cryptonite directory filepath htoml
    http-api-data http-media http-types jose JuicyPixels lens
    monad-logger mtl network-uri servant servant-auth
    servant-auth-server servant-multipart servant-server sqlite-simple
    text time unordered-containers uuid wai wai-cors warp
  ];
  executableToolDepends = [ brittany hlint weeder ];
  testHaskellDepends = [
    aeson base bytestring containers cryptonite directory filepath
    hedgehog hedgehog-corpus htoml http-api-data http-client http-media
    http-types jose JuicyPixels lens mmorph monad-logger mtl
    network-uri servant servant-auth servant-auth-client
    servant-auth-server servant-client servant-multipart servant-server
    sqlite-simple template-haskell text time unordered-containers uuid
    wai wai-cors warp
  ];
  testToolDepends = [ brittany hlint weeder ];
  prePatch = "hpack";
  homepage = "https://github.com/rogryza/servant-photo-gallery#readme";
  description = "Photo gallery API server";
  license = stdenv.lib.licenses.mit;
}
