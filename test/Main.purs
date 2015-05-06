module Test.Main where

import Data.Either
import Control.Monad.Eff
import Debug.Trace
import Data.URI
import Data.URI.Types
import Text.Parsing.StringParser

main = do
  test parseDirURI "mongodb://localhost"
  test parseFileURI "http://en.wikipedia.org/wiki/URI_scheme"
  test parseDirURI "http://local.slamdata.com/?#?sort=asc&q=path%3A%2F&salt=1177214"
  test parseFileURI "mongodb://foo:bar@db1.example.net,db2.example.net:2500/authdb?replicaSet=test&connectTimeoutMS=300000"
  test parseFileURI "mongodb://192.168.0.1"
  test parseFileURI "mongodb://192.168.0.1,192.168.0.2"
  test parseDirURI "mongodb://sysop:moon@localhost"
  test parseFileURI "mongodb://sysop:moon@localhost"
  test parseDirURI "mongodb://sysop:moon@localhost/"
  test parseFileURI "mongodb://sysop:moon@localhost/records"
  test parseDirURI "foo://[2001:cdba:0000:0000:0000:0000:3257:9652]"
  test parseDirURI "foo://[FE80::0202:B3FF:FE1E:8329]"
  test parseDirURI "foo://[2001:db8::1]:80"

test :: forall a. (String -> Either ParseError (URI a)) -> String -> _
test f s = do
  trace $ "\nTrying to parse " ++ s ++ ""
  case f s of
    (Left err) -> trace $ "  Parse failed: " ++ show err
    (Right x) -> do
      trace $ "      printURI: " ++ printURI x
      trace $ "          show: " ++ show x

foreign import traceAny
  """
  function traceAny (x) {
    return function () {
      console.log(JSON.stringify(x));
      return {};
    };
  }
  """ :: forall a e. a -> Eff (trace :: Trace) Unit
