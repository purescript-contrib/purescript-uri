module Test.Main where

import Data.Either
import Control.Monad.Eff
import Debug.Trace
import Network.URI
import Text.Parsing.StringParser
import Data.String.Regex

main = do
  traceEither $ parse "mongodb://localhost"
  traceEither $ parse "http://en.wikipedia.org/wiki/URI_scheme"
  traceEither $ parse "http://local.slamdata.com/?#?sort=asc&q=path%3A%2F&salt=1177214"
  traceEither $ parse "mongodb://db1.example.net,db2.example.net:2500/?replicaSet=test&connectTimeoutMS=300000"
  traceEither $ parse "mongodb://sysop:moon@localhost"
  traceEither $ parse "mongodb://sysop:moon@localhost/records"

traceEither :: forall a. Either ParseError a -> _
traceEither (Left err) = trace $ "Parse failed: " ++ show err
traceEither (Right x) = traceAny x

foreign import traceAny
  """
  function traceAny (x) {
    return function () {
      console.log(JSON.stringify(x));
      return {};
    };
  }
  """ :: forall a e. a -> Eff (trace :: Trace) Unit
