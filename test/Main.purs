module Test.Main where

import Prelude
import Data.Either
import Control.Apply
import Control.Monad.Eff
import qualified Control.Monad.Eff.Console as C
import Control.Monad.Eff.Exception
import Data.URI
import Data.URI.Types
import Text.Parsing.StringParser


main = do
  test runParseURIRef "sql2:///?q=foo&var.bar=baz"
  test runParseURIRef "mongodb://localhost"
  test runParseURIRef "http://en.wikipedia.org/wiki/URI_scheme"
  test runParseURIRef "http://local.slamdata.com/?#?sort=asc&q=path%3A%2F&salt=1177214"
  test runParseURIRef "mongodb://foo:bar@db1.example.net,db2.example.net:2500/authdb?replicaSet=test&connectTimeoutMS=300000"
  test runParseURIRef "mongodb://foo:bar@db1.example.net:666,db2.example.net:2500/authdb?replicaSet=test&connectTimeoutMS=300000"
  test runParseURIRef "mongodb://192.168.0.1"
  test runParseURIRef "mongodb://192.168.0.1,192.168.0.2"
  test runParseURIRef "mongodb://sysop:moon@localhost"
  test runParseURIRef "mongodb://sysop:moon@localhost"
  test runParseURIRef "mongodb://sysop:moon@localhost/"
  test runParseURIRef "mongodb://sysop:moon@localhost/records"
  test runParseURIRef "foo://[2001:cdba:0000:0000:0000:0000:3257:9652]"
  test runParseURIRef "foo://[FE80::0202:B3FF:FE1E:8329]"
  test runParseURIRef "foo://[2001:db8::1]:80"
  test runParseURIRef "ftp://ftp.is.co.za/rfc/rfc1808.txt"
  test runParseURIRef "http://www.ietf.org/rfc/rfc2396.txt"
  test runParseURIRef "ldap://[2001:db8::7]/c=GB?objectClass?one"
  test runParseURIRef "telnet://192.0.2.16:80/"
  test runParseURIRef "foo://example.com:8042/over/there?name=ferret#nose"
  test runParseURIRef "foo://info.example.com?fred"
  test runParseURIRef "ftp://cnn.example.com&story=breaking_news@10.0.0.1/top_story.htm"
  test runParseURIRef "../top_story.htm"
  test runParseURIRef "top_story.htm"

  C.log "\nFailing test cases: "
  testFails runParseURIRef "news:comp.infosystems.www.servers.unix"
  testFails runParseURIRef "tel:+1-816-555-1212"
  testFails runParseURIRef "urn:oasis:names:specification:docbook:dtd:xml:4.1.2"
  testFails runParseURIRef "mailto:John.Doe@example.com"
  testFails runParseURIRef "mailto:fred@example.com"
  testFails runParseURIRef "/top_story.htm"


testCommon :: (String -> Eff _ Unit) -> (String -> Eff _ Unit) ->
              (String -> Either ParseError URIRef) -> String -> _
testCommon leftMsg rightMsg f s = do
  C.log $ "\nTrying to parse " <> s <> ""
  case f s of
    Left err -> leftMsg $ "  Parse failed: " <> show err
    Right x -> do
      rightMsg $ "      printURI: " <> printURIRef x
        <> "\n          show: " <> show x


test :: (String -> Either ParseError URIRef) -> String -> _
test = testCommon (\x -> C.error x *> (throwException $ error x)) C.log

testFails :: (String -> Either ParseError URIRef) -> String -> _
testFails = testCommon C.log (\x -> C.error x *> (throwException $ error x))
