module Test.Main where

import Prelude
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Data.Either (isLeft, Either(..))
import Data.List (singleton)
import Data.Maybe (Maybe(Nothing, Just))
import Data.Path.Pathy (currentDir, parentDir', file, dir, rootDir, (</>))
import Data.StrMap (empty, fromFoldable, fromList)
import Data.Tuple (Tuple(Tuple))
import Data.URI (Authority(Authority), HierarchicalPart(HierarchicalPart), Host(IPv4Address, NameAddress, IPv6Address), Query(Query), RelativePart(RelativePart), RelativeRef(RelativeRef), URI(URI), URIScheme(URIScheme), runParseURIRef)
import Data.URI.Query (printQuery)
import Test.Unit (suite, test, TestSuite)
import Test.Unit.Assert (assert, equal)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)

testRunParseURIRefParses :: forall a. String -> Either URI RelativeRef -> TestSuite a
testRunParseURIRefParses uri expected =
  test
    ("parses: " <> uri)
    (equal (Right expected) (runParseURIRef uri))

testRunParseURIRefFailes :: forall a. String -> TestSuite a
testRunParseURIRefFailes uri =
  test
    ("failes to parse: " <> uri)
    (assert ("parse should fail for: " <> uri) <<< isLeft <<< runParseURIRef $ uri)

main :: forall eff. Eff ( console :: CONSOLE , testOutput :: TESTOUTPUT, avar :: AVAR | eff ) Unit
main = runTest $ suite "Data.URI" do
  suite "runParseURIRef" do
    testRunParseURIRefParses
      "sql2:///?q=foo&var.bar=baz"
      (Left
        (URI
          (Just (URIScheme "sql2"))
          (HierarchicalPart
            (Just (Authority Nothing []))
            (Just (Left rootDir)))
          (Just (Query (fromFoldable [Tuple "q" (Just "foo"), Tuple "var.bar" (Just "baz")])))
          Nothing))
    testRunParseURIRefParses
      "mongodb://localhost"
      (Left
        (URI
          (Just (URIScheme "mongodb"))
          (HierarchicalPart
            (Just (Authority Nothing [(Tuple (NameAddress "localhost") Nothing)]))
            Nothing)
          Nothing
          Nothing))
    testRunParseURIRefParses
      "http://en.wikipedia.org/wiki/URI_scheme"
      (Left
        (URI
          (Just (URIScheme "http"))
          (HierarchicalPart
            (Just (Authority Nothing [Tuple (NameAddress "en.wikipedia.org") Nothing]))
            ((Just (Right ((rootDir </> dir "wiki") </> file "URI_scheme")))))
          Nothing
          Nothing))
    testRunParseURIRefParses
      "http://local.slamdata.com/?#?sort=asc&q=path%3A%2F&salt=1177214"
      (Left
        (URI
          (Just (URIScheme "http"))
          (HierarchicalPart
            (Just (Authority Nothing [Tuple (NameAddress "local.slamdata.com") Nothing]))
            ((Just (Left rootDir))))
          ((Just (Query empty)))
          ((Just "?sort=asc&q=path%3A%2F&salt=1177214"))))
    testRunParseURIRefParses
      "mongodb://foo:bar@db1.example.net,db2.example.net:2500/authdb?replicaSet=test&connectTimeoutMS=300000"
      (Left
        (URI
          (Just (URIScheme "mongodb"))
          (HierarchicalPart
            (Just
              (Authority
                (Just "foo:bar")
                [ Tuple (NameAddress "db1.example.net") Nothing
                , Tuple (NameAddress "db2.example.net") (Just 2500)]))
            (Just (Right (rootDir </> file "authdb"))))
          (Just
            (Query
              (fromFoldable [Tuple "replicaSet" (Just "test"), Tuple "connectTimeoutMS" (Just "300000")])))
          Nothing))
    testRunParseURIRefParses
      "mongodb://foo:bar@db1.example.net:6,db2.example.net:2500/authdb?replicaSet=test&connectTimeoutMS=300000"
      (Left
        (URI
          (Just (URIScheme "mongodb"))
          (HierarchicalPart
            (Just (Authority (Just "foo:bar") [(Tuple (NameAddress "db1.example.net") (Just 6)),(Tuple (NameAddress "db2.example.net") (Just 2500))]))
            (Just (Right (rootDir </> file "authdb"))))
          (Just (Query (fromFoldable [ Tuple "replicaSet" (Just "test"), Tuple "connectTimeoutMS" (Just "300000")])))
          Nothing))
    testRunParseURIRefParses
      "mongodb://192.168.0.1"
      (Left
        (URI
          (Just (URIScheme "mongodb"))
          (HierarchicalPart (Just (Authority Nothing [(Tuple (IPv4Address "192.168.0.1") Nothing)])) Nothing)
          Nothing
          Nothing))
    testRunParseURIRefParses
      "mongodb://192.168.0.1,192.168.0.2"
      (Left
        (URI
          (Just (URIScheme "mongodb"))
          (HierarchicalPart
            (Just
              (Authority
                Nothing
                [ Tuple (IPv4Address "192.168.0.1") Nothing
                , Tuple (IPv4Address "192.168.0.2") Nothing
                ]))
            Nothing)
          Nothing
          Nothing))
    testRunParseURIRefParses
      "mongodb://sysop:moon@localhost"
      (Left
        (URI
          (Just (URIScheme "mongodb"))
          (HierarchicalPart
            (Just(Authority (Just "sysop:moon") [(Tuple (NameAddress "localhost") Nothing)]))
            Nothing)
          Nothing
          Nothing))
    testRunParseURIRefParses
      "mongodb://sysop:moon@localhost/"
      (Left
        (URI
          (Just (URIScheme "mongodb"))
          (HierarchicalPart
            (Just (Authority (Just "sysop:moon") [(Tuple (NameAddress "localhost") Nothing)]))
            (Just (Left rootDir)))
          Nothing
          Nothing))
    testRunParseURIRefParses
      "mongodb://sysop:moon@localhost/records"
      (Left
        (URI
          (Just (URIScheme "mongodb"))
          (HierarchicalPart
            (Just (Authority (Just "sysop:moon") [(Tuple (NameAddress "localhost") Nothing)]))
            (Just (Right (rootDir </> file "records"))))
          Nothing
          Nothing))
    testRunParseURIRefParses
      "foo://[2001:cdba:0000:0000:0000:0000:3257:9652]"
      (Left
        (URI
          (Just (URIScheme "foo"))
          (HierarchicalPart
            (Just (Authority Nothing [Tuple (IPv6Address "2001:cdba:0000:0000:0000:0000:3257:9652") Nothing]))
            Nothing)
          Nothing
          Nothing))
    testRunParseURIRefParses
      "foo://[FE80::0202:B3FF:FE1E:8329]"
      (Left
        (URI
          (Just (URIScheme "foo"))
          (HierarchicalPart
            (Just (Authority Nothing [(Tuple (IPv6Address "FE80::0202:B3FF:FE1E:8329") Nothing)]))
            Nothing)
          Nothing
          Nothing))
    testRunParseURIRefParses
      "foo://[2001:db8::1]:80"
      (Left
        (URI
          (Just (URIScheme "foo"))
          (HierarchicalPart
            (Just (Authority Nothing [(Tuple (IPv6Address "2001:db8::1") (Just 80))]))
            Nothing)
          Nothing
          Nothing))
    testRunParseURIRefParses
      "ftp://ftp.is.co.za/rfc/rfc1808.txt"
      (Left
        (URI
          (Just (URIScheme "ftp"))
          (HierarchicalPart
            (Just (Authority Nothing [(Tuple (NameAddress "ftp.is.co.za") Nothing)]))
            (Just (Right ((rootDir </> dir "rfc") </> file "rfc1808.txt"))))
          Nothing
          Nothing))
    testRunParseURIRefParses
      "http://www.ietf.org/rfc/rfc2396.txt"
      (Left
        (URI
          (Just (URIScheme "http"))
          (HierarchicalPart (Just (Authority Nothing [(Tuple (NameAddress "www.ietf.org") Nothing)])) (Just (Right ((rootDir </> dir "rfc") </> file "rfc2396.txt"))))
          Nothing
          Nothing))
    testRunParseURIRefParses
      "ldap://[2001:db8::7]/c=GB?objectClass?one"
      (Left
        (URI
          (Just (URIScheme "ldap"))
          (HierarchicalPart
            (Just (Authority Nothing [(Tuple (IPv6Address "2001:db8::7") Nothing)]))
            (Just (Right (rootDir </> file "c=GB"))))
          (Just (Query (fromList <<< singleton $ (Tuple "objectClass?one" Nothing))))
          Nothing))
    testRunParseURIRefParses
      "telnet://192.0.2.16:80/"
      (Left
        (URI
          (Just (URIScheme "telnet"))
          (HierarchicalPart
            (Just (Authority Nothing [(Tuple (IPv4Address "192.0.2.16") (Just 80))]))
            (Just (Left rootDir)))
          Nothing
          Nothing))
    testRunParseURIRefParses
      "foo://example.com:8042/over/there?name=ferret#nose"
      (Left
        (URI
          (Just (URIScheme "foo"))
          (HierarchicalPart (Just (Authority Nothing [(Tuple (NameAddress "example.com") (Just 8042))])) (Just (Right ((rootDir </> dir "over") </> file "there"))))
          (Just (Query (fromFoldable [ Tuple "name" (Just "ferret") ])))
          (Just "nose")))
    testRunParseURIRefParses
      "foo://info.example.com?fred"
      (Left
        (URI
          (Just (URIScheme "foo"))
          (HierarchicalPart
            (Just (Authority Nothing [(Tuple (NameAddress "info.example.com") Nothing)]))
            Nothing)
          (Just (Query (fromList <<< singleton $ Tuple "fred" Nothing)))
          Nothing))
    testRunParseURIRefParses
      "ftp://cnn.example.com&story=breaking_news@10.0.0.1/top_story.htm"
      (Left
        (URI
          (Just (URIScheme "ftp"))
          (HierarchicalPart
            (Just
              (Authority
                (Just "cnn.example.com&story=breaking_news")
                [(Tuple (IPv4Address "10.0.0.1") Nothing)]))
            (Just (Right (rootDir </> file "top_story.htm"))))
          Nothing
          Nothing))
    testRunParseURIRefParses
      "../top_story.htm"
      (Right
        (RelativeRef
          (RelativePart
            Nothing
            (Just (Right ((parentDir' currentDir) </> file "top_story.htm"))))
          Nothing
          Nothing))
    testRunParseURIRefParses
      "top_story.htm"
      (Right
        (RelativeRef
          (RelativePart
            Nothing
            (Just (Right (currentDir </> file "top_story.htm"))))
          Nothing
          Nothing))

    testRunParseURIRefFailes  "news:comp.infosystems.www.servers.unix"
    testRunParseURIRefFailes "tel:+1-816-555-1212"
    testRunParseURIRefFailes "urn:oasis:names:specification:docbook:dtd:xml:4.1.2"
    testRunParseURIRefFailes "mailto:John.Doe@example.com"
    testRunParseURIRefFailes "mailto:fred@example.com"
    testRunParseURIRefFailes "/top_story.htm"

    test "query with empty value is printed correctly" $ do
      let query = Query <<< fromFoldable $ [ Tuple "empty" Nothing, Tuple "non-empty" (Just "1") ]
      equal "test" (printQuery query)
