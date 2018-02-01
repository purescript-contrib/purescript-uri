module Test.Main where

import Prelude

import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Random (RANDOM)
import Data.Array as Array
import Data.Either (isLeft, Either(..))
import Data.List (List(..), singleton, (:))
import Data.Maybe (Maybe(Nothing, Just))
import Data.Monoid (mempty)
import Data.Path.Pathy (currentDir, parentDir', file, dir, rootDir, (</>))
import Data.String as String
import Data.Tuple (Tuple(..))
import Data.URI (AbsoluteURI(..), Authority(..), HierarchicalPart(..), Host(..), Fragment, Port(..), RelativePart(..), RelativeRef(..), Query, Scheme(..), URI(..), UserInfo)
import Data.URI.AbsoluteURI as AbsoluteURI
import Data.URI.Authority as Authority
import Data.URI.Fragment as Fragment
import Data.URI.Host as Host
import Data.URI.Host.RegName as RegName
import Data.URI.Host.Gen as Host.Gen
import Data.URI.NonStandard.Path as NP
import Data.URI.NonStandard.QueryPairs as NQP
import Data.URI.Port as Port
import Data.URI.Query as Query
import Data.URI.Scheme as Scheme
import Data.URI.URI as URI
import Data.URI.URIRef as URIRef
import Data.URI.UserInfo as UserInfo
import Test.QuickCheck ((===))
import Test.QuickCheck as QC
import Test.QuickCheck.Gen as QCG
import Test.Unit (Test, suite, test, TestSuite)
import Test.Unit.Assert (assert, equal)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)
import Text.Parsing.StringParser (Parser, runParser)
import Text.Parsing.StringParser.Combinators (sepBy) as SP
import Text.Parsing.StringParser.String (string) as SP

options ∷ Record (URIRef.URIRefOptions UserInfo Array Host Port NP.URIPathAbs NP.URIPathRel Query Fragment)
options =
  { parseUserInfo: pure
  , printUserInfo: id
  , parseHosts: map Array.fromFoldable <<< flip SP.sepBy (SP.string ",")
  , printHosts: String.joinWith ","
  , parseHost: pure
  , printHost: id
  , parsePort: pure
  , printPort: id
  , parseHierPath: NP.parseURIPathAbs
  , printHierPath: NP.printPath
  , parseRelPath: NP.parseURIPathRel
  , printRelPath: NP.printPath
  , parseQuery: pure
  , printQuery: id
  , parseFragment: pure
  , printFragment: id
  }

testPrinter :: forall a b. Show b => (b -> String) -> String -> b -> TestSuite a
testPrinter f expected uri =
  test
    ("prints: " <> expected)
    (equal expected (f uri))

testRunParseSuccess :: forall a b. Eq b => Show b => Parser b -> String -> b -> TestSuite a
testRunParseSuccess p uri expected =
  test
    ("parses: " <> uri)
    (equal (Right expected) (runParser p uri))

testIso :: forall a b. Eq b => Show b => Parser b -> (b -> String) -> String -> b -> TestSuite a
testIso p f uri expected = do
  testRunParseSuccess p uri expected
  testPrinter f uri expected

testIsoURI :: forall a. String -> URI.URI UserInfo Array Host Port NP.URIPathAbs Query Fragment -> TestSuite a
testIsoURI = testIso (URI.parser options) (URI.print options)

testIsoURIRef :: forall a. String -> URIRef.URIRef UserInfo Array Host Port NP.URIPathAbs NP.URIPathRel Query Fragment -> TestSuite a
testIsoURIRef = testIso (URIRef.parser options) (URIRef.print options)

-- testRunParseURIRefParses :: forall a. String -> Either URI RelativeRef -> TestSuite a
-- testRunParseURIRefParses = testRunParseSuccess URIRef.parser

testRunParseURIRefFails :: forall a. String -> TestSuite a
testRunParseURIRefFails uri =
  test
    ("fails to parse: " <> uri)
    (assert ("parse should fail for: " <> uri) <<< isLeft <<< runParser (URIRef.parser options) $ uri)

main :: forall eff. Eff (console :: CONSOLE, testOutput :: TESTOUTPUT, avar :: AVAR, exception :: EXCEPTION, random :: RANDOM | eff) Unit
main = runTest $ suite "Data.URI" do

  suite "parseIPv4Address" do

    test "parseIPv4Address / Host.print roundtrip" do
      forAll do
        ipv4 <- Host.Gen.genIPv4
        let printed = Host.print id ipv4
        let parsed = runParser (Host.parser pure) printed
        pure $ pure ipv4 === parsed

    test "0-lead octets should not parse as an IP address" do
      equal
        (Right (NameAddress (RegName.unsafeFromString "192.168.001.1")))
        (runParser (Host.parser pure) "192.168.001.1")

  suite "Scheme parser" do
    testRunParseSuccess Scheme.parser "http:" (Scheme "http")
    testRunParseSuccess Scheme.parser "git+ssh:" (Scheme "git+ssh")

  suite "UserInfo parser" do
    testRunParseSuccess (UserInfo.parser pure) "user" (UserInfo.fromString "user")
    testRunParseSuccess (UserInfo.parser pure) "spaced%20user" (UserInfo.fromString "spaced user")
    testRunParseSuccess (UserInfo.parser pure) "user:password" (UserInfo.fromString "user:password")
    testRunParseSuccess (UserInfo.parser pure) "spaced%20user:password%25%C2%A3" (UserInfo.fromString "spaced user:password%£")
    testRunParseSuccess (UserInfo.parser pure) "a:b:c" (UserInfo.fromString "a:b:c")

  suite "Host parser" do
    testRunParseSuccess (Host.parser pure) "localhost" (NameAddress (RegName.fromString "localhost"))
    testRunParseSuccess (Host.parser pure) "github.com" (NameAddress (RegName.fromString "github.com"))
    testRunParseSuccess (Host.parser pure) "www.multipart.domain.example.com" (NameAddress (RegName.fromString "www.multipart.domain.example.com"))
    testRunParseSuccess (Host.parser pure) "192.168.0.1" (IPv4Address "192.168.0.1")
    testRunParseSuccess (Host.parser pure) "[2001:cdba:0000:0000:0000:0000:3257:9652]" (IPv6Address "2001:cdba:0000:0000:0000:0000:3257:9652")

  suite "Port parser" do
    testRunParseSuccess (Port.parser pure) "0" (Port 0)
    testRunParseSuccess (Port.parser pure) "1234" (Port 1234)
    testRunParseSuccess (Port.parser pure) "63174" (Port 63174)

  suite "Fragment parser" do
    testRunParseSuccess (Fragment.parser pure) "#" (Fragment.fromString "")
    testRunParseSuccess (Fragment.parser pure) "#foo" (Fragment.fromString "foo")
    testRunParseSuccess (Fragment.parser pure) "#foo%23bar" (Fragment.fromString "foo#bar")
    testRunParseSuccess (Fragment.parser pure) "#foo%23bar" (Fragment.unsafeFromString "foo%23bar")

  suite "Authority parser" do
    testRunParseSuccess
      (Authority.parser options)
      "//localhost"
      (Authority Nothing [Tuple (NameAddress (RegName.fromString "localhost")) Nothing])
    testRunParseSuccess
      (Authority.parser options)
      "//localhost:3000"
      (Authority Nothing [Tuple (NameAddress (RegName.fromString "localhost")) (Just (Port 3000))])

  suite "URIRef.parse" do
    testIsoURIRef
      "sql2:///?q=foo&var.bar=baz"
      (Left
        (URI
          (Scheme "sql2")
          (HierarchicalPart
            (Just (Authority Nothing []))
            (Just (Left rootDir)))
          (Just (Query.unsafeFromString "q=foo&var.bar=baz"))
          Nothing))
    testIsoURIRef
      "mongodb://localhost"
      (Left
        (URI
          (Scheme "mongodb")
          (HierarchicalPart
            (Just (Authority Nothing [(Tuple (NameAddress (RegName.fromString "localhost")) Nothing)]))
            Nothing)
          Nothing
          Nothing))
    testIsoURIRef
      "https://1a.example.com"
      (Left
        (URI
          (Scheme "https")
          (HierarchicalPart
            (Just (Authority Nothing [(Tuple (NameAddress (RegName.fromString "1a.example.com")) Nothing)]))
            Nothing)
          Nothing
          Nothing))
    testIsoURIRef
      "http://en.wikipedia.org/wiki/URI_scheme"
      (Left
        (URI
          (Scheme "http")
          (HierarchicalPart
            (Just (Authority Nothing [Tuple (NameAddress (RegName.fromString "en.wikipedia.org")) Nothing]))
            ((Just (Right ((rootDir </> dir "wiki") </> file "URI_scheme")))))
          Nothing
          Nothing))
    testIsoURIRef
      "mongodb://foo:bar@db1.example.net,db2.example.net:2500/authdb?replicaSet=test&connectTimeoutMS=300000"
      (Left
        (URI
          (Scheme "mongodb")
          (HierarchicalPart
            (Just
              (Authority
                (Just (UserInfo.unsafeFromString "foo:bar"))
                [ Tuple (NameAddress (RegName.fromString "db1.example.net")) Nothing
                , Tuple (NameAddress (RegName.fromString "db2.example.net")) (Just (Port 2500))]))
            (Just (Right (rootDir </> file "authdb"))))
          (Just (Query.unsafeFromString "replicaSet=test&connectTimeoutMS=300000"))
          Nothing))
    testIsoURIRef
      "mongodb://foo:bar@db1.example.net:6,db2.example.net:2500/authdb?replicaSet=test&connectTimeoutMS=300000"
      (Left
        (URI
          (Scheme "mongodb")
          (HierarchicalPart
            (Just
              (Authority
                (Just (UserInfo.unsafeFromString "foo:bar"))
                [ (Tuple (NameAddress (RegName.fromString "db1.example.net")) (Just (Port 6)))
                , (Tuple (NameAddress (RegName.fromString "db2.example.net")) (Just (Port 2500)))]))
            (Just (Right (rootDir </> file "authdb"))))
          (Just (Query.unsafeFromString "replicaSet=test&connectTimeoutMS=300000"))
          Nothing))
    testIsoURIRef
      "mongodb://192.168.0.1"
      (Left
        (URI
          (Scheme "mongodb")
          (HierarchicalPart (Just (Authority Nothing [(Tuple (IPv4Address "192.168.0.1") Nothing)])) Nothing)
          Nothing
          Nothing))
    testIsoURIRef
      "mongodb://192.168.0.1,192.168.0.2"
      (Left
        (URI
          (Scheme "mongodb")
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
    testIsoURIRef
      "mongodb://sysop:moon@localhost"
      (Left
        (URI
          (Scheme "mongodb")
          (HierarchicalPart
            (Just (Authority (Just (UserInfo.unsafeFromString "sysop:moon")) [(Tuple (NameAddress (RegName.fromString "localhost")) Nothing)]))
            Nothing)
          Nothing
          Nothing))
    testIsoURIRef
      "mongodb://sysop:moon@localhost/"
      (Left
        (URI
          (Scheme "mongodb")
          (HierarchicalPart
            (Just (Authority (Just (UserInfo.unsafeFromString "sysop:moon")) [(Tuple (NameAddress (RegName.fromString "localhost")) Nothing)]))
            (Just (Left rootDir)))
          Nothing
          Nothing))
    testIsoURIRef
      "mongodb://sysop:moon@localhost/records"
      (Left
        (URI
          (Scheme "mongodb")
          (HierarchicalPart
            (Just (Authority (Just (UserInfo.unsafeFromString "sysop:moon")) [(Tuple (NameAddress (RegName.fromString "localhost")) Nothing)]))
            (Just (Right (rootDir </> file "records"))))
          Nothing
          Nothing))
    testIsoURIRef
      "foo://[2001:cdba:0000:0000:0000:0000:3257:9652]"
      (Left
        (URI
          (Scheme "foo")
          (HierarchicalPart
            (Just (Authority Nothing [Tuple (IPv6Address "2001:cdba:0000:0000:0000:0000:3257:9652") Nothing]))
            Nothing)
          Nothing
          Nothing))
    testIsoURIRef
      "foo://[FE80::0202:B3FF:FE1E:8329]"
      (Left
        (URI
          (Scheme "foo")
          (HierarchicalPart
            (Just (Authority Nothing [(Tuple (IPv6Address "FE80::0202:B3FF:FE1E:8329") Nothing)]))
            Nothing)
          Nothing
          Nothing))
    testIsoURIRef
      "foo://[2001:db8::1]:80"
      (Left
        (URI
          (Scheme "foo")
          (HierarchicalPart
            (Just (Authority Nothing [(Tuple (IPv6Address "2001:db8::1") (Just (Port 80)))]))
            Nothing)
          Nothing
          Nothing))
    testIsoURIRef
      "ftp://ftp.is.co.za/rfc/rfc1808.txt"
      (Left
        (URI
          (Scheme "ftp")
          (HierarchicalPart
            (Just (Authority Nothing [(Tuple (NameAddress (RegName.fromString "ftp.is.co.za")) Nothing)]))
            (Just (Right ((rootDir </> dir "rfc") </> file "rfc1808.txt"))))
          Nothing
          Nothing))
    testIsoURIRef
      "http://www.ietf.org/rfc/rfc2396.txt"
      (Left
        (URI
          (Scheme "http")
          (HierarchicalPart
            (Just (Authority Nothing [(Tuple (NameAddress (RegName.fromString "www.ietf.org")) Nothing)]))
            (Just (Right ((rootDir </> dir "rfc") </> file "rfc2396.txt"))))
          Nothing
          Nothing))
    testIsoURIRef
      "ldap://[2001:db8::7]/c=GB?objectClass?one"
      (Left
        (URI
          (Scheme "ldap")
          (HierarchicalPart
            (Just (Authority Nothing [(Tuple (IPv6Address "2001:db8::7") Nothing)]))
            (Just (Right (rootDir </> file "c=GB"))))
          (Just (Query.unsafeFromString "objectClass?one"))
          Nothing))
    testIsoURIRef
      "telnet://192.0.2.16:80/"
      (Left
        (URI
          (Scheme "telnet")
          (HierarchicalPart
            (Just (Authority Nothing [(Tuple (IPv4Address "192.0.2.16") (Just (Port 80)))]))
            (Just (Left rootDir)))
          Nothing
          Nothing))
    testIsoURIRef
      "foo://example.com:8042/over/there?name=ferret#nose"
      (Left
        (URI
          (Scheme "foo")
          (HierarchicalPart
            (Just (Authority Nothing [(Tuple (NameAddress (RegName.fromString "example.com")) (Just (Port 8042)))])) (Just (Right ((rootDir </> dir "over") </> file "there"))))
          (Just (Query.unsafeFromString "name=ferret"))
          (Just (Fragment.unsafeFromString "nose"))))
    testIsoURIRef
      "foo://example.com:8042/over/there?name=ferret#"
      (Left
        (URI
          (Scheme "foo")
          (HierarchicalPart
            (Just (Authority Nothing [(Tuple (NameAddress (RegName.fromString "example.com")) (Just (Port 8042)))])) (Just (Right ((rootDir </> dir "over") </> file "there"))))
          (Just (Query.unsafeFromString "name=ferret"))
          (Just (Fragment.unsafeFromString ""))))
    testIsoURIRef
      "foo://info.example.com?fred"
      (Left
        (URI
          (Scheme "foo")
          (HierarchicalPart
            (Just (Authority Nothing [(Tuple (NameAddress (RegName.fromString "info.example.com")) Nothing)]))
            Nothing)
          (Just (Query.unsafeFromString "fred"))
          Nothing))
    testIsoURIRef
      "ftp://cnn.example.com&story=breaking_news@10.0.0.1/top_story.htm"
      (Left
        (URI
          (Scheme "ftp")
          (HierarchicalPart
            (Just
              (Authority
                (Just (UserInfo.unsafeFromString "cnn.example.com&story=breaking_news"))
                [(Tuple (IPv4Address "10.0.0.1") Nothing)]))
            (Just (Right (rootDir </> file "top_story.htm"))))
          Nothing
          Nothing))
    testIsoURIRef
      "../top_story.htm"
      (Right
        (RelativeRef
          (RelativePart
            Nothing
            (Just (Right ((parentDir' currentDir) </> file "top_story.htm"))))
          Nothing
          Nothing))
    testIsoURIRef
      "top_story.htm"
      (Right
        (RelativeRef
          (RelativePart
            Nothing
            (Just (Right (currentDir </> file "top_story.htm"))))
          Nothing
          Nothing))
    testIso
      (AbsoluteURI.parser options)
      (AbsoluteURI.print options)
      "couchbase://localhost/testBucket?password=&docTypeKey="
      (AbsoluteURI
        (Scheme "couchbase")
        (HierarchicalPart
          (Just
            (Authority
              Nothing
              [(Tuple (NameAddress (RegName.fromString "localhost")) Nothing)]))
          (Just (Right (rootDir </> file "testBucket"))))
        (Just (Query.unsafeFromString "password=&docTypeKey=")))
    testIso
      (AbsoluteURI.parser options)
      (AbsoluteURI.print options)
      "couchbase://localhost:99999/testBucket?password=pass&docTypeKey=type&queryTimeoutSeconds=20"
      (AbsoluteURI
        (Scheme "couchbase")
        (HierarchicalPart
          (Just
            (Authority
              Nothing
              [(Tuple (NameAddress (RegName.fromString "localhost")) (Just (Port 99999)))]))
          (Just (Right (rootDir </> file "testBucket"))))
        (Just (Query.unsafeFromString "password=pass&docTypeKey=type&queryTimeoutSeconds=20")))
    testIsoURIRef
      "http://www.example.com/some%20invented/url%20with%20spaces.html"
      (Left
        (URI
          (Scheme "http")
          (HierarchicalPart
            (Just (Authority Nothing [Tuple (NameAddress (RegName.fromString "www.example.com")) Nothing]))
            ((Just (Right ((rootDir </> dir "some invented") </> file "url with spaces.html")))))
          Nothing
          Nothing))
    testIsoURIRef
      "http://localhost:53174/metadata/fs/test/%D0%9F%D0%B0%D1%86%D0%B8%D0%B5%D0%BD%D1%82%D1%8B%23%20%23?"
      (Left
        (URI
          (Scheme "http")
          (HierarchicalPart
            (Just (Authority Nothing [Tuple (NameAddress (RegName.fromString "localhost")) (Just (Port 53174))]))
            ((Just (Right (rootDir </> dir "metadata" </> dir "fs" </> dir "test" </> file "Пациенты# #")))))
          (Just mempty)
          Nothing))

    testIsoURIRef
      "../top_story.htm"
      (Right
        (RelativeRef
          (RelativePart
            Nothing
            (Just (Right (parentDir' currentDir </> file "top_story.htm"))))
          Nothing
          Nothing))

    -- Not an iso in this case as the printed path is normalised
    -- testRunParseURIRefParses
    --   "http://local.slamdata.com/?#?sort=asc&q=path%3A%2F&salt=1177214"
    --   (Left
    --     (URI
    --       (Scheme "http")
    --       (HierarchicalPart
    --         (Just (Authority Nothing [Tuple (NameAddress (RegName.fromString "local.)slamdata.com") Nothing]))
    --         ((Just (Left rootDir))))
    --       ((Just mempty))
    --       ((Just (Fragment.unsafeFromString "?sort=asc&q=path:/&salt=1177214")))))
    -- testPrinter
    --   URIRef.print
    --   "http://local.slamdata.com/?#?sort=asc&q=path:/&salt=1177214"
    --   (Left
    --     (URI
    --       (Scheme "http")
    --       (HierarchicalPart
    --         (Just (Authority Nothing [Tuple (NameAddress (RegName.fromString "local.)slamdata.com") Nothing]))
    --         ((Just (Left rootDir))))
    --       ((Just mempty))
    --       ((Just (Fragment.unsafeFromString "?sort=asc&q=path:/&salt=1177214")))))

    -- testIsoURIRef
    --   "news:comp.infosystems.www.servers.unix"
    --   (Left
    --     (URI
    --       (Scheme "news")
    --       (HierarchicalPart
    --         Nothing
    --         (Just "comp.infosystems.www.servers.unix"))
    --       Nothing
    --       Nothing))
    -- testIsoURIRef
    --   "tel:+1-816-555-1212"
    --   (Left
    --     (URI
    --       (Scheme "tel")
    --       (HierarchicalPart
    --         Nothing
    --         (Just "+1-816-555-1212"))
    --       Nothing
    --       Nothing))
    -- testIsoURIRef
    --   "urn:oasis:names:specification:docbook:dtd:xml:4.1.2"
    --   (Left
    --     (URI
    --       (Scheme "urn")
    --       (HierarchicalPart
    --         Nothing
    --         (Just "oasis:names:specification:docbook:dtd:xml:4.1.2"))
    --       Nothing
    --       Nothing))
    -- testIsoURIRef
    --   "mailto:John.Doe@example.com"
    --   (Left
    --     (URI
    --       (Scheme "mailto")
    --       (HierarchicalPart
    --         Nothing
    --         (Just "John.Doe@example.com"))
    --       Nothing
    --       Nothing))
    -- testIsoURIRef
    --   "mailto:fred@example.com"
    --   (Left
    --     (URI
    --       (Scheme "mailto")
    --       (HierarchicalPart
    --         Nothing
    --         (Just "fred@example.com"))
    --       Nothing
    --       Nothing))

  suite "QueryPairs printer/parser" do
    let testQueryIso = testIso (Query.parser NQP.parse) (Query.print NQP.print)
    testQueryIso
      "?key1=value1&key2=value2&key1=value3"
      (NQP.QueryPairs (Tuple "key1" (Just "value1") : Tuple "key2" (Just "value2") : Tuple "key1" (Just "value3") : Nil))
    testQueryIso
      "?k%3Dey=value%3D1"
      (NQP.QueryPairs (Tuple "k=ey" (Just "value=1") : Nil))
    testQueryIso
      "?"
      (NQP.QueryPairs Nil)
    testQueryIso
      "?key1=&key2="
      (NQP.QueryPairs (Tuple "key1" (Just "") : Tuple "key2" (Just "") : Nil))
    testQueryIso
      "?key1&key2"
      (NQP.QueryPairs (Tuple "key1" Nothing : Tuple "key2" Nothing : Nil))
    testQueryIso
      "?key1=foo%3Bbar"
      (NQP.QueryPairs (Tuple "key1" (Just "foo;bar") : Nil))
    testQueryIso
      "?replicaSet=test&connectTimeoutMS=300000"
      (NQP.QueryPairs (Tuple "replicaSet" (Just "test") : Tuple "connectTimeoutMS" (Just "300000") : Nil))
    testQueryIso
      "?fred"
      (NQP.QueryPairs (pure (Tuple "fred" Nothing)))
    testQueryIso
      "?objectClass?one"
      (NQP.QueryPairs (pure (Tuple "objectClass?one" Nothing)))
    testQueryIso
      "?password=&docTypeKey="
      (NQP.QueryPairs (Tuple "password" (Just "") : Tuple "docTypeKey" (Just "") : Nil))
    testQueryIso
      "?password=pass&docTypeKey=type&queryTimeoutSeconds=20"
      (NQP.QueryPairs (Tuple "password" (Just "pass") : Tuple "docTypeKey" (Just "type") : Tuple "queryTimeoutSeconds" (Just "20") : Nil))

forAll :: forall eff prop. QC.Testable prop => QCG.Gen prop -> Test (console :: CONSOLE, random :: RANDOM, exception :: EXCEPTION | eff)
forAll = quickCheck

quickCheck :: forall eff prop. QC.Testable prop => prop -> Test (console :: CONSOLE, random :: RANDOM, exception :: EXCEPTION | eff)
quickCheck = liftEff <<< QC.quickCheck' 100
