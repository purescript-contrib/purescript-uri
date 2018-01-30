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
import Data.URI (AbsoluteURI(..), Authority(..), HierarchicalPart(..), Host(..), Port(..), RelativePart(..), RelativeRef(..), Scheme(..), URI(..))
import Data.URI.AbsoluteURI as AbsoluteURI
import Data.URI.Authority as Authority
import Data.URI.Fragment as Fragment
import Data.URI.Host as Host
import Data.URI.Host.Gen as Host.Gen
import Data.URI.NonStandard.Fragment as NF
import Data.URI.NonStandard.Path as NP
import Data.URI.NonStandard.Query as NQ
import Data.URI.NonStandard.UserInfo as NU
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

options ∷ Record (URIRef.URIRefOptions NU.UserInfo Array NP.URIPathAbs NP.URIPathRel NQ.Query NF.Fragment)
options =
  { parseUserInfo: NU.parse
  , printUserInfo: NU.print
  , parseHosts: map Array.fromFoldable <<< flip SP.sepBy (SP.string ",")
  , printHosts: String.joinWith ","
  , parseHierPath: NP.parseURIPathAbs
  , printHierPath: NP.printPath
  , parseRelPath: NP.parseURIPathRel
  , printRelPath: NP.printPath
  , parseQuery: NQ.parse
  , printQuery: NQ.print
  , parseFragment: NF.parse
  , printFragment: NF.print
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

testIsoURI :: forall a. String -> URI.URI NU.UserInfo Array NP.URIPathAbs NQ.Query NF.Fragment -> TestSuite a
testIsoURI = testIso (URI.parser options) (URI.print options)

testIsoURIRef :: forall a. String -> URIRef.URIRef NU.UserInfo Array NP.URIPathAbs NP.URIPathRel NQ.Query NF.Fragment -> TestSuite a
testIsoURIRef = testIso (URIRef.parser options) (URIRef.print options)

-- testRunParseURIRefParses :: forall a. String -> Either URI RelativeRef -> TestSuite a
-- testRunParseURIRefParses = testRunParseSuccess URIRef.parser

testRunParseURIRefFails :: forall a. String -> TestSuite a
testRunParseURIRefFails uri =
  test
    ("fails to parse: " <> uri)
    (assert ("parse should fail for: " <> uri) <<< isLeft <<< runParser (URIRef.parser options) $ uri)

testPrintQuerySerializes :: forall a. NQ.Query -> String -> TestSuite a
testPrintQuerySerializes query expected =
  test
    ("serializes: " <> show query)
    (equal expected (Query.print NQ.print query))

testParseQueryParses :: forall a. String -> NQ.Query -> TestSuite a
testParseQueryParses uri query =
  test
    ("parses: \"" <> uri <> "\"")
    (equal (Right query) (runParser (Query.parser NQ.parse) uri))

-- testMatch1FromMatches :: forall a. Either String Regex -> Int -> String -> Maybe String -> TestSuite a
-- testMatch1FromMatches rx' n str expected = case rx' of
--   Left error -> test "faulty regex given" (assert error false)
--   Right rx ->
--     test
--       ("matches: " <> show rx <> " at " <> show n <> " in " <> show str)
--       (equal expected $ Common.match1From rx n str)
--
-- testMatch1FromMisses :: forall a. Either String Regex -> Int -> String -> TestSuite a
-- testMatch1FromMisses rx' n str = case rx' of
--   Left error -> test "faulty regex given" (assert error false)
--   Right rx ->
--     test
--     ("does not match: " <> show rx <> " at " <> show n <> " in " <> show str)
--     (equal Nothing $ Common.match1From rx n str)

main :: forall eff. Eff (console :: CONSOLE, testOutput :: TESTOUTPUT, avar :: AVAR, exception :: EXCEPTION, random :: RANDOM | eff) Unit
main = runTest $ suite "Data.URI" do

  suite "parseIPv4Address" do

    test "parseIPv4Address / Host.print roundtrip" do
      forAll do
        ipv4 <- Host.Gen.genIPv4
        let printed = Host.print ipv4
        let parsed = runParser Host.ipv4AddressParser printed
        pure $ pure ipv4 === parsed

    test "0-lead octets should not parse" do
      assert ("parse should fail for 192.168.001.1") $
        isLeft $ runParser Host.ipv4AddressParser "192.168.001.1"

  suite "Scheme parser" do
    testRunParseSuccess Scheme.parser "http:" (Scheme "http")
    testRunParseSuccess Scheme.parser "git+ssh:" (Scheme "git+ssh")

  suite "UserInfo parser" do
    testRunParseSuccess (UserInfo.parser NU.parse) "user" (NU.UserInfo "user")
    testRunParseSuccess (UserInfo.parser NU.parse) "spaced%20user" (NU.UserInfo "spaced user")
    testRunParseSuccess (UserInfo.parser NU.parse) "user:password" (NU.UserInfo "user:password")
    testRunParseSuccess (UserInfo.parser NU.parse) "spaced%20user:password%25%C2%A3" (NU.UserInfo "spaced user:password%£")

  suite "Host parser" do
    testRunParseSuccess Host.parser "localhost" (NameAddress "localhost")
    testRunParseSuccess Host.parser "github.com" (NameAddress "github.com")
    testRunParseSuccess Host.parser "www.multipart.domain.example.com" (NameAddress "www.multipart.domain.example.com")
    testRunParseSuccess Host.parser "192.168.0.1" (IPv4Address "192.168.0.1")
    testRunParseSuccess Host.parser "[2001:cdba:0000:0000:0000:0000:3257:9652]" (IPv6Address "2001:cdba:0000:0000:0000:0000:3257:9652")

  suite "Port parser" do
    testRunParseSuccess Port.parser "0" (Port 0)
    testRunParseSuccess Port.parser "1234" (Port 1234)
    testRunParseSuccess Port.parser "63174" (Port 63174)

  suite "Fragment' parser" do
    testRunParseSuccess (Fragment.parser Right) "#" ""

  suite "Authority parser" do
    testRunParseSuccess
      (Authority.parser options)
      "//localhost"
      (Authority Nothing [Tuple (NameAddress "localhost") Nothing])
    testRunParseSuccess
      (Authority.parser options)
      "//localhost:3000"
      (Authority Nothing [Tuple (NameAddress "localhost") (Just (Port 3000))])

  suite "URIRef.parse" do
    testIsoURIRef
      "sql2:///?q=foo&var.bar=baz"
      (Left
        (URI
          (Scheme "sql2")
          (HierarchicalPart
            (Just (Authority Nothing []))
            (Just (Left rootDir)))
          (Just (NQ.Query (Tuple "q" (Just "foo") : Tuple "var.bar" (Just "baz") : Nil)))
          Nothing))
    testIsoURIRef
      "mongodb://localhost"
      (Left
        (URI
          (Scheme "mongodb")
          (HierarchicalPart
            (Just (Authority Nothing [(Tuple (NameAddress "localhost") Nothing)]))
            Nothing)
          Nothing
          Nothing))
    testIsoURIRef
      "https://1a.example.com"
      (Left
        (URI
          (Scheme "https")
          (HierarchicalPart
            (Just (Authority Nothing [(Tuple (NameAddress "1a.example.com") Nothing)]))
            Nothing)
          Nothing
          Nothing))
    testIsoURIRef
      "http://en.wikipedia.org/wiki/URI_scheme"
      (Left
        (URI
          (Scheme "http")
          (HierarchicalPart
            (Just (Authority Nothing [Tuple (NameAddress "en.wikipedia.org") Nothing]))
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
                (Just (NU.UserInfo "foo:bar"))
                [ Tuple (NameAddress "db1.example.net") Nothing
                , Tuple (NameAddress "db2.example.net") (Just (Port 2500))]))
            (Just (Right (rootDir </> file "authdb"))))
          (Just
            (NQ.Query
              (Tuple "replicaSet" (Just "test") : Tuple "connectTimeoutMS" (Just "300000") : Nil)))
          Nothing))
    testIsoURIRef
      "mongodb://foo:bar@db1.example.net:6,db2.example.net:2500/authdb?replicaSet=test&connectTimeoutMS=300000"
      (Left
        (URI
          (Scheme "mongodb")
          (HierarchicalPart
            (Just (Authority (Just (NU.UserInfo "foo:bar")) [(Tuple (NameAddress "db1.example.net") (Just (Port 6))),(Tuple (NameAddress "db2.example.net") (Just (Port 2500)))]))
            (Just (Right (rootDir </> file "authdb"))))
          (Just (NQ.Query (Tuple "replicaSet" (Just "test") : Tuple "connectTimeoutMS" (Just "300000") : Nil)))
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
            (Just(Authority (Just (NU.UserInfo "sysop:moon")) [(Tuple (NameAddress "localhost") Nothing)]))
            Nothing)
          Nothing
          Nothing))
    testIsoURIRef
      "mongodb://sysop:moon@localhost/"
      (Left
        (URI
          (Scheme "mongodb")
          (HierarchicalPart
            (Just (Authority (Just (NU.UserInfo "sysop:moon")) [(Tuple (NameAddress "localhost") Nothing)]))
            (Just (Left rootDir)))
          Nothing
          Nothing))
    testIsoURIRef
      "mongodb://sysop:moon@localhost/records"
      (Left
        (URI
          (Scheme "mongodb")
          (HierarchicalPart
            (Just (Authority (Just (NU.UserInfo "sysop:moon")) [(Tuple (NameAddress "localhost") Nothing)]))
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
            (Just (Authority Nothing [(Tuple (NameAddress "ftp.is.co.za") Nothing)]))
            (Just (Right ((rootDir </> dir "rfc") </> file "rfc1808.txt"))))
          Nothing
          Nothing))
    testIsoURIRef
      "http://www.ietf.org/rfc/rfc2396.txt"
      (Left
        (URI
          (Scheme "http")
          (HierarchicalPart (Just (Authority Nothing [(Tuple (NameAddress "www.ietf.org") Nothing)])) (Just (Right ((rootDir </> dir "rfc") </> file "rfc2396.txt"))))
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
          (Just (NQ.Query (singleton $ (Tuple "objectClass?one" Nothing))))
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
          (HierarchicalPart (Just (Authority Nothing [(Tuple (NameAddress "example.com") (Just (Port 8042)))])) (Just (Right ((rootDir </> dir "over") </> file "there"))))
          (Just (NQ.Query (singleton (Tuple "name" (Just "ferret")))))
          (Just (NF.Fragment "nose"))))
    testIsoURIRef
      "foo://example.com:8042/over/there?name=ferret#"
      (Left
        (URI
          (Scheme "foo")
          (HierarchicalPart (Just (Authority Nothing [(Tuple (NameAddress "example.com") (Just (Port 8042)))])) (Just (Right ((rootDir </> dir "over") </> file "there"))))
          (Just (NQ.Query (singleton (Tuple "name" (Just "ferret")))))
          (Just (NF.Fragment ""))))
    testIsoURIRef
      "foo://info.example.com?fred"
      (Left
        (URI
          (Scheme "foo")
          (HierarchicalPart
            (Just (Authority Nothing [(Tuple (NameAddress "info.example.com") Nothing)]))
            Nothing)
          (Just (NQ.Query (singleton $ Tuple "fred" Nothing)))
          Nothing))
    testIsoURIRef
      "ftp://cnn.example.com&story=breaking_news@10.0.0.1/top_story.htm"
      (Left
        (URI
          (Scheme "ftp")
          (HierarchicalPart
            (Just
              (Authority
                (Just (NU.UserInfo "cnn.example.com&story=breaking_news"))
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
              [(Tuple (NameAddress "localhost") Nothing)]))
          (Just (Right (rootDir </> file "testBucket"))))
        (Just (NQ.Query (Tuple "password" (Just "") : Tuple "docTypeKey" (Just "") : Nil))))
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
              [(Tuple (NameAddress "localhost") (Just (Port 99999)))]))
          (Just (Right (rootDir </> file "testBucket"))))
        (Just (NQ.Query (Tuple "password" (Just "pass") : Tuple "docTypeKey" (Just "type") : Tuple "queryTimeoutSeconds" (Just "20") : Nil))))
    testIsoURIRef
      "http://www.example.com/some%20invented/url%20with%20spaces.html"
      (Left
        (URI
          (Scheme "http")
          (HierarchicalPart
            (Just (Authority Nothing [Tuple (NameAddress "www.example.com") Nothing]))
            ((Just (Right ((rootDir </> dir "some invented") </> file "url with spaces.html")))))
          Nothing
          Nothing))
    testIsoURIRef
      "http://localhost:53174/metadata/fs/test/%D0%9F%D0%B0%D1%86%D0%B8%D0%B5%D0%BD%D1%82%D1%8B%23%20%23?"
      (Left
        (URI
          (Scheme "http")
          (HierarchicalPart
            (Just (Authority Nothing [Tuple (NameAddress "localhost") (Just (Port 53174))]))
            ((Just (Right (rootDir </> dir "metadata" </> dir "fs" </> dir "test" </> file "Пациенты# #")))))
          (Just mempty)
          Nothing))

    -- testIsoURIRef
    --   "../top_story.htm"
    --   (Right
    --     (RelativeRef
    --       (RelativePart
    --         Nothing
    --         (Just (Right (parentDir' currentDir </> file "top_story.htm"))))
    --       Nothing
    --       Nothing))

    -- Not an iso in this case as the printed path is normalised
    -- testRunParseURIRefParses
    --   "http://local.slamdata.com/?#?sort=asc&q=path%3A%2F&salt=1177214"
    --   (Left
    --     (URI
    --       (Scheme "http")
    --       (HierarchicalPart
    --         (Just (Authority Nothing [Tuple (NameAddress "local.slamdata.com") Nothing]))
    --         ((Just (Left rootDir))))
    --       ((Just mempty))
    --       ((Just (Fragment "?sort=asc&q=path:/&salt=1177214")))))
    -- testPrinter
    --   URIRef.print
    --   "http://local.slamdata.com/?#?sort=asc&q=path:/&salt=1177214"
    --   (Left
    --     (URI
    --       (Scheme "http")
    --       (HierarchicalPart
    --         (Just (Authority Nothing [Tuple (NameAddress "local.slamdata.com") Nothing]))
    --         ((Just (Left rootDir))))
    --       ((Just mempty))
    --       ((Just (Fragment "?sort=asc&q=path:/&salt=1177214")))))

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

  suite "Query.print" do
    testPrintQuerySerializes
      (NQ.Query (Tuple "key1" (Just "value1") : Tuple "key2" (Just "value2") : Tuple "key1" (Just "value3") : Nil))
      "?key1=value1&key2=value2&key1=value3"
    testPrintQuerySerializes
      (NQ.Query (Tuple "k=ey" (Just "value=1") : Nil))
      "?k%3Dey=value%3D1"
    testPrintQuerySerializes (NQ.Query Nil) "?"
    testPrintQuerySerializes
      (NQ.Query (Tuple "key1" (Just "") : Tuple "key2" (Just "") : Nil))
      "?key1=&key2="
    testPrintQuerySerializes
      (NQ.Query (Tuple "key1" Nothing : Tuple "key2" Nothing : Nil))
      "?key1&key2"
    testPrintQuerySerializes
      (NQ.Query (Tuple "key1" (Just "foo;bar") : Nil))
      "?key1=foo%3Bbar"

  suite "Query.parser" do
    testParseQueryParses
      "?key1=value1&key2=value2&key1=value3"
      (NQ.Query (Tuple "key1" (Just "value1") : Tuple "key2" (Just "value2") : Tuple "key1" (Just "value3") : Nil))
    testParseQueryParses
      "?key1&key2"
      (NQ.Query (Tuple "key1" Nothing : Tuple "key2" Nothing : Nil))
    testParseQueryParses
      "?key1=&key2="
      (NQ.Query (Tuple "key1" (Just "") : Tuple "key2" (Just "") : Nil))
    testParseQueryParses
      "?key1=foo%3Bbar"
      (NQ.Query (Tuple "key1" (Just "foo;bar") : Nil))

forAll :: forall eff prop. QC.Testable prop => QCG.Gen prop -> Test (console :: CONSOLE, random :: RANDOM, exception :: EXCEPTION | eff)
forAll = quickCheck

quickCheck :: forall eff prop. QC.Testable prop => prop -> Test (console :: CONSOLE, random :: RANDOM, exception :: EXCEPTION | eff)
quickCheck = liftEff <<< QC.quickCheck' 100
