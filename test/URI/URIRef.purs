module Test.URI.URIRef where

import Prelude

import Data.Either (Either(..))
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (un)
import Data.Tuple (Tuple(..))
import Data.URI.Fragment as Fragment
import Data.URI.Host.RegName as RegName
import Data.URI.Path.Segment as PathSegment
import Data.URI.Query as Query
import Data.URI.URIRef (Authority(..), Fragment, HierPath, HierarchicalPart(..), Host(..), Path(..), PathAbsolute(..), PathNoScheme(..), Port(..), Query, RelPath, RelativePart(..), RelativeRef(..), Scheme(..), URI(..), URIRefOptions, UserInfo)
import Data.URI.URIRef as URIRef
import Data.URI.UserInfo as UserInfo
import Test.Unit (TestSuite, suite)
import Test.Util (testIso)
import Text.Parsing.StringParser.Combinators (optionMaybe)

spec ∷ ∀ eff. TestSuite eff
spec =
  suite "URIRef parser/printer" do
    testIso
      (URIRef.parser optionsSingle)
      (URIRef.print optionsSingle)
      "sql2:///?q=foo&var.bar=baz"
      (Left
        (URI
          (Scheme "sql2")
          (HierarchicalPartAuth
            (Authority Nothing Nothing)
            (path [""]))
          (Just (Query.unsafeFromString "q=foo&var.bar=baz"))
          Nothing))
    testIso
      (URIRef.parser optionsSingle)
      (URIRef.print optionsSingle)
      "sql2://?q=foo&var.bar=baz"
      (Left
        (URI
          (Scheme "sql2")
          (HierarchicalPartAuth
            (Authority Nothing Nothing)
            Nothing)
          (Just (Query.unsafeFromString "q=foo&var.bar=baz"))
          Nothing))
    testIso
      (URIRef.parser optionsSingle)
      (URIRef.print optionsSingle)
      "sql2:/?q=foo&var.bar=baz"
      (Left
        (URI
          (Scheme "sql2")
          (HierarchicalPartNoAuth (Just (Left (PathAbsolute Nothing))))
          (Just (Query.unsafeFromString "q=foo&var.bar=baz"))
          Nothing))
    testIso
      (URIRef.parser optionsSingle)
      (URIRef.print optionsSingle)
      "sql2:?q=foo&var.bar=baz"
      (Left
        (URI
          (Scheme "sql2")
          (HierarchicalPartNoAuth Nothing)
          (Just (Query.unsafeFromString "q=foo&var.bar=baz"))
          Nothing))
    testIso
      (URIRef.parser optionsSingle)
      (URIRef.print optionsSingle)
      "mongodb://localhost"
      (Left
        (URI
          (Scheme "mongodb")
          (HierarchicalPartAuth
            (Authority
              Nothing
              (Just (Tuple (NameAddress (RegName.unsafeFromString "localhost")) Nothing)))
            Nothing)
          Nothing
          Nothing))
    testIso
      (URIRef.parser optionsSingle)
      (URIRef.print optionsSingle)
      "https://1a.example.com"
      (Left
        (URI
          (Scheme "https")
          (HierarchicalPartAuth
            (Authority
              Nothing
              (Just (Tuple (NameAddress (RegName.unsafeFromString "1a.example.com")) Nothing)))
            Nothing)
          Nothing
          Nothing))
    testIso
      (URIRef.parser optionsSingle)
      (URIRef.print optionsSingle)
      "http://en.wikipedia.org/wiki/URI_scheme"
      (Left
        (URI
          (Scheme "http")
          (HierarchicalPartAuth
            (Authority
              Nothing
              (Just (Tuple (NameAddress (RegName.unsafeFromString "en.wikipedia.org")) Nothing)))
            (path ["wiki", "URI_scheme"]))
          Nothing
          Nothing))
--     -- testIsoURIRef
--     --   "mongodb://foo:bar@db1.example.net,db2.example.net:2500/authdb?replicaSet=test&connectTimeoutMS=300000"
--     --   (Left
--     --     (URI
--     --       (Scheme "mongodb")
--     --       (HierarchicalPart
--     --         (Just
--     --           (Authority
--     --             (Just (UserInfo.unsafeFromString "foo:bar"))
--     --             [ Tuple (NameAddress (RegName.unsafeFromString "db1.example.net")) Nothing
--     --             , Tuple (NameAddress (RegName.unsafeFromString "db2.example.net")) (Just (Port 2500))]))
--     --         (Just (Right (rootDir </> file "authdb"))))
--     --       (Just (Query.unsafeFromString "replicaSet=test&connectTimeoutMS=300000"))
--     --       Nothing))
--     -- testIsoURIRef
--     --   "mongodb://foo:bar@db1.example.net:6,db2.example.net:2500/authdb?replicaSet=test&connectTimeoutMS=300000"
--     --   (Left
--     --     (URI
--     --       (Scheme "mongodb")
--     --       (HierarchicalPart
--     --         (Just
--     --           (Authority
--     --             (Just (UserInfo.unsafeFromString "foo:bar"))
--     --             [ (Tuple (NameAddress (RegName.unsafeFromString "db1.example.net")) (Just (Port 6)))
--     --             , (Tuple (NameAddress (RegName.unsafeFromString "db2.example.net")) (Just (Port 2500)))]))
--     --         (Just (Right (rootDir </> file "authdb"))))
--     --       (Just (Query.unsafeFromString "replicaSet=test&connectTimeoutMS=300000"))
--     --       Nothing))
    testIso
      (URIRef.parser optionsSingle)
      (URIRef.print optionsSingle)
      "mongodb://192.168.0.1"
      (Left
        (URI
          (Scheme "mongodb")
          (HierarchicalPartAuth
            (Authority Nothing (Just (Tuple (IPv4Address "192.168.0.1") Nothing)))
            Nothing)
          Nothing
          Nothing))
--     -- testIsoURIRef
--     --   "mongodb://192.168.0.1,192.168.0.2"
--     --   (Left
--     --     (URI
--     --       (Scheme "mongodb")
--     --       (HierarchicalPart
--     --         (Just
--     --           (Authority
--     --             Nothing
--     --             [ Tuple (IPv4Address "192.168.0.1") Nothing
--     --             , Tuple (IPv4Address "192.168.0.2") Nothing
--     --             ]))
--     --         Nothing)
--     --       Nothing
--     --       Nothing))
    testIso
      (URIRef.parser optionsSingle)
      (URIRef.print optionsSingle)
      "mongodb://sysop:moon@localhost"
      (Left
        (URI
          (Scheme "mongodb")
          (HierarchicalPartAuth
            (Authority
                (Just (UserInfo.unsafeFromString "sysop:moon"))
                (Just (Tuple (NameAddress (RegName.unsafeFromString "localhost")) Nothing)))
            Nothing)
          Nothing
          Nothing))
    testIso
      (URIRef.parser optionsSingle)
      (URIRef.print optionsSingle)
      "mongodb://sysop:moon@localhost/"
      (Left
        (URI
          (Scheme "mongodb")
          (HierarchicalPartAuth
            (Authority
                (Just (UserInfo.unsafeFromString "sysop:moon"))
                (Just (Tuple (NameAddress (RegName.unsafeFromString "localhost")) Nothing)))
            (path [""]))
          Nothing
          Nothing))
    testIso
      (URIRef.parser optionsSingle)
      (URIRef.print optionsSingle)
      "mongodb://sysop:moon@localhost/records"
      (Left
        (URI
          (Scheme "mongodb")
          (HierarchicalPartAuth
            (Authority
                (Just (UserInfo.unsafeFromString "sysop:moon"))
                (Just (Tuple (NameAddress (RegName.unsafeFromString "localhost")) Nothing)))
            (path ["records"]))
          Nothing
          Nothing))
    testIso
      (URIRef.parser optionsSingle)
      (URIRef.print optionsSingle)
      "mongodb://sysop:moon@localhost/records/etc/"
      (Left
        (URI
          (Scheme "mongodb")
          (HierarchicalPartAuth
            (Authority
                (Just (UserInfo.unsafeFromString "sysop:moon"))
                (Just (Tuple (NameAddress (RegName.unsafeFromString "localhost")) Nothing)))
            (path ["records", "etc", ""]))
          Nothing
          Nothing))
    testIso
      (URIRef.parser optionsSingle)
      (URIRef.print optionsSingle)
      "foo://[2001:cdba:0000:0000:0000:0000:3257:9652]"
      (Left
        (URI
          (Scheme "foo")
          (HierarchicalPartAuth
            (Authority
              Nothing
              (Just (Tuple (IPv6Address "2001:cdba:0000:0000:0000:0000:3257:9652") Nothing)))
            Nothing)
          Nothing
          Nothing))
    testIso
      (URIRef.parser optionsSingle)
      (URIRef.print optionsSingle)
      "foo://[FE80::0202:B3FF:FE1E:8329]"
      (Left
        (URI
          (Scheme "foo")
          (HierarchicalPartAuth
            (Authority
              Nothing
              (Just (Tuple (IPv6Address "FE80::0202:B3FF:FE1E:8329") Nothing)))
            Nothing)
          Nothing
          Nothing))
    testIso
      (URIRef.parser optionsSingle)
      (URIRef.print optionsSingle)
      "foo://[2001:db8::1]:80"
      (Left
        (URI
          (Scheme "foo")
          (HierarchicalPartAuth
            (Authority
              Nothing
              (Just (Tuple (IPv6Address "2001:db8::1") (Just (Port 80)))))
            Nothing)
          Nothing
          Nothing))
    testIso
      (URIRef.parser optionsSingle)
      (URIRef.print optionsSingle)
      "ftp://ftp.is.co.za/rfc/rfc1808.txt"
      (Left
        (URI
          (Scheme "ftp")
          (HierarchicalPartAuth
            (Authority
              Nothing
              (Just (Tuple (NameAddress (RegName.unsafeFromString "ftp.is.co.za")) Nothing)))
            (path ["rfc", "rfc1808.txt"]))
          Nothing
          Nothing))
    testIso
      (URIRef.parser optionsSingle)
      (URIRef.print optionsSingle)
      "http://www.ietf.org/rfc/rfc2396.txt"
      (Left
        (URI
          (Scheme "http")
          (HierarchicalPartAuth
            (Authority
              Nothing
              (Just (Tuple (NameAddress (RegName.unsafeFromString "www.ietf.org")) Nothing)))
            (path ["rfc", "rfc2396.txt"]))
          Nothing
          Nothing))
    testIso
      (URIRef.parser optionsSingle)
      (URIRef.print optionsSingle)
      "ldap://[2001:db8::7]/c=GB?objectClass?one"
      (Left
        (URI
          (Scheme "ldap")
          (HierarchicalPartAuth
            (Authority
              Nothing
              (Just (Tuple (IPv6Address "2001:db8::7") Nothing)))
            (path ["c=GB"]))
          (Just (Query.unsafeFromString "objectClass?one"))
          Nothing))
    testIso
      (URIRef.parser optionsSingle)
      (URIRef.print optionsSingle)
      "telnet://192.0.2.16:80/"
      (Left
        (URI
          (Scheme "telnet")
          (HierarchicalPartAuth
            (Authority
              Nothing
              (Just (Tuple (IPv4Address "192.0.2.16") (Just (Port 80)))))
            (path [""]))
          Nothing
          Nothing))
    testIso
      (URIRef.parser optionsSingle)
      (URIRef.print optionsSingle)
      "foo://example.com:8042/over/there?name=ferret#nose"
      (Left
        (URI
          (Scheme "foo")
          (HierarchicalPartAuth
            (Authority
              Nothing
              (Just (Tuple (NameAddress (RegName.unsafeFromString "example.com")) (Just (Port 8042)))))
            (path ["over", "there"]))
          (Just (Query.unsafeFromString "name=ferret"))
          (Just (Fragment.unsafeFromString "nose"))))
    testIso
      (URIRef.parser optionsSingle)
      (URIRef.print optionsSingle)
      "foo://example.com:8042/over/there?name=ferret#"
      (Left
        (URI
          (Scheme "foo")
          (HierarchicalPartAuth
            (Authority
              Nothing
              (Just (Tuple (NameAddress (RegName.unsafeFromString "example.com")) (Just (Port 8042)))))
            (path ["over", "there"]))
          (Just (Query.unsafeFromString "name=ferret"))
          (Just (Fragment.unsafeFromString ""))))
    testIso
      (URIRef.parser optionsSingle)
      (URIRef.print optionsSingle)
      "foo://info.example.com?fred"
      (Left
        (URI
          (Scheme "foo")
          (HierarchicalPartAuth
            (Authority
              Nothing
              (Just (Tuple (NameAddress (RegName.unsafeFromString "info.example.com")) Nothing)))
            Nothing)
          (Just (Query.unsafeFromString "fred"))
          Nothing))
    testIso
      (URIRef.parser optionsSingle)
      (URIRef.print optionsSingle)
      "ftp://cnn.example.com&story=breaking_news@10.0.0.1/top_story.htm"
      (Left
        (URI
          (Scheme "ftp")
          (HierarchicalPartAuth
            (Authority
              (Just (UserInfo.unsafeFromString "cnn.example.com&story=breaking_news"))
              (Just (Tuple (IPv4Address "10.0.0.1") Nothing)))
            (path ["top_story.htm"]))
          Nothing
          Nothing))
    testIso
      (URIRef.parser optionsSingle)
      (URIRef.print optionsSingle)
      "top_story.htm"
      (Right
        (RelativeRef
          (RelativePartNoAuth (Just (Right (PathNoScheme (Tuple (PathSegment.unsafeSegmentNZNCFromString "top_story.htm") [])))))
          Nothing
          Nothing))
    testIso
      (URIRef.parser optionsSingle)
      (URIRef.print optionsSingle)
      "../top_story.htm"
      (Right
        (RelativeRef
          (RelativePartNoAuth (Just (Right (PathNoScheme (Tuple (PathSegment.unsafeSegmentNZNCFromString "..") [PathSegment.unsafeSegmentFromString "top_story.htm"])))))
          Nothing
          Nothing))
    testIso
      (URIRef.parser optionsSingle)
      (URIRef.print optionsSingle)
      "/top_story.htm"
      (Right
        (RelativeRef
          (RelativePartNoAuth (Just (Left (PathAbsolute (Just (Tuple (PathSegment.unsafeSegmentNZFromString "top_story.htm") []))))))
          Nothing
          Nothing))
    testIso
      (URIRef.parser optionsSingle)
      (URIRef.print optionsSingle)
      "/"
      (Right
        (RelativeRef
          (RelativePartNoAuth (Just (Left (PathAbsolute Nothing))))
          Nothing
          Nothing))
    testIso
      (URIRef.parser optionsSingle)
      (URIRef.print optionsSingle)
      ""
      (Right
        (RelativeRef
          (RelativePartNoAuth Nothing)
          Nothing
          Nothing))
    testIso
      (URIRef.parser optionsSingle)
      (URIRef.print optionsSingle)
      "http://www.example.com/some%20invented/url%20with%20spaces.html"
      (Left
        (URI
          (Scheme "http")
          (HierarchicalPartAuth
            (Authority Nothing (Just (Tuple (NameAddress (RegName.fromString "www.example.com")) Nothing)))
            (path ["some%20invented", "url%20with%20spaces.html"]))
          Nothing
          Nothing))
    testIso
      (URIRef.parser optionsSingle)
      (URIRef.print optionsSingle)
      "http://localhost:53174/metadata/fs/test/%D0%9F%D0%B0%D1%86%D0%B8%D0%B5%D0%BD%D1%82%D1%8B%23%20%23?"
      (Left
        (URI
          (Scheme "http")
          (HierarchicalPartAuth
            (Authority Nothing (Just (Tuple (NameAddress (RegName.fromString "localhost")) (Just (Port 53174)))))
            (path ["metadata", "fs", "test", "%D0%9F%D0%B0%D1%86%D0%B8%D0%B5%D0%BD%D1%82%D1%8B%23%20%23"]))
          (Just (Query.unsafeFromString ""))
          Nothing))
--     -- testIsoURIRef
--     --   "http://localhost:53174/metadata/fs/test/%D0%9F%D0%B0%D1%86%D0%B8%D0%B5%D0%BD%D1%82%D1%8B%23%20%23?"
--     --   (Left
--     --     (URI
--     --       (Scheme "http")
--     --       (HierarchicalPart
--     --         (Just (Authority Nothing [Tuple (NameAddress (RegName.fromString "localhost")) (Just (Port 53174))]))
--     --         ((Just (Right (rootDir </> dir "metadata" </> dir "fs" </> dir "test" </> file "Пациенты# #")))))
--     --       (Just mempty)
--     --       Nothing))
--     --
--     -- testIsoURIRef
--     --   "../top_story.htm"
--     --   (Right
--     --     (RelativeRef
--     --       (RelativePart
--     --         Nothing
--     --         (Just (Right (parentDir' currentDir </> file "top_story.htm"))))
--     --       Nothing
--     --       Nothing))

--     -- testIsoURIRef
--     --   "news:comp.infosystems.www.servers.unix"
--     --   (Left
--     --     (URI
--     --       (Scheme "news")
--     --       (HierarchicalPart
--     --         Nothing
--     --         (Just "comp.infosystems.www.servers.unix"))
--     --       Nothing
--     --       Nothing))
--     -- testIsoURIRef
--     --   "tel:+1-816-555-1212"
--     --   (Left
--     --     (URI
--     --       (Scheme "tel")
--     --       (HierarchicalPart
--     --         Nothing
--     --         (Just "+1-816-555-1212"))
--     --       Nothing
--     --       Nothing))
--     -- testIsoURIRef
--     --   "urn:oasis:names:specification:docbook:dtd:xml:4.1.2"
--     --   (Left
--     --     (URI
--     --       (Scheme "urn")
--     --       (HierarchicalPart
--     --         Nothing
--     --         (Just "oasis:names:specification:docbook:dtd:xml:4.1.2"))
--     --       Nothing
--     --       Nothing))
--     -- testIsoURIRef
--     --   "mailto:John.Doe@example.com"
--     --   (Left
--     --     (URI
--     --       (Scheme "mailto")
--     --       (HierarchicalPart
--     --         Nothing
--     --         (Just "John.Doe@example.com"))
--     --       Nothing
--     --       Nothing))
--     -- testIsoURIRef
--     --   "mailto:fred@example.com"
--     --   (Left
--     --     (URI
--     --       (Scheme "mailto")
--     --       (HierarchicalPart
--     --         Nothing
--     --         (Just "fred@example.com"))
--     --       Nothing
--     --       Nothing))

path ∷ Array String → Maybe Path
path = Just <<< Path <<< map PathSegment.unsafeSegmentFromString

optionsSingle ∷ Record (URIRefOptions UserInfo Maybe Host Port Path HierPath RelPath Query Fragment)
optionsSingle =
  { parseUserInfo: pure
  , printUserInfo: id
  , parseHosts: optionMaybe
  , printHosts: fromMaybe ""
  , parseHost: pure
  , printHost: id
  , parsePort: pure
  , printPort: id
  , parsePath: pure
  , printPath: id
  , parseHierPath: pure
  , printHierPath: id
  , parseRelPath: pure
  , printRelPath: id
  , parseQuery: pure
  , printQuery: id
  , parseFragment: pure
  , printFragment: id
  }
