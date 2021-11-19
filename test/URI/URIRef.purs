module Test.URI.URIRef where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String.NonEmpty (nes)
import Type.Proxy (Proxy(..))
import Data.These (These(..))
import Data.Tuple (Tuple(..))
import Test.Spec (Spec, describe)
import Test.Util (testIso)
import URI.Fragment as Fragment
import URI.Host.IPv4Address as IPv4Address
import URI.Host.IPv6Address as IPv6Address
import URI.Host.RegName as RegName
import URI.HostPortPair (HostPortPair)
import URI.HostPortPair as HostPortPair
import URI.Path.Segment as PathSegment
import URI.Port as Port
import URI.Query as Query
import URI.Scheme as Scheme
import URI.URIRef (Authority(..), Fragment, HierPath, HierarchicalPart(..), Host(..), Path(..), PathAbsolute(..), PathNoScheme(..), PathRootless(..), Port, Query, RelPath, RelativePart(..), RelativeRef(..), URI(..), URIRefOptions, UserInfo)
import URI.URIRef as URIRef
import URI.UserInfo as UserInfo

spec :: Spec Unit
spec =
  describe "URIRef parser/printer" do
    testIso
      (URIRef.parser options)
      (URIRef.print options)
      "sql2:///?q=foo&var.bar=baz"
      ( Left
          ( URI
              (Scheme.unsafeFromString "sql2")
              ( HierarchicalPartAuth
                  (Authority Nothing Nothing)
                  (path [ "" ])
              )
              (Just (Query.unsafeFromString "q=foo&var.bar=baz"))
              Nothing
          )
      )
    testIso
      (URIRef.parser options)
      (URIRef.print options)
      "sql2://?q=foo&var.bar=baz"
      ( Left
          ( URI
              (Scheme.unsafeFromString "sql2")
              ( HierarchicalPartAuth
                  (Authority Nothing Nothing)
                  (path [])
              )
              (Just (Query.unsafeFromString "q=foo&var.bar=baz"))
              Nothing
          )
      )
    testIso
      (URIRef.parser options)
      (URIRef.print options)
      "sql2:/?q=foo&var.bar=baz"
      ( Left
          ( URI
              (Scheme.unsafeFromString "sql2")
              (HierarchicalPartNoAuth (Just (Left (PathAbsolute Nothing))))
              (Just (Query.unsafeFromString "q=foo&var.bar=baz"))
              Nothing
          )
      )
    testIso
      (URIRef.parser options)
      (URIRef.print options)
      "sql2:?q=foo&var.bar=baz"
      ( Left
          ( URI
              (Scheme.unsafeFromString "sql2")
              (HierarchicalPartNoAuth Nothing)
              (Just (Query.unsafeFromString "q=foo&var.bar=baz"))
              Nothing
          )
      )
    testIso
      (URIRef.parser options)
      (URIRef.print options)
      "mongodb://localhost"
      ( Left
          ( URI
              (Scheme.unsafeFromString "mongodb")
              ( HierarchicalPartAuth
                  ( Authority
                      Nothing
                      (Just (This (NameAddress (RegName.unsafeFromString $ nes (Proxy :: Proxy "localhost")))))
                  )
                  (path [])
              )
              Nothing
              Nothing
          )
      )
    testIso
      (URIRef.parser options)
      (URIRef.print options)
      "https://1a.example.com"
      ( Left
          ( URI
              (Scheme.unsafeFromString "https")
              ( HierarchicalPartAuth
                  ( Authority
                      Nothing
                      (Just (This (NameAddress (RegName.unsafeFromString $ nes (Proxy :: Proxy "1a.example.com")))))
                  )
                  (path [])
              )
              Nothing
              Nothing
          )
      )
    testIso
      (URIRef.parser options)
      (URIRef.print options)
      "http://en.wikipedia.org/wiki/URI_scheme"
      ( Left
          ( URI
              (Scheme.unsafeFromString "http")
              ( HierarchicalPartAuth
                  ( Authority
                      Nothing
                      (Just (This (NameAddress (RegName.unsafeFromString $ nes (Proxy :: Proxy "en.wikipedia.org")))))
                  )
                  (path [ "wiki", "URI_scheme" ])
              )
              Nothing
              Nothing
          )
      )
    testIso
      (URIRef.parser options)
      (URIRef.print options)
      "mongodb://192.168.0.1"
      ( Left
          ( URI
              (Scheme.unsafeFromString "mongodb")
              ( HierarchicalPartAuth
                  ( Authority
                      Nothing
                      (Just (This (IPv4Address (IPv4Address.unsafeFromInts 192 168 0 1))))
                  )
                  (path [])
              )
              Nothing
              Nothing
          )
      )
    testIso
      (URIRef.parser options)
      (URIRef.print options)
      "mongodb://sysop:moon@localhost"
      ( Left
          ( URI
              (Scheme.unsafeFromString "mongodb")
              ( HierarchicalPartAuth
                  ( Authority
                      (Just (UserInfo.unsafeFromString (nes (Proxy :: Proxy "sysop:moon"))))
                      (Just (This (NameAddress (RegName.unsafeFromString $ nes (Proxy :: Proxy "localhost")))))
                  )
                  (path [])
              )
              Nothing
              Nothing
          )
      )
    testIso
      (URIRef.parser options)
      (URIRef.print options)
      "mongodb://sysop:moon@localhost/"
      ( Left
          ( URI
              (Scheme.unsafeFromString "mongodb")
              ( HierarchicalPartAuth
                  ( Authority
                      (Just (UserInfo.unsafeFromString (nes (Proxy :: Proxy "sysop:moon"))))
                      (Just (This (NameAddress (RegName.unsafeFromString $ nes (Proxy :: Proxy "localhost")))))
                  )
                  (path [ "" ])
              )
              Nothing
              Nothing
          )
      )
    testIso
      (URIRef.parser options)
      (URIRef.print options)
      "mongodb://sysop:moon@localhost/records"
      ( Left
          ( URI
              (Scheme.unsafeFromString "mongodb")
              ( HierarchicalPartAuth
                  ( Authority
                      (Just (UserInfo.unsafeFromString (nes (Proxy :: Proxy "sysop:moon"))))
                      (Just (This (NameAddress (RegName.unsafeFromString $ nes (Proxy :: Proxy "localhost")))))
                  )
                  (path [ "records" ])
              )
              Nothing
              Nothing
          )
      )
    testIso
      (URIRef.parser options)
      (URIRef.print options)
      "mongodb://sysop:moon@localhost/records/etc/"
      ( Left
          ( URI
              (Scheme.unsafeFromString "mongodb")
              ( HierarchicalPartAuth
                  ( Authority
                      (Just (UserInfo.unsafeFromString (nes (Proxy :: Proxy "sysop:moon"))))
                      (Just (This (NameAddress (RegName.unsafeFromString $ nes (Proxy :: Proxy "localhost")))))
                  )
                  (path [ "records", "etc", "" ])
              )
              Nothing
              Nothing
          )
      )
    testIso
      (URIRef.parser options)
      (URIRef.print options)
      "foo://[2001:cdba:0000:0000:0000:0000:3257:9652]"
      ( Left
          ( URI
              (Scheme.unsafeFromString "foo")
              ( HierarchicalPartAuth
                  ( Authority
                      Nothing
                      (Just (This (IPv6Address (IPv6Address.unsafeFromString "2001:cdba:0000:0000:0000:0000:3257:9652"))))
                  )
                  (path [])
              )
              Nothing
              Nothing
          )
      )
    testIso
      (URIRef.parser options)
      (URIRef.print options)
      "foo://[FE80::0202:B3FF:FE1E:8329]"
      ( Left
          ( URI
              (Scheme.unsafeFromString "foo")
              ( HierarchicalPartAuth
                  ( Authority
                      Nothing
                      (Just (This (IPv6Address (IPv6Address.unsafeFromString "FE80::0202:B3FF:FE1E:8329"))))
                  )
                  (path [])
              )
              Nothing
              Nothing
          )
      )
    testIso
      (URIRef.parser options)
      (URIRef.print options)
      "foo://[2001:db8::1]:80"
      ( Left
          ( URI
              (Scheme.unsafeFromString "foo")
              ( HierarchicalPartAuth
                  ( Authority
                      Nothing
                      (Just (Both (IPv6Address (IPv6Address.unsafeFromString "2001:db8::1")) (Port.unsafeFromInt 80)))
                  )
                  (path [])
              )
              Nothing
              Nothing
          )
      )
    testIso
      (URIRef.parser options)
      (URIRef.print options)
      "ftp://ftp.is.co.za/rfc/rfc1808.txt"
      ( Left
          ( URI
              (Scheme.unsafeFromString "ftp")
              ( HierarchicalPartAuth
                  ( Authority
                      Nothing
                      (Just (This (NameAddress (RegName.unsafeFromString $ nes (Proxy :: Proxy "ftp.is.co.za")))))
                  )
                  (path [ "rfc", "rfc1808.txt" ])
              )
              Nothing
              Nothing
          )
      )
    testIso
      (URIRef.parser options)
      (URIRef.print options)
      "http://www.ietf.org/rfc/rfc2396.txt"
      ( Left
          ( URI
              (Scheme.unsafeFromString "http")
              ( HierarchicalPartAuth
                  ( Authority
                      Nothing
                      (Just (This (NameAddress (RegName.unsafeFromString $ nes (Proxy :: Proxy "www.ietf.org")))))
                  )
                  (path [ "rfc", "rfc2396.txt" ])
              )
              Nothing
              Nothing
          )
      )
    testIso
      (URIRef.parser options)
      (URIRef.print options)
      "ldap://[2001:db8::7]/c=GB?objectClass?one"
      ( Left
          ( URI
              (Scheme.unsafeFromString "ldap")
              ( HierarchicalPartAuth
                  ( Authority
                      Nothing
                      (Just (This (IPv6Address (IPv6Address.unsafeFromString "2001:db8::7"))))
                  )
                  (path [ "c=GB" ])
              )
              (Just (Query.unsafeFromString "objectClass?one"))
              Nothing
          )
      )
    testIso
      (URIRef.parser options)
      (URIRef.print options)
      "telnet://192.0.2.16:80/"
      ( Left
          ( URI
              (Scheme.unsafeFromString "telnet")
              ( HierarchicalPartAuth
                  ( Authority
                      Nothing
                      (Just (Both (IPv4Address (IPv4Address.unsafeFromInts 192 0 2 16)) (Port.unsafeFromInt 80)))
                  )
                  (path [ "" ])
              )
              Nothing
              Nothing
          )
      )
    testIso
      (URIRef.parser options)
      (URIRef.print options)
      "foo://example.com:8042/over/there?name=ferret#nose"
      ( Left
          ( URI
              (Scheme.unsafeFromString "foo")
              ( HierarchicalPartAuth
                  ( Authority
                      Nothing
                      (Just (Both (NameAddress (RegName.unsafeFromString $ nes (Proxy :: Proxy "example.com"))) (Port.unsafeFromInt 8042)))
                  )
                  (path [ "over", "there" ])
              )
              (Just (Query.unsafeFromString "name=ferret"))
              (Just (Fragment.unsafeFromString "nose"))
          )
      )
    testIso
      (URIRef.parser options)
      (URIRef.print options)
      "foo://example.com:8042/over/there?name=ferret#"
      ( Left
          ( URI
              (Scheme.unsafeFromString "foo")
              ( HierarchicalPartAuth
                  ( Authority
                      Nothing
                      (Just (Both (NameAddress (RegName.unsafeFromString $ nes (Proxy :: Proxy "example.com"))) (Port.unsafeFromInt 8042)))
                  )
                  (path [ "over", "there" ])
              )
              (Just (Query.unsafeFromString "name=ferret"))
              (Just (Fragment.unsafeFromString ""))
          )
      )
    testIso
      (URIRef.parser options)
      (URIRef.print options)
      "foo://info.example.com?fred"
      ( Left
          ( URI
              (Scheme.unsafeFromString "foo")
              ( HierarchicalPartAuth
                  ( Authority
                      Nothing
                      (Just (This (NameAddress (RegName.unsafeFromString $ nes (Proxy :: Proxy "info.example.com")))))
                  )
                  (path [])
              )
              (Just (Query.unsafeFromString "fred"))
              Nothing
          )
      )
    testIso
      (URIRef.parser options)
      (URIRef.print options)
      "ftp://cnn.example.com&story=breaking_news@10.0.0.1/top_story.htm"
      ( Left
          ( URI
              (Scheme.unsafeFromString "ftp")
              ( HierarchicalPartAuth
                  ( Authority
                      (Just (UserInfo.unsafeFromString (nes (Proxy :: Proxy "cnn.example.com&story=breaking_news"))))
                      (Just (This (IPv4Address (IPv4Address.unsafeFromInts 10 0 0 1))))
                  )
                  (path [ "top_story.htm" ])
              )
              Nothing
              Nothing
          )
      )
    testIso
      (URIRef.parser options)
      (URIRef.print options)
      "top_story.htm"
      ( Right
          ( RelativeRef
              (RelativePartNoAuth (Just (Right (PathNoScheme (Tuple (PathSegment.unsafeSegmentNZNCFromString $ nes (Proxy :: Proxy "top_story.htm")) [])))))
              Nothing
              Nothing
          )
      )
    testIso
      (URIRef.parser options)
      (URIRef.print options)
      "../top_story.htm"
      ( Right
          ( RelativeRef
              (RelativePartNoAuth (Just (Right (PathNoScheme (Tuple (PathSegment.unsafeSegmentNZNCFromString $ nes (Proxy :: Proxy "..")) [ PathSegment.unsafeSegmentFromString "top_story.htm" ])))))
              Nothing
              Nothing
          )
      )
    testIso
      (URIRef.parser options)
      (URIRef.print options)
      "/top_story.htm"
      ( Right
          ( RelativeRef
              (RelativePartNoAuth (Just (Left (PathAbsolute (Just (Tuple (PathSegment.unsafeSegmentNZFromString $ nes (Proxy :: Proxy "top_story.htm")) []))))))
              Nothing
              Nothing
          )
      )
    testIso
      (URIRef.parser options)
      (URIRef.print options)
      "/"
      ( Right
          ( RelativeRef
              (RelativePartNoAuth (Just (Left (PathAbsolute Nothing))))
              Nothing
              Nothing
          )
      )
    testIso
      (URIRef.parser options)
      (URIRef.print options)
      ""
      ( Right
          ( RelativeRef
              (RelativePartNoAuth Nothing)
              Nothing
              Nothing
          )
      )
    testIso
      (URIRef.parser options)
      (URIRef.print options)
      "http://www.example.com/some%20invented/url%20with%20spaces.html"
      ( Left
          ( URI
              (Scheme.unsafeFromString "http")
              ( HierarchicalPartAuth
                  (Authority Nothing (Just (This (NameAddress (RegName.unsafeFromString $ nes (Proxy :: Proxy "www.example.com"))))))
                  (path [ "some%20invented", "url%20with%20spaces.html" ])
              )
              Nothing
              Nothing
          )
      )
    testIso
      (URIRef.parser options)
      (URIRef.print options)
      "http://localhost:53174/metadata/fs/test/%D0%9F%D0%B0%D1%86%D0%B8%D0%B5%D0%BD%D1%82%D1%8B%23%20%23?"
      ( Left
          ( URI
              (Scheme.unsafeFromString "http")
              ( HierarchicalPartAuth
                  (Authority Nothing (Just (Both (NameAddress (RegName.unsafeFromString $ nes (Proxy :: Proxy "localhost"))) (Port.unsafeFromInt 53174))))
                  (path [ "metadata", "fs", "test", "%D0%9F%D0%B0%D1%86%D0%B8%D0%B5%D0%BD%D1%82%D1%8B%23%20%23" ])
              )
              (Just (Query.unsafeFromString ""))
              Nothing
          )
      )
    testIso
      (URIRef.parser options)
      (URIRef.print options)
      "news:comp.infosystems.www.servers.unix"
      ( Left
          ( URI
              (Scheme.unsafeFromString "news")
              (HierarchicalPartNoAuth (Just (Right (PathRootless (Tuple (PathSegment.unsafeSegmentNZFromString $ nes (Proxy :: Proxy "comp.infosystems.www.servers.unix")) [])))))
              Nothing
              Nothing
          )
      )
    testIso
      (URIRef.parser options)
      (URIRef.print options)
      "tel:+1-816-555-1212"
      ( Left
          ( URI
              (Scheme.unsafeFromString "tel")
              (HierarchicalPartNoAuth (Just (Right (PathRootless (Tuple (PathSegment.unsafeSegmentNZFromString $ nes (Proxy :: Proxy "+1-816-555-1212")) [])))))
              Nothing
              Nothing
          )
      )
    testIso
      (URIRef.parser options)
      (URIRef.print options)
      "urn:oasis:names:specification:docbook:dtd:xml:4.1.2"
      ( Left
          ( URI
              (Scheme.unsafeFromString "urn")
              (HierarchicalPartNoAuth (Just (Right (PathRootless (Tuple (PathSegment.unsafeSegmentNZFromString $ nes (Proxy :: Proxy "oasis:names:specification:docbook:dtd:xml:4.1.2")) [])))))
              Nothing
              Nothing
          )
      )
    testIso
      (URIRef.parser options)
      (URIRef.print options)
      "mailto:John.Doe@example.com"
      ( Left
          ( URI
              (Scheme.unsafeFromString "mailto")
              (HierarchicalPartNoAuth (Just (Right (PathRootless (Tuple (PathSegment.unsafeSegmentNZFromString $ nes (Proxy :: Proxy "John.Doe@example.com")) [])))))
              Nothing
              Nothing
          )
      )
    testIso
      (URIRef.parser options)
      (URIRef.print options)
      "mailto:fred@example.com"
      ( Left
          ( URI
              (Scheme.unsafeFromString "mailto")
              (HierarchicalPartNoAuth (Just (Right (PathRootless (Tuple (PathSegment.unsafeSegmentNZFromString $ nes (Proxy :: Proxy "fred@example.com")) [])))))
              Nothing
              Nothing
          )
      )
    testIso
      (URIRef.parser options)
      (URIRef.print options)
      "http://local.slamdata.com/?#?sort=asc&q=path%3A%2F&salt=1177214"
      ( Left
          ( URI
              (Scheme.unsafeFromString "http")
              ( HierarchicalPartAuth
                  ( Authority
                      Nothing
                      (Just (This (NameAddress (RegName.unsafeFromString $ nes (Proxy :: Proxy "local.slamdata.com")))))
                  )
                  (path [ "" ])
              )
              (Just (Query.unsafeFromString ""))
              (Just (Fragment.unsafeFromString "?sort=asc&q=path%3A%2F&salt=1177214"))
          )
      )
    testIso
      (URIRef.parser options)
      (URIRef.print options)
      "http://local.slamdata.com/?#?sort=asc&q=path:/&salt=1177214"
      ( Left
          ( URI
              (Scheme.unsafeFromString "http")
              ( HierarchicalPartAuth
                  ( Authority
                      Nothing
                      (Just (This (NameAddress (RegName.unsafeFromString $ nes (Proxy :: Proxy "local.slamdata.com")))))
                  )
                  (path [ "" ])
              )
              (Just (Query.unsafeFromString ""))
              (Just (Fragment.unsafeFromString "?sort=asc&q=path:/&salt=1177214"))
          )
      )

path :: Array String -> Path
path = Path <<< map PathSegment.unsafeSegmentFromString

options :: Record (URIRefOptions UserInfo (HostPortPair Host Port) Path HierPath RelPath Query Fragment)
options =
  { parseUserInfo: pure
  , printUserInfo: identity
  , parseHosts: HostPortPair.parser pure pure
  , printHosts: HostPortPair.print identity identity
  , parsePath: pure
  , printPath: identity
  , parseHierPath: pure
  , printHierPath: identity
  , parseRelPath: pure
  , printRelPath: identity
  , parseQuery: pure
  , printQuery: identity
  , parseFragment: pure
  , printFragment: identity
  }
