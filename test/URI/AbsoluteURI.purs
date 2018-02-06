module Test.URI.AbsoluteURI where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.These (These(..))
import Data.Tuple (Tuple(..))
import Data.URI.AbsoluteURI (Authority(..), HierPath, HierarchicalPart(..), Host(..), Path(..), PathAbsolute(..), PathRootless(..), Port, Query, AbsoluteURI(..), AbsoluteURIOptions, UserInfo)
import Data.URI.AbsoluteURI as AbsoluteURI
import Data.URI.Host.RegName as RegName
import Data.URI.HostPortPair (HostPortPair)
import Data.URI.HostPortPair as HostPortPair
import Data.URI.Path.Segment as PathSegment
import Data.URI.Port as Port
import Data.URI.Query as Query
import Data.URI.Scheme as Scheme
import Test.Spec (Spec, describe)
import Test.Util (testIso)

spec ∷ ∀ eff. Spec eff Unit
spec =
  describe "AbsoluteURI parser/printer" do
    testIso
      (AbsoluteURI.parser options)
      (AbsoluteURI.print options)
      "couchbase://localhost/testBucket?password=&docTypeKey="
      (AbsoluteURI
        (Scheme.unsafeFromString "couchbase")
        (HierarchicalPartAuth
          (Authority
            Nothing
            (Just (This (NameAddress (RegName.unsafeFromString "localhost")))))
          (path ["testBucket"]))
        (Just (Query.unsafeFromString "password=&docTypeKey=")))
    testIso
      (AbsoluteURI.parser options)
      (AbsoluteURI.print options)
      "couchbase://localhost:99999/testBucket?password=pass&docTypeKey=type&queryTimeoutSeconds=20"
      (AbsoluteURI
        (Scheme.unsafeFromString "couchbase")
        (HierarchicalPartAuth
          (Authority
            Nothing
            (Just (Both (NameAddress (RegName.unsafeFromString "localhost")) (Port.unsafeFromInt 99999))))
          (path ["testBucket"]))
        (Just (Query.unsafeFromString "password=pass&docTypeKey=type&queryTimeoutSeconds=20")))
    testIso
      (AbsoluteURI.parser options)
      (AbsoluteURI.print options)
      "foo:/abc/def"
      (AbsoluteURI
        (Scheme.unsafeFromString "foo")
        (HierarchicalPartNoAuth
          (Just (Left (PathAbsolute (Just (Tuple (PathSegment.unsafeSegmentNZFromString "abc") [PathSegment.unsafeSegmentFromString "def"]))))))
        Nothing)
    testIso
      (AbsoluteURI.parser options)
      (AbsoluteURI.print options)
      "foo:abc/def"
      (AbsoluteURI
        (Scheme.unsafeFromString "foo")
        (HierarchicalPartNoAuth
          (Just (Right (PathRootless (Tuple (PathSegment.unsafeSegmentNZFromString "abc") [PathSegment.unsafeSegmentFromString "def"])))))
        Nothing)

path ∷ Array String → Maybe Path
path = Just <<< Path <<< map PathSegment.unsafeSegmentFromString

options ∷ Record (AbsoluteURIOptions UserInfo (HostPortPair Host Port) Path HierPath Query)
options =
  { parseUserInfo: pure
  , printUserInfo: id
  , parseHosts: HostPortPair.parser pure pure
  , printHosts: HostPortPair.print id id
  , parsePath: pure
  , printPath: id
  , parseHierPath: pure
  , printHierPath: id
  , parseQuery: pure
  , printQuery: id
  }
