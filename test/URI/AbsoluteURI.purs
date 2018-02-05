module Test.URI.AbsoluteURI where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Data.URI.Host.RegName as RegName
import Data.URI.Path.Segment as PathSegment
import Data.URI.Query as Query
import Data.URI.AbsoluteURI (Authority(..), HierPath, HierarchicalPart(..), Host(..), Path(..), PathAbsolute(..), PathRootless(..), Port(..), Query, Scheme(..), AbsoluteURI(..), AbsoluteURIOptions, UserInfo)
import Data.URI.AbsoluteURI as AbsoluteURI
import Test.Unit (TestSuite, suite)
import Test.Util (testIso)
import Text.Parsing.StringParser.Combinators (optionMaybe)

spec ∷ ∀ eff. TestSuite eff
spec =
  suite "AbsoluteURI parser/printer" do
    testIso
      (AbsoluteURI.parser options)
      (AbsoluteURI.print options)
      "couchbase://localhost/testBucket?password=&docTypeKey="
      (AbsoluteURI
        (Scheme "couchbase")
        (HierarchicalPartAuth
          (Authority
            Nothing
            (Just (Tuple (NameAddress (RegName.fromString "localhost")) Nothing)))
          (path ["testBucket"]))
        (Just (Query.unsafeFromString "password=&docTypeKey=")))
    testIso
      (AbsoluteURI.parser options)
      (AbsoluteURI.print options)
      "couchbase://localhost:99999/testBucket?password=pass&docTypeKey=type&queryTimeoutSeconds=20"
      (AbsoluteURI
        (Scheme "couchbase")
        (HierarchicalPartAuth
          (Authority
            Nothing
            (Just (Tuple (NameAddress (RegName.fromString "localhost")) (Just (Port 99999)))))
          (path ["testBucket"]))
        (Just (Query.unsafeFromString "password=pass&docTypeKey=type&queryTimeoutSeconds=20")))
    testIso
      (AbsoluteURI.parser options)
      (AbsoluteURI.print options)
      "foo:/abc/def"
      (AbsoluteURI
        (Scheme "foo")
        (HierarchicalPartNoAuth
          (Just (Left (PathAbsolute (Just (Tuple (PathSegment.unsafeSegmentNZFromString "abc") [PathSegment.unsafeSegmentFromString "def"]))))))
        Nothing)
    testIso
      (AbsoluteURI.parser options)
      (AbsoluteURI.print options)
      "foo:abc/def"
      (AbsoluteURI
        (Scheme "foo")
        (HierarchicalPartNoAuth
          (Just (Right (PathRootless (Tuple (PathSegment.unsafeSegmentNZFromString "abc") [PathSegment.unsafeSegmentFromString "def"])))))
        Nothing)

path ∷ Array String → Maybe Path
path = Just <<< Path <<< map PathSegment.unsafeSegmentFromString

options ∷ Record (AbsoluteURIOptions UserInfo Maybe Host Port Path HierPath Query)
options =
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
  , parseQuery: pure
  , printQuery: id
  }
