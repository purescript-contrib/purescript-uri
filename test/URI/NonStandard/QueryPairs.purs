module Test.URI.NonStandard.QueryPairs where

import Prelude

import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.URI.NonStandard.QueryPairs as NQP
import Data.URI.Query as Query
import Test.Unit (TestSuite, suite)
import Test.Util (testIso)

spec ∷ ∀ eff. TestSuite eff
spec =
  suite "QueryPairs printer/parser" do
    testIso
      (Query.parser NQP.parse)
      (Query.print NQP.print)
      "?key1=value1&key2=value2&key1=value3"
      (NQP.QueryPairs (Tuple "key1" (Just "value1") : Tuple "key2" (Just "value2") : Tuple "key1" (Just "value3") : Nil))
    testIso
      (Query.parser NQP.parse)
      (Query.print NQP.print)
      "?k%3Dey=value%3D1"
      (NQP.QueryPairs (Tuple "k=ey" (Just "value=1") : Nil))
    testIso
      (Query.parser NQP.parse)
      (Query.print NQP.print)
      "?"
      (NQP.QueryPairs Nil)
    testIso
      (Query.parser NQP.parse)
      (Query.print NQP.print)
      "?key1=&key2="
      (NQP.QueryPairs (Tuple "key1" (Just "") : Tuple "key2" (Just "") : Nil))
    testIso
      (Query.parser NQP.parse)
      (Query.print NQP.print)
      "?key1&key2"
      (NQP.QueryPairs (Tuple "key1" Nothing : Tuple "key2" Nothing : Nil))
    testIso
      (Query.parser NQP.parse)
      (Query.print NQP.print)
      "?key1=foo%3Bbar"
      (NQP.QueryPairs (Tuple "key1" (Just "foo;bar") : Nil))
    testIso
      (Query.parser NQP.parse)
      (Query.print NQP.print)
      "?replicaSet=test&connectTimeoutMS=300000"
      (NQP.QueryPairs (Tuple "replicaSet" (Just "test") : Tuple "connectTimeoutMS" (Just "300000") : Nil))
    testIso
      (Query.parser NQP.parse)
      (Query.print NQP.print)
      "?fred"
      (NQP.QueryPairs (pure (Tuple "fred" Nothing)))
    testIso
      (Query.parser NQP.parse)
      (Query.print NQP.print)
      "?objectClass?one"
      (NQP.QueryPairs (pure (Tuple "objectClass?one" Nothing)))
    testIso
      (Query.parser NQP.parse)
      (Query.print NQP.print)
      "?password=&docTypeKey="
      (NQP.QueryPairs (Tuple "password" (Just "") : Tuple "docTypeKey" (Just "") : Nil))
    testIso
      (Query.parser NQP.parse)
      (Query.print NQP.print)
      "?password=pass&docTypeKey=type&queryTimeoutSeconds=20"
      (NQP.QueryPairs (Tuple "password" (Just "pass") : Tuple "docTypeKey" (Just "type") : Tuple "queryTimeoutSeconds" (Just "20") : Nil))
