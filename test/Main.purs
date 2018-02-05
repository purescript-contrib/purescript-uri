module Test.Main where

import Prelude

import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Test.URI.AbsoluteURI as AbsoluteURI
import Test.URI.Authority as Authority
import Test.URI.Fragment as Fragment
import Test.URI.Host as Host
import Test.URI.NonStandard.QueryPairs as NonStandard.QueryPairs
import Test.URI.Path as Path
import Test.URI.Port as Port
import Test.URI.Scheme as Scheme
import Test.URI.UserInfo as UserInfo
import Test.URI.URIRef as URIRef
import Test.Unit (suite)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)
import Test.Util (TestEffects)

main ∷ Eff (TestEffects (avar ∷ AVAR, testOutput ∷ TESTOUTPUT)) Unit
main = runTest $
  suite "Data.URI" do
    Scheme.spec
    UserInfo.spec
    Host.spec
    Port.spec
    Fragment.spec
    Authority.spec
    Path.spec
    NonStandard.QueryPairs.spec
    URIRef.spec
    AbsoluteURI.spec

--     -- Not an iso in this case as the printed path is normalised
--     -- testRunParseURIRefParses
--     --   "http://local.slamdata.com/?#?sort=asc&q=path%3A%2F&salt=1177214"
--     --   (Left
--     --     (URI
--     --       (Scheme "http")
--     --       (HierarchicalPart
--     --         (Just (Authority Nothing [Tuple (NameAddress (RegName.fromString "local.)slamdata.com") Nothing]))
--     --         ((Just (Left rootDir))))
--     --       ((Just mempty))
--     --       ((Just (Fragment.unsafeFromString "?sort=asc&q=path:/&salt=1177214")))))
--     -- testPrinter
--     --   URIRef.print
--     --   "http://local.slamdata.com/?#?sort=asc&q=path:/&salt=1177214"
--     --   (Left
--     --     (URI
--     --       (Scheme "http")
--     --       (HierarchicalPart
--     --         (Just (Authority Nothing [Tuple (NameAddress (RegName.fromString "local.)slamdata.com") Nothing]))
--     --         ((Just (Left rootDir))))
--     --       ((Just mempty))
--     --       ((Just (Fragment.unsafeFromString "?sort=asc&q=path:/&salt=1177214")))))
