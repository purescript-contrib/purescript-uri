module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Control.Monad.Reader (runReaderT)
import Test.URI.AbsoluteURI as AbsoluteURI
import Test.URI.Authority as Authority
import Test.URI.Extra.MultiHostPortPair as Extra.MultiHostPortPair
import Test.URI.Extra.QueryPairs as Extra.QueryPairs
import Test.URI.Extra.UserPassInfo as Extra.UserPassInfo
import Test.URI.Fragment as Fragment
import Test.URI.Host as Host
import Test.URI.Path as Path
import Test.URI.Port as Port
import Test.URI.Scheme as Scheme
import Test.URI.URIRef as URIRef
import Test.URI.UserInfo as UserInfo

main :: Effect Unit
main = launchAff_ $ flip runReaderT 0 do
  Scheme.spec
  UserInfo.spec
  Host.spec
  Port.spec
  Fragment.spec
  Authority.spec
  Path.spec
  URIRef.spec
  AbsoluteURI.spec
  Extra.QueryPairs.spec
  Extra.MultiHostPortPair.spec
  Extra.UserPassInfo.spec
