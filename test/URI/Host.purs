module Test.URI.Host where

import Prelude

import Data.Either (Either(..))
import Data.String.NonEmpty as NES
import Partial.Unsafe (unsafePartial)
import Test.QuickCheck ((===))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Util (TestEffects, forAll, testIso)
import Text.Parsing.Parser (runParser)
import URI.Host (Host(..))
import URI.Host as Host
import URI.Host.Gen as Host.Gen
import URI.Host.IPv4Address as IPv4Address
import URI.Host.IPv6Address as IPv6Address
import URI.Host.RegName as RegName

spec ∷ ∀ eff. Spec (TestEffects eff) Unit
spec = do
  describe "parseIPv4Address" do

    it "Should successfully roundtrip values sent through parseIPv4Address / Host.print" do
      forAll do
        ipv4 <- Host.Gen.genIPv4
        let printed = IPv4Address.print ipv4
        let parsed = runParser printed (Host.parser pure)
        pure $ pure (IPv4Address ipv4) === parsed

    it "should not parse 0-lead octets as an IP address" do
      shouldEqual
        (Right (NameAddress (RegName.unsafeFromString $ unsafePartial $ NES.unsafeFromString "192.168.001.1")))
        (runParser "192.168.001.1" (Host.parser pure))

  describe "Host parser/printer" do
    testIso (Host.parser pure) Host.print "localhost" (NameAddress (RegName.unsafeFromString $ unsafePartial $ NES.unsafeFromString "localhost"))
    testIso (Host.parser pure) Host.print "github.com" (NameAddress (RegName.unsafeFromString $ unsafePartial $ NES.unsafeFromString "github.com"))
    testIso (Host.parser pure) Host.print "www.multipart.domain.example.com" (NameAddress (RegName.unsafeFromString $ unsafePartial $ NES.unsafeFromString "www.multipart.domain.example.com"))
    testIso (Host.parser pure) Host.print "192.168.0.1" (IPv4Address (IPv4Address.unsafeFromInts 192 168 0 1))
    testIso (Host.parser pure) Host.print "[2001:cdba:0000:0000:0000:0000:3257:9652]" (IPv6Address (IPv6Address.unsafeFromString "2001:cdba:0000:0000:0000:0000:3257:9652"))
