module Test.URI.Host where

import Prelude

import Data.Either (Either(..))
import Data.URI.Host (Host(..))
import Data.URI.Host as Host
import Data.URI.Host.Gen as Host.Gen
import Data.URI.Host.RegName as RegName
import Test.QuickCheck ((===))
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (equal)
import Test.Util (TestEffects, forAll, testIso)
import Text.Parsing.StringParser (runParser)

spec ∷ ∀ eff. TestSuite (TestEffects eff)
spec = do
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

  suite "Host parser/printer" do
    testIso (Host.parser pure) (Host.print id) "localhost" (NameAddress (RegName.fromString "localhost"))
    testIso (Host.parser pure) (Host.print id) "github.com" (NameAddress (RegName.fromString "github.com"))
    testIso (Host.parser pure) (Host.print id) "www.multipart.domain.example.com" (NameAddress (RegName.fromString "www.multipart.domain.example.com"))
    testIso (Host.parser pure) (Host.print id) "192.168.0.1" (IPv4Address "192.168.0.1")
    testIso (Host.parser pure) (Host.print id) "[2001:cdba:0000:0000:0000:0000:3257:9652]" (IPv6Address "2001:cdba:0000:0000:0000:0000:3257:9652")
