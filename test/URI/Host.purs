module Test.URI.Host where

import Prelude

import Data.Either (Either(..))
import Data.String.NonEmpty (nes)
import Type.Proxy (Proxy(..))
import Test.QuickCheck ((===))
import Test.Spec (Spec, describe, it, shouldEqual)
import Test.Util (forAll, testIso)
import Text.Parsing.Parser (runParser)
import URI.Host (Host(..))
import URI.Host as Host
import URI.Host.Gen as Host.Gen
import URI.Host.IPv4Address as IPv4Address
import URI.Host.IPv6Address as IPv6Address
import URI.Host.RegName as RegName

spec ∷ Spec Unit
spec = do
  describe "Host parser/printer" do
    testIso Host.parser Host.print "localhost" (NameAddress (RegName.unsafeFromString $ nes (Proxy :: Proxy "localhost")))
    testIso Host.parser Host.print "github.com" (NameAddress (RegName.unsafeFromString $ nes (Proxy :: Proxy "github.com")))
    testIso Host.parser Host.print "www.multipart.domain.example.com" (NameAddress (RegName.unsafeFromString $ nes (Proxy :: Proxy "www.multipart.domain.example.com")))
    testIso Host.parser Host.print "192.168.0.1" (IPv4Address (IPv4Address.unsafeFromInts 192 168 0 1))
    testIso Host.parser Host.print "[2001:cdba:0000:0000:0000:0000:3257:9652]" (IPv6Address (IPv6Address.unsafeFromString "2001:cdba:0000:0000:0000:0000:3257:9652"))

  describe "IPv4Address" do

    it "should successfully roundtrip values sent through Host parse/print" do
      forAll do
        ipv4 ← Host.Gen.genIPv4
        let printed = IPv4Address.print ipv4
        let parsed = runParser printed Host.parser
        pure $ pure (IPv4Address ipv4) === parsed

    it "should not parse 0-lead octets as an IP address" do
      shouldEqual
        (Right (NameAddress (RegName.unsafeFromString $ nes (Proxy :: Proxy "192.168.001.1"))))
        (runParser "192.168.001.1" Host.parser)

  describe "NameAddress" do

    it "should uphold toString / fromString property" do
      forAll do
        regName ← Host.Gen.genRegName
        pure $ RegName.fromString (RegName.toString regName) === regName
