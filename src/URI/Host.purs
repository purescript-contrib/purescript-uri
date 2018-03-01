module URI.Host
  ( Host(..)
  , parser
  , print
  , _IPv6Address
  , _IPv4Address
  , _NameAddress
  , module URI.Host.IPv4Address
  , module URI.Host.IPv6Address
  , module URI.Host.RegName
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Prism', prism')
import Data.Maybe (Maybe(..))
import Data.String.NonEmpty as NES
import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.Combinators (try)
import URI.Host.IPv4Address (IPv4Address)
import URI.Host.IPv4Address as IPv4Address
import URI.Host.IPv6Address (IPv6Address)
import URI.Host.IPv6Address as IPv6Address
import URI.Host.RegName (RegName)
import URI.Host.RegName as RegName

-- | A host address.
data Host
  = IPv6Address IPv6Address
  | IPv4Address IPv4Address
  | NameAddress RegName

derive instance eqHost ∷ Eq Host
derive instance ordHost ∷ Ord Host
derive instance genericHost ∷ Generic Host _
instance showHost ∷ Show Host where show = genericShow

parser ∷ Parser String Host
parser =
  (IPv6Address <$> IPv6Address.parser)
    <|> try (IPv4Address <$> IPv4Address.parser)
    <|> (NameAddress <$> RegName.parser)

print ∷ Host → String
print = case _ of
  IPv6Address addr → IPv6Address.unsafeToString addr
  IPv4Address addr → IPv4Address.print addr
  NameAddress addr → NES.toString (RegName.unsafeToString addr)

_IPv6Address ∷ Prism' Host IPv6Address
_IPv6Address = prism' IPv6Address case _ of
  IPv6Address addr → Just addr
  _ → Nothing

_IPv4Address ∷ Prism' Host IPv4Address
_IPv4Address = prism' IPv4Address case _ of
  IPv4Address addr → Just addr
  _ → Nothing

_NameAddress ∷ Prism' Host RegName
_NameAddress = prism' NameAddress case _ of
  NameAddress addr → Just addr
  _ → Nothing
