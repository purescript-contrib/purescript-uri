module Data.URI.Fragment (parser, print) where

import Prelude

import Data.URI (Fragment(..))
import Data.URI.Common (joinWith, parseFragmentOrQuery, printFragmentOrQuery)
import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser.Combinators (many)

parser ∷ Parser Fragment
parser = Fragment <<< joinWith "" <$> many parseFragmentOrQuery

print ∷ Fragment → String
print (Fragment f) = printFragmentOrQuery f
