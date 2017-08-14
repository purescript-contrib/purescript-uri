-- | Functions to pretty-print URIs.
-- |
-- | All the functions in this module run encodeURI on their output, so you
-- | shouldn't call encodeURI on your hierarchical part or Query before
-- | constructing URIs.
module Data.URI.Printer where

import Prelude

import Data.Array (catMaybes)
import Data.Either (either)
import Data.Maybe (Maybe(..))
import Data.String as S
import Data.URI.HierarchicalPart (printHierPart)
import Data.URI.Query (printQuery)
import Data.URI.RelativePart (printRelativePart)
import Data.URI.Scheme (printScheme)
import Data.URI.Types (AbsoluteURI(..), RelativeRef(..), URI(..), URIRef)
import Global (encodeURI)

printURIRef ∷ URIRef → String
printURIRef = either printURI printRelativeRef

printURI ∷ URI → String
printURI (URI s h q f) =
  S.joinWith "" $ catMaybes
    [ printScheme <$> s
    , Just (encodeURI (printHierPart h))
    , printQuery <$> q
    , ("#" <> _) <$> f
    ]

printAbsoluteURI ∷ AbsoluteURI → String
printAbsoluteURI (AbsoluteURI s h q) =
  S.joinWith "" $ catMaybes
    [ printScheme <$> s
    , Just (encodeURI (printHierPart h))
    , printQuery <$> q
    ]

printRelativeRef ∷ RelativeRef → String
printRelativeRef (RelativeRef h q f) =
  S.joinWith "" $ catMaybes
    [ Just (encodeURI (printRelativePart h))
    , printQuery <$> q
    , ("#" <> _) <$> f
    ]
