module Data.URI
  ( module Data.URI.AbsoluteURI
  , module Data.URI.Authority
  , module Data.URI.Fragment
  , module Data.URI.HierarchicalPart
  , module Data.URI.Host
  , module Data.URI.Path
  , module Data.URI.Path.Absolute
  , module Data.URI.Path.NoScheme
  , module Data.URI.Path.Rootless
  , module Data.URI.Port
  , module Data.URI.Query
  , module Data.URI.RelativePart
  , module Data.URI.RelativeRef
  , module Data.URI.Scheme
  , module Data.URI.URI
  , module Data.URI.URIRef
  , module Data.URI.UserInfo
  ) where

import Data.URI.AbsoluteURI (AbsoluteURI(..))
import Data.URI.Authority (Authority(..))
import Data.URI.Fragment (Fragment)
import Data.URI.HierarchicalPart (HierarchicalPart(..), HierPath)
import Data.URI.Host (Host(..), RegName)
import Data.URI.Path (Path(..))
import Data.URI.Path.Absolute (PathAbsolute(..))
import Data.URI.Path.NoScheme (PathNoScheme(..))
import Data.URI.Path.Rootless (PathRootless(..))
import Data.URI.Port (Port(..))
import Data.URI.Query (Query)
import Data.URI.RelativePart (RelativePart(..), RelPath)
import Data.URI.RelativeRef (RelativeRef(..))
import Data.URI.Scheme (Scheme(..))
import Data.URI.URI (URI(..))
import Data.URI.URIRef (URIRef)
import Data.URI.UserInfo (UserInfo)
