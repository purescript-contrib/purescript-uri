module Data.URI.Types where

import Prelude

import Data.Either (Either)
import Data.Maybe (Maybe)
import Data.Path.Pathy (Path, File, Dir, Abs, Rel, Sandboxed, Unsandboxed)
import Data.StrMap (StrMap)
import Data.Tuple (Tuple)

-- | A generic URI
data URI = URI (Maybe URIScheme) HierarchicalPart (Maybe Query) (Maybe Fragment)

-- | An absolute URI.
data AbsoluteURI = AbsoluteURI (Maybe URIScheme) HierarchicalPart (Maybe Query)

-- | A relative reference for a URI.
data RelativeRef = RelativeRef RelativePart (Maybe Query) (Maybe Fragment)

-- | A general URI path, can be used to represent relative or absolute paths
-- | that are sandboxed or unsandboxed.
type URIPath a s = Either (Path a Dir s) (Path a File s)

-- | The path part for a generic or absolute URI.
type URIPathAbs = URIPath Abs Sandboxed

-- | The path part for a relative reference.
type URIPathRel = URIPath Rel Unsandboxed

-- | An alias for the most common use case of resource identifiers.
type URIRef = Either URI RelativeRef

-- | The scheme part of an absolute URI. For example: `http`, `ftp`, `git`.
newtype URIScheme = URIScheme String

-- | The "hierarchical part" of a generic or absolute URI.
data HierarchicalPart = HierarchicalPart (Maybe Authority) (Maybe URIPathAbs)

-- | The "relative part" of a relative reference.
data RelativePart = RelativePart (Maybe Authority) (Maybe URIPathRel)

-- | The authority part of a URI. For example: `purescript.org`,
-- | `localhost:3000`, `user@example.net`
data Authority = Authority (Maybe UserInfo) (Array (Tuple Host (Maybe Port)))

-- | The user info part of an `Authority`. For example: `user`, `foo:bar`.
type UserInfo = String

-- | A host address.
data Host
  = IPv6Address String
  | IPv4Address String
  | NameAddress String

-- | A port number.
type Port = Int

-- | The query component of a URI.
newtype Query = Query (StrMap (Maybe String))

-- | The hash fragment of a URI.
type Fragment = String

derive instance eqURI ∷ Eq URI

instance showURI ∷ Show URI where
  show (URI s h q f) = "URI (" ++ show s ++ ") (" ++ show h ++ ") (" ++ show q ++ ") (" ++ show f ++ ")"

derive instance eqAbsoluteURI ∷ Eq AbsoluteURI

instance showAbsoluteURI ∷ Show AbsoluteURI where
  show (AbsoluteURI s h q) = "AbsoluteURI (" ++ show s ++ ") (" ++ show h ++ ") (" ++ show q ++ ")"

derive instance eqRelativeRef ∷ Eq RelativeRef

instance showRelativeRef ∷ Show RelativeRef where
  show (RelativeRef r q f) = "RelativeRef (" ++ show r ++ ") (" ++ show q ++ ") (" ++ show f ++ ")"

derive instance eqURIScheme ∷ Eq URIScheme
derive instance ordURIScheme ∷ Ord URIScheme

instance showURIScheme ∷ Show URIScheme where
  show (URIScheme s) = "URIScheme " ++ show s

derive instance eqHierarchicalPart ∷ Eq HierarchicalPart

instance showHierarchicalPart ∷ Show HierarchicalPart where
  show (HierarchicalPart authority path) = "HierarchicalPart (" ++ show authority ++ ") (" ++ show path ++ ")"

derive instance eqRelativePart ∷ Eq RelativePart

instance showRelativePart ∷ Show RelativePart where
  show (RelativePart authority path) = "RelativePart (" ++ show authority ++ ") (" ++ show path ++ ")"

derive instance eqAuthority ∷ Eq Authority
derive instance ordAuthority ∷ Ord Authority

instance showAuthority ∷ Show Authority where
  show (Authority userinfo hosts) = "Authority (" ++ show userinfo ++ ") " ++ show hosts

derive instance eqHost ∷ Eq Host
derive instance ordHost ∷ Ord Host

instance showHost ∷ Show Host where
  show (IPv6Address ip) = "IPv6Address " ++ show ip
  show (IPv4Address ip) = "IPv4Address " ++ show ip
  show (NameAddress name) = "NameAddress " ++ show name

derive instance eqQuery ∷ Eq Query

instance showQuery ∷ Show Query where
  show (Query m) = "Query (" ++ show m ++ ")"
