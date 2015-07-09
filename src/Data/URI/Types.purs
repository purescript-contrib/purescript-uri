module Data.URI.Types where

import Prelude
import Data.Array (length)
import Data.Either (Either())
import Data.Maybe (Maybe())
import Data.Path.Pathy (Path(), File(), Dir(), Abs(), Rel(), Sandboxed(), Unsandboxed())
import Data.StrMap (StrMap())
import Data.Tuple (Tuple())

import qualified Data.Array.Unsafe as U

-- | A generic URI
data URI = URI (Maybe URIScheme) HierarchicalPart (Maybe Query) (Maybe Fragment)

-- | An absolute URI.
data AbsoluteURI = AbsoluteURI (Maybe URIScheme) HierarchicalPart (Maybe Query)

-- | A relative reference for a URI.
data RelativeRef = RelativeRef RelativePart (Maybe Query) (Maybe Fragment)

-- | A general URI path, can be used to represent relative or absolute paths
-- | that are sandboxed or unsandboxed.
type URIPath a s = Either (Path a File s) (Path a Dir s)

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

instance eqURI :: Eq URI where
  eq (URI s1 h1 q1 f1) (URI s2 h2 q2 f2) = s1 == s2 && h1 == h2 && q1 == q2 && f1 == f2
  

instance showURI :: Show URI where
  show (URI s h q f) = "URI (" ++ show s ++ ") (" ++ show h ++ ") (" ++ show q ++ ") (" ++ show f ++ ")"

instance eqAbsoluteURI :: Eq AbsoluteURI where
  eq (AbsoluteURI s1 h1 q1) (AbsoluteURI s2 h2 q2) = s1 == s2 && h1 == h2 && q1 == q2

instance showAbsoluteURI :: Show AbsoluteURI where
  show (AbsoluteURI s h q) = "AbsoluteURI (" ++ show s ++ ") (" ++ show h ++ ") (" ++ show q ++ ")"

instance eqRelativeRef :: Eq RelativeRef where
  eq (RelativeRef r1 q1 f1) (RelativeRef r2 q2 f2) = r1 == r2 && q1 == q2 && f1 == f2

instance showRelativeRef :: Show RelativeRef where
  show (RelativeRef r q f) = "RelativeRef (" ++ show r ++ ") (" ++ show q ++ ") (" ++ show f ++ ")"

instance eqURIScheme :: Eq URIScheme where
  eq (URIScheme s1) (URIScheme s2) = s1 == s2

instance showURIScheme :: Show URIScheme where
  show (URIScheme s) = "URIScheme " ++ show s

instance eqHierarchicalPart :: Eq HierarchicalPart where
  eq (HierarchicalPart a1 p1) (HierarchicalPart a2 p2) = a1 == a2 && p1 == p2

instance showHierarchicalPart :: Show HierarchicalPart where
  show (HierarchicalPart authority path) = "HierarchicalPart (" ++ show authority ++ ") (" ++ show path ++ ")"

instance eqRelativePart :: Eq RelativePart where
  eq (RelativePart a1 p1) (RelativePart a2 p2) = a1 == a2 && p1 == p2

instance showRelativePart :: Show RelativePart where
  show (RelativePart authority path) = "RelativePart (" ++ show authority ++ ") (" ++ show path ++ ")"

instance eqAuthority :: Eq Authority where
  eq (Authority u1 hs1) (Authority u2 hs2) = u1 == u2 && hs1 == hs2

instance showAuthority :: Show Authority where
  show (Authority userinfo hosts) = "Authority (" ++ show userinfo ++ ") " ++ show hosts

instance eqHost :: Eq Host where
  eq (IPv6Address i1) (IPv6Address i2) = i1 == i2
  eq (IPv4Address i1) (IPv4Address i2) = i1 == i2
  eq (NameAddress n1) (NameAddress n2) = n1 == n2
  eq _ _ = false

instance showHost :: Show Host where
  show (IPv6Address ip) = "IPv6Address " ++ show ip
  show (IPv4Address ip) = "IPv4Address " ++ show ip
  show (NameAddress name) = "NameAddress " ++ show name

instance eqQuery :: Eq Query where
  eq (Query m1) (Query m2) = m1 == m2

instance showQuery :: Show Query where
  show (Query m) = "Query (" ++ show m ++ ")"
