module Data.URI.Types where

import Data.Array (length)
import Data.Either (Either())
import Data.Int (Int())
import Data.Maybe (Maybe())
import Data.Path.Pathy (Path(), File(), Dir(), Abs(), Rel(), Sandboxed(), Unsandboxed())
import Data.StrMap (StrMap())

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
data Authority = Authority (Maybe UserInfo) Host (Maybe Port)

-- | The user info part of an `Authority`. For example: `user`, `foo:bar`.
type UserInfo = String

-- | A host address.
data Host
  = IPv6Address String
  | IPv4Address String
  | NameAddress String
  | MultipleHosts [Host]

-- | A port number.
type Port = Int

-- | The query component of a URI.
newtype Query = Query (StrMap (Maybe String))

-- | The hash fragment of a URI.
type Fragment = String

instance eqURI :: Eq URI where
  (==) (URI s1 h1 q1 f1) (URI s2 h2 q2 f2) = s1 == s2 && h1 == h2 && q1 == q2 && f1 == f2
  (/=) x y = not (x == y)

instance showURI :: Show URI where
  show (URI s h q f) = "URI (" ++ show s ++ ") (" ++ show h ++ ") (" ++ show q ++ ") (" ++ show f ++ ")"

instance eqAbsoluteURI :: Eq AbsoluteURI where
  (==) (AbsoluteURI s1 h1 q1) (AbsoluteURI s2 h2 q2) = s1 == s2 && h1 == h2 && q1 == q2
  (/=) x y = not (x == y)

instance showAbsoluteURI :: Show AbsoluteURI where
  show (AbsoluteURI s h q) = "AbsoluteURI (" ++ show s ++ ") (" ++ show h ++ ") (" ++ show q ++ ")"

instance eqRelativeRef :: Eq RelativeRef where
  (==) (RelativeRef r1 q1 f1) (RelativeRef r2 q2 f2) = r1 == r2 && q1 == q2 && f1 == f2
  (/=) x y = not (x == y)

instance showRelativeRef :: Show RelativeRef where
  show (RelativeRef r q f) = "RelativeRef (" ++ show r ++ ") (" ++ show q ++ ") (" ++ show f ++ ")"

instance eqURIScheme :: Eq URIScheme where
  (==) (URIScheme s1) (URIScheme s2) = s1 == s2
  (/=) x y = not (x == y)

instance showURIScheme :: Show URIScheme where
  show (URIScheme s) = "URIScheme " ++ show s

instance eqHierarchicalPart :: Eq HierarchicalPart where
  (==) (HierarchicalPart a1 p1) (HierarchicalPart a2 p2) = a1 == a2 && p1 == p2
  (/=) x y = not (x == y)

instance showHierarchicalPart :: Show HierarchicalPart where
  show (HierarchicalPart authority path) = "HierarchicalPart (" ++ show authority ++ ") (" ++ show path ++ ")"

instance eqRelativePart :: Eq RelativePart where
  (==) (RelativePart a1 p1) (RelativePart a2 p2) = a1 == a2 && p1 == p2
  (/=) x y = not (x == y)

instance showRelativePart :: Show RelativePart where
  show (RelativePart authority path) = "RelativePart (" ++ show authority ++ ") (" ++ show path ++ ")"

instance eqAuthority :: Eq Authority where
  (==) (Authority u1 h1 p1) (Authority u2 h2 p2) = u1 == u2 && h1 == h2 && p1 == p2
  (/=) x y = not (x == y)

instance showAuthority :: Show Authority where
  show (Authority userinfo host port) = "Authority (" ++ show userinfo ++ ") (" ++ show host ++ ") (" ++ show port ++ ")"

instance eqHost :: Eq Host where
  (==) (IPv6Address i1) (IPv6Address i2) = i1 == i2
  (==) (IPv4Address i1) (IPv4Address i2) = i1 == i2
  (==) (NameAddress n1) (NameAddress n2) = n1 == n2
  (==) (MultipleHosts hs1) (MultipleHosts hs2) = hs1 == hs2
  (==) (MultipleHosts hs) h2 | length hs == 1 = U.head hs == h2
  (==) h1 (MultipleHosts hs) | length hs == 1 = h1 == U.head hs
  (==) _ _ = false
  (/=) x y = not (x == y)

instance showHost :: Show Host where
  show (IPv6Address ip) = "IPv6Address " ++ show ip
  show (IPv4Address ip) = "IPv4Address " ++ show ip
  show (NameAddress name) = "NameAddress " ++ show name
  show (MultipleHosts hs) = "MultipleHosts " ++ show hs

instance eqQuery :: Eq Query where
  (==) (Query m1) (Query m2) = m1 == m2
  (/=) x y = not (x == y)

instance showQuery :: Show Query where
  show (Query m) = "Query (" ++ show m ++ ")"
