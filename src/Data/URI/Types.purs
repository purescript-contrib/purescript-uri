module Data.URI.Types where

import Data.Array (length)
import Data.Maybe (Maybe())
import Data.StrMap (StrMap())
import Data.Path.Pathy (Path(), Abs(), Unsandboxed())
import qualified Data.Array.Unsafe as U

data URI a = URI (Maybe URIScheme) (HierarchicalPart a) (Maybe Query) (Maybe Fragment)

newtype URIScheme = URIScheme String

data HierarchicalPart a = HierarchicalPart (Maybe Authority) (Maybe (URIPath a))

type URIPath a = Path Abs a Unsandboxed

data Authority = Authority (Maybe UserInfo) Host (Maybe Port)

type UserInfo = String

data Host
  = IPv6Address String
  | IPv4Address String
  | NameAddress String
  | MultipleHosts [Host]

type Port = String

newtype Query = Query (StrMap String)

type Fragment = String

instance eqURI :: Eq (URI a) where
  (==) (URI s1 h1 q1 f1) (URI s2 h2 q2 f2) = s1 == s2 && h1 == h2 && q1 == q2 && f1 == f2
  (/=) x y = not (x == y)

instance showURI :: Show (URI a) where
  show (URI scheme hierPart query fragment) = "URI (" ++ show scheme ++ ") (" ++ show hierPart ++ ") (" ++ show query ++ ") (" ++ show fragment ++ ")"

instance eqURIScheme :: Eq URIScheme where
  (==) (URIScheme s1) (URIScheme s2) = s1 == s2
  (/=) x y = not (x == y)

instance showURIScheme :: Show URIScheme where
  show (URIScheme s) = "URIScheme " ++ show s

instance eqHierarchicalPart :: Eq (HierarchicalPart a) where
  (==) (HierarchicalPart a1 p1) (HierarchicalPart a2 p2) = a1 == a2 && p1 == p2
  (/=) x y = not (x == y)

instance showHierarchicalPart :: Show (HierarchicalPart a) where
  show (HierarchicalPart authority path) = "HierarchicalPart (" ++ show authority ++ ") (" ++ show path ++ ")"

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
