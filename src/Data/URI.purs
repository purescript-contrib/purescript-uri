module Data.URI where

import Prelude

import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List (List)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Path.Pathy (Path, File, Dir, Abs, Rel, Sandboxed, Unsandboxed)
import Data.Tuple (Tuple)

-- | A generic URI
data URI = URI (Maybe Scheme) HierarchicalPart (Maybe Query) (Maybe Fragment)

derive instance eqURI ∷ Eq URI
derive instance ordURI ∷ Ord URI
derive instance genericURI ∷ Generic URI _
instance showURI ∷ Show URI where show = genericShow

-- | An absolute URI.
data AbsoluteURI = AbsoluteURI (Maybe Scheme) HierarchicalPart (Maybe Query)

derive instance eqAbsoluteURI ∷ Eq AbsoluteURI
derive instance ordAbsoluteURI ∷ Ord AbsoluteURI
derive instance genericAbsoluteURI ∷ Generic AbsoluteURI _
instance showAbsoluteURI ∷ Show AbsoluteURI where show = genericShow

-- | A relative reference for a URI.
data RelativeRef = RelativeRef RelativePart (Maybe Query) (Maybe Fragment)

derive instance eqRelativeRef ∷ Eq RelativeRef
derive instance ordRelativeRef ∷ Ord RelativeRef
derive instance genericRelativeRef ∷ Generic RelativeRef _
instance showRelativeRef ∷ Show RelativeRef where show = genericShow

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
newtype Scheme = Scheme String

derive instance eqScheme ∷ Eq Scheme
derive instance ordScheme ∷ Ord Scheme
derive instance genericScheme ∷ Generic Scheme _
derive instance newtypeScheme ∷ Newtype Scheme _
instance showScheme ∷ Show Scheme where show = genericShow

-- | The "hierarchical part" of a generic or absolute URI.
data HierarchicalPart = HierarchicalPart (Maybe Authority) (Maybe URIPathAbs)

derive instance eqHierarchicalPart ∷ Eq HierarchicalPart
derive instance ordHierarchicalPart ∷ Ord HierarchicalPart
derive instance genericHierarchicalPart ∷ Generic HierarchicalPart _
instance showHierarchicalPart ∷ Show HierarchicalPart where show = genericShow

-- | The "relative part" of a relative reference.
data RelativePart = RelativePart (Maybe Authority) (Maybe URIPathRel)

derive instance eqRelativePart ∷ Eq RelativePart
derive instance ordRelativePart ∷ Ord RelativePart
derive instance genericRelativePart ∷ Generic RelativePart _
instance showRelativePart ∷ Show RelativePart where show = genericShow

-- | The authority part of a URI. For example: `purescript.org`,
-- | `localhost:3000`, `user@example.net`
data Authority = Authority (Maybe UserInfo) (Array (Tuple Host (Maybe Port)))

derive instance eqAuthority ∷ Eq Authority
derive instance ordAuthority ∷ Ord Authority
derive instance genericAuthority ∷ Generic Authority _
instance showAuthority ∷ Show Authority where show = genericShow

-- | The user info part of an `Authority`. For example: `user`, `foo:bar`.
newtype UserInfo = UserInfo String

derive instance eqUserInfo ∷ Eq UserInfo
derive instance ordUserInfo ∷ Ord UserInfo
derive instance genericUserInfo ∷ Generic UserInfo _
derive instance newtypeUserInfo ∷ Newtype UserInfo _
instance showUserInfo ∷ Show UserInfo where show = genericShow

-- | A host address.
data Host
  = IPv6Address String
  | IPv4Address String
  | NameAddress String

derive instance eqHost ∷ Eq Host
derive instance ordHost ∷ Ord Host
derive instance genericHost ∷ Generic Host _
instance showHost ∷ Show Host where show = genericShow

-- | A port number.
newtype Port = Port Int

derive instance eqPort ∷ Eq Port
derive instance ordPort ∷ Ord Port
derive instance genericPort ∷ Generic Port _
derive instance newtypePort ∷ Newtype Port _
instance showPort ∷ Show Port where show = genericShow

-- | The query component of a URI.
newtype Query = Query (List (Tuple String (Maybe String)))

derive instance eqQuery ∷ Eq Query
derive instance ordQuery ∷ Ord Query
derive instance genericQuery ∷ Generic Query _
derive instance newtypeQuery ∷ Newtype Query _
instance showQuery ∷ Show Query where show = genericShow

-- | The hash fragment of a URI.
newtype Fragment = Fragment String

derive instance eqFragment ∷ Eq Fragment
derive instance ordFragment ∷ Ord Fragment
derive instance genericFragment ∷ Generic Fragment _
derive instance newtypeFragment ∷ Newtype Fragment _
instance showFragment ∷ Show Fragment where show = genericShow
