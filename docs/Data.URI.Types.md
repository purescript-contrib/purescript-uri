# Module Documentation

## Module Data.URI.Types

#### `URI`

``` purescript
data URI
  = URI (Maybe URIScheme) HierarchicalPart (Maybe Query) (Maybe Fragment)
```

A generic URI

#### `AbsoluteURI`

``` purescript
data AbsoluteURI
  = AbsoluteURI (Maybe URIScheme) HierarchicalPart (Maybe Query)
```

An absolute URI.

#### `RelativeRef`

``` purescript
data RelativeRef
  = RelativeRef RelativePart (Maybe Query) (Maybe Fragment)
```

A relative reference for a URI.

#### `URIPath`

``` purescript
type URIPath a s = Either (Path a File s) (Path a Dir s)
```

A general URI path, can be used to represent relative or absolute paths
that are sandboxed or unsandboxed.

#### `URIPathAbs`

``` purescript
type URIPathAbs = URIPath Abs Sandboxed
```

The path part for a generic or absolute URI.

#### `URIPathRel`

``` purescript
type URIPathRel = URIPath Rel Unsandboxed
```

The path part for a relative reference.

#### `URIRef`

``` purescript
type URIRef = Either URI RelativeRef
```

An alias for the most common use case of resource identifiers.

#### `URIScheme`

``` purescript
newtype URIScheme
  = URIScheme String
```

The scheme part of an absolute URI. For example: `http`, `ftp`, `git`.

#### `HierarchicalPart`

``` purescript
data HierarchicalPart
  = HierarchicalPart (Maybe Authority) (Maybe URIPathAbs)
```

The "hierarchical part" of a generic or absolute URI.

#### `RelativePart`

``` purescript
data RelativePart
  = RelativePart (Maybe Authority) (Maybe URIPathRel)
```

The "relative part" of a relative reference.

#### `Authority`

``` purescript
data Authority
  = Authority (Maybe UserInfo) Host (Maybe Port)
```

The authority part of a URI. For example: `purescript.org`,
`localhost:3000`, `user@example.net`

#### `UserInfo`

``` purescript
type UserInfo = String
```

The user info part of an `Authority`. For example: `user`, `foo:bar`.

#### `Host`

``` purescript
data Host
  = IPv6Address String
  | IPv4Address String
  | NameAddress String
  | MultipleHosts [Host]
```

A host address.

#### `Port`

``` purescript
type Port = Int
```

A port number.

#### `Query`

``` purescript
newtype Query
  = Query (StrMap (Maybe String))
```

The query component of a URI.

#### `Fragment`

``` purescript
type Fragment = String
```

The hash fragment of a URI.

#### `eqURI`

``` purescript
instance eqURI :: Eq URI
```


#### `showURI`

``` purescript
instance showURI :: Show URI
```


#### `eqAbsoluteURI`

``` purescript
instance eqAbsoluteURI :: Eq AbsoluteURI
```


#### `showAbsoluteURI`

``` purescript
instance showAbsoluteURI :: Show AbsoluteURI
```


#### `eqRelativeRef`

``` purescript
instance eqRelativeRef :: Eq RelativeRef
```


#### `showRelativeRef`

``` purescript
instance showRelativeRef :: Show RelativeRef
```


#### `eqURIScheme`

``` purescript
instance eqURIScheme :: Eq URIScheme
```


#### `showURIScheme`

``` purescript
instance showURIScheme :: Show URIScheme
```


#### `eqHierarchicalPart`

``` purescript
instance eqHierarchicalPart :: Eq HierarchicalPart
```


#### `showHierarchicalPart`

``` purescript
instance showHierarchicalPart :: Show HierarchicalPart
```


#### `eqRelativePart`

``` purescript
instance eqRelativePart :: Eq RelativePart
```


#### `showRelativePart`

``` purescript
instance showRelativePart :: Show RelativePart
```


#### `eqAuthority`

``` purescript
instance eqAuthority :: Eq Authority
```


#### `showAuthority`

``` purescript
instance showAuthority :: Show Authority
```


#### `eqHost`

``` purescript
instance eqHost :: Eq Host
```


#### `showHost`

``` purescript
instance showHost :: Show Host
```


#### `eqQuery`

``` purescript
instance eqQuery :: Eq Query
```


#### `showQuery`

``` purescript
instance showQuery :: Show Query
```




