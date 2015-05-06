# Module Documentation

## Module Data.URI.Types

#### `URI`

``` purescript
data URI a
  = URI (Maybe URIScheme) (HierarchicalPart a) (Maybe Query) (Maybe Fragment)
```


#### `URIScheme`

``` purescript
newtype URIScheme
  = URIScheme String
```


#### `HierarchicalPart`

``` purescript
data HierarchicalPart a
  = HierarchicalPart (Maybe Authority) (Maybe (URIPath a))
```


#### `URIPath`

``` purescript
type URIPath a = Path Abs a Unsandboxed
```


#### `Authority`

``` purescript
data Authority
  = Authority (Maybe UserInfo) Host (Maybe Port)
```


#### `UserInfo`

``` purescript
type UserInfo = String
```


#### `Host`

``` purescript
data Host
  = IPv6Address String
  | IPv4Address String
  | NameAddress String
  | MultipleHosts [Host]
```


#### `Port`

``` purescript
type Port = String
```


#### `Query`

``` purescript
newtype Query
  = Query (StrMap String)
```


#### `Fragment`

``` purescript
type Fragment = String
```


#### `eqURI`

``` purescript
instance eqURI :: Eq (URI a)
```


#### `showURI`

``` purescript
instance showURI :: Show (URI a)
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
instance eqHierarchicalPart :: Eq (HierarchicalPart a)
```


#### `showHierarchicalPart`

``` purescript
instance showHierarchicalPart :: Show (HierarchicalPart a)
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




