# Module Documentation

## Module Data.URI.Path

#### `parsePath`

``` purescript
parsePath :: forall p. Parser p -> Parser (Maybe p)
```


#### `parsePathAbEmpty`

``` purescript
parsePathAbEmpty :: forall p. Parser p -> Parser (Maybe p)
```


#### `parsePathAbsolute`

``` purescript
parsePathAbsolute :: forall p. Parser p -> Parser p
```


#### `parsePathNoScheme`

``` purescript
parsePathNoScheme :: forall p. Parser p -> Parser p
```


#### `parsePathRootless`

``` purescript
parsePathRootless :: forall p. Parser p -> Parser p
```


#### `parseSegment`

``` purescript
parseSegment :: Parser String
```


#### `parseSegmentNonZero`

``` purescript
parseSegmentNonZero :: Parser String
```


#### `parseSegmentNonZeroNoColon`

``` purescript
parseSegmentNonZeroNoColon :: Parser String
```


#### `parseURIPathAbs`

``` purescript
parseURIPathAbs :: Parser URIPathAbs
```


#### `parseURIPathRel`

``` purescript
parseURIPathRel :: Parser URIPathRel
```


#### `printPath`

``` purescript
printPath :: forall a s. URIPath a s -> String
```




