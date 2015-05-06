# Module Documentation

## Module Data.URI.Path

#### `parsePath`

``` purescript
parsePath :: forall a. Parser (URIPath a) -> Parser (Maybe (URIPath a))
```


#### `parsePathAbEmpty`

``` purescript
parsePathAbEmpty :: forall a. Parser (URIPath a) -> Parser (Maybe (URIPath a))
```


#### `parsePathAbsolute`

``` purescript
parsePathAbsolute :: forall a. Parser (URIPath a) -> Parser (URIPath a)
```


#### `parsePathNoScheme`

``` purescript
parsePathNoScheme :: forall a. Parser (URIPath a) -> Parser (URIPath a)
```


#### `parsePathRootless`

``` purescript
parsePathRootless :: forall a. Parser (URIPath a) -> Parser (URIPath a)
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


#### `parseFilePath`

``` purescript
parseFilePath :: Parser (URIPath File)
```


#### `parseDirPath`

``` purescript
parseDirPath :: Parser (URIPath Dir)
```




