# Module Documentation

## Module Network.URI

#### `parse`

``` purescript
parse :: String -> Either ParseError _
```


#### `parseURI`

``` purescript
parseURI :: Parser _
```

#### `parseHierarchicalPart`

``` purescript
parseHierarchicalPart :: Parser _
```

#### `uriReference`

``` purescript
uriReference :: Parser _
```

#### `parseAbsoluteURI`

``` purescript
parseAbsoluteURI :: Parser _
```

#### `parseRelativeRef`

``` purescript
parseRelativeRef :: Parser _
```

#### `parseRelativePart`

``` purescript
parseRelativePart :: Parser _
```

#### `parseScheme`

``` purescript
parseScheme :: Parser String
```

#### `parseAuthority`

``` purescript
parseAuthority :: Parser _
```

#### `parseUserInfo`

``` purescript
parseUserInfo :: Parser String
```

#### `parseHost`

``` purescript
parseHost :: Parser String
```

#### `parsePort`

``` purescript
parsePort :: Parser String
```

#### `parseIPLiteral`

``` purescript
parseIPLiteral :: Parser String
```

#### `parseIPvFuture`

``` purescript
parseIPvFuture :: Parser String
```

#### `parseIPv6Address`

``` purescript
parseIPv6Address :: Parser String
```

#### `parseIPv4Address`

``` purescript
parseIPv4Address :: Parser String
```

#### `patIPv4Address`

``` purescript
patIPv4Address :: String
```


#### `patDecOctet`

``` purescript
patDecOctet :: String
```

#### `parseRegName`

``` purescript
parseRegName :: Parser String
```

#### `parsePath`

``` purescript
parsePath :: Parser String
```

#### `parsePathAbEmpty`

``` purescript
parsePathAbEmpty :: Parser String
```

#### `parsePathAbsolute`

``` purescript
parsePathAbsolute :: Parser String
```

#### `parsePathNoScheme`

``` purescript
parsePathNoScheme :: Parser String
```

#### `parsePathRootless`

``` purescript
parsePathRootless :: Parser String
```

#### `parseSegment`

``` purescript
parseSegment :: Parser String
```

#### `parseSegmentNZ`

``` purescript
parseSegmentNZ :: Parser String
```

#### `parseSegmentNZNC`

``` purescript
parseSegmentNZNC :: Parser String
```

#### `parsePChar`

``` purescript
parsePChar :: Parser String
```

#### `parseQuery`

``` purescript
parseQuery :: Parser String
```

#### `parseFragment`

``` purescript
parseFragment :: Parser String
```

#### `parsePCTEncoded`

``` purescript
parsePCTEncoded :: Parser String
```

#### `parseUnreserved`

``` purescript
parseUnreserved :: Parser String
```

#### `parseSubDelims`

``` purescript
parseSubDelims :: Parser String
```

#### `rep`

``` purescript
rep :: Number -> Parser String -> Parser String
```


#### `anyMatch`

``` purescript
anyMatch :: Rx.Regex -> Parser String
```


#### `match1From`

``` purescript
match1From :: Rx.Regex -> Number -> String -> Maybe String
```


#### `match1FromImpl`

``` purescript
match1FromImpl :: forall a. Fn5 (a -> Maybe a) (Maybe a) Rx.Regex Number String (Maybe String)
```




