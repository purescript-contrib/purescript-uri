# Module Documentation

## Module Data.URI.Common

#### `rep`

``` purescript
rep :: Number -> Parser String -> Parser String
```


#### `rxPat`

``` purescript
rxPat :: String -> Parser String
```


#### `wrapParser`

``` purescript
wrapParser :: forall a. Parser a -> Parser String -> Parser a
```


#### `parsePChar`

``` purescript
parsePChar :: Parser String
```


#### `parseUnreserved`

``` purescript
parseUnreserved :: Parser String
```


#### `parsePCTEncoded`

``` purescript
parsePCTEncoded :: Parser String
```


#### `parseSubDelims`

``` purescript
parseSubDelims :: Parser String
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




