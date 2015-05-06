# Module Documentation

## Module Data.URI

#### `parseFileURI`

``` purescript
parseFileURI :: String -> Either ParseError (URI File)
```


#### `parseDirURI`

``` purescript
parseDirURI :: String -> Either ParseError (URI Dir)
```


#### `parserFileURI`

``` purescript
parserFileURI :: Parser (URI File)
```


#### `parserDirURI`

``` purescript
parserDirURI :: Parser (URI Dir)
```


#### `printURI`

``` purescript
printURI :: forall a. URI a -> String
```




