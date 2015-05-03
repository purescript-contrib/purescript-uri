module Network.URI where

import Control.Alt ((<|>))
import Control.Apply ((<*), (*>))
import Control.Monad (replicateM)
import Data.Array (null)
import Data.Either (Either(..))
import Data.Function (Fn5(), runFn5)
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.String (joinWith, charAt, fromChar, length)
import Text.Parsing.StringParser (Parser(..), ParseError(..), runParser, try)
import Text.Parsing.StringParser.Combinators ((<?>), many, many1, sepBy, sepBy1, optionMaybe, manyTill, lookAhead)
import Text.Parsing.StringParser.String (string, anyChar, eof)
import qualified Data.String.Regex as Rx

parse :: String -> Either ParseError _
parse = runParser parseURI

-- URI = scheme ":" hier-part [ "?" query ] [ "#" fragment ]
parseURI :: Parser _
parseURI = { scheme: _, hierarchicalPart: _, query: _, fragment: _ }
       <$> (parseScheme <* string ":")
       <*> parseHierarchicalPart
       <*> optionMaybe (string "?" *> parseQuery)
       <*> optionMaybe (string "#" *> parseFragment)

-- hier-part = "//" authority path-abempty
--           / path-absolute
--           / path-rootless
--           / path-empty
parseHierarchicalPart :: Parser _
parseHierarchicalPart = ({ authority: _, path: _ } <$> optionMaybe (string "//" *> parseAuthority) <*> parsePathAbEmpty)
                    <|> ({ authority: Nothing, path: _ } <$> (parsePathAbsolute
                                                          <|> parsePathRootless
                                                          <|> pure ""))

-- URI-reference = URI / relative-ref
uriReference :: Parser _
uriReference = { uri: _, relativeRef: _ }
           <$> parseURI
           <*> parseRelativeRef

-- absolute-URI = scheme ":" hier-part [ "?" query ]
parseAbsoluteURI :: Parser _
parseAbsoluteURI = { scheme: _, hierarchicalPart: _, query: _ }
               <$> (parseScheme <* string ":")
               <*> parseHierarchicalPart
               <*> optionMaybe (string "?" *> parseQuery)

-- relative-ref = relative-part [ "?" query ] [ "#" fragment ]
parseRelativeRef :: Parser _
parseRelativeRef = { relativePart: _, query: _, fragment: _ }
               <$> parseRelativePart
               <*> optionMaybe (string "?" *> parseQuery)
               <*> optionMaybe (string "#" *> parseFragment)

-- relative-part = "//" authority path-abempty
--               / path-absolute
--               / path-noscheme
--               / path-empty
parseRelativePart :: Parser _
parseRelativePart = ({ authority: _, path: _ } <$> optionMaybe (string "//" *> parseAuthority) <*> parsePathAbEmpty)
                <|> ({ authority: Nothing, path: _ } <$> (parsePathAbsolute
                                                      <|> parsePathNoScheme
                                                      <|> pure ""))

-- scheme = ALPHA *( ALPHA / DIGIT / "+" / "-" / "." )
parseScheme :: Parser String
parseScheme = anyMatch $ Rx.regex "[a-z][a-z0-9+\\.\\-]*" (Rx.noFlags { ignoreCase = true })

-- authority = [ userinfo "@" ] host [ ":" port ]
parseAuthority :: Parser _
parseAuthority = { userInfo: _, host: _, port: _ }
             <$> optionMaybe (parseUserInfo)
             <*> parseHost
             <*> optionMaybe (string ":" *> parsePort)

-- userinfo = *( unreserved / pct-encoded / sub-delims / ":" )
parseUserInfo :: Parser String
parseUserInfo = try ((joinWith "" <$> many1 (parseUnreserved
                                   <|> parsePCTEncoded
                                   <|> parseSubDelims
                                   <|> string ":")) <* string "@")

-- host = IP-literal / IPv4address / reg-name
parseHost :: Parser String
parseHost = parseIPLiteral <|> parseIPv4Address <|> parseRegName

-- port = *DIGIT
parsePort :: Parser String
parsePort = anyMatch $ Rx.regex "[0-9]" Rx.noFlags

-- IP-literal = "[" ( IPv6address / IPvFuture  ) "]"
parseIPLiteral :: Parser String
parseIPLiteral = string "[" *> (parseIPv6Address <|> parseIPvFuture) <* string "]"

-- IPvFuture = "v" 1*HEXDIG "." 1*( unreserved / sub-delims / ":" )
parseIPvFuture :: Parser String
parseIPvFuture = do
  string "v"
  start <- anyMatch $ Rx.regex "[0-9a-f]+" (Rx.noFlags { ignoreCase = true })
  string "."
  end <- joinWith "" <$> many1 (parseUnreserved <|> parseSubDelims <|> string ":")
  return $ "v" ++ start ++ "." ++ end

-- IPv6address =                            6( h16 ":" ) ls32
--             /                       "::" 5( h16 ":" ) ls32
--             / [               h16 ] "::" 4( h16 ":" ) ls32
--             / [ *1( h16 ":" ) h16 ] "::" 3( h16 ":" ) ls32
--             / [ *2( h16 ":" ) h16 ] "::" 2( h16 ":" ) ls32
--             / [ *3( h16 ":" ) h16 ] "::"    h16 ":"   ls32
--             / [ *4( h16 ":" ) h16 ] "::"              ls32
--             / [ *5( h16 ":" ) h16 ] "::"              h16
--             / [ *6( h16 ":" ) h16 ] "::"
-- h16         = 1*4HEXDIG
-- ls32        = ( h16 ":" h16 ) / IPv4address
parseIPv6Address :: Parser String
parseIPv6Address = pat ["(", h16, ":){6}", ls32]
               <|> pat ["::", "(", h16, ":){5}", ls32]
               <|> pat [h16, "?", "::", "(", h16, ":){4}", ls32]
               <|> pat ["(", h16, "{,1}", ":", h16, ")?", "::", "(", h16, ":){3}", ls32]
               <|> pat ["(", h16, "{,2}", ":", h16, ")?", "::", "(", h16, ":){2}", ls32]
               <|> pat ["(", h16, "{,3}", ":", h16, ")?", "::", h16, ":", ls32]
               <|> pat ["(", h16, "{,4}", ":", h16, ")?", "::", ls32]
               <|> pat ["(", h16, "{,5}", ":", h16, ")?", "::", h16]
               <|> pat ["(", h16, "{,6}", ":", h16, ")?", "::"]
  where
  pat ps = anyMatch $ Rx.regex (joinWith "" ps) (Rx.noFlags { ignoreCase = true })
  h16 = "([a-f0-9]{1,4})"
  ls32 = joinWith "" ["(", h16, ":", h16, "|", patIPv4Address, ")"]


-- IPv4address = dec-octet "." dec-octet "." dec-octet "." dec-octet
parseIPv4Address :: Parser String
parseIPv4Address = anyMatch $ Rx.regex patIPv4Address (Rx.noFlags { ignoreCase = true })

patIPv4Address :: String
patIPv4Address = joinWith "" ["(", patDecOctet, ".", patDecOctet, ".", patDecOctet, ".", patDecOctet, ")"]

-- dec-octet = DIGIT                 ; 0-9
--           / %x31-39 DIGIT         ; 10-99
--           / "1" 2DIGIT            ; 100-199
--           / "2" %x30-34 DIGIT     ; 200-249
--           / "25" %x30-35          ; 250-255
patDecOctet :: String
patDecOctet = "([0-9]" ++
              "|([1-9][0-9])" ++
              "|(1[0-9]{2})" ++
              "|(2[0-4][0-9])" ++
              "|(25[0-5]))"

-- reg-name = *( unreserved / pct-encoded / sub-delims )
parseRegName :: Parser String
parseRegName = try (joinWith "" <$> many1 (parseUnreserved <|> parsePCTEncoded <|> parseSubDelims))

-- path = path-abempty    ; begins with "/" or is empty
--      / path-absolute   ; begins with "/" but not "//"
--      / path-noscheme   ; begins with a non-colon segment
--      / path-rootless   ; begins with a segment
--      / path-empty      ; zero characters
parsePath :: Parser String
parsePath = parsePathAbEmpty
        <|> parsePathAbsolute
        <|> parsePathNoScheme
        <|> parsePathRootless
        <|> pure ""

-- path-abempty = *( "/" segment )
parsePathAbEmpty :: Parser String
parsePathAbEmpty = try ((++) <$> string "/" <*> parseSegment) <|> pure ""

-- path-absolute = "/" [ segment-nz *( "/" segment ) ]
parsePathAbsolute :: Parser String
parsePathAbsolute = do
  string "/"
  start <- parseSegmentNZ
  rest <- (++) <$> string "/" <*> parseSegment
  return $ "/" ++ start ++ rest

-- path-noscheme = segment-nz-nc *( "/" segment )
parsePathNoScheme :: Parser String
parsePathNoScheme = (++) <$> parseSegmentNZNC <*> (joinWith "" <$> many ((++) <$> string "/" <*> parseSegment))

-- path-rootless = segment-nz *( "/" segment )
parsePathRootless :: Parser String
parsePathRootless = (++) <$> parseSegmentNZ <*> (joinWith "" <$> many ((++) <$> string "/" <*> parseSegment))

-- segment = *pchar
parseSegment :: Parser String
parseSegment = joinWith "" <$> many parsePChar

-- segment-nz = 1*pchar
parseSegmentNZ :: Parser String
parseSegmentNZ = joinWith "" <$> many1 parsePChar

-- segment-nz-nc = 1*( unreserved / pct-encoded / sub-delims / "@" )
--               ; non-zero-length segment without any colon ":"
parseSegmentNZNC :: Parser String
parseSegmentNZNC = joinWith "" <$> many1 (parseUnreserved
                                      <|> parsePCTEncoded
                                      <|> parseSubDelims
                                      <|> string "@")

-- pchar = unreserved / pct-encoded / sub-delims / ":" / "@"
parsePChar :: Parser String
parsePChar = parseUnreserved
         <|> parsePCTEncoded
         <|> parseSubDelims
         <|> string ":"
         <|> string "@"

-- query = *( pchar / "/" / "?" )
parseQuery :: Parser String
parseQuery = try (joinWith "" <$> many (parsePChar <|> string "/" <|> string "?"))

-- fragment = *( pchar / "/" / "?" )
parseFragment :: Parser String
parseFragment = try (joinWith "" <$> many (parsePChar <|> string "/" <|> string "?"))

-- pct-encoded = "%" HEXDIG HEXDIG
parsePCTEncoded :: Parser String
parsePCTEncoded = anyMatch $ Rx.regex "%[0-9a-f]{2}" (Rx.noFlags { ignoreCase = true })

-- unreserved = ALPHA / DIGIT / "-" / "." / "_" / "~"
parseUnreserved :: Parser String
parseUnreserved = anyMatch $ Rx.regex "[0-9a-z\\-\\._~]" (Rx.noFlags { ignoreCase = true })

-- sub-delims = "!" / "$" / "&" / "'" / "(" / ")"
--            / "*" / "+" / "," / ";" / "="
parseSubDelims :: Parser String
parseSubDelims = anyMatch $ Rx.regex "[!$&'()*+,;=]" (Rx.noFlags { ignoreCase = true })

rep :: Number -> Parser String -> Parser String
rep n p = joinWith "" <$> replicateM n p

anyMatch :: Rx.Regex -> Parser String
anyMatch rx = Parser \{ str: str, pos: i } fc sc -> case match1From rx i str of
  Just m -> sc m { str: str, pos: i + (length m) }
  Nothing -> fc i (ParseError $ "Expected " ++ show rx)

match1From :: Rx.Regex -> Number -> String -> Maybe String
match1From rx i str = runFn5 match1FromImpl Just Nothing rx i str

foreign import match1FromImpl
  """
  function match1FromImpl(just, nothing, rx, i, str) {
    var rxStr = rx.toString();
    var flagIndex = rxStr.lastIndexOf("/");
    var pattern = rxStr.substring(1, flagIndex);
    var flags = rxStr.substring(flagIndex + 1);
    rx = new RegExp(pattern, flags.indexOf("g") === -1 ? flags + "g" : flags);
    rx.lastIndex = i;
    var result = rx.exec(str);
    return result && result.index === i ? just(result[0]) : nothing;
  }
  """ :: forall a. Fn5 (a -> Maybe a) (Maybe a) Rx.Regex Number String (Maybe String)
