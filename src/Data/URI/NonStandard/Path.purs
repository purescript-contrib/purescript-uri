module Data.URI.NonStandard.Path
  ( URIPath
  , URIPathAbs
  , URIPathRel
  , parseURIPathAbs
  , parseURIPathRel
  , printPath
  ) where

import Prelude

import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Path.Pathy (Abs, Dir, Escaper(..), File, Path, Rel, Sandboxed, Unsandboxed, parseAbsDir, parseAbsFile, parseRelDir, parseRelFile, rootDir, sandbox, unsafePrintPath', (</>))
import Data.String as Str
import Global (encodeURI)
import Text.Parsing.StringParser (ParseError(..), Parser(..))

-- | A general URI path, can be used to represent relative or absolute paths
-- | that are sandboxed or unsandboxed.
type URIPath a s = Either (Path a Dir s) (Path a File s)

-- | The path part for a generic or absolute URI.
type URIPathAbs = URIPath Abs Sandboxed

-- | The path part for a relative reference.
type URIPathRel = URIPath Rel Unsandboxed

parseURIPathAbs ∷ Parser URIPathAbs
parseURIPathAbs = Parser \{ str: str, pos: i } →
  case sandbox rootDir =<< parseAbsFile (Str.drop i str) of
    Just file → Right { result: (Right $ rootDir </> file), suffix: { str: str, pos: Str.length str }}
    Nothing → case sandbox rootDir =<< parseAbsDir (Str.drop i str) of
      Just dir → Right { result: (Left $ rootDir </> dir), suffix: { str: str, pos: Str.length str }}
      Nothing → Left { error: (ParseError $ "Expected a valid path"), pos: i }

parseURIPathRel ∷ Parser URIPathRel
parseURIPathRel = Parser \{ str: str, pos: i } →
  case parseRelFile (Str.drop i str) of
    Just file → Right { result : Right file, suffix: { str: str, pos: Str.length str }}
    Nothing → case parseRelDir (Str.drop i str) of
      Just dir →  Right { result : Left dir,  suffix: { str: str, pos: Str.length str }}
      Nothing → Left { error: (ParseError $ "Expected a valid path"), pos: i}

printPath ∷ ∀ a s. URIPath a s → String
printPath = either printPath' printPath'

printPath' ∷ ∀ a' b s'. Path a' b s' → String
printPath' path =
  let printed = unsafePrintPath' escaper path
  in fromMaybe printed $ Str.stripPrefix (Str.Pattern "./") printed

escaper ∷ Escaper
escaper = Escaper $
  Str.replaceAll (Str.Pattern "#") (Str.Replacement "%23") <<< encodeURI
