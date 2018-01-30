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
import Data.String as String
import Global (encodeURI)
import Text.Parsing.StringParser (ParseError(..))

-- | A general URI path, can be used to represent relative or absolute paths
-- | that are sandboxed or unsandboxed.
type URIPath a s = Either (Path a Dir s) (Path a File s)

-- | The path part for a generic or absolute URI.
type URIPathAbs = URIPath Abs Sandboxed

-- | The path part for a relative reference.
type URIPathRel = URIPath Rel Unsandboxed

parseURIPathAbs ∷ String → Either ParseError URIPathAbs
parseURIPathAbs str =
  case sandbox rootDir =<< parseAbsFile str of
    Just file → Right $ Right (rootDir </> file)
    Nothing → case sandbox rootDir =<< parseAbsDir str of
      Just dir → Right $ Left (rootDir </> dir)
      Nothing → Left (ParseError "Expected a valid path")

parseURIPathRel ∷ String → Either ParseError URIPathRel
parseURIPathRel str =
  case parseRelFile str of
    Just file → Right (Right file)
    Nothing → case parseRelDir str of
      Just dir → Right (Left dir)
      Nothing → Left (ParseError "Expected a valid path")

printPath ∷ ∀ a s. URIPath a s → String
printPath = either printPath' printPath'

printPath' ∷ ∀ a' b s'. Path a' b s' → String
printPath' path =
  let printed = unsafePrintPath' escaper path
  in fromMaybe printed $ String.stripPrefix (String.Pattern "./") printed

escaper ∷ Escaper
escaper = Escaper $
  String.replaceAll (String.Pattern "#") (String.Replacement "%23") <<< encodeURI
