{-# LANGUAGE FlexibleContexts #-}

module Main where

import Conduit (Source, (.|), mapC, runConduitRes, sinkFile, yield)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString.Char8 as C
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import Data.List (isPrefixOf)
import Data.List.NonEmpty (NonEmpty((:|)))
import System.Environment (lookupEnv)
import System.FilePath (makeRelative, takeExtensions)
import Text.Megaparsec
import Text.Megaparsec.ByteString.Lazy (Parser)
import Text.Megaparsec.Lexer hiding (space)

data Entry = Blob Mark Data
           | Reset Ref
           | Commit
             { cRef :: Ref
             , cMark :: Mark
             , cAuthor :: Author
             , cCommitter :: Committer
             , cComment :: Comment
             , cParents :: [Parent]
             , cFileMods :: [FileMod]
             }
           deriving (Show)

type Mark = Integer
type Data = ByteString
type Ref = String
type Author = Actor
type Committer = Actor
type Comment = ByteString

data Actor = Actor Name Email Timestamp
  deriving (Show)
type Name = String
type Email = String
type Timestamp = String

data FileMod = FileMod FileMode DataRef FilePath
  deriving (Show)
data DataRef = MarkRef Mark
             | HashRef Hash
             deriving (Show)
type Parent = DataRef
type Hash = String
data FileMode = Normal | Executable | Symlink | Gitlink | Subdir
  deriving (Show)

toOctal :: FileMode -> String
toOctal Normal = "100644"
toOctal Executable = "100755"
toOctal Symlink = "120000"
toOctal Gitlink = "160000"
toOctal Subdir = "040000"

fromOctal :: String -> Maybe FileMode
fromOctal "100644" = Just Normal
fromOctal "644" = Just Normal
fromOctal "100755" = Just Executable
fromOctal "755" = Just Executable
fromOctal "120000" = Just Symlink
fromOctal "160000" = Just Gitlink
fromOctal "040000" = Just Subdir
fromOctal _ = Nothing

simpleShow :: Entry -> String
simpleShow (Blob m d) =
  concat [ "Blob"
         , " Mark=" ++ show m
         , " Data=" ++ show (BS.length d)
         ]
simpleShow other = show other

fastexport :: Parser [Entry]
fastexport = some entry

pack :: String -> ByteString
pack = BS.fromStrict . C.pack

lines' :: ByteString -> [ByteString]
lines' = fmap BS.fromStrict . C.lines . BS.toStrict

datalen :: Parser Integer
datalen = toInteger <$> (string "data " >> integer)

entry :: Parser Entry
entry = choice [blob, reset, commit]
  where
    markref :: Parser Mark
    markref = char ':' >> integer
    mark' :: Parser Mark
    mark' = string "mark " >> markref
    data' :: Parser ByteString
    data' = do
      len <- datalen <* newline
      pack <$> (count (fromIntegral len) anyChar <* skipMany newline)
    blob :: Parser Entry
    blob = do
      mark <- string "blob" >> newline >> mark' <* newline
      content <- data'
      return $ Blob mark content
    toeol :: Parser String
    toeol = anyChar `someTill` newline
    reset :: Parser Entry
    reset = Reset <$> (string "reset " >> toeol)
    actor :: Parser Actor
    actor = do
      name <- anyChar `someTill` string " <"
      email <- anyChar `someTill` string "> "
      timestamp <- toeol
      return $ Actor name email timestamp
    filemode' :: Parser FileMode
    filemode' = do
      modestring <- (count 6 octDigitChar <|> count 3 octDigitChar)
      case fromOctal modestring of
        Nothing -> fail "Expected a valid Git file mode"
        Just mode -> return mode
    cQuoted :: Parser Char
    cQuoted = literalChar <|> escapeSequence
    literalChar :: Parser Char
    literalChar = noneOf "\\\"\n"
    escapeSequence :: Parser Char
    escapeSequence = char '\\' >>
      escapedLiteral <|> specialChar <|> octalEscape
    escapedLiteral :: Parser Char
    escapedLiteral = oneOf "\\\""
    specialChar :: Parser Char
    specialChar = do
      c <- anyChar
      case c of
        'n' -> return '\n'
        'r' -> return '\r'
        't' -> return '\t'
        ' ' -> return ' '
        _ -> fail "Not a valid escape sequence" -- TODO others, I'm sure...
    octalEscape :: Parser Char
    octalEscape = do
      digits <- count 3 octDigitChar
      let val = read ("0o" ++ digits)
       in if val <= 255 then return (toEnum val) else fail "Invalid escape"
    quotedPath :: Parser String
    quotedPath = char '"' >> cQuoted `manyTill` char '"'
    filemod :: Parser FileMod
    filemod = do
      filemode <- char 'M' >> spaceChar >> filemode' <* spaceChar
      mark <- markref <* spaceChar
      path <- (quotedPath <* newline) <|> toeol
      return $ FileMod filemode (MarkRef mark) path
    parent :: Parser Parent
    parent = MarkRef <$> (string "from " >> markref <* newline)
    commit :: Parser Entry
    commit = do
      ref <- string "commit " >> toeol
      mark <- mark' <* newline
      author <- string "author " >> actor
      committer <- string "committer " >> actor
      comment <- data'
      parents <- many parent
      entries <- filemod `manyTill` (try (lookAhead newline)) <* newline
      return $ Commit ref mark author committer comment parents entries

entrySource :: MonadIO m => FilePath -> Source m Entry
entrySource filename = do
  contents <- liftIO (BS.readFile filename)
  readEntries (parseState contents)
  where
    readEntries :: MonadIO m => State ByteString -> Source m Entry
    readEntries s = do
      let (s'@(State rst _ _ _), result) = runParser' entry s
       in case result of
            Left _ -> do
              fail "Failed to parse entry"
            Right e -> do
              yield e
              if rst /= BS.empty
                 then readEntries s'
                 else return ()
    parseState :: ByteString -> State ByteString
    parseState input = State input (initialPos "" :| []) 0 (unsafePos 1)

unparse :: Entry -> ByteString
unparse (Blob m d) = BS.concat [header, d, pack "\n"]
  where
    header = pack $ unlines ["blob", mark, len]
    mark = "mark :" ++ show m
    len = unwords ["data", show $ BS.length d]
unparse (Reset ref) = pack $ unlines ["reset " ++ ref]
unparse commit = BS.concat $ [header] ++ comment ++ parents ++ filemods ++ nl
  where
    header = pack $ unlines [cref, mark, author, committer]
    cref = unwords ["commit", cRef commit]
    mark = "mark :" ++ show (cMark commit)
    actor (Actor n e t) = n ++ " <" ++ e ++ "> " ++ t
    author = "author " ++ actor (cAuthor commit)
    committer = "committer " ++ actor (cCommitter commit)
    commentData = cComment commit
    comment = [ pack $ unlines ["data " ++ show (BS.length commentData)]
              , commentData ]
    showref :: DataRef -> String
    showref (MarkRef m) = ":" ++ show m
    showref (HashRef h) = h
    parents = parentLine <$> cParents commit
    parentLine ref = pack $ unlines ["from " ++ showref ref]
    filemods = fileLine <$> cFileMods commit
    fileLine (FileMod mode ref path) =
      pack $ unlines [unwords ["M", toOctal mode, showref ref, path]]
    nl = [pack $ unlines [""]]

fixref :: Ref -> Entry -> Entry
fixref ref (Reset _) = Reset ref
fixref ref c@(Commit {}) = c { cRef = ref }
fixref _ blob = blob

fixemail :: String -> Entry -> Entry
fixemail email c@(Commit {}) =
  let Actor aname _ atime = cAuthor c
      Actor cname _ ctime = cCommitter c
   in c { cAuthor = Actor aname email atime
        , cCommitter = Actor cname email ctime
        }
fixemail _ e = e

fixpaths :: String -> String -> Entry -> Entry
fixpaths base ext e =
  case e of
    (Commit { cFileMods = files }) -> e { cFileMods = fixfile <$> files }
    _ -> e
  where
    fixfile :: FileMod -> FileMod
    fixfile (FileMod mode ref path) =
      let path' = if base `isPrefixOf` path && (ext == takeExtensions path)
                     then makeRelative base path
                     else path
       in FileMod mode ref path'

main :: IO ()
main = do
  mref <- lookupEnv "REF"
  memail <- lookupEnv "EMAIL"
  mbase <- lookupEnv "BASE"
  mext <- lookupEnv "EXT"
  let refMunge = maybe id fixref mref
      emailMunge = maybe id fixemail memail
      munger = \base -> fixpaths base (maybe "" id mext)
      pathMunge = maybe id munger mbase
   in runConduitRes $ entrySource "/dev/stdin"
        .| mapC (pathMunge . emailMunge . refMunge)
        .| mapC (BS.toStrict . unparse)
        .| sinkFile "/dev/stdout"
