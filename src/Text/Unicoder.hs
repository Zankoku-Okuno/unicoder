{-|
Load unicoder configurations and oerform unicoder transformations on text and strings.

Unicoder replaces simple macros with configured strings, e.g.

> \E.x. \A.y. x \-> y
> \l.x,y. x \of x \of y

becomes

> ∃x ∀y x → y
> λx,y. x ∘ x ∘ y

For more information, see [the documentation](http://zankoku-okuno.viewdocs.io/unicoder/).
-}
module Text.Unicoder (
    -- * Unicoder Algorithm
      unicodize
    , unicodizeLazy
    , unicodizeStr
    -- * Configuration
    , Config
    , loadConfig
    , parseConfig
    ) where

import System.IO
import System.FilePath
import System.Directory

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.IO as T
import Data.Attoparsec.Text
import Data.Attoparsec.Combinator

import Data.Either
import Data.Maybe
import Data.List
import Data.Monoid
import Control.Applicative
import Control.Monad


{-| Perform the unicoder transformation on a 'Text' value. -}
unicodize :: Config -> Text -> Text
unicodize config input = case parseOnly (xform config) input of
    Left err -> error "unicoder: internal error"
    Right val -> val

{-| Perform the unicoder transformation on a lazy 'TL.Text' value. -}
unicodizeLazy :: Config -> TL.Text -> TL.Text
unicodizeLazy config = TL.fromStrict . unicodize config . TL.toStrict

{-| Perform the unicoder transformation on a 'String' value. -}
unicodizeStr :: Config -> String -> String
unicodizeStr config = T.unpack . unicodize config . T.pack


{-| Aggregate all settings needed to unicodize. -}
data Config = Config
    { _fromFile     :: FilePath
    , _idChars      :: Char -> Bool
    , _beginMark    :: Text
    , _endMark      :: Maybe Text
    , _betweenMarks :: Maybe (Text, Text)
    , _macros0      :: [(Text, Text)]
    , _macros1      :: [(Text, (Text, Text))]
    }

{-| Parse a config file, possibly failing. -}
parseConfig :: FilePath -> Text -> Maybe Config
parseConfig path contents = case removeBlanks $ T.lines contents of
    [] -> Nothing
    (lexer:raw_macros) -> do
        let empty = Config
                    { _fromFile = path
                    , _idChars = undefined
                    , _beginMark = "\\"
                    , _endMark = Nothing
                    , _betweenMarks = Nothing
                    , _macros0 = [], _macros1 = []
                    }
        lexerConfig <- case removeBlanks $ T.splitOn " " lexer of
            [idChars] -> return $
                empty { _idChars = inClass (T.unpack idChars)
                      }
            [begin, idChars] -> return $
                empty { _idChars = inClass (T.unpack idChars)
                      , _beginMark = begin
                      }
            [begin, end, idChars] -> return $
                empty { _idChars = inClass (T.unpack idChars)
                      , _beginMark = begin
                      , _endMark = Just end
                      }
            [begin, end, open, close, idChars] -> return $
                empty { _idChars = inClass (T.unpack idChars)
                      , _beginMark = begin
                      , _endMark = Just end
                      , _betweenMarks = Just (open, close)
                      }
            _ -> Nothing
        let (macros0, macros1) = partitionEithers . catMaybes $ parseMacro <$> raw_macros
        Just $ lexerConfig { _macros0 = macros0, _macros1 = macros1 }
    where
    parseMacro :: T.Text -> Maybe (Either (Text, Text) (Text, (Text, Text)))
    parseMacro input = case T.words input of
        [k, v] -> Just $ Left (k, v)
        [k, v1, v2] -> Just $ Right (k, (v1, v2))
        _ -> Nothing

{-| Parse the contents of the passed file as  unicoder `Config` -}
loadConfig :: FilePath -> IO (Maybe Config)
loadConfig path = do
    contents <- T.readFile path
    return $ parseConfig path contents


{-| The unicoder algorithm, implemented as an Attoparsec combinator. -}
xform :: Config -> Parser Text
xform config = mconcat <$> many (passthrough <|> macro <|> strayBegin) <* endOfInput
    where
    (beginStr, beginChr) = (_beginMark config, T.head beginStr)
    passthrough = takeWhile1 (/= beginChr)
    macro = do
        string beginStr
        full <|> half
    strayBegin = T.singleton <$> char beginChr
    full = do
        name <- takeWhile1 (_idChars config)
        mono name <|> di name
        where
        mono name = do
            replace <- name `lookupM` _macros0 config
            endMark
            return replace
        di name = do
            (open, close) <- betweenMarks
            (rOpen, rClose) <- name `lookupM` _macros1 config
            string open
            inner <- T.pack <$> anyChar `manyTill` string close
            return $ rOpen <> recurse inner <> rClose
    half = do
        (open, close) <- betweenMarks
        which <- (const fst <$> string open) <|> (const snd <$> string close)
        name <- takeWhile1 (_idChars config)
        replace <- which <$> name `lookupM` _macros1 config
        endMark
        return replace
    endMark = case _endMark config of
        Nothing -> return ()
        Just end -> option () $ void (string end)
    betweenMarks = case _betweenMarks config of
        Nothing -> fail "" 
        Just x -> return x
    lookupM k v = maybe (fail "") return (k `lookup` v)
    recurse = unicodize config

removeBlanks :: [Text] -> [Text]
removeBlanks = filter (not . T.null)
