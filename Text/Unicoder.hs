{-# LANGUAGE OverloadedStrings #-}
module Text.Unicoder (
      unicodize
    , unicodizeStr
    , Config
    , parseConfigFile
    ) where

import System.IO

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Attoparsec.Text
import Data.Attoparsec.Combinator

import Data.Maybe
import Data.Either
import Data.Monoid
import Control.Applicative
import Control.Monad


unicodize :: Config -> Text -> Text
unicodize config input = case parseOnly (xform config) input of
    Left err -> error "unicoder: internal error"
    Right val -> val

unicodizeStr :: Config -> String -> String
unicodizeStr config = T.unpack . unicodize config . T.pack


data Config = Config { _idChars      :: Char -> Bool
                     , _beginMark    :: Text
                     , _endMark      :: Maybe Text
                     , _betweenMarks :: Maybe (Text, Text)
                     , _macros0      :: [(Text, Text)]
                     , _macros1      :: [(Text, (Text, Text))]
                     }

parseConfigFile :: FilePath -> IO (Maybe Config)
parseConfigFile path = withFile path ReadMode $ \fp -> do
    xs <- filter (not . T.null) . T.lines <$> T.hGetContents fp
    return $ case xs of
        [] -> Nothing
        (lexer:raw_macros) -> do
            emptyConfig <- case filter (not . T.null) $ T.splitOn " " lexer of
                [idChars] -> return
                    Config { _idChars = inClass (T.unpack idChars)
                           , _beginMark = "\\"
                           , _endMark = Nothing
                           , _betweenMarks = Nothing
                           , _macros0 = [], _macros1 = []
                           }
                [begin, idChars] -> return
                    Config { _idChars = inClass (T.unpack idChars)
                           , _beginMark = begin
                           , _endMark = Nothing
                           , _betweenMarks = Nothing
                           , _macros0 = [], _macros1 = []
                           }
                [begin, end, idChars] -> return
                    Config { _idChars = inClass (T.unpack idChars)
                           , _beginMark = begin
                           , _endMark = Just end
                           , _betweenMarks = Nothing
                           , _macros0 = [], _macros1 = []
                           }
                [begin, end, open, close, idChars] -> return
                    Config { _idChars = inClass (T.unpack idChars)
                           , _beginMark = begin
                           , _endMark = Just end
                           , _betweenMarks = Just (open, close)
                           , _macros0 = [], _macros1 = []
                           }
                _ -> Nothing
            let (macros0, macros1) = partitionEithers . catMaybes $ parseMacro <$> raw_macros
            return $ emptyConfig { _macros0 = macros0, _macros1 = macros1 }

parseMacro :: T.Text -> Maybe (Either (Text, Text) (Text, (Text, Text)))
parseMacro input = case T.words input of
    [k, v] -> Just $ Left (k, v)
    [k, v1, v2] -> Just $ Right (k, (v1, v2))
    _ -> Nothing


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

{-TODO
a config lint 
    characters don't appear twice in the lexer
    open and end are distinguishable
    macros are defined only using idChars
    macro lines are word-length 2 or 3
-}