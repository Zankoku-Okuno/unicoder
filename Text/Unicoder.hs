module Text.Unicoder (
      unicodize
    , unicodize'
    , unicodizeStr
    , Config
    , parseConfigFile
    ) where

import System.IO

import qualified Data.Text as ST
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT

import Data.Maybe
import Data.Either
import Control.Applicative
import Control.Monad


unicodize :: Config -> LT.Text -> LT.Text
unicodize = undefined

unicodize' :: Config -> ST.Text -> ST.Text
unicodize' = undefined

unicodizeStr :: Config -> String -> String
unicodizeStr config = ST.unpack . unicodize' config . ST.pack


data Config = Config { _idChars :: Char -> Bool
                     , _endMark :: Maybe String
                     , _betweenMarks :: Maybe (String, String)
                     , _macros0 :: [(String, String)]
                     , _macros1 :: [(String, (String, String))]
                     }

parseConfigFile :: FilePath -> IO (Maybe Config)
parseConfigFile path = withFile path ReadMode $ \fp -> do
    xs <- filter (not . LT.null) . LT.lines <$> LT.hGetContents fp
    return $ case xs of
        [] -> Nothing
        (lexer:raw_macros) -> do
            emptyConfig <- case LT.words lexer of
                [idChars] -> return $
                    Config { _idChars = parseIdChars (LT.unpack idChars)
                           , _endMark = Nothing
                           , _betweenMarks = Nothing
                           , _macros0 = [], _macros1 = []
                           }
                [end, idChars] -> return $
                    Config { _idChars = parseIdChars (LT.unpack idChars)
                           , _endMark = Just (LT.unpack end)
                           , _betweenMarks = Nothing
                           , _macros0 = [], _macros1 = []
                           }
                [end, open, close, idChars] -> return $
                    Config { _idChars = parseIdChars (LT.unpack idChars)
                           , _endMark = Just (LT.unpack end)
                           , _betweenMarks = Just (LT.unpack open, LT.unpack close)
                           , _macros0 = [], _macros1 = []
                           }
                _ -> Nothing
            let (macros0, macros1) = partitionEithers . catMaybes $ parseMacro <$> raw_macros
            return $ emptyConfig { _macros0 = macros0 }

parseIdChars :: String -> (Char -> Bool)
parseIdChars = go "" []
    where
    go singles ranges "" = \c -> c `elem` singles || any (between c) ranges
    go singles ranges (a:'-':b:rest) = go singles ((a,b):ranges) rest
    go singles ranges (c:rest) = go (c:singles) ranges rest
    between c (a, b) = a <= c && c <= b

parseMacro :: LT.Text -> Maybe (Either (String, String) (String, (String, String)))
parseMacro input = case LT.words input of
    [k, v] -> Just $ Left (LT.unpack k, LT.unpack v)
    [k, v1, v2] -> Just $ Right (LT.unpack k, (LT.unpack v1, LT.unpack v2))
    _ -> Nothing

