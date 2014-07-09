{-------------------------------------------------------------------------------
Copyright (c) 2013, 2014, Okuno Zankoku
All rights reserved. 

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

  * Redistributions of source code must retain the above copyright notice, this 
  list of conditions and the following disclaimer.

  * Redistributions in binary form must reproduce the above copyright notice,
  this list of conditions and the following disclaimer in the documentation
  and/or other materials provided with the distribution.

  * Neither the name of Okuno Zankoku nor the names of contributors may be used
  to endorse or promote products derived from this software without specific
  prior written permission.

THIS SOFTWARE IS PROVIDED BY OKUNO ZANKOKU "AS IS" AND ANY EXPRESS OR
IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO
EVENT SHALL OKUNO ZANKOKU BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, 
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-------------------------------------------------------------------------------}
import System.Environment
import System.IO
import System.Directory
import System.Exit
import System.Console.GetOpt
import Data.List (nub)
import Data.Char
import Text.Parsec
import Text.Parsec.Combinator
import Text.Parsec.Char
import Control.Monad
import Control.Monad.Trans

import Paths_unicoder
import Data.Version

symbolFile :: Options -> IO FilePath
symbolFile Options { optConfig = which } = 
    if '/' `elem` which
        then return which
        else getDataFileName (which ++ ".conf")

main :: IO ()
main = do
        (opts, args) <- getOptions
        (config, symbols) <- parseSymbols =<< readFile =<< symbolFile opts
        -- TODO -o/--output flag, but how for many files?
        mapM (mainLoop config symbols) args
        exitSuccess
    where
    parseSymbols input = case lines input of
        (top:rest) -> do
            config <- processTop top
            return (config, processSymbols rest)
        _ -> die "bad configuration: missing allowed characters"
    processTop line = case words line of
        [] -> die "missing allowed characters"
        [chars] -> return (chars, Nothing)
        [delim, chars] -> return (chars, Just delim)
        _ -> die "too many fields on top line"
    processSymbols = map makePair . filter ((==2) . length) . map words

mainLoop :: Config -> Lookup -> FilePath -> IO ()    
mainLoop config symbols filename = do
    --I think I can make use of lazy text and Haskell's inherent laziness to strem a file through
        source <- readFile filename
        handle <- openFile tmpname WriteMode
        result <- runPT (cleaner config) (symbols, handle) filename source
        hClose handle
        case result of
            Left err -> die (show err)
            Right val -> renameFile tmpname filename
    where tmpname = filename ++ ".unicoder.tmp" -- FIXME use a real tmp file

type Config = (String, Maybe String)
type Lookup = [(String, String)]
type Parser a = ParsecT String (Lookup, Handle) IO a

cleaner :: Config -> Parser ()
cleaner (chars, delim) = many (anything <|> try replace <|> backslash) >> eof
    where
    anything = many1 (noneOf "\\") >>= putCode
    replace = do
        name <- char '\\' >> many1 (oneOf chars)
        case delim of
            Nothing -> return ()
            Just delim -> optional . void $  try (string delim)
        (table, _) <- getState
        case lookup name table of
            Nothing -> fail ""
            Just val -> putCode val
    backslash = char '\\' >> putCode "\\"
    putCode x = do
        (_, handle) <- getState
        liftIO $ (hPutStr handle) x

die err = do
    putErrLn err
    exitFailure

makePair [a, b] = (a, b)
putErrLn = hPutStrLn stderr


data Options = Options  { optConfig :: String
                        , optOutput :: Maybe FilePath
                        } deriving (Show)
startOptions = Options  { optConfig = "default"
                        , optOutput = Nothing
                        }

getOptions :: IO (Options, [FilePath])
getOptions = do
    (actions, args, errors) <- return . getOpt Permute options =<< getArgs
    if errors == []
      then do
        opts <- foldl (>>=) (return startOptions) actions
        when (null args) $ die "no input files"
        return (opts, args)
      else do
        mapM putErrLn errors
        exitFailure

options :: [OptDescr (Options -> IO Options)]
options =
    [ Option "c" ["config"]
        (ReqArg 
            (\arg opt -> return opt { optConfig = arg })
            "file")
        "Specify a configuration (usually a programming language)."

    , Option "V" ["version"]
        (NoArg
            (\_ -> do
                hPutStrLn stderr (showVersion version)
                exitSuccess))
        "Print version."
    , Option "" ["show-config-dir"]
        (NoArg
            (\_ -> do
                hPutStrLn stderr =<< getDataFileName ""
                exitSuccess))
        "Print directory where configuration files are stored."
    , Option "h" ["help"]
        (NoArg
            (\_ -> do
                prg <- getProgName
                putErrLn "Ease input of unicode glyphs."
                putErrLn "This program is lisenced under the 3-clause BSD lisence."
                putErrLn (usageInfo (prg ++ " [options] files...") options)
                putErrLn "Run on one or more files to replace special sequences of characters (as defined in a config file) with replacements."
                putErrLn "The idea is to put in useful unicode symbols and then use those symbols in source code."
                putErrLn " -- Config Files"
                putErrLn "    Configuration files are stored in `/etc/zankoku-okuno/unicoder`, and end in `.conf`."
                putErrLn "    Passing a file to `-c` that contains a slash will use exactly the file you specify, instead of looking one up."
                putErrLn "    Configuration files consist of a top line and a body. The body is simply a database with two whitespace-separated fields. \
                             \The first is the a name, and the second is the replacement. The top line contains one or two whitespace-separated fields. \
                             \The first (optional) is a separator, which is optionally consumed after matching a name in the database. The second holds all the characters than can be used to define and use a name."
                exitSuccess))
        "Show help"
    ]

