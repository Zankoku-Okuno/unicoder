{-------------------------------------------------------------------------------
Unicoder v0.2.0
Copyright (c) 2013, Okuno Zankoku
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
import Data.Char
import Text.Parsec
import Text.Parsec.Combinator
import Text.Parsec.Char
import Control.Monad.IO.Class

progVersion = "v0.2.0"
symbolFile Options { optConfig = which } = "/etc/zankoku-okuno/unicoder/" ++ which ++ ".conf"

main :: IO ()
main = do
        (opts, args) <- getOptions
        symbols <- return . parseSymbols =<< readFile (symbolFile opts)
        -- TODO -o/--output flag, but how for many files?
        if args == []
          then die "no input files"
          else mapM (mainLoop symbols) args
        exitSuccess
    where parseSymbols = map makePair . filter ((==2) . length) . map words . lines

mainLoop :: Lookup -> FilePath -> IO ()    
mainLoop symbols filename = do
        source <- readFile filename
        handle <- openFile tmpname WriteMode
        result <- runPT cleaner (symbols, handle) filename source
        hClose handle
        case result of
            Left err -> die (show err)
            Right val -> renameFile tmpname filename
    where tmpname = filename ++ ".tmp" -- FIXME use a real tmp file


type Lookup = [(String, String)]
type Parser a = ParsecT String (Lookup, Handle) IO a

cleaner :: Parser ()
cleaner = many (anything <|> try replace <|> backslash) >> eof
    where
    anything = many1 (noneOf "\\") >>= putCode >> return ()
    replace = do
        char '\\'
        name <- many1 letter
        delim <- char '.' <|> space <|> (eof >> return '\n')
        (table, _) <- getState
        case lookup name table of
            Nothing -> fail ""
            Just val -> putCode $ val ++ if delim == '.' then "" else delim:""
    backslash = char '\\' >> putCode "\\"
    putCode x = do
        (_, handle) <- getState
        liftIO $ (hPutStr handle) x

die err = do
    putErrLn err
    exitFailure

makePair [a, b] = (a, b)
putErrLn = hPutStrLn stderr

-- The following architecture is taken from http://www.haskell.org/haskellwiki/High-level_option_handling_with_GetOpt

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
                hPutStrLn stderr progVersion
                exitWith ExitSuccess))
        "Print version"
    , Option "h" ["help"]
        (NoArg
            (\_ -> do
                prg <- getProgName
                putErrLn "Ease input of unicode glyphs."
                putErrLn "This program is lisenced under the 3-clause BSD lisence."
                putErrLn (usageInfo (prg ++ " [options] files...") options)
                exitWith ExitSuccess))
        "Show help"
    ]

