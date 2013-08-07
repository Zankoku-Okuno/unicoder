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
import Data.Char
import Text.Parsec
import Text.Parsec.Combinator
import Text.Parsec.Char
import Control.Monad.IO.Class


symbolFile = "/etc/zankoku-okuno/unicoder/symbols.conf"

main :: IO ()
main = do
    symbols <- return . map makePair . filter (\x -> length x == 2) . map words . lines =<< readFile symbolFile
    args <- getArgs
    if args == []
      then die "no input files"
      else mapM (mainLoop symbols) args
    exitSuccess

mainLoop :: Lookup -> FilePath -> IO ()    
mainLoop symbols filename = do
        source <- readFile filename
        handle <- openFile tmpname WriteMode
        result <- runPT cleaner (symbols, handle) filename source
        hClose handle
        case result of
            Left err -> die (show err)
            Right val -> renameFile tmpname filename
    where tmpname = filename ++ ".tmp"


type Lookup = [(String, String)]
type Parser a = ParsecT String (Lookup, Handle) IO a

cleaner :: Parser ()
cleaner = many (anything <|> try replace <|> backslash) >> eof

anything :: Parser ()
anything = many1 (noneOf "\\") >>= putCode >> return ()

replace :: Parser ()
replace = do
    char '\\'
    name <- many1 letter
    delim <- char '.' <|> space <|> (eof >> return '\n')
    (table, _) <- getState
    case lookup name table of
        Nothing -> putCode $ "\\" ++ name ++ [delim]
        Just val -> putCode $ val ++ if delim == '.' then "" else delim:""

backslash :: Parser ()
backslash = char '\\' >> putCode "\\"

putCode x = do
    (_, handle) <- getState
    liftIO $ (hPutStr handle) x

die err = do
    hPutStrLn stderr err
    exitFailure

makePair [a, b] = (a, b)
