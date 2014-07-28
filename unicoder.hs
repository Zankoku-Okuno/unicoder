import System.Environment
import System.IO
import System.Directory
import System.Exit
import System.Console.GetOpt
import qualified Data.Text.IO as T
import Control.Monad

import Text.Unicoder

import Paths_unicoder
import Data.Version


main :: IO ()
main = do
        (opts, args) <- getOptions
        m_config <- loadConfig =<< locateConfig "." (optConfig opts)
        config <- case m_config of
            Nothing -> die "bad configuration file"
            Just config -> return config
        -- TODO -o/--output flag, but how for many files?
        mapM_ (mainLoop config) args
        exitSuccess

mainLoop :: Config -> FilePath -> IO ()    
mainLoop config filename = do
    source <- T.readFile filename
    let result = unicodize config source
    T.writeFile filename result


die err = do
    putErrLn err
    exitFailure

makePair [a, b] = (a, b)
putErrLn = hPutStrLn stderr


data Options = Options  { optConfig :: FilePath
                        , optOutput :: Maybe FilePath
                        } deriving (Show)
startOptions = Options  { optConfig = "default"
                        , optOutput = Nothing
                        }

getOptions :: IO (Options, [FilePath])
getOptions = do
    (actions, args, errors) <- return . getOpt Permute options =<< getArgs
    if null errors
      then do
        opts <- foldl (>>=) (return startOptions) actions
        when (null args) $ die "no input files"
        return (opts, args)
      else do
        mapM_ putErrLn errors
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
                putStrLn (showVersion version)
                exitSuccess))
        "Print version."
    , Option "" ["show-config-dir"]
        (NoArg
            (\_ -> do
                putStrLn =<< getDataFileName ""
                exitSuccess))
        "Print directory where configuration files are stored."
    , Option "h" ["help"]
        (NoArg
            (\_ -> do
                prg <- getProgName
                putStrLn "Ease input of unicode glyphs."
                putStrLn "This program is lisenced under the 3-clause BSD lisence."
                putStrLn (usageInfo (prg ++ " [options] files...") options)
                putStrLn "Run on one or more files to replace special sequences of characters (as defined in a config file) with replacements."
                putStrLn "The idea is to put in useful unicode symbols and then use those symbols in source code."
                putStrLn " -- Config Files"
                putStrLn "    Configuration files are stored in `/etc/zankoku-okuno/unicoder`, and end in `.conf`."
                putStrLn "    Passing a file to `-c` that contains a slash will use exactly the file you specify, instead of looking one up."
                putStrLn "    Configuration files consist of a top line and a body. The body is simply a database with two whitespace-separated fields. \
                             \The first is the a name, and the second is the replacement. The top line contains one or two whitespace-separated fields. \
                             \The first (optional) is a separator, which is optionally consumed after matching a name in the database. The second holds all the characters than can be used to define and use a name."
                exitSuccess))
        "Show help"
    ]

