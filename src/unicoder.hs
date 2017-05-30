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
                putStrLn $ concat ["Unicoder v", showVersion version, "-beta"]
                putStrLn "Ease input of unicode glyphs."
                putStrLn "This program is lisenced under the 3-clause BSD lisence."
                putStrLn (usageInfo (prg ++ " [options] files...") options)
                putStrLn "Run on one or more files to replace special sequences of characters (as defined in a config file) with replacements."
                putStrLn "The idea is to make it easy to insert unicode symbols: type an escape sequence and run this tool over the source."
                putStrLn "--Config Files"
                putStrLn "    Configuration files end in `.conf`. Use `--show-config-dir` to see where they are stored."
                putStrLn "    Passing a file to `-c` that contains a slash will use exactly the file you specify, instead of looking one up."
                putStrLn "    Check out an installed config file to see the syntax."
                putStrLn "--More Info"
                putStrLn "    For more documentation, go to: http://zankoku-okuno.viewdocs.io/unicoder/"
                exitSuccess))
        "Show help"
    ]

