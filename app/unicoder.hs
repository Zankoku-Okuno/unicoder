import Data.String (IsString(..))

import Data.Default
import Control.Monad

import System.FilePath
import System.Directory
import System.IO
import System.Exit
import System.Console.GetOpt
import System.Environment

import Twitch (Dep, (|>))
import qualified Twitch
import qualified Data.Text.IO as T

import Text.Unicoder
import Paths_unicoder
import Data.Version


main :: IO ()
main = do
        (opts, _) <- getOptions
        m_config <- loadConfig =<< locateConfig (optConfig opts)
        config <- case m_config of
            Nothing -> die "bad configuration file"
            Just config -> return config
        -- TODO -o/--output flag, but how for many files?
        case optMode opts of
            StdPipes -> print "stdpipes"
            InPlace files -> mapM_ (mainLoop config) files
            FileWatch globs ->
                Twitch.defaultMainWithOptions def $ do
                    forM_ (fromString <$> globs) $ \glob ->
                        glob |> mainLoop config
        exitSuccess

mainLoop :: Config -> FilePath -> IO ()
mainLoop config filename = do
    source <- T.readFile filename
    let result = unicodize config source
    T.writeFile filename result

{-| Determine the filesystem location of a config file path.
    If the path does not include a slash, then it is resolved using
    the unicoder built-in locations. If it does include a slash, then
    it is resolved relative to the passed working directory.
-}
locateConfig :: FilePath -- ^ the config path to resolve
             -> IO FilePath -- ^ resolved, absolute path to the file
locateConfig path
    | pathSeparator `elem` path = do
        cwd <- getCurrentDirectory
        return $ normalise (cwd </> path)
    | otherwise = getDataFileName (path <.> "conf")


putErrLn = hPutStrLn stderr


data Mode
    = StdPipes
    | InPlace [FilePath]
    | FileWatch [String]
    deriving (Show)
data Options = Options 
    { optConfig :: FilePath
    , optOutput :: Maybe FilePath
    , optMode :: Mode
    }
    deriving (Show)
instance Default Options where
    def =  Options
            { optConfig = "default"
            , optOutput = Nothing
            , optMode = InPlace (error "internal error (uninit'd in-place mode)")
            }


getOptions :: IO (Options, [FilePath])
getOptions = do
    (actions, args, errors) <- getOpt Permute options <$> getArgs
    if null errors
      then do
        opts <- foldl (>>=) (return def) actions
        case optMode opts of
            InPlace _ -> return (opts { optMode = InPlace args }, [])
            FileWatch _ -> return (opts { optMode = FileWatch args }, [])
            _ -> return (opts, args)
      else do
        mapM_ putErrLn errors
        exitFailure

options :: [OptDescr (Options -> IO Options)]
options =
    [ Option "w" ["file-watch"]
        (NoArg
            (\opt -> return opt { optMode = FileWatch [] }))
        (unlines [ "Run on files whenever they are changed."
                 , "Glob syntax is supported (including `**`) so that we can watch for new files."
                 , "NOTE: Quote glob patterns whenever your shell already supports glob."
                 ])
    , Option "c" ["config"]
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
                putStrLn (usageInfo (prg ++ " [options] files/globs...") options)
                putStrLn "Replace special sequences of characters (as defined in a config file) with otherwise hard-to-type replacements."
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

