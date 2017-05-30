import System.IO
import System.Exit hiding (die)
import Control.Monad
import qualified Data.Text.IO as T

import Text.Unicoder

main :: IO ()
main = do
    m_config <- loadConfig "test/test.config"
    testFile <- case m_config of
        Nothing -> die "Could not parse config file."
        Just config -> return $ testFile config
    let files = ["passthrough", "mono", "di"]
    results <- mapM testFile files
    unless (all id results) $ do
        mapM_ putStrLn $ zipWith (\a b -> a ++ ": " ++ if b then "OK" else "FAILURE") files results
        exitFailure

testFile :: Config -> FilePath -> IO Bool
testFile config path = do
    input <- T.readFile $ "test/" ++ path ++ ".in"
    output <- T.readFile $ "test/" ++ path ++ ".out"
    return $ unicodize config input == output

die :: String -> IO a
die msg = putStrLn msg >> exitFailure