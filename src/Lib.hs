module Lib
    ( search10
    , search20
    , search21
    , search30
    ) where

import Control.Concurrent.Async  (mapConcurrently, async, waitAnyCancel)
import Control.Concurrent        (threadDelay)
import Control.Monad             (forM)
import System.Random             (getStdRandom, randomR)
import System.Timeout            (timeout)
import Text.Printf               (printf)

type SearchQuery = String

data SearchKind
    = Image
    | Web
    | Video
    deriving (Show)

-- Simulated search function
--
-- https://talks.golang.org/2012/concurrency.slide#43
fakeSearch :: SearchQuery -> SearchKind -> IO String
fakeSearch query kind = do
    delayMs <- getStdRandom $ randomR (1, 100)
    threadDelay $ microseconds delayMs -- simulating random work
    return $ printf "%s results for '%s' in %d ms" (show kind) query delayMs

    where
        microseconds = (* 1000)

-- Helper function to print the results
printResults :: Maybe [String] -> IO ()
printResults req = case req of
    Just res -> print res
    Nothing  -> putStrLn "timed out"

-- Invoke Web, Image, and Video searches serially, appending them to the
-- results slice.
--
-- https://talks.golang.org/2012/concurrency.slide#45
search10 :: SearchQuery -> IO ()
search10 query = do
    req <- mapM (fakeSearch query) [Image, Web, Video]
    printResults (Just req)

-- Run the Web, Image, and Video searches concurrently, and wait for all
-- results.
--
-- https://talks.golang.org/2012/concurrency.slide#47
search20 :: SearchQuery -> IO ()
search20 query = do
    req <- mapConcurrently (fakeSearch query) [Image, Web, Video]
    printResults (Just req)

-- Don't wait for slow servers.
--
-- https://talks.golang.org/2012/concurrency.slide#47
search21 :: SearchQuery -> IO ()
search21 query = do
    req <- timeout maxDelay $
        mapConcurrently (fakeSearch query) [Image, Web, Video]
    printResults req

maxDelay :: Int
maxDelay = 80 * 1000 -- us

-- Reduce tail latency using replicated search servers.
--
-- https://talks.golang.org/2012/concurrency.slide#50
search30 :: SearchQuery -> IO ()
search30 query = do
    req <- timeout maxDelay $
        mapConcurrently (fastest query) [Image, Web, Video]
    printResults req

-- Send requests to multiple replicas, and use the first response.
--
-- https://talks.golang.org/2012/concurrency.slide#48
fastest :: SearchQuery -> SearchKind ->  IO String
fastest query kind = do
    requests <- forM [1..numReplicas] $ \i ->
        async $ servedFrom i <$> fakeSearch query kind
    (_, result) <- waitAnyCancel requests
    return result

  where numReplicas = 3
        servedFrom i result = "Server " ++ show i ++ ": " ++ result
