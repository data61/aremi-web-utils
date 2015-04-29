module Util.Periodic where

import           Data.Time.Units    hiding (Day)

import           Control.Concurrent

import           Data.Time.Clock    (NominalDiffTime, UTCTime, addUTCTime,
                                     diffUTCTime, getCurrentTime)

--- | Run an event every n time units. Does not guarantee that it will be run
--- each occurance of n time units if the action takes longer than n time to run.
--- It will run each action at the next block of n time (it can miss deadlines).
every :: TimeUnit t => IO a -> t -> IO (ThreadId,MVar ())
every act t = do
    mv <- newEmptyMVar

    now <- getCurrentTime
    let ms = toMicroseconds t
        ps = ms * 1000000
        d = toEnum . fromIntegral $ ps :: NominalDiffTime
        activations = iterate (addUTCTime d) now

    tid <- forkIO (run activations >> putMVar mv ())
    return (tid, mv)
    where
        run :: [UTCTime] -> IO ()
        run ts = do
            _ <- act
            now <- getCurrentTime
            case dropWhile (<= now) ts of
                (nxt:ts') ->
                    let delay = fromEnum (diffUTCTime nxt now) `div` 1000000
                    in threadDelay delay >> run ts'
