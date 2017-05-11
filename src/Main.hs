-- vim: cc=70
module Main where

import System.Environment (getArgs)
import System.Process (system)
import Control.Monad (void, forever, forM_)
import Control.Exception (catch, IOException)
import Control.Concurrent (threadDelay)
import Data.Maybe (isJust, isNothing, fromJust)
import Data.List (find, partition)
import qualified Data.Text.IO as TIO
import qualified Network.RedEclipse.RedFlare as RF
import Config

exec :: String -> IO ()
exec = void . system

resultToMaybe :: RF.Result a -> Maybe a
resultToMaybe (Left err) = Nothing
resultToMaybe (Right val) = Just val

-- like RedFlare's serverQuery, but ignores all errors.
serverQuery :: RF.Address -> IO (Maybe RF.Report)
serverQuery addr = (resultToMaybe <$> RF.serverQuery addr) `catch`
  (const (pure Nothing) :: IOException -> IO (Maybe RF.Report))

serversFull :: Int -> [(ServerCfg, Maybe Int)] -> Bool
serversFull max = all (isStoppedOrFull . snd)
  where isStoppedOrFull (Just cnt) = cnt >= max
        isStoppedOrFull Nothing   = True

firstDown :: [(ServerCfg, Maybe Int)] -> Maybe ServerCfg
firstDown = (fst <$>) . find (isNothing . snd)

needToStop :: Int -> [(ServerCfg, Maybe Int)] -> [ServerCfg]
needToStop max srvs = map fst $ case nonEmpty of
                                  [] -> drop 1 empty
                                  _ -> empty
  where running = map (fmap fromJust) $ filter (isJust . snd) srvs
        nonFull = filter ((< max) . snd) running
        (nonEmpty, empty) = partition ((> 0) . snd) nonFull

decide :: Int
       -> [(ServerCfg, Maybe Int)]
       -> Either [ServerCfg] ServerCfg
decide max reports = if serversFull max reports
                       then case firstDown reports of
                              Just p -> Right p
                              Nothing -> Left []
                       else Left (needToStop max reports)

addr :: ServerCfg -> RF.Address
addr srv = RF.IP (host srv) (port srv + 1)

run :: Config -> IO ()
run cfg = forever $ do
  reports <- RF.mapConcurrently serverQuery (map addr srvs)
  let mcounts = map (RF.playerCnt <$>) reports
  case decide (maxPlayers . app $ cfg) (zip srvs mcounts) of
    Left toStop -> mapM_ (exec . stop) toStop
    Right toStart -> exec (start toStart)
  threadDelay ((delay . app $ cfg) * 1000000)
    where srvs = servers cfg

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> do
      text <- TIO.readFile file
      case parseConfig text of
        Left err -> error err
        Right cfg -> run cfg
    _ -> error "Usage: reduelctl path/to/config"
