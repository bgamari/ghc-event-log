import Pipes
import Data.Word
import qualified Pipes.Prelude as PP
import System.IO
import qualified Data.Map.Strict as M
import Control.Arrow (first)

import Data.List (sortBy)
import Data.Ord (comparing)
import qualified IntRangeMap as RM
import EventLog
import Profiler

main = do
    hSetBuffering stderr NoBuffering
    evlog <- either error id <$> fromFile "test.eventlog"
    --runEffect $ for (records evlog) $ \(_,r) -> liftIO $ print r
    --PP.foldM (\indent -> printTree indent . snd) (pure 0) pure (records evlog)
    --histogram (records evlog >-> PP.map snd) >>= putStrLn . unlines . map show . M.assocs
    --runEffect $ for (records evlog >-> PP.map snd >-> getSamples) (liftIO . mapM_ (\(Sample addr _) -> print addr))
    (blkMap,_) <- {-# SCC "blks" #-}
              PP.fold' (\a b -> a `mappend` blockToBlockMap b) mempty id
              $ parseBlocks $ {-# SCC "records" #-}records evlog >-> PP.map snd
    blkMap `seq` print ()

    --hist <- {-# SCC "hist" #-}histogram (each (recordsList evlog) >-> PP.map snd >-> getSamples)
    --putStrLn $ unlines $ map show $ M.assocs hist
    --let showHistAddr (addr, n) =
    --      let blks = RM.values $ addrRange addr addr `RM.containing` blkMap
    --      in show (n, addr, map blkName blks)
    --mapM_ (putStrLn . showHistAddr) $ sortBy (flip $ comparing snd) $ M.assocs hist
    return ()

printTree :: Int -> Record -> IO Int
printTree n r
  | r `isOfType` 200 = return (n+1)
  | r `isOfType` 201 = return (n-1)
  | otherwise        = putStrLn (replicate (2*n) ' ' ++ show r) >> return n

isOfType :: Record -> Word16 -> Bool
isOfType r n = evtEventType (recEventType r) == EventType n
