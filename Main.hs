import Pipes
import Data.Word
import qualified Pipes.Prelude as PP
import System.IO
import qualified Data.Map.Strict as M
import Control.Monad.State
import Control.Monad (forever)

import Data.List (sortBy)
import Data.Ord (comparing)
import qualified IntRangeMap as RM
import EventLog
import Profiler

main :: IO ()
main = do
    hSetBuffering stderr NoBuffering
    evlog <- either error id <$> fromFile "test.eventlog"
    runStateT (runEffect $ void (blockEvents $ records evlog >-> PP.map snd) >-> printTree >-> PP.stdoutLn) 0

    (blkMap, rest) <- buildBlockMap $ blockEvents $ records evlog >-> PP.map snd
    hist <- histogram (rest >-> getSamples)

    let showHistAddr (addr, n) =
          let blks = RM.values $ addrRange addr addr `RM.containing` blkMap
          in show (n, addr, map blkName blks, map (map showSourceNote . blkSrcNotes) blks)
    mapM_ (putStrLn . showHistAddr) $ sortBy (flip $ comparing snd) $ M.assocs hist
    return ()

printTree :: Monad m => Pipe BlockEvent String (StateT Int m) r
printTree = zipWithDepth >-> PP.map printEvent
  where
    printEvent (r, n) = replicate (2*n) ' ' ++ show r

zipWithDepth :: Monad m => Pipe BlockEvent (BlockEvent, Int) (StateT Int m) r
zipWithDepth = forever $ do
    r <- await
    let yieldWithDepth = do depth <- get
                            yield (r, depth)
    case r of
      StartBlockEv _ -> do
        yieldWithDepth
        modify (+1)
      EndBlockEv     -> modify (subtract 1)
      _              -> yieldWithDepth

isOfType :: Record -> Word16 -> Bool
isOfType r n = evtEventType (recEventType r) == EventType n
