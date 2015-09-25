import Pipes
import Data.Word
import qualified Pipes.Prelude as PP
import EventLog
import System.IO

main = do
    hSetBuffering stderr NoBuffering
    evlog <- either error id <$> fromFile "test.eventlog"
    --runEffect $ for (records evlog) $ \(_,r) -> liftIO $ print r
    PP.foldM (\indent -> printTree indent . snd) (pure 0) pure (records evlog)


printTree :: Int -> Record -> IO Int
printTree n r
  | r `isOfType` 200 = return (n+1)
  | r `isOfType` 201 = return (n-1)
  | otherwise        = putStrLn (replicate (2*n) ' ' ++ show r) >> return n

isOfType :: Record -> Word16 -> Bool
isOfType r n = evtEventType (recEventType r) == EventType n
