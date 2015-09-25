import Pipes
import EventLog
import System.IO

main = do
    hSetBuffering stderr NoBuffering
    evlog <- either error id <$> fromFile "test.eventlog"
    runEffect $ for (records evlog) $ \(_,r) -> liftIO $ print r
