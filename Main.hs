import Pipes
import EventLog

main = do
    evlog <- either error id <$> fromFile "test.eventlog"
    runEffect $ for (records evlog) $ \(_,r) -> liftIO $ print r
