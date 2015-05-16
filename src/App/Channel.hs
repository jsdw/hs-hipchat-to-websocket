module App.Channel (
    makeChan
) where

import Control.Concurrent.MVar
import Control.Monad.Trans     (MonadIO(..))

--
-- make a channel that doesnt block on write but does on read.
-- if the reader is severed some messages are discarded.
--
makeChan :: MonadIO m => m (IO a, a -> IO ())
makeChan = do
    mv <- liftIO $ newEmptyMVar
    let reader = liftIO $ takeMVar mv
        writer a = liftIO $ isEmptyMVar mv >>= \b -> if b then putMVar mv a else modifyMVar_ mv (\_ -> return a)
    return (reader, writer)