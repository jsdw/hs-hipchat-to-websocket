
import           Control.Concurrent.MVar
import           Control.Lens                          hiding ((.=))
import qualified Web.Scotty                            as W
import           Network.Wai.Middleware.RequestLogger

import           App.GetParams
import           App.Views
import           App.Types

main = do

    -- extract config params from file:
    _botParams <- getParams
    botParams' <- case _botParams of
        Right p -> return p
        Left err -> error $ show err

    -- a list of message outputters by oauthId so that
    -- webhooks can send messages back to the bot server
    oauthSenders' <- newMVar []

    -- merge all shared params into a data object
    let params = GlobalState botParams' oauthSenders'

    -- use botids as server address parts and
    -- provide the info hipchat wants
    W.scotty (params^.botParams.thisPort) $ do

        --W.middleware logStdoutDev

        --global homepage. list general info
        W.get "/" $ homepageView params

        -- bot homepage
        W.get "/:botid" $ botHomepageView params

        -- return the bot capabilities descriptor
        -- for each bot we listed in the config file
        W.get "/:botid/capabilities" $ getCapabilitiesView params

        -- install bot, kicking off the cycle of regetting the auth token as needed
        W.post "/:botid/install" $ installBotView params

        -- receives messages sent to our bots.
        W.post "/:botid/webhook/room_message" $ webhookCallbackView params


    return ()






