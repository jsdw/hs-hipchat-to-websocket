module App.Views (
    homepageView,
    botHomepageView,
    getCapabilitiesView,
    installBotView,
    webhookCallbackView,
) where

import           Prelude              hiding (print)
import qualified Network.WebSockets   as WS
import qualified Network.Wreq         as R
import           Control.Lens         hiding ((.=))
import           Control.Concurrent 
import qualified Control.Exception    as E
import           Control.Monad.Catch  (Exception, MonadCatch, throwM)
import           Control.Monad
import           Control.Monad.Trans  (MonadIO(..))
import           Data.Aeson.Lens
import           Data.Aeson           ((.=),object,Value,toJSON,encode)
import qualified Web.Scotty           as W
import qualified Data.Text            as T
import qualified Data.Text.IO         as T
import qualified Data.ByteString.Lazy as BL
import           Data.Text.Lazy       (fromStrict)
import           Data.Text.Encoding   (encodeUtf8)
import           Data.Monoid
import           Data.List            (lookup)
import           Network.Wai.Middleware.RequestLogger

import           App.GetParams
import           App.Format           (print, printLn, format, formatLn, Only(..))
import           App.Channel
import           App.Types

--
-- Global homepage
--
homepageView params = do
    let bots = params ^.. botParams.thisBots.traverse
    W.html $ mconcat $ flip fmap bots $ \b -> 
        let url = format "http://{}:{}/{}/capabilities" 
                  (params^.botParams.thisAddress, params^.botParams.thisPort, b^.botId)
        in format "{} ({}:{}): <a href='{}'>{}</a><br/>" 
           (b^.botName, b^.botAddress, b^.botPort, url, url)

--
-- Home page for bot
--
botHomepageView params = do
    bot <- lookupBot params
    W.text $ formatLn "bot homepage reached for {}\n\nGo to /{}/capabilities for more" (bot^.botName, bot^.botId)

--
-- Capabilities Descriptor for bot
--
getCapabilitiesView params = do

    bot <- lookupBot params

    let bName = bot^.botName
        bId = bot^.botId
        tPort = (T.pack $ show $ params^.botParams.thisPort)
        tUrl = "http://" <> (params^.botParams.thisAddress) <> ":" <> tPort <> "/" <> bId

    -- tell the world about our bot when we ask for its capabilities.
    W.json $ object [ 
            "name" .= bName,
            "description" .= (bName <> ", Connected with HipchatToWebsocket"),
            "key" .= ("hipchat.to.websocket." <> bId),
            "links" .= object [
                "homepage" .= tUrl,
                "self" .= (tUrl <> "/capabilities")
            ],
            "capabilities" .= object [
                "hipchatApiConsumer" .= object [
                    "scopes" .= botScopes,
                    "fromName" .= bName
                ],
                "webhook" .= [
                    object [
                        "url" .= (tUrl <> "/webhook/room_message"),
                        "event" .= ("room_message" :: T.Text)
                    ]
                ],
                "installable" .= object [
                    "callbackUrl" .= (tUrl <> "/install")
                ]
            ],
            "vendor" .= object [
                "url" .= ("https://github.com/jsdw/hs-hipchat-to-websocket" :: T.Text),
                "name" .= bName
            ]
        ]

--
-- This is executed when a bot is "installed" by hipchat.
--
installBotView params = do

    bot <- lookupBot params
    (resp :: Value) <- W.jsonData

    -- use these to get auth token. also use oauthId to tie webhook messages to the right
    -- subscriber incase multiple hipchats install the same bot.
    let oauthId = resp ^?! key "oauthId" . _String
        oauthSecret = resp ^?! key "oauthSecret" . _String

    -- follow this link to get a URL to the hipchat API.
    -- we assume that the key will exist else we're a bit screwed anyway.
    caps <- liftIO $ R.get $ T.unpack (resp ^?! key "capabilitiesUrl" . _String)

    let hipchatApiUrl = caps ^?! R.responseBody . key "capabilities" . key "hipchatApiProvider" . key "url" . _String
        tokenUrl = caps ^?! R.responseBody . key "capabilities" . key "oauth2Provider" . key "tokenUrl" . _String

    printLn "Installing {}" (Only $ bot^.botName)

    -- fork a thread to handle keeping the token refreshed
    authToken <- liftIO $ newEmptyMVar
    liftIO $ forkIO $ forever $ do
        mCurToken <- tryTakeMVar authToken
        let opts = R.defaults & R.auth ?~ R.basicAuth (encodeUtf8 oauthId) (encodeUtf8 oauthSecret)

        tokenDetails <- R.postWith opts (T.unpack tokenUrl) $ toJSON $ object [ 
                "grant_type" .= ("client_credentials" :: T.Text),
                "scope" .= botScopes
            ]

        putMVar authToken $ tokenDetails ^?! R.responseBody . key "access_token" . _String

        --sleep until we need to refresh token (give 2 mins leeway)
        threadDelay $ ((tokenDetails ^?! R.responseBody . key "expires_in" . _Integral) - 120) * 1000000

    --start a websocket client for our new bot:
    printLn "Starting socket server pointed at {}:{}" (bot^.botAddress, bot^.botPort)
    (m_in,m_out) <- startSocketClient (T.unpack $ bot^.botAddress) (bot^.botPort)

    --add sending mvar to list so that webhooks can use:
    liftIO $ modifyMVar_ (params^.oauthSenders) $ \arr -> return ((oauthId,m_out):arr) 

    --handle messages coming in here. expect room and message else ignore:
    liftIO $ forkIO $ forever $ do
        msgData <- m_in
        auth <- readMVar authToken

        case (msgData ^? key "message" . _String, msgData ^? key "room" . _String) of
            (Just msg, Just room) -> do

                printLn "server_message ({}): {} says {}" (room, bot^.botName, msg)

                let url = T.unpack hipchatApiUrl <> "room/" <> T.unpack room <> "/notification"
                R.postWith (R.defaults & R.param "auth_token" .~ [auth]) url $ toJSON $ object [
                        "message" .= msg, 
                        "notify" .= True,
                        "color" .= (bot^.botColour)
                    ]
                return ()   
            otherwise -> printLn "server_message received but not understood: {}" (Only msgData)

        return ()

    W.text "Thanks!"

--
-- webhook things are sent here to be handled
--
webhookCallbackView params = do

    bot <- lookupBot params
    (resp :: Value) <- W.jsonData

    let oauthId = resp ^?! key "oauth_client_id" . _String
        room = resp ^?! key "item" . key "room" . key "name" . _String
        msg = resp ^?! key "item" . key "message" . key "message" . _String
        -- this may not be present:
        mMentionName = resp ^? key "item" . key "message" . key "from" . key "mention_name" . _String

    printLn "room_message ({}): {} says {}" (room, mMentionName, msg)

    -- lookup socket client by oauthId and send message to it if possible
    senders <- liftIO $ readMVar (params^.oauthSenders)
    liftIO $ case lookup oauthId senders of
        Just m_out -> m_out $ encode $ object [
                "room" .= room,
                "message" .= msg,
                "name" .= fmap ("@"<>) mMentionName
            ]
        otherwise -> printLn "Bot doesnt know about room with oauth ID '{}' (message: {})" (oauthId,msg)

    W.text "Ooh, interesting room message!"

--
-- Utility bits and pieces
--

-- kick off a socket client, giving back mvars for
-- message passing. tries to reestablish link if its lost
startSocketClient address port = liftIO $ do

    --in_read reads messages we get back from the bot server
    (in_read :: IO BL.ByteString, in_write) <- makeChan
    --out_write is to send messages back to the bot server.
    --if the bot server doesnt pick them up (eg disconnected)
    --the messages are discarded.
    (out_read :: IO BL.ByteString, out_write) <- makeExpiringChan 10000000

    let runConnection = WS.runClient address port "/" $ \conn -> do
            liftIO $ forkIO $ forever $ do
                resp <- WS.receiveData conn
                in_write resp
            forever $ do
                m <- out_read
                WS.sendTextData conn m

        -- loop catching *any* (should limit scope more) exception and retrying.
        -- allows bots to be taken offline/tweaked without this program needing a restart.
        connectionLoop = runConnection `E.catches` [ E.Handler $ \(err :: E.SomeException) -> handleError ]
          where
            handleError = do
                printLn "connection issue with client at {}:{}, attempting reconnect in 10s" (address, port)
                threadDelay 10000000
                connectionLoop

    forkIO $ connectionLoop

    return (in_read, out_write)

--key hold of a bot from within an ActionM
lookupBot params = do
    id <- W.param "botid"
    case params ^? botParams . thisBots . traverse . filtered (\b -> b^.botId == id) of
        Just bot -> return bot
        Nothing -> W.next

--scopes that all bots will be granted
botScopes = [
        ("view_group" :: T.Text),
        "send_notification",
        "view_messages"
    ]

