--import qualified Network.WebSockets   as WS
--import           Network.Socket       (withSocketsDo) --only really necessary for windows
--import           Data.Text            (Text)
--import qualified Data.Text            as T
--import qualified Data.Text.IO         as T
--import           Data.Default         (Default, def)
--import           Control.Concurrent 
--import           Control.Applicative  ((<|>),(<$>),(<*>))
--import           Control.Monad.Trans  (liftIO)
--import           Control.Monad        (forever, forM_, forM)
--import           System.Environment   (getArgs)
--import           Text.Read            (readMaybe)
--import qualified Data.Map             as M
--import           Data.Monoid          ((<>))
--import           Data.List            (lookup,any)
--import           Data.Aeson           ((.=),object,encode,decode,toJSON)
--import qualified System.IO            as IO
--import           System.Posix.Signals (installHandler, Handler(Catch), sigINT, sigTERM)

--import qualified Data.ByteString.Lazy.Char8  as BL

import           Prelude              hiding (print)
import qualified Network.WebSockets   as WS
import           Network.Wreq         as R
import           Control.Lens         hiding ((.=))
import           Control.Concurrent 
import           Control.Monad
import           Control.Monad.Trans  (liftIO)
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
import           App.Format           (print, printLn, formatLn, Only(..))


main = do

    -- extract params from file:
    _botParams <- getParams
    botParams <- case _botParams of
        Right p -> return p
        Left err -> error err

    -- a list of message outputters by oauthId so that
    -- webhooks can send messages back to the bot server
    sendersByOauth <- newMVar []

    --key hold of a bot from within an ActionM
    let lookupBot = do
            id <- W.param "botid"
            case botParams ^? thisBots . traverse . filtered (\b -> b^.botId == id) of
                Just bot -> return bot
                Nothing -> W.next

    -- use botids as server address parts and
    -- provide the info hipchat wants
    W.scotty (botParams^.thisPort) $ do

        W.middleware logStdoutDev

        -- bot homepage
        W.get "/:botid" $ do

            bot <- lookupBot 
            printLn "bot homepage reached: {}" (Only $ bot^.botName)
            W.text $ formatLn "bot homepage reached for {}\n\nGo to /{}/capabilities for more" (bot^.botName, bot^.botId)

        -- return the bot capabilities descriptor
        -- for each bot we listed in the config file
        W.get "/:botid/capabilities" $ do

            bot <- lookupBot 

            let bName = bot^.botName
                bId = bot^.botId
                tPort = (T.pack $ show $ botParams^.thisPort)
                tUrl = (botParams^.thisAddress) <> ":" <> tPort <> "/" <> bId

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
                            "scopes" .= [
                                ("view_group" :: T.Text),
                                "send_notification",
                                "view_messages"
                            ],
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

        -- install bot, kicking off the cycle of regetting the auth token as needed
        W.post "/:botid/install" $ do

            bot <- lookupBot 
            (resp :: Value) <- W.jsonData

            -- use these to get auth token. also use oauthId to tie webhook messages to the right
            -- subscriber incase multiple hipchats install the same bot.
            let oauthId = resp ^?! key "oauthId" . _String
                oauthSecret = resp ^?! key "oauthSecret" . _String

            -- follow this link to get a URL to the hipchat API.
            -- we assume that the key will exist else we're a bit screwed anyway.
            caps <- liftIO $ R.get $ T.unpack (resp ^?! key "capabilitiesUrl" . _String)

            let hipchatApiUrl = caps ^?! responseBody . key "capabilities" . key "hipchatApiProvider" . key "url" . _String
                tokenUrl = caps ^?! responseBody . key "capabilities" . key "oauth2Provider" . key "tokenUrl" . _String

            printLn "Installing {} for {}" (bot^.botName, hipchatApiUrl)

            -- fork a thread to handle keeping the token refreshed
            authToken <- liftIO $ newEmptyMVar
            liftIO $ forkIO $ forever $ do
                bFirst <- isEmptyMVar authToken
                let opts = R.defaults & auth ?~ basicAuth (encodeUtf8 oauthId) (encodeUtf8 oauthSecret)
                let gt = if bFirst then "client_credentials" else "refresh_token" :: T.Text

                tokenDetails <- R.postWith opts (T.unpack tokenUrl) $ toJSON $ object [ "grant_type" .= gt ]
                putMVar authToken $ tokenDetails ^?! responseBody . key "access_token" . _String

                --sleep until we need to refresh token (give 2 mins leeway)
                threadDelay $ ((tokenDetails ^?! responseBody . key "expires_in" . _Integral) - 120) * 1000000

            --start a websocket client for our new bot:
            printLn "Starting socket server pointed at {}:{}" (bot^.botAddress, bot^.botPort)
            (m_in,m_out) <- liftIO $ startSocketClient (T.unpack $ bot^.botAddress) (bot^.botPort)
            printLn "Socket server started" ()

            --add sending mvar to list so that webhooks can use:
            liftIO $ modifyMVar_ sendersByOauth $ \arr -> return ((oauthId,m_out):arr) 

            --handle messages coming in here. expect room and message else ignore:
            liftIO $ forkIO $ forever $ do
                msgData <- takeMVar m_in
                auth <- readMVar authToken

                case (msgData ^? key "message" . _String, msgData ^? key "room" . _String) of
                    (Just msg, Just room) -> do

                        printLn "server_message ({}): {} says {}" (room, bot^.botName, msg)

                        T.putStrLn $ "server_message ("<>room<>"): "<>msg
                        let url = T.unpack hipchatApiUrl <> "room/" <> T.unpack room <> "/notification"
                        R.postWith (R.defaults & param "auth_token" .~ [auth]) url $ toJSON $ object [
                                "message" .= msg, 
                                "notify" .= True
                            ]
                        return ()   
                    otherwise -> printLn "server_message received but not understood: {}" (Only msgData)

                return ()


            W.text "Thanks!"

        -- receives messages sent to our bots.
        W.post "/:botid/webhook/room_message" $ do

            bot <- lookupBot 
            (resp :: Value) <- W.jsonData

            let oauthId = resp ^?! key "oauth_client_id" . _String
                room = resp ^?! key "item" . key "room" . key "name" . _String
                msg = resp ^?! key "item" . key "message" . key "message" . _String
                -- this may not be present:
                mMentionName = resp ^? key "item" . key "message" . key "from" . key "mention_name" . _String

            printLn "room_message ({}): {} says {}" (room, mMentionName, msg)

            -- lookup socket client by oauthId and send message to it if possible
            senders <- liftIO $ readMVar sendersByOauth
            liftIO $ case lookup oauthId senders of
                Just m_out -> putMVar m_out $ encode $ object [
                        "room" .= room,
                        "message" .= msg,
                        "name" .= mMentionName
                    ]
                otherwise -> printLn "Bot doesnt know about room with oauth ID '{}' (message: {})" (oauthId,msg)


            W.text "Ooh, interesting room message!"


    return ()



-- kick off a socket client, giving back mvars for
-- message passing. doesnt do anything fancy
startSocketClient address port = do

    (message_in :: MVar BL.ByteString) <- newEmptyMVar
    (message_out :: MVar BL.ByteString) <- newEmptyMVar

    forkIO $ WS.runClient address port "/" $ \conn -> do

        liftIO $ forkIO $ forever $ do
            resp <- WS.receiveData conn
            putMVar message_in resp

        forever $ do
            m <- takeMVar message_out
            WS.sendTextData conn m

    return (message_in, message_out)





