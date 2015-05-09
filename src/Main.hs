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

import qualified Network.WebSockets   as WS
import           Network.Wreq
import           Control.Lens         hiding ((.=))
import           Data.Aeson.Lens
import           Data.Aeson           ((.=),object)
import qualified Web.Scotty           as W
import qualified Data.Text            as T
import           Data.Text.Lazy       (fromStrict)
import           Data.Monoid

import           App.GetParams

main = do

    --extract params from file:
    _botParams <- getParams
    botParams <- case _botParams of
        Right p -> return p
        Left err -> error err

    let lookupBot = do
            id <- W.param "botid"
            case botParams ^? thisBots . traverse . filtered (\b -> b^.botId == id) of
                Just bot -> return bot
                Nothing -> W.next

    --use botids as sevrer address parts and
    --provide the info hipchat wants
    W.scotty (botParams^.thisPort) $ do

        -- return the bot capabilities descriptor
        -- for each bot we listed in the config file
        W.get "/:botid/capabilities" $ do

            bot <- lookupBot 

            let bName = bot^.botName
                bId = bot^.botId
                tPort = (T.pack $ show $ botParams^.thisPort)
                tUrl = (botParams^.thisAddress) <> ":" <> tPort <> "/" <> bId

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
                                "send_notification"
                            ],
                            "fromName" .= bName
                        ]
                    ],
                    "webhook" .= object [
                        "url" .= (tUrl <> "/webhook"),
                        "event" .= ("room_message" :: T.Text)
                    ],
                    "installable" .= object [
                        "callbackUrl" .= (tUrl <> "/install")
                    ]
                ]

        W.post "/:botid/install" $ do

            bot <- lookupBot 
            d <- W.jsonData

            W.json d



    return ()