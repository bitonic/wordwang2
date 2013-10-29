module WordWang
    ( module WordWang.Messages
    , module WordWang.Monad
    , module WordWang.Objects
    , wordwang
    ) where

import           Data.Monoid ((<>))

import           Control.Monad.Trans (liftIO)
import qualified Data.Text as Text

import           Control.Lens
import           Crypto.Random (newGenIO, genBytes)
import           Crypto.Random.DRBG (HashDRBG)
import qualified Data.ByteString.Base64.URL as Base64.URL

import           WordWang.Messages
import           WordWang.Monad
import           WordWang.Objects

internalError :: Text -> WWT IO a
internalError err = do
    -- TODO do some logging
    terminate (RespError ("internal error: " <> err))

makeSecret :: WWT IO UserSecret
makeSecret = do
    gen <- liftIO (newGenIO :: IO HashDRBG)
    case genBytes 15 gen of
        Left err -> internalError (Text.pack (show err))
        Right (bs, _) -> return (Base64.URL.encode bs)

wordwang :: WWT IO ()
wordwang = do
    req <- view wwReq
    case req^.reqBody of
        ReqCreate -> internalError "wordwang: received ReqCreate"
        ReqJoin -> do
            -- TODO should we check if the user is already authenticated?
            uid <- liftIO newId
            secret <- makeSecret
            terminate (RespJoined uid secret)
        _ -> undefined
