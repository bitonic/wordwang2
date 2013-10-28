module WordWang
    ( module WordWang.Messages
    , module WordWang.Monad
    , module WordWang.Objects
    , wordwang
    ) where

import           Control.Monad.Trans (MonadIO)

import           WordWang.Messages
import           WordWang.Monad
import           WordWang.Objects

wordwang :: MonadIO m => WWT m ()
wordwang = respond Resp{ _respRecipients = This, _respBody = RespOk }
