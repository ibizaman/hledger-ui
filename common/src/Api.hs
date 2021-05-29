{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api (API, api) where

import Data.Data (Proxy (Proxy))
import Servant.API ((:<|>), (:>))
import qualified Servant.API as S
import Types (Entity, Item, ItemId)

type API =
  "list" :> S.Get '[S.JSON] [Entity Item]
    :<|> "add" :> S.ReqBody '[S.JSON] Item :> S.Post '[S.JSON] ItemId
    :<|> "remove" :> S.ReqBody '[S.JSON] ItemId :> S.Post '[S.JSON] ()

api :: Proxy API
api = Proxy
