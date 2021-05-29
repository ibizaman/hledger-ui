{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import Api (API)
import Control.Monad.Fix (MonadFix)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Proxy (Proxy (Proxy))
import qualified Data.Set as Set
import Data.Text (Text, pack, unpack)
import Map (dynamicMap)
import qualified Reflex as R
import qualified Reflex.Dom as Dom
import Servant.API ((:<|>) (..))
import qualified Servant.Reflex as SR
import Types (Entity, Item (..), ItemId, itemListToMap)

main :: IO ()
main = Dom.mainWidget $ run

run ::
  ( Dom.DomBuilder t m,
    SR.SupportsServantReflex t m,
    Dom.MonadHold t m,
    Dom.PostBuild t m,
    MonadFix m
  ) =>
  m ()
run = do
  Dom.el "div" $ do
    newItemEvent <- addWidget
    _ <- listWidget newItemEvent
    journalLocationWidget
    return ()

srList ::
  SR.SupportsServantReflex t m =>
  Dom.Event t tag ->
  m (Dom.Event t (SR.ReqResult tag [Entity Item]))
srAdd ::
  SR.SupportsServantReflex t m =>
  Dom.Dynamic t (Either Text Item) ->
  Dom.Event t tag ->
  m (Dom.Event t (SR.ReqResult tag ItemId))
srRemove ::
  SR.SupportsServantReflex t m =>
  Dom.Dynamic t (Either Text ItemId) ->
  Dom.Event t tag ->
  m (Dom.Event t (SR.ReqResult tag ()))
srSetJournalLocation ::
  SR.SupportsServantReflex t m =>
  Dom.Dynamic t (Either Text Text) ->
  Dom.Event t tag ->
  m (Dom.Event t (SR.ReqResult tag ()))
srGetJournalLocation ::
  SR.SupportsServantReflex t m =>
  Dom.Event t tag ->
  m (Dom.Event t (SR.ReqResult tag (Maybe Text)))
srList :<|> srAdd :<|> srRemove :<|> srSetJournalLocation :<|> srGetJournalLocation =
  SR.client
    (Proxy :: Proxy API)
    (Proxy :: Proxy (m :: * -> *))
    (Proxy :: Proxy tag)
    (Dom.constDyn (SR.BasePath "http://localhost:8080"))

-- | Widget that maintains a list of item from the backend.
listWidget ::
  ( SR.SupportsServantReflex t m,
    Dom.DomBuilder t m,
    Dom.MonadHold t m,
    Dom.PostBuild t m,
    MonadFix m
  ) =>
  Dom.Event t (Map ItemId Item) ->
  m ()
listWidget newEvent = do
  rec butn <- Dom.button "Refresh"
      onload <- R.getPostBuild
      replaceEvent :: (R.Event t (Map ItemId Item)) <-
        fmap itemListToMap <$> R.fmapMaybe SR.reqSuccess <$> srList (R.leftmost [onload, butn])
      itemMap :: R.Dynamic t (Map.Map ItemId Item) <-
        dynamicMap Map.empty replaceEvent newEvent deleteEvent
      deleteEvent' :: R.Dynamic t (Map.Map ItemId (R.Event t ItemId)) <-
        Dom.el "ul" $
          Dom.listWithKey itemMap $
            \itemId item -> Dom.el "li" $ do
              Dom.dynText $ fmap (pack . itemName) item
              delbutn <- Dom.button "X"
              R.fmapMaybe (fmap snd . success)
                <$> srRemove
                  (R.constDyn $ Right itemId)
                  (R.tagPromptlyDyn (R.constDyn itemId) delbutn)
      let deleteEvent =
            R.switch . R.current $
              fmap
                (R.mergeWith (<>) . fmap (fmap Set.singleton) . Map.elems)
                deleteEvent'
  return ()

-- | Widget that adds a item to the backend.
addWidget ::
  ( SR.SupportsServantReflex t m,
    Dom.DomBuilder t m
  ) =>
  m (Dom.Event t (Map ItemId Item))
addWidget = Dom.el "div" $ do
  uName <- fmap (Item . unpack) . Dom.value <$> Dom.inputElement Dom.def
  butn <- Dom.button "Add Item"
  fmap (uncurry Map.singleton) . R.fmapMaybe success <$> srAdd (fmap Right uName) (R.tagPromptlyDyn uName butn)

journalLocationWidget ::
  ( SR.SupportsServantReflex t m,
    Dom.DomBuilder t m,
    Dom.MonadHold t m,
    Dom.PostBuild t m,
    MonadFix m
  ) =>
  m ()
journalLocationWidget = Dom.el "div" $ do
  rec refreshButn <- Dom.button "Refresh"
      onload <- R.getPostBuild
      journalLocation <- Dom.holdDyn "No Location Set" . R.fmapMaybe (fmap (maybe "No Location Set" id . fst) . success) =<< srGetJournalLocation (R.leftmost [onload, refreshButn])
      Dom.el "div" $ Dom.dynText journalLocation
      newJournalLocation <- Dom.value <$> Dom.inputElement Dom.def
      saveButn <- Dom.button "Save"
      _ <- srSetJournalLocation (fmap Right newJournalLocation) saveButn
  return ()

success :: SR.ReqResult a k -> Maybe (k, a)
success (SR.ResponseSuccess v tag _) = Just (tag, v)
success _ = Nothing
