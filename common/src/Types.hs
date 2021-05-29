{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Types
  ( Item (..),
    ItemId,
    Settings (..),
    P.Entity,
    migrateAll,
    itemListToMap,
  )
where

import qualified Data.Map as Map
import qualified Database.Persist as P
import qualified Database.Persist.TH as PTH

PTH.share
  [PTH.mkPersist PTH.sqlSettings, PTH.mkMigrate "migrateAll"]
  [PTH.persistLowerCase|
Item json
    name String
    deriving Eq Show

Settings json
    journalLocation String
|]

itemListToMap :: [P.Entity Item] -> Map.Map ItemId Item
itemListToMap = Map.fromList . map (\eu -> (P.entityKey eu, P.entityVal eu))
