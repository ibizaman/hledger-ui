{-# LANGUAGE ScopedTypeVariables #-}

module Map
  ( dynamicMap,
  )
where

import Control.Monad.Fix (MonadFix)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Reflex as R
import qualified Reflex.Dom as Dom

data DynamicMapEvent k v
  = DMEReplace (Map k v)
  | DMEAdd (Map k v)
  | DMERemove (Set k)

-- | Maintains a Map from an initial map, an event that replaces the
-- whole map, an event that adds items to the map and an event that
-- removes items from the map.
dynamicMap ::
  forall m t k v.
  ( R.Reflex t,
    Dom.MonadHold t m,
    MonadFix m,
    Ord k
  ) =>
  -- | The initial map.
  Map k v ->
  -- | Replaces the whole map.
  Dom.Event t (Map k v) ->
  -- | Merges the map with the existing one.
  Dom.Event t (Map k v) ->
  -- | Removes the key from the existing map.
  Dom.Event t (Set k) ->
  m (Dom.Dynamic t (Map k v))
dynamicMap initial replace add delete = R.foldDyn update initial (R.mergeList [replace', add', delete'])
  where
    update :: NonEmpty (DynamicMapEvent k v) -> Map k v -> Map k v
    update events map' = foldr update' map' (NonEmpty.toList events)

    update' :: DynamicMapEvent k v -> Map k v -> Map k v
    update' (DMEReplace m') _ = m'
    update' (DMEAdd m') m = Map.union m m'
    update' (DMERemove keys) m = multiDelete keys m

    multiDelete :: Set k -> Map k v -> Map k v
    multiDelete keys m = Set.foldr Map.delete m keys

    replace' :: Dom.Event t (DynamicMapEvent k v)
    replace' = DMEReplace <$> replace

    add' :: Dom.Event t (DynamicMapEvent k v)
    add' = DMEAdd <$> add

    delete' :: Dom.Event t (DynamicMapEvent k v)
    delete' = DMERemove <$> delete
