module TypesSpec
  ( spec,
  )
where

import qualified Test.Hspec as T
import Types

spec :: T.Spec
spec = do
  T.describe "An Item" $ do
    T.it "should be printable" (show (Item "me") `T.shouldBe` "Item {itemName = \"me\"}")
