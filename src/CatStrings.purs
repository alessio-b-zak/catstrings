module CatStrings where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Functor.Coproduct (Coproduct)
import Data.Either (Either)
import Control.Monad.Eff (Eff)
import Control.Monad.Aff (Aff)


import DOM (DOM)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type State = Unit

data Query a = DoSomething a
data Message

type ChildSlot = Either Unit Unit

type ChildQuery = Coproduct Query Query

csApp :: forall eff. H.Component HH.HTML Query Unit Void (Aff (dom :: DOM | eff))
csApp =
  H.parentComponent
    { initialState: const unit
    , render
    , eval
    , receiver: const Nothing
    }
  where
  render :: State -> H.ParentHTML Query ChildQuery ChildSlot (Aff (dom :: DOM | eff))
  render state = HH.div_ []
  
  eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void (Aff (dom :: DOM | eff))
  eval = case _ of
    DoSomething reply ->
      pure reply