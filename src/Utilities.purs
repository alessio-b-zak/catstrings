module Utilities where

import Prelude
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Array (filter, last, singleton)
import Data.Monoid (class Monoid, mempty)
import Data.String as Str
import Control.Monad.Eff (Eff)

import Halogen.HTML.Core as HCore
import Halogen.HTML.Properties as HP
import DOM (DOM)
import DOM.Event.Types (Event)
import DOM.HTML.Event.Types (DragEvent, dragEventToEvent)
import DOM.Event.Event (stopPropagation, preventDefault)
import Color (Color, fromHexString)

classes :: forall r i . Array String -> HP.IProp ("class" :: String | r) i
classes = HP.classes <<< map HCore.ClassName <<< filter (not <<< Str.null)

splitAt :: forall a . Int -> Array a -> Maybe {before :: Array a, after :: Array a}
splitAt = _splitAt Just Nothing

foreign import _splitAt :: forall b . 
                           (forall a . a -> Maybe a)
                        -> (forall a . Maybe a)
                        -> Int
                        -> Array b
                        -> Maybe {before :: Array b, after :: Array b}

lastOrZero :: Array Int -> Int
lastOrZero xs = fromMaybe 0 $ last xs

if_ :: forall a . Monoid a => a -> Boolean -> a
if_ x bool = if bool then x else mempty

foreign import inputValue :: forall eff. Event -> Eff (dom :: DOM | eff) String

colourValue :: forall eff . Event -> Eff (dom :: DOM | eff) (Maybe Color)
colourValue ev = fromHexString <$> inputValue ev

maybeToArray :: forall a. Maybe a -> Array a
maybeToArray = maybe [] singleton