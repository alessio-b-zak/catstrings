module Utilities where

import Prelude
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Array (filter, last)
import Data.String as Str

import Halogen.HTML.Core as HCore
import Halogen.HTML.Properties as HP
import DOM (DOM)
import DOM.Event.Types (Event)
import DOM.HTML.Event.Types (DragEvent, dragEventToEvent)
import DOM.Event.Event (stopPropagation, preventDefault)

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
