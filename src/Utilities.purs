module Utilities where

import Prelude
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Array (filter, last, singleton, mapWithIndex)
import Data.Monoid (class Monoid, mempty)
import Data.String as Str
import Data.Char (fromCharCode)
import Data.Tuple (Tuple(..))
import Control.Monad.Eff (Eff)

import Halogen.HTML.Core as HCore
import Halogen.HTML.Properties as HP
import DOM (DOM)
import DOM.HTML.Types (HTMLDocument, htmlDocumentToEventTarget)
import DOM.Event.Types (Event)
import DOM.Event.EventTarget (eventListener, addEventListener, removeEventListener)
import DOM.HTML.Event.EventTypes (keypress)
import Color (Color, fromHexString, rgba)

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

eventKey :: forall eff. Event -> Eff (dom :: DOM | eff) (Maybe Char)
eventKey e = map fromCharCode <$> _eventKey Just Nothing e

foreign import _eventKey :: forall a eff. (a -> Maybe a) -> (Maybe a)
												 -> Event -> Eff (dom :: DOM | eff) (Maybe Int)

zipIndex :: forall a . Array a -> Array (Tuple Int a)
zipIndex = mapWithIndex Tuple

onKeyPress :: forall e
   . HTMLDocument
  -> (Event -> Eff (dom :: DOM | e) Unit)
  -> Eff (dom :: DOM | e) (Eff (dom :: DOM | e) Unit)
onKeyPress doc callback = do
  let eventTarget = (htmlDocumentToEventTarget doc)
  let listener = eventListener callback

  -- Add the EventListener to the
  -- document so it will fire on mouseup
  addEventListener
    keypress
    (eventListener callback)
    true
    (htmlDocumentToEventTarget doc)

  -- Return the function that will later 
  -- remove the event listener
  pure $ removeEventListener
    keypress
    listener true eventTarget

maybeColour :: Maybe Color -> Color
maybeColour = fromMaybe transparent

transparent :: Color
transparent = rgba 0 0 0 0.0

foreign import infinity :: Int
