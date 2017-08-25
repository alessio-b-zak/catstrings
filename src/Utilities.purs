module Utilities where

import Prelude
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Array (cons, filter, head, tail, last, singleton
                  , mapWithIndex, slice, take, drop, takeWhile, dropWhile)
import Data.Monoid (class Monoid, mempty)
import Data.Either (Either, either)
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

transparent :: Color
transparent = rgba 0 0 0 0.0

foreign import infinity :: Int

pairs :: forall a. Array a -> Array (Tuple a a)
pairs xs = fromMaybe [] do
  first <- head xs
  rest <- tail xs
  second <- head rest
  pure $ Tuple first second `cons` pairs rest

splitInThree :: forall a. Int -> Int -> Array a -> {left :: Array a, mid :: Array a, right :: Array a}
splitInThree lsize msize arr =
  { left: take lsize arr
  , mid: slice lsize (lsize + msize) arr
  , right: drop (lsize + msize) arr
  }

splitInThreeBy :: forall a. (a -> Boolean) -> (a -> Boolean) -> Array a -> {left :: Array a, mid :: Array a, right :: Array a}
splitInThreeBy f g arr =
  { left: takeWhile f arr
  , mid: takeWhile g (dropWhile f arr)
  , right: dropWhile g (dropWhile f arr)
  }

forEither :: forall a b c. Either a b -> (a -> c) -> (b -> c) -> c
forEither value left right = either left right value

forEither_ :: forall a b c d f. Functor f => Either a b -> (a -> f c) -> (b -> f d) -> f Unit
forEither_ value left right = either (void <<< left) (void <<< right) value

foreign import rangeExcl :: Int -> Int -> Array Int
infix 8 rangeExcl as ...