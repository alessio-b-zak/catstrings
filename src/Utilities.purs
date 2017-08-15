module Utilities where
  
import Data.Maybe
import Prelude
import Data.Array

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
