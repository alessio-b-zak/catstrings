module CSS.SVG where

import Prelude
import CSS.String (fromString)
import CSS.Stylesheet (CSS, key)
import CSS.Size (Size, Abs, Rel)
import Color

stroke :: Color -> CSS
stroke = key $ fromString "stroke"

strokeWidth :: Size Abs -> CSS
strokeWidth = key $ fromString "stroke-width"

fill :: Color -> CSS
fill = key $ fromString "fill"

opacity :: Number -> CSS
opacity = key (fromString "opacity") <<< show