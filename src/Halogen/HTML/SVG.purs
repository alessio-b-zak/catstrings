module Halogen.HTML.SVG where

import Prelude
import Data.Array (zipWith)
import Data.String as Str
import Data.Tuple (Tuple(..))

import DOM.HTML.Indexed as I
import Halogen.HTML.Elements as HE
import Halogen.HTML.Core (AttrName(..), ElemName(..), HTML(..), Namespace, Prop)
import Halogen.HTML.Core as Core
import Halogen.HTML.Properties (IProp, attr, attrNS)
import Halogen.VDom.Types as VDomTypes

type Attr = String

svgNS :: Namespace
svgNS = VDomTypes.Namespace "http://www.w3.org/2000/svg"

elementSVG :: forall r p i. ElemName -> Array (IProp r i) -> Array (HTML p i) -> HTML p i
elementSVG = HE.elementNS svgNS

attrSVG :: forall r i. AttrName -> String -> IProp r i
attrSVG = attrNS svgNS


type SVGAttrs r = (id::Attr, "class"::Attr, style::Attr | r)

-- Elements

svg :: forall p i. HE.Node (I.GlobalAttributes (height::Attr, width::Attr, viewBox::Attr)) p i
svg = elementSVG (ElemName "svg")

g :: forall p i. HE.Node () p i
g = elementSVG (ElemName "g")

circle :: forall p i. HE.Leaf (SVGAttrs (cx::Attr, cy::Attr, r::Attr)) p i
circle props = elementSVG (ElemName "circle") props []

ellipse :: forall p i. HE.Leaf (SVGAttrs (cx::Attr, cy::Attr, rx::Attr, ry::Attr)) p i
ellipse props = elementSVG (ElemName "ellipse") props []

rect :: forall p i. HE.Leaf (SVGAttrs (x::Attr, y::Attr, width::Attr, height::Attr)) p i
rect props = elementSVG (ElemName "rect") props []

polygon :: forall p i. HE.Leaf (SVGAttrs (points::Attr)) p i
polygon props = elementSVG (ElemName "polygon") props []

polyline :: forall p i. HE.Leaf (SVGAttrs (points::Attr)) p i
polyline props = elementSVG (ElemName "polyline") props []

path :: forall p i. HE.Leaf (SVGAttrs (d::Attr)) p i
path props = elementSVG (ElemName "path") props []

-- Attributes

viewBox :: forall r i. Int -> Int -> Int -> Int -> IProp (viewBox :: Attr | r) i
viewBox minx miny w h =
    attr (AttrName "viewBox") (Str.joinWith " " $ map show [minx, miny, w, h])

height :: forall r i. Int -> IProp (height :: Attr | r) i
height n = attr  (AttrName "height") (show n)

width :: forall r i. Int -> IProp (width :: Attr | r) i
width n = attr (AttrName "width") (show n)

cx :: forall r i. Int -> IProp (cx :: Attr | r) i
cx n = attr  (AttrName "cx") (show n)

cy :: forall r i. Int -> IProp (cy :: Attr | r) i
cy n = attr  (AttrName "cy") (show n)

r :: forall r i. Int -> IProp (r :: Attr | r) i
r n = attr (AttrName "r") (show n)

rx :: forall r i. Int -> IProp (rx :: Attr | r) i
rx n = attr  (AttrName "rx") (show n)

ry :: forall r i. Int -> IProp (ry :: Attr | r) i
ry n = attr  (AttrName "ry") (show n)

x :: forall r i. Int -> IProp (x :: Attr | r) i
x n = attr (AttrName "x") (show n)

y :: forall r i. Int -> IProp (y :: Attr | r) i
y n = attr (AttrName "y") (show n)

x1 :: forall r i. Int -> IProp (x1 :: Attr | r) i
x1 n = attr (AttrName "x1") (show n)

x2 :: forall r i. Int -> IProp (x2 :: Attr | r) i
x2 n = attr (AttrName "x2") (show n)

y1 :: forall r i. Int -> IProp (y1 :: Attr | r) i
y1 n = attr (AttrName "y1") (show n)

y2 :: forall r i. Int -> IProp (y2 :: Attr | r) i
y2 n = attr (AttrName "y2") (show n)

d :: forall r i. String -> IProp (d :: Attr | r) i
d p = attr (AttrName "d") p

points :: forall r i. Array (Tuple Int Int) -> IProp (points :: String | r) i
points n = attr  (AttrName "points")
                (Str.joinWith " " $ map (\(Tuple x y) -> show x <> "," <> show y) n)
