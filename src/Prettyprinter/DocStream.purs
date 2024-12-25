module Prettyprinter.DocStream where

import Prelude

import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Ord.Generic (genericCompare)
import Data.Array ((:))
import Data.Array as A
import Data.String.CodeUnits (fromCharArray)
import Data.String.CodeUnits as S
import Partial.Unsafe (unsafeCrashWith)

data DocStream ann
  = SFail
  | SEmpty
  | SChar Char (DocStream ann)
  | SText Int String (DocStream ann)
  | SLine Int (DocStream ann)
  | SAnnPush ann (DocStream ann)
  | SAnnPop (DocStream ann)

derive instance genericDocStream :: Generic (DocStream ann) _

instance eqDocStream :: Eq ann => Eq (DocStream ann) where
  eq = genericEq

instance ordDocStream :: Ord ann => Ord (DocStream ann) where
  compare = genericCompare

render :: forall ann. DocStream ann -> String
render ds =
  case ds of
    SFail        -> unsafeCrashWith "An unrecoverable error occurred during rendering."
    SEmpty       -> ""
    SChar c x    -> fromCharArray [c] <> render x
    SText _l t x -> t <> render x
    SLine i x    -> S.fromCharArray ('\n' : A.replicate i ' ') <> render x
    SAnnPush _ x -> render x
    SAnnPop x    -> render x
