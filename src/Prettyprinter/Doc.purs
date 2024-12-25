module Prettyprinter.Doc where

import Prelude

import Data.Array (catMaybes, foldr)
import Data.Array as A
import Data.Foldable as F
import Data.Maybe (Maybe(..), maybe)
import Data.String as S
import Data.String.CodeUnits as C
import Data.Tuple.Nested ((/\))
import Prettyprinter.PageWidth (PageWidth)

data Doc ann
  = Fail
  | Empty
  | Char Char
  | Text Int String
  | Line
  | FlatAlt (Doc ann) (Doc ann)
  | Cat (Doc ann) (Doc ann)
  | Nest Int (Doc ann)
  | Union (Doc ann) (Doc ann)
  | Column (Int -> Doc ann)
  | WithPageWidth (PageWidth -> Doc ann)
  | Nesting (Int -> Doc ann)
  | Annotated ann (Doc ann)

instance functorDoc :: Functor Doc where
  map _ Fail = Fail
  map _ Line = Line
  map _ Empty = Empty
  map _ (Char c) = Char c
  map _ (Text l t) = Text l t
  map f (FlatAlt l r) = FlatAlt (map f l) (map f r)
  map f (Cat l r) = Cat (map f l) (map f r)
  map f (Nest i x) = Nest i (map f x)
  map f (Union l r) = Union (map f l) (map f r)
  map f (Column c) = Column (map f <<< c)
  map f (Nesting n) = Column (map f <<< n)
  map f (WithPageWidth wp) = WithPageWidth (map f <<< wp)
  map f (Annotated ann a) = foldr Annotated (map f a) ((pure <<< f) ann)

instance semigroupDoc :: Semigroup (Doc a) where
  append Empty y     = y
  append x     Empty = x
  append x     y     = Cat x y

instance monoidDoc :: Monoid (Doc a) where
  mempty = empty

class Pretty a where
  pretty :: forall ann. Show a => a -> Doc ann
  prettyList ∷ forall ann. Pretty a => Show a => Array a -> Doc ann

instance Pretty (Doc ann) where
  pretty = unsafeViaShow
  prettyList = prettyList'

instance Pretty String where
  pretty x = Text (S.length x) x
  prettyList = prettyList'

instance Pretty Char where
  pretty '\n' = hardline
  pretty c = Char c
  prettyList = vsep <<< map unsafeTextWithoutNewlines <<< S.split (S.Pattern "\n") <<< C.fromCharArray

instance Pretty Int where
  pretty = unsafeViaShow
  prettyList = prettyList'

instance Pretty Number where
  pretty = unsafeViaShow
  prettyList = prettyList'

instance Pretty Boolean where
  pretty true  = pretty "true"
  pretty false = pretty "false"
  prettyList = prettyList'

instance Pretty Unit where
  pretty _ = pretty "()"
  prettyList = prettyList'

instance (Pretty a, Show a) => Pretty (Array a) where
  pretty = prettyList'
  prettyList = prettyList'

instance (Pretty a, Show a) => Pretty (Maybe a) where
  pretty = maybe mempty pretty
  prettyList = prettyList <<< catMaybes

data FlattenResult a
  = Flattened a
  | AlreadyFlat
  | NeverFlat

instance functorFlattenResult :: Functor FlattenResult where
  map f (Flattened a) = Flattened (f a)
  map _ AlreadyFlat   = AlreadyFlat
  map _ NeverFlat     = NeverFlat

prettyList' ∷ forall ann a. Pretty a => Show a => Array a -> Doc ann
prettyList' = align <<< list <<< map pretty

viaShow :: forall a ann. Show a => a -> Doc ann
viaShow = pretty <<< show

unsafeViaShow :: forall a ann. Show a => a -> Doc ann
unsafeViaShow = unsafeTextWithoutNewlines <<< show

unsafeTextWithoutNewlines :: forall ann. String -> Doc ann
unsafeTextWithoutNewlines t =
  case S.uncons t of
    Nothing -> Empty
    Just {head, tail}
      | S.null tail -> Text 1 (S.singleton head)
      | otherwise -> Text (S.length t) t

group :: forall ann. Doc ann -> Doc ann
group (Union l r) = Union l r
group (FlatAlt l r) =
  case changesUponFlattening r of
    Flattened r' -> Union r' l
    AlreadyFlat  -> Union r l
    NeverFlat    -> l
group doc =
  case changesUponFlattening doc of
    Flattened doc' -> Union doc' doc
    AlreadyFlat  -> doc
    NeverFlat    -> doc

changesUponFlattening :: forall ann. Doc ann -> FlattenResult (Doc ann)
changesUponFlattening (Char _) = AlreadyFlat
changesUponFlattening (Text _ _) = AlreadyFlat
changesUponFlattening (Union l _) = Flattened l
changesUponFlattening (FlatAlt _ r) = Flattened (flatten r)
changesUponFlattening (Column f) = Flattened (Column (flatten <<< f))
changesUponFlattening (Nesting f) = Flattened (Nesting (flatten <<< f))
changesUponFlattening (Nest i x) = map (Nest i) (changesUponFlattening x)
changesUponFlattening (Annotated ann x) = map (Annotated ann) (changesUponFlattening x)
changesUponFlattening (WithPageWidth f) = Flattened (WithPageWidth (flatten <<< f))
changesUponFlattening (Cat l r) =
  case (changesUponFlattening l /\ changesUponFlattening r) of
    (NeverFlat /\  _          ) -> NeverFlat
    (_         /\ NeverFlat   ) -> NeverFlat
    (Flattened l' /\ Flattened r') -> Flattened (Cat l' r')
    (Flattened l' /\ AlreadyFlat ) -> Flattened (Cat l' r)
    (AlreadyFlat  /\ Flattened r') -> Flattened (Cat l r')
    (AlreadyFlat  /\ AlreadyFlat ) -> AlreadyFlat
changesUponFlattening Fail = AlreadyFlat
changesUponFlattening Empty = AlreadyFlat
changesUponFlattening Line = NeverFlat

flatten :: forall ann. Doc ann -> Doc ann
flatten (FlatAlt _ r) = flatten r
flatten (Cat l r) = Cat (flatten l) (flatten r)
flatten (Nest i x) = Nest i (flatten x)
flatten (Union l _) = flatten l
flatten (Column f) = Column (flatten <<< f)
flatten (WithPageWidth f) = WithPageWidth (flatten <<< f)
flatten (Nesting f) = Nesting (flatten <<< f)
flatten (Annotated ann x) = Annotated ann (flatten x)
flatten (Char c) = Char c
flatten (Text l t) = Text l t
flatten Empty = Empty
flatten Fail = Fail
flatten Line = Fail

encloseSep :: forall ann. Doc ann -> Doc ann -> Doc ann -> Array (Doc ann) -> Doc ann
encloseSep l r s docs =
  case A.length docs of
    0 -> l <> r
    1 ->
      case A.head docs of
        Just d -> l <> d <> r
        Nothing -> Empty
    _ -> cat (A.zipWith (<>) (l A.: (A.replicate (A.length docs) s)) docs) <> r

flatAlt :: forall ann. Doc ann -> Doc ann -> Doc ann
flatAlt = FlatAlt

align :: forall ann. Doc ann -> Doc ann
align d = column (\k -> nesting (\i -> nest (k - i) d))

nesting :: forall ann. (Int -> Doc ann) -> Doc ann
nesting = Nesting

column :: forall ann. (Int -> Doc ann) -> Doc ann
column = Column

char :: forall ann. Char -> Doc ann
char = Char

text :: forall ann. String -> Doc ann
text str = Text (S.length str) str

empty :: forall ann. Doc ann
empty = Empty

softline :: forall ann. Doc ann
softline = Union (Char ' ') Line

softline' :: forall ann. Doc ann
softline' = Union mempty Line

hardline :: forall ann. Doc ann
hardline = Line

line :: forall ann. Doc ann
line = FlatAlt Line (Char ' ')

line' :: forall ann. Doc ann
line' = FlatAlt Line mempty

nest :: forall ann. Int -> Doc ann -> Doc ann
nest 0 doc = doc
nest i doc = Nest i doc

enclose :: forall ann. Doc ann -> Doc ann -> Doc ann -> Doc ann
enclose l r x = l <> x <> r

cat :: forall ann. Array (Doc ann) -> Doc ann
cat = group <<< vcat

vcat :: forall ann. Array (Doc ann) -> Doc ann
vcat = concatWith (\x y -> x <> line' <> y)

vsep :: forall ann. Array (Doc ann) -> Doc ann
vsep = concatWith (\x y -> x <> line <> y)

sep :: forall ann. Array (Doc ann) -> Doc ann
sep = group <<< vsep

concatWithSpace :: forall ann. Doc ann -> Doc ann -> Doc ann
concatWithSpace x y = x <> Char ' ' <> y
infixr 5 concatWithSpace as <+>

concatWith :: forall t ann. F.Foldable t => (Doc ann -> Doc ann -> Doc ann) -> t (Doc ann) -> Doc ann
concatWith f ds
  | F.null ds = mempty
  | otherwise = F.foldr f mempty ds

list :: forall ann. Array (Doc ann) -> Doc ann
list = group <<< encloseSep
  (flatAlt (Text 2 "[ ") (Text 1 "["))
  (flatAlt (Text 2 " ]") (Text 1 "]"))
  (Text 2 ", ")
