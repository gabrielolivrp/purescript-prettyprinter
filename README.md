# Pretty Printer for PureScript

```purescript
customLayoutOptions :: LayoutOptions
customLayoutOptions = LayoutOptions { layoutPageWidth: AvailablePerLine 30 1.0 }

prettyType :: forall ann. Array String -> Doc ann
prettyType types =
  let
    arrows = pretty "::" `A.cons` A.replicate (A.length types) (pretty "->")
  in
    align $ sep (A.zipWith (\a b -> a <+> pretty b) arrows types)

prettySig :: forall ann. String -> Array String -> Doc ann
prettySig name types = pretty name <+> prettyType types

main ∷ Effect Unit
main = do
  let doc = prettySig "example" ["Int", "Bool", "Char", "IO ()"]
  log $ render (layoutPretty defaultLayoutOptions doc)
  log $ render (layoutPretty customLayoutOptions doc)

-- Output for wide enough formats:
-- example :: Int -> Bool -> Char -> IO ()

-- Output for narrow formats:
-- example :: Int
--         -> Bool
--         -> Char
--         -> IO ()
```

This package is a port of [https://github.com/quchen/prettyprinter](https://github.com/quchen/prettyprinter)
