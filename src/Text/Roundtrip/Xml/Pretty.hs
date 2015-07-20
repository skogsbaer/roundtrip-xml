module Text.Roundtrip.Xml.Pretty (

    Pretty(..)

) where

import qualified Data.Text as T

import Data.XML.Types
import Text.PrettyPrint.HughesPJ

class Pretty p where
    pp :: p -> Doc
    ppStr :: p -> String
    ppStr = render . pp

instance Pretty Name where
    pp n =
        case n of
          Name localName _ (Just prefix) ->
              text (T.unpack prefix) <> char ':' <>
              text (T.unpack localName)
          Name localName (Just ns) Nothing ->
              char '{' <> text (T.unpack ns) <> char '}' <>
              text (T.unpack localName)
          Name localName Nothing Nothing -> text (T.unpack localName)
