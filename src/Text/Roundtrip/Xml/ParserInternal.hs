module Text.Roundtrip.Xml.ParserInternal (

    PxState, PxParser, GenXmlParser(..)
  , AttrMap, RtEvent(..)
  , WithPos(..), RtEventWithPos
  , EventWithPos, eventWithPos, eventWithoutPos

) where

import Data.XML.Types
import qualified Data.Text as T

import Text.PrettyPrint.HughesPJ

import Text.Roundtrip.Parser
import Text.Roundtrip.Xml.Pretty

type PxState = Maybe AttrMap
type PxParser s m a = PParser s PxState m a
newtype GenXmlParser s m a = GenXmlParser (PxParser s m a)

type AttrMap = [(Name, T.Text)]

data RtEvent
    = RtBeginDocument
    | RtEndDocument
    | RtBeginElement Name AttrMap
    | RtText T.Text
    | RtEndElement Name
    | RtInvalidEntity T.Text
      deriving (Eq)

instance Pretty RtEvent where
    pp RtBeginDocument = text "begin-document"
    pp RtEndDocument = text "end-document"
    pp (RtBeginElement n m) = text "<" <> pp n <>
                                 (if null m
                                     then empty
                                     else Prelude.foldl (\d (n, t) -> d <+> pp n <> text "=" <>
                                                                      doubleQuotes (text (T.unpack t)))
                                                        (text "") m)
                                 <> text ">"
    pp (RtText t) = text "text" <> parens (shorten 32 t)
        where shorten n t | T.length t <= n = text (T.unpack t)
                          | otherwise = text (T.unpack (T.take n t)) <+> text "..."
    pp  (RtEndElement n) = text "</" <> pp n <> text ">"

instance Show RtEvent where
    showsPrec _ ev = showString (render (pp ev))

data WithPos a = WithPos {
                     wp_data :: a
                   , wp_pos  :: SourcePos
                 }

eventWithPos :: a -> SourceName -> Line -> Column -> WithPos a
eventWithPos x s l c = WithPos x (newPos s l c)

eventWithoutPos :: a -> WithPos a
eventWithoutPos x = WithPos x (newPos "" (-1) (-1))

type EventWithPos = WithPos Event
type RtEventWithPos = WithPos RtEvent

instance Pretty a => Pretty (WithPos a) where
    pp (WithPos a p) = pp a <+> text " at " <+> text (show p)

instance Pretty a => Show (WithPos a) where
    showsPrec _ p = showString (render (pp p))
