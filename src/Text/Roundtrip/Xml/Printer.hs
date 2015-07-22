{-# LANGUAGE OverloadedStrings, BangPatterns #-}
module Text.Roundtrip.Xml.Printer (

    XmlPrinter, runXmlPrinter,

    runXmlPrinterByteString, runXmlPrinterLazyByteString,
    runXmlPrinterText, runXmlPrinterLazyText,
    runXmlPrinterString

) where

import Control.Monad (mplus, liftM2)
import Control.Monad.Except
import Control.Monad.Identity
import Control.Exception (SomeException)

import Control.Monad.Primitive

import System.IO.Unsafe (unsafePerformIO)

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Text.XML.Stream.Render as CXR

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

import Data.XML.Types

import Control.Isomorphism.Partial
import Text.Roundtrip
import Text.Roundtrip.Printer

data PxEvent = XmlTypesEvent Event
             | PxBeginElement Name
             | PxAttribute Name T.Text
               deriving (Show)

-- FIXME: don't use lists for  collecting the events, this makes an inefficient
-- monoid. Better use Data.Sequence?!

-- Still relevant?

newtype XmlPrinter a = XmlPrinter { unXmlPrinter :: Printer Identity [PxEvent] a }

instance IsoFunctor XmlPrinter where
    iso <$> (XmlPrinter p) = XmlPrinter $ iso `printerApply` p

instance ProductFunctor XmlPrinter where
    XmlPrinter p <*> XmlPrinter q = XmlPrinter (p `printerConcat` q)

instance Alternative XmlPrinter where
    XmlPrinter p <||> XmlPrinter q = XmlPrinter (p `printerAlternative` q)
    empty = XmlPrinter printerEmpty

instance Syntax XmlPrinter where
    pure x = XmlPrinter (printerPure x)

-- Rendering a list of events into a string/text/bytestring is done via
-- enumerators. This is not optimal because the resulting list is too strict.
-- However, currently no other functions exists for such a conversion.

-- Switched to conduit. Don't know the relevance of the above statement with regards to that.

runXmlPrinterGen :: (PrimMonad m) => XmlPrinter a -> a
                 -> (m (Either SomeException [c]) -> Either SomeException [c])
                 -> (CXR.RenderSettings -> C.ConduitM Event c (ExceptT SomeException m) ()) -> Maybe [c]
runXmlPrinterGen p x run render =
    case runXmlPrinter p x of
      Nothing -> Nothing
      Just l ->
          case run $ runExceptT $ CL.sourceList l C.=$= render CXR.def C.$$ CL.consume of
            Left _ -> Nothing
            Right t -> Just t

runXmlPrinterByteString :: XmlPrinter a -> a -> Maybe BS.ByteString
runXmlPrinterByteString p x =
    do l <- runXmlPrinterGen p x unsafePerformIO CXR.renderBytes
       return $ BS.concat l

runXmlPrinterLazyByteString :: XmlPrinter a -> a -> Maybe BSL.ByteString
runXmlPrinterLazyByteString p x =
    do l <- runXmlPrinterGen p x unsafePerformIO CXR.renderBytes
       return $ BSL.fromChunks l

runXmlPrinterText :: XmlPrinter a -> a -> Maybe T.Text
runXmlPrinterText p x =
    do l <- runXmlPrinterGen p x unsafePerformIO CXR.renderText
       return $ T.concat l

runXmlPrinterLazyText :: XmlPrinter a -> a -> Maybe TL.Text
runXmlPrinterLazyText p x =
    do l <- runXmlPrinterGen p x unsafePerformIO CXR.renderText
       return $ TL.fromChunks l

runXmlPrinterString :: XmlPrinter a -> a -> Maybe String
runXmlPrinterString p x =
    do tl <- runXmlPrinterLazyText p x
       case TL.unpack tl of
         ('<':'?':'x':'m':'l':z) -> Just (eat z)
         str -> Just str
    where
      eat l =
          case dropWhile (/= '?') l of
            '>':xs -> xs
            [] -> []
            _:xs -> eat xs

runXmlPrinter :: XmlPrinter a -> a -> Maybe [Event]
runXmlPrinter (XmlPrinter (Printer p)) x =
    fmap convEvents $ runIdentity (p x)
    where
      convEvents :: [PxEvent] -> [Event]
      convEvents pxes =
          case pxes of
            [] -> []
            XmlTypesEvent ev : rest -> ev : convEvents rest
            PxBeginElement name : rest ->
                let (attrs, nonAttrs) = convAttrs rest
                in EventBeginElement name attrs : convEvents nonAttrs
            attr@(PxAttribute _ _) : _ -> error $ "unexpected " ++ show attr
      convAttrs :: [PxEvent] -> ([(Name, [Content])], [PxEvent])
      convAttrs pxes =
          case pxes of
            PxAttribute name t : rest ->
                let (attrs, nonAttrs) = convAttrs rest
                in ((name, [ContentText t]) : attrs, nonAttrs)
            _ -> ([], pxes)

instance XmlSyntax XmlPrinter where
    xmlBeginDoc = xmlPrinterBeginDoc
    xmlEndDoc = xmlPrinterEndDoc
    xmlBeginElem = xmlPrinterBeginElem
    xmlEndElem = xmlPrinterEndElem
    xmlAttrValue = xmlPrinterAttrValue
    xmlTextNotEmpty = xmlPrinterTextNotEmpty

mkXmlPrinter :: (a -> [PxEvent]) -> XmlPrinter a
mkXmlPrinter f = XmlPrinter $ Printer $ \x -> return (Just (f x))

xmlPrinterBeginDoc :: XmlPrinter ()
xmlPrinterBeginDoc = mkXmlPrinter $ \() -> [XmlTypesEvent EventBeginDocument]

xmlPrinterEndDoc :: XmlPrinter ()
xmlPrinterEndDoc = mkXmlPrinter $ \() -> [XmlTypesEvent EventEndDocument]

xmlPrinterBeginElem :: Name -> XmlPrinter ()
xmlPrinterBeginElem name = mkXmlPrinter $ \() -> [PxBeginElement name]

xmlPrinterEndElem :: Name -> XmlPrinter ()
xmlPrinterEndElem name = mkXmlPrinter $ \() -> [XmlTypesEvent (EventEndElement name)]

xmlPrinterAttrValue :: Name -> XmlPrinter T.Text
xmlPrinterAttrValue aName = mkXmlPrinter $ \value -> [(PxAttribute aName value)]

xmlPrinterTextNotEmpty :: XmlPrinter T.Text
xmlPrinterTextNotEmpty = mkXmlPrinter $ \value ->
                         if T.null value
                            then []
                            else [XmlTypesEvent $ EventContent (ContentText value)]
