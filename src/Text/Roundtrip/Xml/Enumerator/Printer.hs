{-# LANGUAGE DeriveDataTypeable #-}
module Text.Roundtrip.Xml.Enumerator.Printer (

  XmlPrinter, xmlPrintEnumerator

) where

import Control.Monad.Identity
import Control.Exception (SomeException, Exception, toException)
import Data.Typeable (Typeable)
import Data.Monoid (mconcat)

import System.IO.Unsafe (unsafePerformIO)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

import qualified Data.Enumerator as E
import qualified Data.Enumerator.List as EL
import qualified Data.Enumerator.Binary as EB
import qualified Data.Enumerator.Text as ET
import qualified Data.Text.Lazy as TL
import qualified Text.XML.Stream.Render as Xml
import Blaze.ByteString.Builder (Builder, toLazyByteString, toByteString)

import Data.XML.Types

import Text.Roundtrip.Xml.Printer

data PrinterError = PrinterError
                  deriving (Show, Typeable)

instance Exception PrinterError

_CHUNK_SIZE_ :: Integer
_CHUNK_SIZE_ = 20

xmlPrintEnumerator :: Monad m => XmlPrinter a -> a -> E.Enumerator Event m b
xmlPrintEnumerator p x =
    case runXmlPrinter p x of
      Just l -> E.enumList _CHUNK_SIZE_ l
      Nothing -> \step -> case step of
                            E.Error e -> E.throwError e
                            _ -> E.throwError PrinterError
