{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
module Text.Roundtrip.Xml.Parser (

    GenXmlParser, XmlParser, runXmlParser, runXmlParser', runXmlParser''

  , WithPos, EventWithPos, eventWithPos, eventWithoutPos

  , SourceName, Line, Column, ParseError

  , EntityRenderer, defaultEntityRenderer

  , runXmlParserString, runXmlParserText, runXmlParserLazyText
  , runXmlParserByteString, runXmlParserLazyByteString

) where

import Prelude hiding ((*>),(<*))

import Control.Monad (unless, foldM)
import Control.Monad.State
import Control.Monad.Identity (Identity, runIdentity)
import Control.Exception (ErrorCall(..), SomeException, Exception, toException)

import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Text.XML.Stream.Parse as CXP
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.List as List
import Data.Typeable (Typeable)
import Data.Either (partitionEithers)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL


import qualified Debug.Trace

import qualified Text.PrettyPrint.HughesPJ as Pp

import Data.XML.Types

import Text.Roundtrip
import Text.Roundtrip.Parser
import Text.Roundtrip.Xml.ParserInternal
import Text.Roundtrip.Xml.Pretty

type EntityRenderer = T.Text -> Maybe T.Text

defaultEntityRenderer :: EntityRenderer
defaultEntityRenderer = const Nothing

type XmlParser a = GenXmlParser [RtEventWithPos] Identity a

type EventGen a = CXP.ParseSettings -> C.Conduit a (Either SomeException) Event

genEvents :: [a] -> EventGen a -> Either SomeException [Event]
genEvents items f =
    CL.sourceList items C.=$= f CXP.def C.$$ CL.consume

-- Parsing a string/text/bytestring into a list of events into is done via
-- enumerators. This is not optimal because the resulting list is too strict.
-- However, currently no other functions exists for such a conversion.

-- Switched to conduit. Don't know the relevance of the above statement with regards to that.

runXmlParserGen :: XmlParser a -> SourceName -> EntityRenderer -> [b] -> EventGen b -> (Either ParseError a)
runXmlParserGen p src er items gen =
    case genEvents items gen of
      Left err -> Left $ mkParseError (initialPos src) (show err)
      Right events -> runXmlParser p src er events

runXmlParserString :: XmlParser a -> SourceName -> EntityRenderer -> String -> (Either ParseError a)
runXmlParserString p src e str = runXmlParserGen p src e [T.pack str] CXP.parseText'

runXmlParserText :: XmlParser a -> SourceName -> EntityRenderer -> T.Text -> (Either ParseError a)
runXmlParserText p src e t = runXmlParserGen p src e [t] CXP.parseText'

runXmlParserLazyText :: XmlParser a -> SourceName -> EntityRenderer -> TL.Text -> (Either ParseError a)
runXmlParserLazyText p src e t = runXmlParserGen p src e (TL.toChunks t) CXP.parseText'

runXmlParserByteString :: XmlParser a -> SourceName -> EntityRenderer -> BS.ByteString -> (Either ParseError a)
runXmlParserByteString p src e bs = runXmlParserGen p src e [bs] CXP.parseBytes

runXmlParserLazyByteString :: XmlParser a -> SourceName -> EntityRenderer -> BSL.ByteString -> (Either ParseError a)
runXmlParserLazyByteString p src e bs = runXmlParserGen p src e (BSL.toChunks bs) CXP.parseBytes

runXmlParser :: XmlParser a -> SourceName -> EntityRenderer -> [Event] -> (Either ParseError a)
runXmlParser p sourceName renderer events =
    runXmlParser'' p sourceName renderer (map eventWithoutPos events)

runXmlParser' :: XmlParser a -> EntityRenderer -> [EventWithPos] -> (Either ParseError a)
runXmlParser' p renderer events = runXmlParser'' p src renderer events
    where
      src = case events of
              [] -> ""
              (e:_) -> sourceName (wp_pos e)

runXmlParser'' :: XmlParser a -> SourceName -> EntityRenderer -> [EventWithPos] -> (Either ParseError a)
runXmlParser'' p sourceName entityRenderer events =
    let GenXmlParser q = xmlBeginDoc *> p <* xmlEndDoc
        rtEvents = List.unfoldr (simplifyEvents entityRenderer) events
    in runParser q Nothing sourceName rtEvents

simplifyEvents :: EntityRenderer -> [EventWithPos] -> Maybe (RtEventWithPos, [EventWithPos])
simplifyEvents renderEntity evs = go evs
    where
      go evs =
        case evs of
          [] -> Nothing
          (WithPos EventBeginDocument pos : rest) -> Just (WithPos RtBeginDocument pos, rest)
          (WithPos EventEndDocument pos : rest) -> Just (WithPos RtEndDocument pos, rest)
          (WithPos (EventInstruction _) _ : rest) -> go rest
          (WithPos (EventBeginDoctype _ _) _ : rest) -> go rest
          (WithPos EventEndDoctype _ : rest) -> go rest
          (WithPos (EventBeginElement n as) pos : rest) ->
              let insertAttr :: Either T.Text AttrMap -> Attribute
                             -> Either T.Text AttrMap
                  insertAttr em (k, vs) =
                      case em of
                        Right m ->
                            case partitionEithers (map contentToText vs) of
                              ((t:_), _) -> Left t
                              ([], vs') -> Right ((k, T.concat vs') : m)
                        Left t -> Left t
              in case Prelude.foldl insertAttr (Right []) as of
                   Right as' ->  as' `seq` Just (WithPos (RtBeginElement n (reverse as')) pos, rest)
                   Left t -> Just (WithPos (RtInvalidEntity t) pos, [])
          (WithPos (EventEndElement n) pos : rest) -> Just (WithPos (RtEndElement n) pos, rest)
          (WithPos (EventContent c) pos : rest) ->
              case contentToText c of
                Left t -> Just (WithPos (RtInvalidEntity t) pos, [])
                Right t ->
                    let (cs, rest') = splitContent rest
                    in case partitionEithers (map contentToText cs) of
                         ((t:_), _) -> Just (WithPos (RtInvalidEntity t) pos, [])
                         ([], ts) -> let text = T.strip $ t `T.append` T.concat ts
                                     in if T.null text
                                           then go rest'
                                           else Just (WithPos (RtText text) pos, rest')
          (WithPos (EventComment _) _ : rest) -> go rest
      splitContent (WithPos (EventContent c) pos : rest) =
          let (cs, rest') = splitContent rest
          in (c:cs, rest')
      splitContent l = ([], l)
      contentToText c =
          case c of
            ContentText t -> Right t
            ContentEntity t ->
                case renderEntity t of
                  Just t' -> Right t'
                  Nothing -> Left t

instance (Monad m, Stream s m RtEventWithPos) => IsoFunctor (GenXmlParser s m) where
    iso <$> (GenXmlParser p) = GenXmlParser $ parsecApply iso p

instance (Monad m, Stream s m RtEventWithPos) => ProductFunctor (GenXmlParser s m) where
    (GenXmlParser p) <*> (GenXmlParser q) = GenXmlParser $ parsecConcat p q

instance (Monad m, Stream s m RtEventWithPos) => Alternative (GenXmlParser s m) where
    GenXmlParser p <|> GenXmlParser q = GenXmlParser $ parsecAlternative1Lookahead p q
    GenXmlParser p <||> GenXmlParser q = GenXmlParser $ parsecAlternativeInfLookahead p q
    empty = GenXmlParser parsecEmpty

instance (Monad m, Stream s m RtEventWithPos) => Syntax (GenXmlParser s m) where
    pure x = GenXmlParser (parsecPure x)

instance (Monad m, Stream s m RtEventWithPos) => XmlSyntax (GenXmlParser s m) where
    xmlBeginDoc = GenXmlParser xmlParserBeginDoc
    xmlEndDoc = GenXmlParser xmlParserEndDoc
    xmlBeginElem = GenXmlParser . xmlParserBeginElem
    xmlAttrValue = GenXmlParser . xmlParserAttrValue
    xmlTextNotEmpty = GenXmlParser xmlParserTextNotEmpty
    xmlEndElem = GenXmlParser . xmlParserEndElem

matchEvent :: (Show a, Monad m, Stream s m RtEventWithPos)
           => (RtEvent -> Maybe a) -> String -> PxParser s m a
matchEvent matcher desc =
    do state <- getState
       case state of
         Just _ -> ("cannot match " ++ desc ++ " in state " ++ show state) `debug` parserZero
         Nothing -> tokenPrim show (\_ t _ -> wp_pos t) debugMatcher
   where
     debugMatcher ev =
         let res = matcher (wp_data ev)
         in ("matching " ++ show ev ++ " against " ++ desc ++ ", result: " ++ show res) `debug` res

mkPxParser :: Monad m => String -> PxParser s m a -> PxParser s m a
mkPxParser msg p = (p <?> msg)

xmlParserBeginDoc :: (Monad m, Stream s m RtEventWithPos) => PxParser s m ()
xmlParserBeginDoc = mkPxParser "begin-document" $
    let f RtBeginDocument = Just ()
        f _ = Nothing
    in matchEvent f "begin-document"

xmlParserEndDoc :: (Monad m, Stream s m RtEventWithPos) => PxParser s m ()
xmlParserEndDoc = mkPxParser "end-document" $
    let f RtEndDocument = Just ()
        f _ = Nothing
    in matchEvent f "end-document"

xmlParserBeginElem :: (Monad m, Stream s m RtEventWithPos) => Name -> PxParser s m ()
xmlParserBeginElem name = mkPxParser ("<" ++ ppStr name ++ " ...>") $
    do let f (RtBeginElement name' attrs) | name == name' = Just attrs
           f _ = Nothing
       attrs <- matchEvent f ("begin-element " ++ ppStr name)
       unless (null attrs) (putStateDebug $ Just attrs)
       return ()

xmlParserAttrValue :: Monad m => Name -> PxParser s m T.Text
xmlParserAttrValue name = mkPxParser ("attribute " ++ ppStr name) $
    do state <- getState
       case state of
         Nothing -> parserZero
         Just m ->
             case List.break (\(x,_) -> x == name) m of
               (prefix, (_, t) : suffix) ->
                   do let m' = prefix ++ suffix
                      if null m'
                         then putStateDebug Nothing
                         else putStateDebug (Just m')
                      return t
               _ -> parserZero

xmlParserEndElem :: (Monad m, Stream s m RtEventWithPos) => Name -> PxParser s m ()
xmlParserEndElem name = mkPxParser ("</" ++ ppStr name ++ ">") $
    let f (RtEndElement name') | name == name' = Just ()
        f _ = Nothing
    in matchEvent f ("end-element " ++ ppStr name)

xmlParserTextNotEmpty :: (Monad m, Stream s m RtEventWithPos) => PxParser s m T.Text
xmlParserTextNotEmpty = mkPxParser "text node" $
    let f (RtText t) = Just t
        f _ = Nothing
    in matchEvent f "text node"

-- debug = Debug.Trace.trace
debug _ x = x

putStateDebug x = ("setting state to " ++ show x) `debug` putState x
