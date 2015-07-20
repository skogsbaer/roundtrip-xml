{-# OPTIONS_GHC -ddump-minimal-imports #-}
{-# LANGUAGE DeriveDataTypeable, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, RankNTypes #-}
module Text.Roundtrip.Xml.Enumerator.Parser (

    XmlParseIteratee, PureXmlParseIteratee, parseXml, parseXml', parseXml''
  , XmlException(..)

) where

import Control.Monad.State
import Control.Exception
import Control.Monad.ST
import Data.STRef
import qualified Data.Enumerator as E
import qualified Data.Enumerator.List as EL
import qualified Data.Text as T
import Data.XML.Types
import Data.Typeable

import qualified Debug.Trace

import Text.Roundtrip
import Text.Roundtrip.Parser
import Text.Roundtrip.Xml
import Text.Roundtrip.Xml.ParserInternal

import Data.Reference
import Data.IORef

data (Monad m, Reference r m) => Cursor r m a = Cursor (r (NextCursor r m a)) [a]
data (Monad m, Reference r m) => NextCursor r m a = NextCursor (Cursor r m a)
                                                  | None
                                                  | Uneval

liftIteratee :: Monad m => m b -> E.Iteratee a m b
liftIteratee m = E.Iteratee (m >>= E.runIteratee . return)

unconsStream :: (Monad m, Reference r m)
            => Cursor r m a -> E.Iteratee a m (Maybe (a, Cursor r m a))
unconsStream p@(Cursor r c)
    | null c = do x <- liftIteratee (readRef r)
                  unconsCursor r x
    | otherwise = return $! unconsChunk p

unconsCursor :: (Monad m, Reference r m)
             => r (NextCursor r m a)
             -> NextCursor r m a
             -> E.Iteratee a m (Maybe (a, Cursor r m a))
unconsCursor _ (NextCursor c) = return $! unconsChunk c
unconsCursor _ None = return Nothing
unconsCursor r Uneval = E.continue (extendCursor r)

unconsChunk :: (Monad m, Reference r m)
            => Cursor r m a -> Maybe (a, Cursor r m a)
unconsChunk (Cursor r c) = Just (head c, Cursor r (tail c))

extendCursor :: (Monad m, Reference r m)
             => r (NextCursor r m a)
             -> E.Stream a
             -> E.Iteratee a m (Maybe (a, Cursor r m a))
extendCursor r (E.Chunks l)
    | null l = E.continue (extendCursor r)
    | otherwise = do x <- liftIteratee (insertCursor r l)
                     return $ unconsChunk x
extendCursor r E.EOF =
    do liftIteratee (writeRef r None)
       return Nothing

insertCursor :: (Monad m, Reference r m)
             => r (NextCursor r m a)
             -> [a]
             -> m (Cursor r m a)
insertCursor r l =
    do r' <- newRef Uneval
       let c = Cursor r' l
       writeRef r (NextCursor c)
       return c

mkCursor :: (Monad m, Reference r m) => m (Cursor r m a)
mkCursor =
    do r <- newRef Uneval
       return (Cursor r [])

instance (Monad m, Reference r m) => Stream (Cursor r m a) (E.Iteratee a m) a where
  uncons = unconsStream

type XmlParseIteratee r m = GenXmlParser (Cursor r m RtEventWithPos) (E.Iteratee RtEventWithPos m)

type PureXmlParseIteratee a = forall s . XmlParseIteratee (STRef s) (ST s) a

data XmlException = ParseError ParseError
                  | InvalidEntity T.Text SourcePos
    deriving (Typeable, Show)

instance Exception XmlException

concatCursor :: (Monad m, Reference r m) => Cursor r m a -> m [a]
concatCursor (Cursor r l) =
    liftM (l ++) (readRef r >>= concatCursor')

concatCursor' :: (Monad m, Reference r m) => NextCursor r m a -> m [a]
concatCursor' (NextCursor n) = concatCursor n
concatCursor' _              = return []

parseXml :: (Reference r m, Monad m) => SourceName -> EntityRenderer -> XmlParseIteratee r m a -> E.Iteratee Event m a
parseXml sourceName entityRenderer p =
    E.joinI $ EL.map eventWithoutPos E.$$
              E.joinI $ simplify entityRenderer E.$$
              parseXml''' sourceName p

parseXml' :: (Reference r m, Monad m) => EntityRenderer -> XmlParseIteratee r m a -> E.Iteratee EventWithPos m a
parseXml' entityRenderer p =
    do first <- E.peek
       let src = case first of
                   Just (WithPos _ pos) -> sourceName pos
                   Nothing -> ""
       E.joinI $ simplify entityRenderer E.$$ parseXml''' src p

parseXml'' :: (Reference r m, Monad m) => SourceName -> EntityRenderer -> XmlParseIteratee r m a -> E.Iteratee EventWithPos m a
parseXml'' sourceName entityRenderer p =
       E.joinI $ simplify entityRenderer E.$$ parseXml''' sourceName p

parseXml''' :: (Reference r m, Monad m) => SourceName -> XmlParseIteratee r m a -> E.Iteratee RtEventWithPos m a
parseXml''' sourceName p =
    let GenXmlParser q = xmlBeginDoc *> p <* xmlEndDoc
    in do cursor <- liftIteratee mkCursor
          res <- runParserT (liftM2 (,) q getInput) Nothing sourceName cursor
          case res of
            Left err -> E.returnI (E.Error $ toException $ ParseError err)
            Right (x, restCursor) ->
                do rest <- liftIteratee $ concatCursor restCursor
                   E.yield x (E.Chunks rest)

simplify :: Monad m => (T.Text -> Maybe T.Text) -> E.Enumeratee EventWithPos RtEventWithPos m b
simplify renderEntity = loop
  where
    loop = E.checkDone go
    go k =
        do x <- EL.head
           case x of
             Nothing -> k (E.Chunks []) E.>>== return
             Just (WithPos EventBeginDocument pos) ->
                 k (E.Chunks [WithPos RtBeginDocument pos]) E.>>== loop
             Just (WithPos EventEndDocument pos) ->
                 k (E.Chunks [WithPos RtEndDocument pos]) E.>>== loop
             Just (WithPos (EventInstruction{}) pos) -> go k
             Just (WithPos (EventBeginDoctype{}) pos) -> go k
             Just (WithPos (EventEndDoctype{}) pos) -> go k
             Just (WithPos (EventBeginElement n as) pos) ->
                 let insertAttr m (k, vs) =
                      do vs' <- mapM (contentToText pos) vs
                         return $ (k, (T.concat vs')) : m
                 in do as' <- foldM insertAttr [] as
                       as' `seq` k (E.Chunks [WithPos (RtBeginElement n (reverse as')) pos])
                                 E.>>== loop
             Just (WithPos (EventEndElement n) pos) ->
                 k (E.Chunks [WithPos (RtEndElement n) pos]) E.>>== loop
             Just (WithPos (EventContent c) pos) -> do
                 t <- contentToText pos c
                 ts <- takeContents $ (:) t
                 let text = T.strip $ T.concat $ ts []
                 if T.null text
                    then go k
                    else k (E.Chunks [WithPos (RtText text) pos]) E.>>== loop
             Just (WithPos (EventComment{}) _) -> go k
    contentToText pos c =
        case c of
          ContentEntity e ->
              case renderEntity e of
                Nothing -> E.throwError $ InvalidEntity e pos
                Just t -> return t
          ContentText t -> return t
    takeContents front = do
      do x <- E.peek
         case x of
           Nothing -> return front
           Just (WithPos EventBeginElement{} pos) -> return front
           Just (WithPos EventEndElement{} pos) -> return front
           Just (WithPos (EventContent c) pos) ->
               do EL.drop 1
                  t <- contentToText pos c
                  takeContents $ front . (:) t
           Just (WithPos EventBeginDocument pos) -> return front
           Just (WithPos EventEndDocument pos) -> return front
           Just (WithPos EventInstruction{} pos) -> helper
           Just (WithPos EventBeginDoctype{} pos) -> helper
           Just (WithPos EventEndDoctype{} pos) -> helper
           Just (WithPos EventComment{} pos) -> helper
      where
        helper = EL.drop 1 >> takeContents front

warn = Debug.Trace.trace

-- debug = Debug.Trace.trace
debug _ x = x
