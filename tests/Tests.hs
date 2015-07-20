{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings, RankNTypes, TemplateHaskell #-}

import Control.Exception (throw)
import System.Environment (getArgs)
import System.FilePath
import Control.Monad (liftM2)
import Data.Maybe (isJust, fromJust)

import qualified Data.ByteString as BS

import Data.Text (Text)
import qualified Data.Text as T

import qualified Data.List as List
import Data.Char (isSpace)

import Text.Roundtrip
import Text.Roundtrip.Xml

import Test.Framework
import Test.Framework.TestManager

--
-- Specification for expressions
--

data Expr = Var String
          | Lit Int
          | Plus Expr Expr
            deriving (Show, Eq)

$(defineIsomorphisms ''Expr)

xmlVariable :: XmlSyntax d => d String
xmlVariable = xmlElem "var" (xmlAttr "name" textStringIso)

xmlInteger :: XmlSyntax d => d Int
xmlInteger = xmlElem "lit" (xmlAttr "value" readShowTextIso)

pXmlExpr :: XmlSyntax d => d Expr
pXmlExpr = var  <$> xmlVariable
       <|> lit  <$> xmlInteger
       <|> plus <$> xmlElem "plus" (pXmlExpr <*> pXmlExpr)

instance Arbitrary Expr where
    arbitrary = sized arbExpr
        where arbExpr 0 = frequency simpleExprs
              arbExpr n = frequency (simpleExprs ++
                                     [(5, liftM2 Plus (arbExpr (n `div` 2))
                                                      (arbExpr (n `div` 2)))])
              simpleExprs = [(1, do n <- arbitrary
                                    return (Lit n)),
                             (1, do v <- elements letters
                                    vs <- listOf (elements lettersOrDigits)
                                    return (Var (v:vs)))]
              letters = ['a'..'z'] ++ ['A'..'Z']
              lettersOrDigits = letters ++ ['0'..'9']
    shrink (Var _) = []
    shrink (Lit _) = []
    shrink (Plus e1 e2) = [e1, e2]

test_exprParser :: IO ()
test_exprParser =
    do let epe = runXmlParserString pXmlExpr "<string>" defaultEntityRenderer
                   "<plus><lit value=\"1\"/><plus><var name=\"foo\"/><lit value=\"2\"/></plus></plus>"
       pe <- assertRight epe
       assertEqual (Plus (Lit 1) (Plus (Var "foo") (Lit 2))) pe

test_exprPrinter :: IO ()
test_exprPrinter =
    do let ms = runXmlPrinterString pXmlExpr (Plus (Lit 1) (Plus (Var "foo") (Lit 2)))
       s <- assertJust ms
       assertEqual "<plus><lit value=\"1\"/><plus><var name=\"foo\"/><lit value=\"2\"/></plus></plus>" s

prop_exprPrinterDoesNotFail :: Expr -> Bool
prop_exprPrinterDoesNotFail expr = isJust (runXmlPrinterString pXmlExpr expr)

prop_exprPrinterParserInverse :: Expr -> Bool
prop_exprPrinterParserInverse expr =
    let code = fromJust (runXmlPrinterString pXmlExpr expr)
    in case runXmlParserString pXmlExpr "<string>" defaultEntityRenderer code of
         Left err -> error (show err)
         Right expr' -> expr == expr'


--
-- Parsing, invalid lookahead, David, 2011-07-23
--

pilSpec1 :: XmlSyntax d => d (Either [Text] [Text])
pilSpec1 =
    xmlElem "root"
    (xmlElem "list" (left <$> many1 (xmlElem "foo" xmlText)) <||>
     xmlElem "list" (right <$> many (xmlElem "bar" xmlText)))

pilSpec2 :: XmlSyntax d => d (Either [Text] [Text])
pilSpec2 =
    xmlElem "root"
    (xmlElem "list" ((left <$> many1 (xmlElem "foo" xmlText)) <|>
                     (right <$> many (xmlElem "bar" xmlText))))

prop_pilSpec1Roundtrip :: Either [Text] [Text] -> Property
prop_pilSpec1Roundtrip arg =
    (case arg of
       Left [] -> False
       _ -> True)
    ==>
    checkRoundtrip pilSpec1 arg

prop_pilSpec2Roundtrip :: Either [Text] [Text] -> Property
prop_pilSpec2Roundtrip arg =
    (case arg of
       Left [] -> False
       _ -> True)
    ==>
    checkRoundtrip pilSpec2 arg

--
-- Utils & main
--

instance Arbitrary Text where
    arbitrary =
        do s <- arbitrary
           return $ T.pack $ trim s
      where
        trim = List.dropWhile isSpace . reverse . List.dropWhile isSpace . reverse

testFile f = "tests" </> f

checkRoundtrip :: (Eq a, Show a) => (forall d . XmlSyntax d => d a) -> a -> Bool
checkRoundtrip spec val =
    case runXmlPrinterString spec val of
      Nothing -> error ("could not print " ++ show val)
      Just t ->
          case runXmlParserString spec "<text>" defaultEntityRenderer t of
            Right val' ->
                if val == val'
                   then True
                   else error (show val ++ " /= " ++ show val')
            Left err -> error ("Parsing of " ++ show t ++ " failed: " ++ show err)

main =
    do args <- getArgs
       runTestWithArgs args htf_thisModulesTests
