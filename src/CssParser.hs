{-
 -Copyright (c) 2015 Edward Langley
 -All rights reserved.
 -
 -Redistribution and use in source and binary forms, with or without
 -modification, are permitted provided that the following conditions
 -are met:
 -
 -Redistributions of source code must retain the above copyright notice,
 -this list of conditions and the following disclaimer.
 -
 -Redistributions in binary form must reproduce the above copyright
 -notice, this list of conditions and the following disclaimer in the
 -documentation and/or other materials provided with the distribution.
 -
 -Neither the name of the project's author nor the names of its
 -contributors may be used to endorse or promote products derived from
 -this software without specific prior written permission.
 -
 -THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 -"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 -LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 -FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 -HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 -SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
 -TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 -PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 -LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 -NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 -SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 -}

{-# LANGUAGE RankNTypes,KindSignatures #-}

module CssParser
  where

import Prelude hiding (init,fst,length)
import Control.Monad
import qualified Text.Parsec as NP
import Text.ParserCombinators.Parsec
import Data.Functor.Identity()
import Text.Parsec.Prim()
import Data.Map hiding (foldr,null,map)
import Data.Maybe

data URL = URLFunc String |
           URLString String |
           URLMedia [String] URL
  deriving (Show, Eq)


type Query = [String]
data CSSDeclaration = CSSDeclaration CSSSelector CSSBody |
                      PageDeclaration (Maybe String) CSSBody |
                      Charset String |
                      Import URL |
                      MediaQuery Query CSSDeclaration |
                      DeclarationList [CSSDeclaration]
  deriving (Show,Eq)


type CSSBody = Map String String

data CSSSelector =
  Ident String |
  TagName String |
  Pseudo String |
  Attribute String String |
  Id String |
  Class String |
  Sibling CSSSelector CSSSelector | -- These are all XRelation Head Tail
  DirectChild CSSSelector CSSSelector |
  Child CSSSelector CSSSelector |
  SelectorList [CSSSelector]
  deriving (Show,Eq)

matchUpToX :: (Eq a,Num a) => GenParser Char st Char -> a -> GenParser Char st String
matchUpToX p = scan id
  where
    scan f x = if x == 0
      then return (f [])
      else do {
        nxt <- p;
        scan (\tail_ -> f (nxt:tail_)) (x-1)
      } <|> return (f [])

match1ToX :: (Eq a,Num a) => GenParser Char st Char -> a -> GenParser Char st String
match1ToX p n = do
  result <- p
  tail_ <- matchUpToX p (n-1)
  return (result:tail_)

-- Char classes
h :: GenParser Char st Char
h = hexDigit

nonascii  :: GenParser Char st Char
nonascii  = satisfy isNonAscii
  where
    isNonAscii a = '\240' <= a && a <= '\1114111'

unicode :: GenParser Char st String
unicode = do
  _ <- char '\\'
  result <- match1ToX h (6 :: Integer)
  spaces
  return result

escape :: GenParser Char st String
escape = liftM ('\\':) (try unicode) <|> char '\\' <:> many1 (noneOf "\r\n\f0123456789abcdef")

nmstart :: GenParser Char st Char
nmstart = oneOf "-" <|> letter

nmchar :: GenParser Char st Char
nmchar = oneOf "_-" <|> alphaNum


space_ :: GenParser Char st Char
space_ = oneOf " \t\r\n\f"

s :: GenParser Char st String
s = many1 space_

w :: GenParser Char st String
w = many space_

nl :: GenParser Char st String
nl = (string "\n" <|> try (string "\r\n") <|> string "\r" <|> string "\f") >> return "\n"

genBadString :: Char -> GenParser Char st String
genBadString c = do
  _ <- char c
  result <- liftM concat $
    many $ many1 (noneOf (c:"\n\r\f")) <|> (char '\\' >> nl) <|> escape
  _ <- char '\\'
  _ <- anyChar
  return result

badstring :: GenParser Char st String
badstring = genBadString '"' <|> genBadString '\''

string_ :: GenParser Char st String
string_ = do
  a <- char '"' <|> char '\''
  result <- liftM concat $ many $
    try (char '\\' <:> nl) <|> try escape <|> liftM (:[]) (noneOf (a:"\n\r\f"))
  _ <- char a
  return result

-- badcomment, baduri, comment undefined

-- tested
ident :: GenParser Char st String
ident = do
  head_ <- optionMaybe $ string "-"
  result <- nmstart
  tail_ <- many nmchar
  return $ fromMaybe "" head_ ++ (result:tail_)

--tested
name :: GenParser Char st String
name = many1 nmchar

-- tested
num :: GenParser Char st String
num = floating <|> many1 digit
  where
    floating = try $ do
      radix <- many digit
      mantissa <- char '.' <:> many1 digit
      return $ radix ++ mantissa

cdo :: GenParser Char st String
cdo = string "<!--"

cdc :: GenParser Char st String
cdc = string "-->"

-- TODO: figger out if / should be here
url :: GenParser Char st String
url = liftM concat $ many $ many1 (oneOf "!#$%&*-~" <|> nonascii) <|> escape

atEnd :: forall b s u (m :: * -> *) t. (Show t, NP.Stream s m t) => NP.ParsecT s u m b -> NP.ParsecT s u m b
atEnd p = p >>= ((eof >>) . return)

-- partially tested
urlParse :: GenParser Char st URL
urlParse = do
  result <- between (string "url(" >> spaces) (char ')') urlChars
  return $ URLFunc result
  where
    urlChars = withSpace $ string_ <|> many1 (oneOf "=?.,:/" <|> alphaNum) <|> url
stylesheet :: GenParser Char st CSSDeclaration
stylesheet = do -- TODO: allow the letters to be escaped in these strings
  chrset <- charset
  let cset  = case chrset of
              Nothing -> [];
              Just x -> [x]
  _ <- many $ s <|> cdo <|> cdc

  imports <- liftM concat $ many $ do
    imports <- many1 import_
    _ <- many ((cdo >> spaces) <|> (cdc >> spaces))
    return imports

  result <- many $ ruleset  <|> media <|> page
  _ <- many $ (cdo >> many s) <|> (cdc >> many s)

  return $ DeclarationList $ cset ++ imports ++ result

page :: GenParser Char st CSSDeclaration
page = do
  _ <- string "@page"
  spaces
  pseudo_ <- optionMaybe pseudoPage

  body <- rulebody
  return $ PageDeclaration pseudo_ $ fromList body

pseudoPage :: GenParser Char st String
pseudoPage = char ':' >> ident >>= (spaces >>) . return 


charset :: GenParser Char st (Maybe CSSDeclaration)
charset = optionMaybe $ do
  _ <- try $ string "@charset "
  result <- string_
  _ <- char ';'
  return $ Charset result

medium :: GenParser Char st String
medium = ident >>= (\q -> spaces >> return q)

mediaList :: GenParser Char st [String]
mediaList = sepBy medium (char ',' >> spaces)

media :: GenParser Char st CSSDeclaration
media =  do
  _ <- try $ string "@media"
  spaces
  query <- mediaList
  body <- between (char '{') (char '}') $ do
    spaces
    rules <- many ruleset
    spaces
    return rules
  return $ MediaQuery query $ DeclarationList body

import_ :: GenParser Char st CSSDeclaration
import_ = do
  _ <- try $ string "@import "
  spaces
  result <- liftM URLString string_ <|> urlParse
  spaces
  tail_ <- optionMaybe mediaList
  _ <- char ';'
  spaces
  return $ Import $ case tail_ of
    Nothing -> result
    Just x -> URLMedia x result



cssRule :: GenParser Char st CSSSelector
cssRule = liftM SelectorList (sepBy1 selector $ char ',' >> spaces)

selector :: GenParser Char st CSSSelector
selector = do
  result <- simpleSelector
  tail_ <- optionMaybe $ parseCombinator result <|> try (parseOCombinator result)
  spaces
  return $ fromMaybe result tail_

-- This returns the value of a combinator, ignoring trailing spaces
withSpace :: GenParser Char u b -> GenParser Char u b
withSpace = (>>= ((spaces >>) . return))

oneOfWithSpace :: String -> GenParser Char st Char
oneOfWithSpace ss = foldr1 (<|>) parsers
  where
    parsers = map (withSpace . char) ss

operator :: GenParser Char st Char
operator = oneOfWithSpace "/,"

combinator :: GenParser Char st Char
combinator = oneOfWithSpace "+>"

unaryOperator :: GenParser Char st Char
unaryOperator = oneOf "-+"

-- parse an optional combinator
parseOCombinator :: CSSSelector -> GenParser Char st CSSSelector
parseOCombinator head_ = do
  spaces
  combine <- optionMaybe combinator
  sel <- selector
  return $ case combine of
    Nothing -> Child head_ sel
    (Just x) -> case x of
      '+' -> Sibling head_ sel
      '>' -> DirectChild head_ sel
      _ -> undefined

-- parse a combinator
parseCombinator :: CSSSelector -> GenParser Char st CSSSelector
parseCombinator head_ = do
  combine <- combinator
  sel <- selector
  return $ case combine of
    '+' -> Sibling head_ sel
    '>' -> DirectChild head_ sel
    _ -> undefined

simpleSelector :: GenParser Char st CSSSelector
simpleSelector = do
  result <- (try elementName <:> many modifier) <|> many1 modifier
  return $ if null $ tail result
    then head result
    else SelectorList result

modifier :: GenParser Char st CSSSelector
modifier = hash <|> class_ <|> attrib <|> pseudo

elementName :: GenParser Char st CSSSelector
elementName = liftM TagName $ ident <|> string "*"

hash :: GenParser Char st CSSSelector
hash = char '#' >> liftM Id ident

class_ :: GenParser Char st CSSSelector
class_ = char '.' >> liftM Class ident

attrib :: GenParser Char st CSSSelector
attrib = between (char '[') (char ']') $ do
  spaces
  key <- ident
  spaces
  value <- char '=' >> ident
  spaces
  return $ Attribute key value

pseudo :: GenParser Char st CSSSelector
pseudo = char ':' >> liftM Pseudo selectorIdent
  where
    selectorIdent = do
      init <- optionMaybe $ string ":"
      idnt <- ident
      return $ fromMaybe "" init ++ idnt


{-# ANN (<++>) "HLint: ignore" #-}
(<++>) :: Monad m => m [a] -> m [a] -> m [a]
a <++> b = liftM2 (++) a b

(<:>) :: Monad m => m a -> m [a] -> m [a]
a <:> b = liftM2 (:) a b


ruleset :: GenParser Char st CSSDeclaration
ruleset = do
  selector_ <- cssRule
  spaces
  result <- rulebody
  return $ CSSDeclaration selector_ $ fromList result
  
rulebody :: GenParser Char st [(String, String)]
rulebody = between (withSpace $ char '{') (withSpace $ char '}') $ do
    head_ <- declaration
    tail_ <- many $ do
      char ';' >> spaces
      optionMaybe declaration
    _ <- spaces
    return (head_:catMaybes tail_)

declaration :: GenParser Char st (String,String)
declaration = do
  prop <- property
  value <- withSpace (char ':') >> expr
  optional prio
  return (prop,value)

prio :: GenParser Char st String
prio = char '!' >> spaces >> string "important"

-- TODO: Still need to implement comments . . . 

expr :: GenParser Char st String
expr = do
  fst <- term
  rst <- many $ do
    op <- optionMaybe $ liftM (:[]) operator
    trm <- term
    return $ case op of
      Nothing -> trm
      Just x -> x ++ (' ':trm)
  return $ fst ++ foldr (\x y -> (' ':x++(' ':y))) [] rst

term :: GenParser Char st String
term = withSpace numerical <|> withSpace uri <|> withSpace string_ <|> withSpace ident <|> hexcolor <|> function

numerical :: GenParser Char st String
numerical = do
  unop <- optionMaybe unaryOperator
  value <- withSpace (try percentage) <|> withSpace (try length) <|> withSpace (try ems) <|> withSpace (try exs)
           <|> withSpace (try angle) <|> withSpace (try time) <|> withSpace (try freq) <|> withSpace number 
  return $ case unop of
    Just '-' -> '-':value
    _ -> value
  where
    number  :: GenParser Char st String
    number = num

    percentage :: GenParser Char st String
    percentage = num <++> string "%"

    length :: GenParser Char st String
    length = num <++> foldr1 (<|>) (map string ["px", "cm", "mm", "in", "pt", "pc", "rem", "vw", "vh"])

    ems  :: GenParser Char st String
    ems = num <++> string "em"

    exs  :: GenParser Char st String
    exs = num <++> string "ex"

    angle  :: GenParser Char st String
    angle = num <++> (string "deg" <|> string "rad" <|> string "grad")

    time  :: GenParser Char st String
    time = num <++> (string "ms" <|> string "s")

    freq  :: GenParser Char st String
    freq = num <++> (string "hz" <|> string "khz")
      

function :: GenParser Char st String
function = do
  _ <- ident
  _ <- string "("
  spaces
  d <- expr
  _ <- string ")"
  spaces
  return d

hexcolor :: GenParser Char st String
hexcolor = char '#' <:> many1 hexDigit
  
uri :: GenParser Char st String
uri = do
  a <- string "url("
  result <- w >> (try string_ <|> url)
  d <- w >> string ")"
  return $ a ++ result ++ d


property :: GenParser Char st String
property = do
  result <- ident
  spaces
  return result

parseCSS :: String -> Either ParseError CSSDeclaration
parseCSS = parse stylesheet  "(unknown)"
