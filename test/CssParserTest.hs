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

module Main
  where

import Test.HUnit (Assertion, (@=?), runTestTT, Test(..), Counts(..))
import Data.Functor.Identity(Identity)
import Text.Parsec hiding (runParser)
import qualified CssParser as C
import System.Exit (ExitCode(..), exitWith)
import Data.Map hiding (foldr,null,map)
{-import Data.Char (toUpper)-}

exitProperly :: IO Counts -> IO ()
exitProperly m = do
  counts <- m
  exitWith $ if failures counts /= 0 || errors counts /= 0 then ExitFailure 1 else ExitSuccess

testCase :: String -> Assertion -> Test
testCase label_ assertion = TestLabel label_ (TestCase assertion)

main :: IO ()
main = exitProperly $ runTestTT $ TestList [ TestList parserTests ]

runParser :: ParsecT String () Identity a -> String -> Either ParseError a
runParser parser = flip parse "" $ atEnd parser

testSuccParser :: forall b. (Eq b, Show b) => ParsecT String () Identity b -> String -> b -> Assertion
testSuccParser parser in_ out = (Right out) @=? (runParser parser in_)

testFailParser :: forall a. (Eq a) => ParsecT String () Identity a -> String -> Assertion
testFailParser parser in_ =
  case (runParser parser in_) of
    Left _ -> 1 @=? 1
    Right _ -> 1 @=? 2
  
parserSuccEqualsTestCase :: forall a. (Eq a, Show a) => String -> ParsecT String () Identity a -> String -> a -> Test
parserSuccEqualsTestCase name = ((testCase name .) .) . testSuccParser

parserFailsTestCase :: forall a. Eq a => String -> ParsecT String () Identity a -> String -> Test
parserFailsTestCase name parser in_ = testCase name $ testFailParser parser in_

atEnd :: forall b s u (m :: * -> *) t. (Show t, Stream s m t) => ParsecT s u m b -> ParsecT s u m b
atEnd p = p >>= ((eof >>) . return)

parserTests :: [Test]
parserTests =
  [ parserSuccEqualsTestCase "matchUpToX" (C.matchUpToX digit 2) "" "",
    parserSuccEqualsTestCase "matchUpToX" (C.matchUpToX digit 2) "1" "1",
    parserSuccEqualsTestCase "matchUpToX" (C.matchUpToX digit 2) "11" "11",
    parserFailsTestCase "matchUpToX" (C.matchUpToX digit 2) "111", -- Here, it should complain

    parserFailsTestCase "match1ToX" (C.match1ToX digit 2) "",
    parserSuccEqualsTestCase "match1ToX" (C.match1ToX digit 2) "1" "1",
    parserSuccEqualsTestCase "match1ToX" (C.match1ToX digit 2) "11" "11",
    parserFailsTestCase "match1ToX" (C.match1ToX digit 2) "111", -- Here, it should complain

    parserSuccEqualsTestCase "nonascii" C.nonascii "\240" '\240',
    parserSuccEqualsTestCase "nonascii" C.nonascii "\241" '\241',
    parserSuccEqualsTestCase "nonascii" C.nonascii "\1114110" '\1114110',
    parserSuccEqualsTestCase "nonascii" C.nonascii "\1114111" '\1114111',
    parserFailsTestCase "nonascii" C.nonascii "\230",

    parserSuccEqualsTestCase "unicode" C.unicode "\\222230 " "222230",
    parserFailsTestCase "unicode" C.unicode "\\2222301",
    parserFailsTestCase "unicode" C.unicode "\\",

    parserSuccEqualsTestCase "lone \\n" C.nl "\n" "\n",
    parserSuccEqualsTestCase "lone \\r" C.nl "\r" "\n",
    parserSuccEqualsTestCase "lone \\f" C.nl "\f" "\n",
    parserSuccEqualsTestCase "\\r\\n" C.nl "\r\n" "\n",
    parserFailsTestCase "nl" C.nl "a",

    parserSuccEqualsTestCase "test double quote" C.string_ "\"aaa\"" "aaa",
    parserSuccEqualsTestCase "test double quote" C.string_ "\"aa\\\"a\"" "aa\\\"a",
    parserSuccEqualsTestCase "test single quote" C.string_ "'aaa'" "aaa",
    parserSuccEqualsTestCase "test escaped quote" C.string_ "'aa\\'a'" "aa\\'a",
    parserSuccEqualsTestCase "test escaped quote" C.string_ "'aa\\\na'" "aa\\\na",
    parserFailsTestCase "unended string" C.string_ "'aaa",
    parserFailsTestCase "unended string" C.string_ "\"aaa",
    parserFailsTestCase "no string" C.string_ "aaa",

    parserSuccEqualsTestCase "escape" C.escape "\\j" "\\j",
    parserSuccEqualsTestCase "escape" C.escape "\\00" "\\00",
    parserSuccEqualsTestCase "escape" C.escape "\\'" "\\'",
    parserFailsTestCase "no escaped letter should fail" C.escape "\\",
    parserFailsTestCase "escaped newline fails" C.escape "\\\n",

    parserSuccEqualsTestCase "test simple ident" C.ident "ident" "ident",
    parserSuccEqualsTestCase "test alnum ident"  C.ident "ident0" "ident0",
    parserSuccEqualsTestCase "test alnum ident"  C.ident "-ident0" "-ident0",
    parserFailsTestCase "ident can't begin with num" C.ident "1dent",
    parserFailsTestCase "ident can't contain space" C.ident "dent ",

    parserSuccEqualsTestCase "test simple name" C.name "name" "name",
    parserSuccEqualsTestCase "test alnum name"  C.name "name0" "name0",
    parserSuccEqualsTestCase "test alnum name"  C.name "-name0" "-name0",
    parserSuccEqualsTestCase "name begins with num" C.name "1dent" "1dent",
    parserFailsTestCase "name can't contain space" C.name "dent ",

    parserSuccEqualsTestCase "test num" C.num "123" "123",           
    parserSuccEqualsTestCase "test floating num" C.num "1.23" "1.23",
    parserSuccEqualsTestCase "test floating num" C.num ".23" ".23",

    parserSuccEqualsTestCase "url with string" C.urlParse "url('something')" $ C.URLFunc "something",
    parserSuccEqualsTestCase "url with string" C.urlParse "url(\"something\")" $ C.URLFunc "something",
    parserSuccEqualsTestCase "url with string and spaces" C.urlParse "url( \"something\" )" $ C.URLFunc "something",
    parserSuccEqualsTestCase "url without string" C.urlParse "url(/a/b/c)" $ C.URLFunc "/a/b/c",
    parserSuccEqualsTestCase "url without string" C.urlParse "url(a/b/c)" $ C.URLFunc "a/b/c",

    parserSuccEqualsTestCase "page rule" C.page "@page{a:b}" $ C.PageDeclaration Nothing (fromList [("a","b")]),
    parserSuccEqualsTestCase "page whitespace" C.page "@page { a  : b ; }" $ C.PageDeclaration Nothing (fromList [("a","b")]),
    parserSuccEqualsTestCase "pseudo-page" C.page "@page:first{a:b;}" $ C.PageDeclaration (Just "first") (fromList [("a","b")]),

    parserSuccEqualsTestCase "charset" C.charset "@charset \"UTF-8\";" $ (Just . C.Charset)  "UTF-8",

    parserSuccEqualsTestCase "Medium identifier" C.medium "print" $ "print",
    parserSuccEqualsTestCase "Medium identifier" C.medium "print " $ "print",

    parserSuccEqualsTestCase "Media Declarations" C.mediaList "print, screen" $ ["print", "screen"],

    parserSuccEqualsTestCase "Media Declarations" C.media "@media screen {}" $ C.MediaQuery ["screen"] $ C.DeclarationList [],
    parserSuccEqualsTestCase "Media Declarations" C.media "@media screen { a {b:c;} }" $ C.MediaQuery ["screen"] $ C.DeclarationList [C.CSSDeclaration (C.SelectorList [C.TagName "a"]) (fromList [("b", "c")])]
    ]
  {-
   -[ testCase "empty accumulation" $
   -  [] @=? accumulate square []
   -, testCase "accumulate squares" $
   -  [1, 4, 9] @=? accumulate square [1, 2, 3]
   -, testCase "accumulate upcases" $
   -  ["HELLO", "WORLD"] @=? accumulate (map toUpper) ["hello", "world"]
   -, testCase "accumulate reversed strings" $
   -  ["eht", "kciuq", "nworb", "xof", "cte"] @=?
   -  accumulate reverse ["the", "quick", "brown", "fox", "etc"]
   -, testCase "accumulate recursively" $
   -  [["a1", "a2", "a3"], ["b1", "b2", "b3"], ["c1", "c2", "c3"]] @=?
   -  accumulate (\c -> accumulate ((c:) . show) ([1, 2, 3] :: [Int])) "abc"
   -, testCase "accumulate non-strict" $
   -  ["nice work!"] @=?
   -  take 1 (accumulate id
   -    ("nice work!" :
   -     error "accumulate should be even lazier, don't use reverse!"))
   -]
   -}
