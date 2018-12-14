--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lab 6: Functors                                                            --
--------------------------------------------------------------------------------

module Lab6 where

--------------------------------------------------------------------------------

import Data.Char
import Data.Maybe

--------------------------------------------------------------------------------
-- Parsers

data Parser a = MkParser (String -> Maybe (a, String))

-- | Runs a parser on some input. If successful, the result of the parser
-- is returned along with any remaining input.
parse :: Parser a -> String -> Maybe (a, String)
parse (MkParser f) xs = f xs

-- | A function which, given a predicate, constructs a parser that succeeds
-- if the first character in the input satisfies the predicate.
ch :: (Char -> Bool) -> Parser Char
ch p = MkParser $ \xs -> case xs of
    (y:ys) | p y -> undefined
    _            -> undefined

--------------------------------------------------------------------------------
-- Parsers are functors

instance Functor Parser where
    fmap f (MkParser g) =
        MkParser $ \xs -> undefined

--------------------------------------------------------------------------------
