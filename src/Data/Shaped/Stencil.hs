{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Shaped.Stencil
-- Copyright   :  (c) Christopher Chalmers
-- License     :  BSD3
--
-- Maintainer  :  Christopher Chalmers
-- Stability   :  provisional
-- Portability :  non-portable
--
-- Stencils can be used to sum (or any fold) over neighbouring sites to
-- the current position on a 'Focused'.
-----------------------------------------------------------------------------
module Data.Shaped.Stencil
  ( -- * The Stencil type
    Stencil (..)
  , mkStencil
  , mkStencilUnboxed

    -- ** Using stencils
  , stencilSum

    -- * Template haskell

    -- -- ** Stencils
  -- , stencil
  -- , stencilList

  -- -- ** Unrolling lists
  -- , ShapeLift (..)
  -- , mkStencilTH
  -- , mkStencilTHWith
  ) where

import           Control.Lens
import           Data.Functor.Classes
import           Data.Shaped.Base
import           Data.Shaped.Generic          (Boundary (..), peekRelativeB)
import           Data.Shaped.Index
import qualified Data.Vector.Unboxed          as U
import           Text.Show

-- Types ---------------------------------------------------------------

-- | Stencils are used to fold over neighbouring array sites.
newtype Stencil f a = Stencil (forall b. (f Int -> a -> b -> b) -> b -> b)

instance (Show1 f, Show a) => Show (Stencil f a) where
  showsPrec _ s = showListWith g (itoList s) where
    g (i,x) = showChar '(' . showsPrec1 0 i . showChar ',' . showsPrec 0 x . showChar ')'

instance Foldable (Stencil f) where
  foldr f z (Stencil s) = s (\_ a b -> f a b) z
  {-# INLINE foldr #-}

instance FoldableWithIndex (f Int) (Stencil f) where
  ifoldr f b (Stencil s) = s f b
  {-# INLINE ifoldr #-}
  ifoldMap = ifoldMapOf (ifoldring ifoldr)
  {-# INLINE ifoldMap #-}

instance Functor (Stencil f) where
  fmap f (Stencil s) = Stencil $ \g z -> s (\x a b -> g x (f a) b) z
  {-# INLINE [0] fmap #-}

-- | Make a stencil folding over a list.
--
--   If the list is staticlly known this should expand at compile time
--   via rewrite rules, similar to 'makeStencilTH' but less reliable. If
--   that does not happen the resulting could be slow. If the list is
--   not know at compile time, 'mkStencilUnboxed' can be signifcantly
--   faster (but isn't subject expending via rewrite rules).
mkStencil :: [(f Int, a)] -> Stencil f a
mkStencil l = Stencil $ \g z -> myfoldr (\(i,a) b -> g i a b) z l
{-# INLINE mkStencil #-}

-- Version of foldr that recursivly expands the list via rewrite rules.
myfoldr :: (a -> b -> b) -> b -> [a] -> b
myfoldr f b = go where
  go []     = b
  go (a:as) = f a (go as)
{-# INLINE [0] myfoldr #-}

{-# RULES
"mkStencil/cons" forall f b a as.
 myfoldr f b (a:as) = f a (myfoldr f b as)
 #-}

-- | Make a stencil folding over an unboxed vector from the list.
mkStencilUnboxed :: (U.Unbox (f Int), U.Unbox a) => [(f Int, a)] -> Stencil f a
mkStencilUnboxed l = Stencil $ \g z -> U.foldr (\(i,a) b -> g i a b) z v
  where !v = U.fromList l
{-# INLINE mkStencilUnboxed #-}

-- | Sum the elements around a 'Focused' using a 'Boundary' condition
--   and a 'Stencil'.
--
--   This is often used in conjunction with 'extendFocus'.
stencilSum :: (Shape f, Num a) => Boundary -> Stencil f a -> Focused f a -> a
stencilSum bnd s = \w ->
  let f i b a = b + a * peekRelativeB bnd i w
      {-# INLINE [0] f #-}
  in  ifoldl' f 0 s
{-# INLINE stencilSum #-}

-- ------------------------------------------------------------------------
-- -- Template Haskell
-- ------------------------------------------------------------------------

-- -- | Class of shapes that can be 'lift'ed.
-- --
-- --   This is to prevent orphans for the 'Lift' class. (I may make a PR
-- --   for 'Lift' instances to be in `linear`.)
-- class Shape f => ShapeLift f where
--   -- | 'lift' for 'Shape's.
--   liftShape :: Lift a => f a -> Q Exp

-- instance ShapeLift V1 where
--   liftShape (V1 x) = [| V1 x |]

-- instance ShapeLift V2 where
--   liftShape (V2 x y) = [| V2 x y |]

-- instance ShapeLift V3 where
--   liftShape (V3 x y z) = [| V3 x y z |]

-- instance ShapeLift V4 where
--   liftShape (V4 x y z w) = [| V4 x y z w |]

-- -- | QuasiQuoter for producing a static stencil definition. This is a
-- --   versatile parser for 1D, 2D and 3D stencils.
-- --
-- --     - 1D stencils are of the form
-- --       @
-- --       [stencil| 5 3 1 3 5 |] :: Num a => Stencil V1 a
-- --       @
-- --
-- --     - 2D stencils are of the form
-- --       @
-- --       [stencil|
-- --         0 1 0
-- --         1 0 1
-- --         0 1 0 |] :: Num a => Stencil V2 a
-- --       @
-- --
-- --     - 3D stencils are of the form
-- --       @
-- --       [stencil|
-- --         1/20 3/10 1/20
-- --         3/10  1   3/10
-- --         1/20 3/10 1/20
-- --
-- --         3/10  1   3/10
-- --          1   1/10  1
-- --         3/10  1   3/10
-- --
-- --         1/20 3/10 1/20
-- --         3/10  1   3/10
-- --         1/20 3/10 1/20
-- --       |] :: Fractional a => Stencil V3 a
-- --       @
-- --
-- --   There are two different numbers that can be represented:
-- --
-- --     - @'Num' a => 'Stencil' f a@ for when each number in the stencil
-- --       is an integral.
-- --
-- --     - @'Fractional' a => 'Stencil' f a@ if the integral parse fails,
-- --       it will fall back to a fractional parser. Fractional numbers
-- --       can either be written as @1.5@, @15e-1$, @3/2@ or $3 % 2@.
-- --
-- --   The formating of the stencil quite strict. This is to help prevent
-- --   typos in creating the stencil. The number of rows/columns must
-- --   stay remain the same and be odd. Any values with @0@ are ignored so
-- --   asymmetric shapes can still be represented.
-- --
-- stencil :: QuasiQuoter
-- stencil = QuasiQuoter
--   { quoteExp  = parseStencil
--   , quotePat  = error "stencil can't be used in pattern"
--   , quoteType = error "stencil can't be used in type"
--   , quoteDec  = error "stencil can't be used in dec"
--   }

-- -- | Variation of 'stencil' that returns a list of indexes relative the
-- --   centre of the stencil and the elements.
-- --
-- -- @
-- --       [stencilList|
-- --         0 1 0
-- --         1 0 1
-- --         0 1 0 |] :: Num a => [(V2 Int, a)]
-- -- @
-- --
-- --   This can be useful is you want to apply a function to this list
-- --   before turning it into a stencil (via 'mkStencilTH' or
-- --   'mkStencil').
-- stencilList :: QuasiQuoter
-- stencilList = QuasiQuoter
--   { quoteExp  = parseStencilList
--   , quotePat  = error "stencilList can't be used in pattern"
--   , quoteType = error "stencilList can't be used in type"
--   , quoteDec  = error "stencilList can't be used in dec"
--   }

-- -- Parsing stencils ----------------------------------------------------

-- -- | Generate a stencil
-- parseStencil :: String -> Q Exp
-- parseStencil str = unfoldExp <$> parseStencilList' str

-- -- [(f Int, a)] -> Stencil f a
-- unfoldExp :: [Exp] -> Exp
-- unfoldExp exps =
--   AppE (ConE 'Stencil) (LamE [VarP f,VarP b] $ foldr g (VarE b) exps)
--   where
--     g (TupE [iE, aE]) e = AppE (AppE (AppE (VarE f) iE) aE) e
--     g badExp          _ = error $ "unexpected value " ++ show badExp
--     f = mkName "f"
--     b = mkName "b"

-- -- | Generate a stencil list.
-- parseStencilList :: String -> Q Exp
-- parseStencilList str = ListE <$> parseStencilList' str

-- -- | List of expressions forming a stencil. To be either turned into
-- --   either a real list or an unfolded stencil.
-- parseStencilList' :: String -> Q [Exp]
-- parseStencilList' str =
--   case mapM (mapM parseLine) (map lines ps) of
--     Nothing      -> error "number parse error"
--     Just []      -> pure []
--     Just [[]]    -> pure []
--     Just [[[]]]  -> pure []
--     Just [[[a]]] -> fmap pure $ case rToInt a of
--                       Just a' -> [| (zero,a') |]
--                       Nothing -> [| (zero,a) |]
--     Just [[as]]  -> run $ parse1D as
--     Just [ass]   -> run $ parse2D (List.transpose ass)
--     Just asss    -> run $ parse3D (map List.transpose asss)
--   where
--     ps              = paragraphs str
--     run (Left err)  = error err
--     run (Right ias) = case generaliseCoeffs ias of
--                         Left iss  -> liftIndexed iss
--                         Right rss -> liftIndexed rss

-- -- | Remove zeros. Return 'Integer's if it can be done exactly.
-- generaliseCoeffs :: [(a, Rational)] -> Either [(a, Rational)] [(a, Integer)]
-- generaliseCoeffs = attempt (mapM (_2 rToInt)) . filter ((/=0) . snd)
--   where attempt f a = maybe (Left a) Right (f a)


-- liftIndexed :: (ShapeLift f, Lift a) => [(f Int, a)] -> Q [Exp]
-- liftIndexed xs = traverse f xs where
--   f (i,a) = do
--     iE <- liftShape i
--     aE <- lift a
--     pure $ TupE [iE, aE]

-- -- | Split a string up into paragraphs separated by a new line. Extra
-- --   newlines inbetween paragraphs are stripped.
-- paragraphs :: String -> [String]
-- paragraphs = go [] . strip where
--   go ps ('\n':'\n':xs) = [reverse ps] ++ go [] (strip xs)
--   go ps (x:xs)         = go (x:ps) xs
--   go [] []             = []
--   go ps []             = [reverse $ strip ps]

--   strip = dropWhile (\x -> x == '\n' || x == ' ')

-- -- Parsing specific dimensions -----------------------------------------

-- -- | Parse a 1D stencil. If the system is not valid, return a string
-- --   with error message. 0 elements are not yet removed.
-- parse1D :: [Rational] -> Either String [(V1 Int, Rational)]
-- parse1D as
--   | null as   = err "empty list"
--   | even x    = err "x is even (it must be odd)"
--   | otherwise = Right $ zip (stencilIxes (V1 x)) as
--   where
--     x = length as

--     err str = Left $ "1d stencil: " ++ str

-- -- | Parse a 2D stencil. If the system is not valid, return a string
-- --   with error message.
-- parse2D :: [[Rational]] -> Either String [(V2 Int, Rational)]
-- parse2D as
--   | null as            = err "empty list"
--   | even x             = err "x is even (it must be odd)"
--   | even y             = err "y is even (it must be odd)"
--   | Just (i,a) <- badX =
--       err $ "number of columns not consistent. Should be " ++
--             show x ++ " columns but row " ++ show i ++ " has " ++ show a
--             ++ "columns."
--   | otherwise = Right $ zip (stencilIxes (V2 x y)) (concat as)
--    where
--     y    = length as
--     x    = length (head as)
--     badX = ifind (const (/= x)) (map length as)

--     err str = Left $ "2d stencil: " ++ str

-- -- | Parse a 3D stencil. If the system is not valid, return a string
-- --   with error message. 0 elements are not yet removed.
-- parse3D :: [[[Rational]]] -> Either String [(V3 Int, Rational)]
-- parse3D as -- = error "3D stencil parser not yet implemented"
--   | even x    = Left "x is even (it must be odd)"
--   | even y    = Left "y is even (it must be odd)"
--   | even z    = Left "z is even (it must be odd)"
--   -- | Just (i,a) <- badX =
--   --     err $ "number of columns not consistent. Should be " ++
--   --           show x ++ " columns but row " ++ show i ++ " has " ++ show a
--   --           ++ "columns."
--   -- | Just (i,a) <- badY =
--   --     err $ "number of rows not consistent. First column has " ++
--   --           show x ++ " rows but column " ++ show i ++ " has " ++ show a
--   --           ++ "rows."
--   | otherwise = Right $ zip (stencilIxes (V3 x y z)) (concatMap concat as)
--    where
--     -- xs = map words (lines str)
--     -- integrals = mapM (mapM (mapM readMaybe)) xs :: Maybe [[[Integer]]]
--     -- fractions = mapM (mapM (mapM readMaybe)) xs :: Maybe [[[Double]]]

--     y = length as
--     x = length (head as)
--     z = length (head (head as))
--     -- consistent = odd x && odd y && odd z && all ((==x) . length) as
--     -- ixs = stencilIxes (V3 x y z)

--     -- addIndexes :: (Num a, Eq a) => [[a]] -> [(V2 Int, a)]
--     -- addIndexes = filter ((/=0) . snd) . zip ixs . concat

-- -- | A list of ordered indexes with the centre element at 'zero'.
-- stencilIxes :: Shape f => f Int -> [f Int]
-- stencilIxes l = map (^-^ fmap (`div` 2) l) (toListOf shapeIndexes l)

-- -- Parsing numbers ----------------------------------------------------

-- -- | Convert a 'Rational' to an 'Integer' if it can be done exactly.
-- rToInt :: Rational -> Maybe Integer
-- rToInt a = if denominator a == 1 then Just (numerator a) else Nothing

-- -- | Parse a single line for the stencil QQ.
-- parseLine :: String -> Maybe [Rational]
-- parseLine = preview _last . map fst . readP_to_S (many parseFractional)

-- -- | Parse a fracional number that may be a normal number or @2\3@ or
-- --   @2+3@.
-- --
-- --   In the future this could be used parse more general expressions.
-- parseFractional :: ReadP Rational
-- parseFractional = operated <|> num
--   where
--     r = Lex.numberToRational
--     num = do
--       Number n <- Lex.lex
--       pure $ r n
--     operated = do
--       Number n1 <- Lex.lex
--       Symbol s  <- Lex.lex
--       Number n2 <- Lex.lex
--       pure $ case s of
--             "/" -> r n1 / r n2
--             "%" -> r n1 / r n2
--             "+" -> r n1 + r n2
--             x   -> error $ "unknown symbol " ++ x

-- -- Unrolled stencils ---------------------------------------------------

-- -- | Construct a 'Stencil' by unrolling the list at compile time. For
-- --   example
-- --
-- -- @
-- -- 'ifoldr' f b $('makeStencilTH' [('V1' (-1), 5), ('V1' 0, 3), ('V1' 1, 5)])
-- -- @
-- --
-- --   will be get turned into
-- --
-- -- @
-- -- f ('V1' (-1)) 5 (f ('V1' 0) 3 (f ('V1' 1) 5 b))
-- -- @
-- --
-- --   at compile time. Since there are no loops and all target indexes
-- --   are known at compile time, this can lead to more optimisations and
-- --   faster execution times. This usually leads to around a 2x speed up.
-- --
-- -- @
-- -- $('makeStencilTH' (as :: [(f 'Int', a)])) :: 'Stencil' f a
-- -- @
-- mkStencilTH :: (ShapeLift f, Lift a) => [(f Int, a)] -> Q Exp
-- mkStencilTH = mkStencilTHWith lift

-- -- | 'mkStencilTH' with a custom 'lift' function for @a@.
-- mkStencilTHWith :: ShapeLift f => (a -> Q Exp) -> [(f Int, a)] -> Q Exp
-- mkStencilTHWith liftA as = do
--   let f = mkName "f"
--       b = mkName "b"
--       appF (i,a) e = do
--         iE <- liftShape i
--         aE <- liftA a
--         pure $ AppE (AppE (AppE (VarE f) iE) aE) e

--   e <- foldrM appF (VarE b) as
--   pure $ AppE (ConE 'Stencil) (LamE [VarP f,VarP b] e)

