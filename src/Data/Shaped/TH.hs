{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Shaped.TH
-- Copyright   :  (c) Christopher Chalmers
-- License     :  BSD3
--
-- Maintainer  :  Christopher Chalmers
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Contains QuasiQuotes and TemplateHaskell utilities for creating dense
-- arrays, stencils and fixed length vectors.
--
-- The parser for the QuasiQuotes is still a work in progress.
-----------------------------------------------------------------------------
module Data.Shaped.TH
  ( -- * Creating dense arrays
    dense

    -- * Fixed length vector
  , v

    -- * Stencils
  , stencil

    -- ** Stencils from lists
  , ShapeLift (..)
  , mkStencilTH
  , mkStencilTHWith

  ) where

import           Control.Applicative          hiding (many, empty)
import           Control.Lens
import           Control.Monad
import           Data.Char
import           Data.Foldable                as F
import           Data.Function                (on)
import qualified Data.List                    as List
import           Data.Maybe
import           Data.Monoid                  (Endo)
import qualified Data.Vector                  as Vector
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Language.Haskell.TH.Syntax
import           Linear
import qualified Linear.V                     as V
import           Text.ParserCombinators.ReadP
import qualified Text.Read.Lex                as Lex

import           Data.Shaped.Generic          (empty, fromListInto_)
import           Data.Shaped.Index
import           Data.Shaped.Stencil

-- | QuasiQuoter for producing a dense arrays using a custom parser.
--   Values are space separated, while also allowing infix expressions
--   (like @5/7@). If you want to apply a function, it should be done in
--   brackets. Supports 1D, 2D and 3D arrays.
--
--   The number of rows/columns must be consistent thought out the
--   array.
--
-- === __Examples__
--
--   - 1D arrays are of the following form form. Note these can be
--     used as 'V1', 'V2' or 'V3' arrays.
--
-- @
-- ['dense'| 5 -3 1 -3 5 |] :: ('R1' f, 'Vector.Vector' v a, 'Num' a) => 'Data.Shaped.Array' v f a
-- @
--
--
--   - 2D arrays are of the following form. Note these can be used as
--     'V2' or 'V3' arrays.
--
-- @
-- chars :: 'Data.Shaped.UArray' 'V2' 'Char'
-- chars :: ['dense'|
--   \'a\' \'b\' \'c\'
--   \'d\' \'e\' \'f\'
--   \'g\' \'h\' \'i\'
-- |]
-- @
--
--   - 3D arrays are of the following form. Note the order in which
--     'dense' formats the array. The array @a@ is such that @a ! 'V3'
--     x y z = "xyz"@
--
-- @
-- a :: 'Data.Shaped.BArray' 'V3' 'String'
-- a = ['dense'|
--   "000" "100" "200"
--   "010" "110" "210"
--   "020" "120" "220"
--
--   "001" "101" "201"
--   "011" "111" "211"
--   "021" "121" "221"
--
--   "002" "102" "202"
--   "012" "112" "212"
--   "022" "122" "222"
-- |]
-- @
--
dense :: QuasiQuoter
dense = QuasiQuoter
  { quoteExp  = parseDense
  , quotePat  = error "stencil can't be used in pattern"
  , quoteType = error "stencil can't be used in type"
  , quoteDec  = error "stencil can't be used in dec"
  }

-- | List of expressions forming a stencil. To be either turned into
--   either a real list or an unfolded stencil.
parseDense :: String -> Q Exp
parseDense str =
  case mapM (mapM parseLine) (map lines ps) of
    Left err      -> fail err
    Right []      -> [| empty |]
    Right [[as]]  -> uncurry mkArray $ parse1D as
    Right [ass]   -> uncurry mkArray $ parse2D ass
    Right asss    -> uncurry mkArray $ parse3D asss
  where ps = paragraphs str

-- | Split a string up into paragraphs separated by a new line. Extra
--   newlines inbetween paragraphs are stripped.
paragraphs :: String -> [String]
paragraphs = go [] . strip where
  go ps ('\n':'\n':xs) = [reverse ps] ++ go [] (strip xs)
  go ps (x:xs)         = go (x:ps) xs
  go [] []             = []
  go ps []             = [reverse $ strip ps]

  strip = dropWhile (\x -> x == '\n' || x == ' ')

-- Creating arrays -----------------------------------------------------

mkArray :: ShapeLift f => Layout f -> [Exp] -> Q Exp
mkArray l as = do
  lE <- liftShape' l
  let fromListE = AppE (VarE 'fromListInto_) lE
  pure $ AppE fromListE (ListE as)

------------------------------------------------------------------------
-- V n a
------------------------------------------------------------------------

-- | Type safe 'QuasiQuoter' for fixed length vectors 'V.V'. Values are
--   space separated. Can be used as expressions or patterns.
--
-- @
-- [v| x y z w q r |] :: 'V.V' 6 a
-- @
--
--   Note this requires @DataKinds@. Also requires @ViewPatterns@ if 'v'
--   is used as a pattern.
--
-- === __Examples__
--
-- @
-- >>> let a = [v| 1 2 3 4 5 |]
-- >>> :t a
-- a :: Num a => V 5 a
-- >>> a
-- V {toVector = [1,2,3,4,5]}
-- >>> let f [v| a b c d e |] = (a,b,c,d,e)
-- >>> :t f
-- f :: V 5 t -> (t, t, t, t, t)
-- >>> f a
-- (1,2,3,4,5)
-- @
--
-- Variables and infix expressions are also allowed. Negative values can
-- be expressed by a leading @-@ with a space before but no space
-- after.
--
-- @
-- >>> let b x = [v| 1\/x 2 \/ x (succ x)**2 x-2 x - 3 -x |]
-- >>> b Debug.SimpleReflect.a
-- V {toVector = [1 \/ a,2 \/ a,succ a**2,a - 2,a - 3,negate a]}
-- @
v :: QuasiQuoter
v = QuasiQuoter
  { quoteExp  = parseV
  , quotePat  = patternV
  , quoteType = error "stencil can't be used as type"
  , quoteDec  = error "stencil can't be used as dec"
  }

parseV :: String -> Q Exp
parseV s = case parseLine s of
  Right as ->
    let e = pure $ ListE as
        n = pure . LitT $ NumTyLit (toInteger $ length as)
    in  [| (V.V :: Vector.Vector a -> V.V $n a) (Vector.fromList $e) |]
  Left err -> fail $ "v: " ++ err

------------------------------------------------------------------------
-- Stencils
------------------------------------------------------------------------

parseStencilLine :: String -> Either String [Maybe Exp]
parseStencilLine s =
  case List.sortBy (compare `on` (length . snd)) rs of
    (xs,"") : _ -> Right xs
    (_ , x) : _ -> Left $ "parse error on input " ++ head (words x)
    _           -> Left "no parse"
  where
    rs = readP_to_S (many $ mExp <* skipSpaces) s
    mExp = fmap Just noAppExpression <|> skip
    skip = do
      Lex.Ident "_" <- Lex.lex
      pure Nothing

-- | List of expressions forming a stencil. To be either turned into
--   either a real list or an unfolded stencil.
parseStencil :: String -> Q Exp
parseStencil str =
  case mapM (mapM parseStencilLine) (map lines ps) of
    Left err      -> fail err
    Right []      -> [| mkStencil [] |]
    Right [[as]]  -> uncurry mkStencilE $ parse1D as
    Right [ass]   -> uncurry mkStencilE $ parse2D ass
    Right asss    -> uncurry mkStencilE $ parse3D asss
  where ps = paragraphs str

mkStencilE :: ShapeLift f => Layout f -> [Maybe Exp] -> Q Exp
mkStencilE l as = do
  when (any even l) $ reportWarning
    "stencil has an even size in some dimension, the centre element may be incorrect"

  let ixes = map (^-^ fmap (`div` 2) l) (toListOf shapeIndexes l)
      -- indexes zipped with expressions, discarding 'Nothing's
      xs = mapMaybe (sequenceOf _2) (zip ixes as)

  mkStencilTHWith pure xs

-- | QuasiQuoter for producing a static stencil definition. This is a
--   versatile parser for 1D, 2D and 3D stencils. The parsing is similar
--   to 'dense' but 'stencil' also supports @_@, which means ignore this
--   element. Also, stencils should have an odd length in all dimensions
--   so there is always a center element (which is used as 'zero').
--
-- === __Examples__
--
--     - 1D stencils are of the form
--
-- @
-- ['stencil'| 5 -3 1 -3 5 |] :: 'Num' a => 'Stencil' 'V1' a
-- @
--
--     - 2D stencils are of the form
--
-- @
-- myStencil2 :: 'Num' a => 'Stencil' 'V2' a
-- myStencil2 = ['stencil'|
--   0 1 0
--   1 0 1
--   0 1 0
-- |]
-- @
--
--     - 3D stencils have gaps between planes.
--
-- @
-- myStencil3 :: 'Fractional' a => 'Stencil' 'V3' a
-- myStencil3 :: ['stencil'|
--   1\/20 3\/10 1\/20
--   3\/10  1   3\/10
--   1\/20 3\/10 1\/20
--
--   3\/10  1   3\/10
--    1    _    1
--   3\/10  1   3\/10
--
--   1\/20 3\/10 1\/20
--   3\/10  1   3\/10
--   1\/20 3\/10 1\/20
-- |]
-- @
--
--  Variables can also be used
--
-- @
-- myStencil2' :: a -> a -> a -> 'Stencil' 'V2' a
-- myStencil2' a b c = ['stencil'|
--   c b c
--   b a b
--   c b c
-- |]
-- @
--
--
stencil :: QuasiQuoter
stencil = QuasiQuoter
  { quoteExp  = parseStencil
  , quotePat  = error "stencil can't be used in pattern"
  , quoteType = error "stencil can't be used in type"
  , quoteDec  = error "stencil can't be used in dec"
  }

-- | Construct a 'Stencil' by unrolling the list at compile time. For
--   example
--
-- @
-- 'ifoldr' f b $('mkStencilTH' [('V1' (-1), 5), ('V1' 0, 3), ('V1' 1, 5)])
-- @
--
--   will be get turned into
--
-- @
-- f ('V1' (-1)) 5 (f ('V1' 0) 3 (f ('V1' 1) 5 b))
-- @
--
--   at compile time. Since there are no loops and all target indexes
--   are known at compile time, this can lead to more optimisations and
--   faster execution times. This usually leads to around a 2x speed up
--   compared to folding over unboxed vectors.
--
-- @
-- $('mkStencilTH' (as :: [(f 'Int', a)])) :: 'Stencil' f a
-- @
mkStencilTH :: (ShapeLift f, Lift a) => [(f Int, a)] -> Q Exp
mkStencilTH = mkStencilTHWith lift

-- | 'mkStencilTH' with a custom 'lift' function for @a@.
mkStencilTHWith :: ShapeLift f => (a -> Q Exp) -> [(f Int, a)] -> Q Exp
mkStencilTHWith aLift as = do
  -- See Note [mkName-capturing]
  f <- newName "mkStencilTHWith_f"
  b <- newName "mkStencilTHWith_b"
  let appF (i,a) e = do
        iE <- liftShape' i
        aE <- aLift a
        pure $ AppE (AppE (AppE (VarE f) iE) aE) e

  e <- foldrM appF (VarE b) as
  pure $ AppE (ConE 'Stencil) (LamE [VarP f,VarP b] e)

{-
~~~~ Note [mkName-capturing]

Since 'newName' will capture any other names below it with the same
name. So if we simply used @newName "b"@, [stencil| a b c |] where
a=1; b=2; c=3 would convert @b@ to @b_a5y0@ (or w/e the top level b
is) and fail. To prevent this I've used a name that's unlikely
conflict.

Another solution would be to use lookupValueName on all variables.
But this would either require traversing over all 'Name's in every
'Exp' (shown below) or parse in the Q monad.

-- | Lookup and replace all names made with 'mkName' using
--   'lookupValueName'; failing if not in scope.
replaceMkName :: Exp -> Q Exp
replaceMkName = template f where
  f (Name (OccName s) NameS) =
    lookupValueName s >>= \case
      Just nm -> pure nm
      -- Sometimes a variable may not be in scope yet because it's
      -- generated in a TH splice that hasn't been run yet.
      Nothing -> fail $ "Not in scope: ‘" ++ s ++ "’"
  f nm = pure nm

-}

------------------------------------------------------------------------
-- Parsing expressions
------------------------------------------------------------------------

parseLine :: String -> Either String [Exp]
parseLine s =
  case List.sortBy (compare `on` (length . snd)) rs of
    (xs,"") : _ -> Right xs
    (_ , x) : _ -> Left $ "parse error on input " ++ head (words x)
    _           -> Left "no parse"
  where
    rs = readP_to_S (many noAppExpression <* skipSpaces) s

-- | Fail the parser if the next non-space is a @-@ directly followed by
--   a non-space.
closeNegateFail :: ReadP ()
closeNegateFail = do
  s <- look
  case s of
    ' ' : s' -> case dropWhile isSpace s' of
                  '-' : c : _ -> if isSpace c then pure () else pfail
                  _           -> pure ()
    _        -> pure ()

-- | If there is a space before but not after a @-@, it is treated as a
--   separate expression.
--
-- @
-- "1 2 -3 4"        -> [1, 2, -3, 4]
-- "1 2 - 3 4"       -> [1, -1, 4]
-- "1 2-3 4"         -> [1, -1, 4]
-- "11 -3/2  -3/2 4" -> [1, -1, 4]
-- "1 -3/2 4"        -> [1.0,-1.5,4.0]
-- @
noAppExpression :: ReadP Exp
noAppExpression = do
  aE <- anExpr True

  option aE $ do
    closeNegateFail
    i  <- infixExp
    bE <- noAppExpression
    pure $ UInfixE aE i bE

-- | Parse an express without any top level application. Infix functions
--   are still permitted.
--
--   This is only a small subset of the full haskell syntax. The
--   following syntax is supported:
--
--     - Variables/constructors: @a@, @'Just'@ etc.
--     - Numbers: @3@, @-6@, @7.8@, @1e-6@, @0x583fa@
--     - Parenthesis/tuples: @()@ @(f a)@, @(a,b)@
--     - Lists
--     - Strings
--     - Function application
--     - Infix operators: symbols (@+@, @/@ etc.) and blackticked (like @`mod`@)
--
--   More advanced haskell syntax are not yet supported:
--
--     - let bindings
--     - lambdas
--     - partial infix application (+) (1+) (+2)
--     - type signatures
--     - comments
--
--   This could be replaced by haskell-src-meta but since I want a
--   custom parser for 'noAppExpression' it doesn't seem worth the extra
--   dependencies.
expression :: ReadP Exp
expression = do
  f    <- anExpr True
  args <- many (anExpr False)
  let aE = F.foldl AppE f args

  option aE $ do
    -- if the next lex isn't a symbol, we move on to the next statement
    i  <- infixExp
    bE <- expression
    pure $ UInfixE aE i bE

-- | Parse an infix expression. Either a symbol or a name wrapped in @`@.
infixExp :: ReadP Exp
infixExp = do
  a <- Lex.lex
  case a of
    Lex.Symbol s -> pure $ symbol s
    Lex.Punc "`" -> do
      Lex.Ident x  <- Lex.lex
      Lex.Punc "`" <- Lex.lex
      ident x
    _            -> pfail

-- Lexing --------------------------------------------------------------

-- | Parse a single expression.
anExpr
  :: Bool      -- ^ Allow a leading @-@ to mean 'negate'
  -> ReadP Exp
anExpr new = do
  a <- Lex.lex
  case a of
    Lex.Char c   -> pure $ LitE (CharL c)
    Lex.String s -> pure $ LitE (StringL s)
    Lex.Punc s   -> punc s
    Lex.Ident s  -> ident s
    Lex.Symbol s -> if new then prefix s else pfail
    Lex.Number n -> pure $ LitE (number n)
    Lex.EOF      -> pfail

-- | Convert a name to an expression.
ident :: String -> ReadP Exp
ident "_"                 = pfail
ident s@(x:_) | isUpper x = pure $ ConE (mkName s)
ident s                   = pure $ VarE (mkName s)

-- | Convert a symbol to an expression.
symbol :: String -> Exp
symbol s@(':':_) = ConE (mkName s)
symbol s         = VarE (mkName s)

-- | Parse from some punctuation.
punc :: String -> ReadP Exp
punc = \case
  -- parenthesis / tuples
  "(" -> do as           <- expression `sepBy` comma
            Lex.Punc ")" <- Lex.lex
            pure $ TupE as
  -- lists
  "[" -> do as           <- expression `sepBy` comma
            Lex.Punc "]" <- Lex.lex
            pure $ ListE as
  _   -> pfail

prefix :: String -> ReadP Exp
prefix "-" = do
  e <- anExpr False
  pure $ AppE (VarE 'negate) e
prefix _   = pfail

comma :: ReadP ()
comma = do
  Lex.Punc "," <- Lex.lex
  pure ()

-- | Turn a 'Number' into a literal 'Integer' if possible, otherwise
--   make a literal `Rational`.
number :: Lex.Number -> Lit
number n =
  maybe (RationalL $ Lex.numberToRational n)
        IntegerL
        (Lex.numberToInteger n)

------------------------------------------------------------------------
-- Parsing patterns
------------------------------------------------------------------------

patternV :: String -> Q Pat
patternV s = do
  case parsePattern s of
    Left err -> fail err
    Right pats -> do
      fE <- vTuple (length pats)
      pure $ ViewP fE (TupP pats)

parsePattern :: String -> Either String [Pat]
parsePattern s =
  case List.sortBy (compare `on` (length . snd)) rs of
    (xs,"") : _ -> Right xs
    (_ , x) : _ -> Left $ "parse error on input " ++ head (words x)
    _           -> Left "no parse"
  where rs = readP_to_S (many pattern <* skipSpaces) s

pattern :: ReadP Pat
pattern = do
  a <- Lex.lex
  case a of
    Lex.Char c   -> pure $ LitP (CharL c)
    Lex.String s -> pure $ LitP (StringL s)
    Lex.Punc s   -> puncP s
    Lex.Ident n  -> pure $ identP n
    Lex.Symbol s -> prefixP s
    Lex.Number n -> pure $ LitP (number n)
    Lex.EOF      -> pfail

-- | Convert a name to an expression.
identP :: String -> Pat
identP "_"                 = WildP
identP s@(x:_) | isUpper x = ConP (mkName s) []
identP s                   = VarP (mkName s)

-- | Parse from some punctuation.
puncP :: String -> ReadP Pat
puncP = \case
  "~" -> TildeP <$> pattern

  "(" -> do as           <- pattern `sepBy` comma
            Lex.Punc ")" <- Lex.lex
            pure $ TupP as

  "[" -> do as           <- pattern `sepBy` comma
            Lex.Punc "]" <- Lex.lex
            pure $ ListP as
  _   -> pfail

prefixP :: String -> ReadP Pat
prefixP "!" = do
  c:_ <- look
  when (isSpace c) pfail
  BangP <$> pattern
prefixP "~" = TildeP <$> pattern
prefixP _   = pfail

-- | Create an expression for converting a (V n a) to an n-tuple.
vTuple :: Int -> Q Exp
vTuple n
  | n > 62 = error "max supported length is 62 for v pattern"
  | otherwise = do
      vN <- newName "v"
      let idx i = AppE (AppE (VarE 'Vector.unsafeIndex) (VarE vN)) (intE i)
      let xs = TupE $ map idx [0..n-1]
      a   <- newName "a"
      let tup = iterate (\x -> AppT x (VarT a)) (TupleT n) !! n
          typ = ForallT [PlainTV a] []
                  (AppT (AppT ArrowT (AppT (AppT (ConT ''V.V) (intT n)) (VarT a))) tup)

      [| (\(V.V $(pure $ VarP vN)) -> $(pure xs)) :: $(pure typ) |]
  where
    intE = LitE . IntegerL . toInteger
    intT = LitT . NumTyLit . toInteger

-- Parsing specific dimensions -----------------------------------------

-- | Parse a 1D list. If the system is not valid, return a string
--   with error message.
parse1D :: [a] -> (V1 Int, [a])
parse1D as = (V1 x, as) where
  x = length as

-- | Parse a 2D list of lists. If the system is not valid, returns an
--   error
parse2D :: [[a]] -> (V2 Int, [a])
parse2D as
  | Just e <- badX = error ("parse2D: " ++ errMsg e)
  | otherwise      = (V2 x y, concat $ List.transpose as)
  where
    x  = head xs
    y  = length as
    xs = map length as

    badX         = ifind (const (/= x)) xs
    errMsg (i,j) =
      "row " ++ show i ++ " has " ++ show j ++ " columns but the first"
      ++ " row has " ++ show x ++ " columns"

-- | Parse a 3D list of list of lists. If the system is not valid,
--  return a string with error message.
--
--   The element are reordered in the appropriate way for the array:
--
-- @
-- >>> parse3D [["abc","def","ghi"],["jkl","mno","pqr"],["stu","vwx","yz!"]]
-- ((V3 3 3 3), "ajsdmvgpybktenwhqzclufoxir!")
-- @
--
parse3D :: [[[a]]] -> (V3 Int, [a])
parse3D as
  | nullOf (each.each.each) as = (zero, [])
  | Just e <- badX = error $ errorCol e
  | Just e <- badY = error $ errorRow e
  | otherwise      = (V3 x y z, as')
  where
    z  = length as
    y  = length (head as)
    x  = length (head (head as))

    -- reorder and concatenate so it's the correct order for the array
    as' = concatMap concat (List.transpose $ map List.transpose $ List.transpose as)

    -- check for inconsistencies
    badY = ifind (const (/= y)) (map length as)
    badX = ifindOf' (traversed <.> traversed <. to length) (const (/= x)) as

    -- error messages for inconsistent rows/columns
    errorCol ((k,j),i) =
      "plane " ++ show k ++ ", row " ++ show j ++ " has " ++ show i ++
      " columns" ++ ", but the first row has " ++ show x ++ " columns"
    errorRow (k,j) =
      "plane " ++ show k ++ " has " ++ show j ++ " rows but the first"
      ++ " plane has " ++ show x ++ " rows"

-- | Version of ifindOf which is consistent with ifind (in that it also returns the index).
ifindOf' :: IndexedGetting i (Endo (Maybe (i, a))) s a -> (i -> a -> Bool) -> s -> Maybe (i, a)
ifindOf' l p = ifoldrOf l (\i a y -> if p i a then Just (i, a) else y) Nothing
{-# INLINE ifindOf' #-}

-- Shape lift class ----------------------------------------------------

-- | Class of shapes that can be 'lift'ed.
--
--   This is to prevent orphans for the 'Lift' class. (I may make a PR
--   for 'Lift' instances to be in `linear`.)
class Shape f => ShapeLift f where
  -- | 'lift' for 'Shape's.
  liftShape :: Lift a => f a -> Q Exp

  -- | Polymorphic 'lift' for a 'Shape's.
  liftShape' :: Lift a => f a -> Q Exp

instance ShapeLift V1 where
  liftShape (V1 x) = [| V1 x |]
  liftShape' (V1 x) = [| v1 x |]

instance ShapeLift V2 where
  liftShape (V2 x y) = [| V2 x y |]
  liftShape' (V2 x y) = [| v2 x y |]

instance ShapeLift V3 where
  liftShape (V3 x y z) = [| V3 x y z |]
  liftShape' (V3 x y z) = [| v3 x y z |]

instance ShapeLift V4 where
  liftShape (V4 x y z w) = [| V4 x y z w |]
  liftShape' (V4 x y z w) = [| v4 x y z w |]

v1 :: (R1 f, Shape f, Num a) => a -> f a
v1 x = set _x x one
{-# INLINE [0] v1 #-}

v2 :: (R2 f, Shape f, Num a) => a -> a -> f a
v2 x y = set _xy (V2 x y) one
{-# INLINE [0] v2 #-}

v3 :: (R3 f, Shape f, Num a) => a -> a -> a -> f a
v3 x y z = set _xyz (V3 x y z) one
{-# INLINE [0] v3 #-}

v4 :: (R4 f, Shape f, Num a) => a -> a -> a -> a -> f a
v4 x y z w = set _xyzw (V4 x y z w) one
{-# INLINE [0] v4 #-}

one :: (Shape f, Num a) => f a
one = 1 <$ (zero :: Additive f => f Int)

-- are these nessesary?
{-# RULES
 "v1/V1" v1 = V1;
 "v1/V2" forall a. v1 a = V2 a 1;
 "v1/V3" forall a. v1 a = V3 a 1 1;
 "v2/V2" v2 = V2;
 "v2/V3" forall a b. v2 a b = V3 a b 1;
 "v3/V3" v3 = V3;
 "v4/V4" v4 = V4
  #-}

