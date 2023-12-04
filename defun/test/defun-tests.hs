module Main where

import Data.SOP (NP (..), NS (..))
import GHC.Generics ((:*:) (..))
import Prelude (IO, putStrLn, Either (..))

import qualified Prelude as P

import DeFun

main :: IO ()
main = putStrLn "OK"

-------------------------------------------------------------------------------
-- mapAppend
-------------------------------------------------------------------------------

mapAppend :: NP a xs -> NP (NP a) xss -> NP (NP a) (Map (AppendSym1 xs) xss)
mapAppend xs yss = map (appendSym @@ xs) yss

-------------------------------------------------------------------------------
-- split_NP
-------------------------------------------------------------------------------

-- | inverse of 'append'
split_NP :: NP pa xs -> NP a (Append xs ys) -> (NP a xs, NP a ys)
split_NP Nil       xys        = (Nil, xys)
split_NP (_ :* ps) (x :* xys) = let (xs, ys) = split_NP ps xys in (x :* xs, ys)

-------------------------------------------------------------------------------
-- FLATTEN utils
-------------------------------------------------------------------------------

map_NS :: Lam a b f -> NS a xs -> NS b (Map f xs)
map_NS f (Z x)  = Z (f @@ x)
map_NS f (S xs) = S (map_NS f xs)

map_NS' :: Lam (px :*: a) b f -> NP px xs -> NS a xs -> NS b (Map f xs)
map_NS' _ Nil         x      = case x of {}
map_NS' f (px :* _)   (Z x)  = Z (f @@ (px :*: x))
map_NS' f (_  :* pxs) (S xs) = S (map_NS' f pxs xs)

append_NS :: forall xs ys f sing. NP sing xs -> Either (NS f xs) (NS f ys) -> NS f (Append xs ys)
append_NS _ (Left xs) = goLeft xs where
    goLeft :: NS f xs' -> NS f (Append xs' ys)
    goLeft (Z x) = Z x
    goLeft (S x) = S (goLeft x)
append_NS xs (Right ys0) = goRight xs ys0 where
    goRight :: NP sing xs' -> NS f ys -> NS f (Append xs' ys)
    goRight Nil        ys = ys
    goRight (_ :* xs') ys = S (goRight xs' ys)

concatMap_NS :: Lam pa (NP pb) f -> NP pa xs -> Lam a (NS b) f -> NS a xs -> NS b (ConcatMap f xs)
concatMap_NS pf pxs f = concatMap_NS' pf pxs (Lam (\(_ :*: x) -> f @@ x))

concatMap_NS' :: Lam pa (NP pb) f -> NP pa xs -> Lam (pa :*: a) (NS b) f -> NS a xs -> NS b (ConcatMap f xs)
concatMap_NS' _  Nil         = \_ xs -> case xs of {}
concatMap_NS' pf (px :* pxs) = concatMap_NS_aux' pf px pxs

concatMap_NS_aux' :: forall a b f x xs pa pb. Lam pa (NP pb) f -> pa x -> NP pa xs -> Lam (pa :*: a) (NS b) f -> NS a (x : xs) -> NS b (ConcatMap f (x : xs))
concatMap_NS_aux' pf px _pxs f (Z x) = append_NS
    @_
    @(ConcatMap f xs)
    (pf @@ px)
    (Left (f @@ (px :*: x)))
concatMap_NS_aux' pf px pxs f (S xs) = append_NS
    @_
    @(ConcatMap f xs)
    (pf @@ px)
    (Right (concatMap_NS' pf pxs f xs))

map2_NS :: forall a b c f xs ys pa pb pc.
    Lam2 pa pb pc f -> NP pa xs -> NP pb ys ->
    Lam2 a b c f -> NS a xs -> NS b ys -> NS c (Map2 f xs ys)
map2_NS pf pxs pys f xs ys = concatMap_NS
    (compSym2 (flipSym2 mapSym pys) pf)
    pxs
    (compSym2 (flipSym2 (mkLam2 map_NS) ys) f)
    xs

sequence_NSNP :: NP (NP sing) xss -> NP (NS f) xss -> NS (NP f) (Sequence xss)
sequence_NSNP Nil         Nil         = Z Nil
sequence_NSNP (xs :* xss) (ys :* yss) = map2_NS
    (con2 (:*))
    xs
    (sequence xss)
    (con2 (:*))
    ys
    (sequence_NSNP xss yss)

sequence_NSNP_sym :: Lam (NP (NP sing) :*: NP (NS f)) (NS (NP f)) SequenceSym
sequence_NSNP_sym = Lam (\(pxss :*: xss) -> sequence_NSNP pxss xss)

-------------------------------------------------------------------------------
-- FLATTEN
-------------------------------------------------------------------------------

-- given as sum-of-products of sum-of-products,
-- we can turn it into big sum-of-products.

flattenList :: [[[[k]]]] -> [[k]]
flattenList = P.concatMap (P.map P.concat P.. P.sequence)

type FLATTEN xsss = ConcatMap (CompSym2 (MapSym1 ConcatSym) SequenceSym) xsss

-- This is an isomorphism.
flatten_NSNP :: forall xssss f sing. NP (NP (NP (NP sing))) xssss -> NS (NP (NS (NP f))) xssss -> NS (NP f) (FLATTEN xssss)
flatten_NSNP pxssss xssss = concatMap_NS'
    (compSym2 (mapSym1 concatSym) sequenceSym)
    pxssss
    (compSym2 (Lam (map_NS concatSym)) sequence_NSNP_sym)
    xssss
