{-# LANGUAGE Trustworthy #-}
-- |
--
-- This module is designed to imported qualified:
--
-- @
-- import qualified Data.SOP.NP.DeFun as NP
-- @
--
module Data.SOP.NP.DeFun (
    -- * Append
    Append, AppendSym, AppendSym1,
    append, appendSym, appendSym1,
    -- * Map
    Map, MapSym, MapSym1,
    map, mapSym, mapSym1,
    -- * Concat
    Concat, ConcatSym,
    concat, concatSym,
    -- * ConcatMap
    ConcatMap, ConcatMapSym, ConcatMapSym1,
    concatMap, concatMapSym, concatMapSym1,
    -- * Map2
    Map2, Map2Sym, Map2Sym1, Map2Sym2,
    map2, map2Sym, map2Sym1, map2Sym2,
    -- * Sequence
    Sequence, SequenceSym,
    sequence, sequenceSym,
    -- * Foldr
    Foldr, FoldrSym, FoldrSym1, FoldrSym2,
    foldr, foldrSym, foldrSym1, foldrSym2,
    -- * ZipWith
    ZipWith, ZipWithSym, ZipWithSym1, ZipWithSym2,
    zipWith, zipWithSym, zipWithSym1, zipWithSym2,
) where

import DeFun.List

import Data.SOP.NP (NP (..))

import DeFun.Core
import DeFun.Function

-- $setup
-- >>> import Prelude (Char, Maybe (..))
-- >>> import Numeric.Natural (Natural)
-- >>> import DeFun.Core
-- >>> :set -dppr-cols9999

-------------------------------------------------------------------------------
-- Append
-------------------------------------------------------------------------------

append :: NP a xs -> NP a ys -> NP a (Append xs ys)
append Nil       ys = ys
append (x :* xs) ys = x :* append xs ys

appendSym :: Lam2 (NP a) (NP a) (NP a) AppendSym
appendSym = Lam appendSym1

appendSym1 :: NP a xs -> Lam (NP a) (NP a) (AppendSym1 xs)
appendSym1 xs = Lam (append xs)

-------------------------------------------------------------------------------
-- Map
-------------------------------------------------------------------------------

map :: Lam a b f -> NP a xs -> NP b (Map f xs)
map _ Nil       = Nil
map f (x :* xs) = f @@ x :* map f xs

mapSym :: Lam (a :~> b) (Lam (NP a) (NP b)) MapSym
mapSym = Lam mapSym1

mapSym1 :: Lam a b f -> Lam (NP a) (NP b) (MapSym1 f)
mapSym1 f = Lam (map f) 

-------------------------------------------------------------------------------
-- Concat
-------------------------------------------------------------------------------

concat :: NP (NP a) xss -> NP a (Concat xss)
concat Nil         = Nil
concat (xs :* xss) = append xs (concat xss)

concatSym :: Lam (NP (NP a)) (NP a) ConcatSym
concatSym = Lam concat

-------------------------------------------------------------------------------
-- ConcatMap
-------------------------------------------------------------------------------

concatMap :: Lam a (NP b) f -> NP a xs -> NP b (ConcatMap f xs)
concatMap _ Nil       = Nil
concatMap f (x :* xs) = append (f @@ x) (concatMap f xs) 

concatMapSym :: Lam2 (a :~> NP b) (NP a) (NP b) ConcatMapSym
concatMapSym = Lam concatMapSym1

concatMapSym1 :: Lam a (NP b) f -> Lam (NP a) (NP b) (ConcatMapSym1 f)
concatMapSym1 f = Lam (concatMap f)

-------------------------------------------------------------------------------
-- Map2
-------------------------------------------------------------------------------

map2 :: Lam2 a b c f -> NP a xs -> NP b ys -> NP c (Map2 f xs ys)
map2 f xs ys = concatMap (compSym2 (flipSym2 mapSym ys) f) xs

map2Sym :: Lam3 (a :~> b :~> c) (NP a) (NP b) (NP c) Map2Sym
map2Sym = Lam map2Sym1

map2Sym1 :: Lam2 a b c f -> Lam2 (NP a) (NP b) (NP c) (Map2Sym1 f)
map2Sym1 f = Lam (map2Sym2 f)

map2Sym2 :: Lam2 a b c f -> NP a xs -> Lam (NP b) (NP c) (Map2Sym2 f xs)
map2Sym2 f xs = Lam (map2 f xs)

-------------------------------------------------------------------------------
-- Sequence
-------------------------------------------------------------------------------

sequence :: NP (NP a) xss -> NP (NP a) (Sequence xss)
sequence Nil         = Nil :* Nil
sequence (xs :* xss) = map2 (con2 (:*)) xs (sequence xss)

sequenceSym :: Lam (NP (NP a)) (NP (NP a)) SequenceSym
sequenceSym = Lam sequence

-------------------------------------------------------------------------------
-- Foldr
-------------------------------------------------------------------------------

foldr :: Lam2 a b b f -> b x -> NP a ys -> b (Foldr f x ys)
foldr _ z Nil       = z 
foldr f z (x :* xs) = f @@ x @@ (foldr f z xs)

foldrSym :: Lam3 (a :~> b :~> b) b (NP a) b FoldrSym
foldrSym = Lam foldrSym1

foldrSym1 :: Lam2 a b b f -> Lam2 b (NP a) b (FoldrSym1 f)
foldrSym1 f = Lam (foldrSym2 f)

foldrSym2 :: Lam2 a b b f -> b x -> Lam (NP a) b (FoldrSym2 f x)
foldrSym2 f z = Lam (foldr f z)

-------------------------------------------------------------------------------
-- ZipWith
-------------------------------------------------------------------------------

zipWith :: Lam2 a b c f -> NP a xs -> NP b ys -> NP c (ZipWith f xs ys)
zipWith _ Nil       _         = Nil
zipWith _ (_ :* _)  Nil       = Nil
zipWith f (x :* xs) (y :* ys) = f @@ x @@ y :* zipWith f xs ys 

zipWithSym :: Lam3 (a :~> b :~> c) (NP a) (NP b) (NP c) ZipWithSym
zipWithSym = Lam zipWithSym1

zipWithSym1 :: Lam2 a b c f -> Lam2 (NP a) (NP b) (NP c) (ZipWithSym1 f)
zipWithSym1 f = Lam (zipWithSym2 f)

zipWithSym2 :: Lam2 a b c f -> NP a xs -> Lam (NP b) (NP c) (ZipWithSym2 f xs)
zipWithSym2 f xs = Lam (zipWith f xs)
