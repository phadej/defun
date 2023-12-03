{-# LANGUAGE Trustworthy #-}
module DeFun.List (
    -- * Append
    Append, AppendSym, AppendSym1,
    append, appendSym, appendSym1,
    -- * Map
    Map, MapSym, MapSym1,
    map, mapSym, mapSym1,
    -- * Concat
    Concat, ConcatSym,
    concat, concatSym,
    -- * ConcatMap,
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
    -- * Filter
    Filter, FilterSym, FilterSym1,
    filter, filterSym, filterSym1,
) where

import Data.SOP.NP (NP (..))
import Prelude (Bool (..))
import Data.Singletons.Bool (SBool (..))

import DeFun.Core
import DeFun.Function

-- $setup
-- >>> import Prelude (Bool (..), Char, Maybe (..))
-- >>> import Data.Singletons.Bool (SBool (..))
-- >>> import Numeric.Natural (Natural)
-- >>> import DeFun
-- >>> :set -dppr-cols9999

-------------------------------------------------------------------------------
-- Append
-------------------------------------------------------------------------------

-- | List append.
--
-- >>> :kind! Append [1, 2, 3] [4, 5, 6]
-- Append [1, 2, 3] [4, 5, 6] :: [Natural]
-- = [1, 2, 3, 4, 5, 6]
-- 
type Append :: [a] -> [a] -> [a]
type family Append xs ys where
    Append '[]       ys = ys
    Append (x ': xs) ys = x ': Append xs ys

type AppendSym :: [a] ~> [a] ~> [a]
data AppendSym xs
type instance App AppendSym xs = AppendSym1 xs

type AppendSym1 :: [a] -> [a] ~> [a]
data AppendSym1 xs ys
type instance App (AppendSym1 xs) ys = Append xs ys

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

-- | List map
--
-- >>> :kind! Map NotSym [True, False]
-- Map NotSym [True, False] :: [Bool]
-- = [False, True]
--
-- >>> :kind! Map (Con1 Just) [1, 2, 3]
-- Map (Con1 Just) [1, 2, 3] :: [Maybe Natural]
-- = [Just 1, Just 2, Just 3]
--
type Map :: (a ~> b) -> [a] -> [b]
type family Map f xs where
    Map f '[]    = '[]
    Map f (x:xs) = f @@ x : Map f xs

type MapSym :: (a ~> b) ~> [a] ~> [b]
data MapSym f
type instance App MapSym f = MapSym1 f

type MapSym1  :: (a ~> b) -> [a] ~> [b]
data MapSym1 f xs
type instance App (MapSym1 f) xs = Map f xs

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

-- | List concat
--
-- >>> :kind! Concat [ [1, 2, 3], [4, 5, 6], [7, 8, 9] ]
-- Concat [ [1, 2, 3], [4, 5, 6], [7, 8, 9] ] :: [Natural]
-- = [1, 2, 3, 4, 5, 6, 7, 8, 9]
--
type Concat :: [[a]] -> [a]
type family Concat xss where
    Concat '[] = '[]
    Concat (xs : xss) = Append xs (Concat xss)

type ConcatSym :: [[a]] ~> [a]
data ConcatSym xss
type instance App ConcatSym xss = Concat xss

concat :: NP (NP a) xss -> NP a (Concat xss)
concat Nil         = Nil
concat (xs :* xss) = append xs (concat xss)

concatSym :: Lam (NP (NP a)) (NP a) ConcatSym
concatSym = Lam concat

-------------------------------------------------------------------------------
-- ConcatMap
-------------------------------------------------------------------------------

-- | List concatMap
type ConcatMap :: (a ~> [b]) -> [a] -> [b]
type family ConcatMap f xs where
    ConcatMap f '[]    = '[]
    ConcatMap f (x:xs) = Append (f @@ x) (ConcatMap f xs)

type ConcatMapSym :: (a ~> [b]) ~> [a] ~> [b]
data ConcatMapSym f
type instance App ConcatMapSym f = ConcatMapSym1 f

type ConcatMapSym1  :: (a ~> [b]) -> [a] ~> [b]
data ConcatMapSym1 f xs
type instance App (ConcatMapSym1 f) xs = ConcatMap f xs

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

-- | List binary map. I.e. 'liftA2' for lists.
--
-- Note: this is not 'ZipWith'.
--
-- >>> :kind! Map2 (Con2 '(,)) [1, 2, 3] ['x', 'y']
-- Map2 (Con2 '(,)) [1, 2, 3] ['x', 'y'] :: [(Natural, Char)]
-- = ['(1, 'x'), '(1, 'y'), '(2, 'x'), '(2, 'y'), '(3, 'x'), '(3, 'y')]
--
-- This function is a good example to highlight how to defunctionalize
-- definitions with anonymous functions.
--
-- The simple definition can be written using @concatMap@ and @map@ from
-- "Prelude":
--
-- >>> import Prelude as P (concatMap, map, (.), flip)
-- >>> let map2 f xs ys = P.concatMap (\x -> P.map (f x) ys) xs
-- >>> map2 (,) "abc" "xy"
-- [('a','x'),('a','y'),('b','x'),('b','y'),('c','x'),('c','y')]
--
-- However, to make it easier (arguably) to defunctionalize, the @concatMap@ argument
-- lambda can be written in point-free form using combinators:
--
-- >>> let map2 f xs ys = P.concatMap (P.flip P.map ys P.. f) xs
-- >>> map2 (,) "abc" "xy"
-- [('a','x'),('a','y'),('b','x'),('b','y'),('c','x'),('c','y')]
--
-- Alternatively, we could define a new "top-level" function
--
-- >>> let map2Aux f ys x = P.map (f x) ys
--
-- and use it to define @map2:
--
-- >>> let map2 f xs ys = P.concatMap (map2Aux f ys) xs
-- >>> map2 (,) "abc" "xy"
-- [('a','x'),('a','y'),('b','x'),('b','y'),('c','x'),('c','y')]
--
type Map2 :: (a ~> b ~> c) -> [a] -> [b] -> [c]
type family Map2 f xs ys where
    Map2 f xs ys = ConcatMap (CompSym2 (FlipSym2 MapSym ys) f) xs

type Map2Sym :: (a ~> b ~> c) ~> [a] ~> [b] ~> [c]
data Map2Sym f
type instance App Map2Sym f = Map2Sym1 f

type Map2Sym1 :: (a ~> b ~> c) -> [a] ~> [b] ~> [c]
data Map2Sym1 f xs
type instance App (Map2Sym1 f) xs = Map2Sym2 f xs

type Map2Sym2 :: (a ~> b ~> c) -> [a] -> [b] ~> [c]
data Map2Sym2 f xs ys
type instance App (Map2Sym2 f xs) ys = Map2 f xs ys

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

-- | List sequence
--
-- >>> :kind! Sequence [[1,2,3],[4,5,6]]
-- Sequence [[1,2,3],[4,5,6]] :: [[Natural]]
-- = [[1, 4], [1, 5], [1, 6], [2, 4], [2, 5], [2, 6], [3, 4], [3, 5], [3, 6]]
--
type Sequence :: [[a]] -> [[a]]
type family Sequence xss where
    Sequence '[]         = '[ '[] ]
    Sequence (xs ': xss) = Map2 (Con2 '(:)) xs (Sequence xss)

type SequenceSym :: [[a]] ~> [[a]]
data SequenceSym xss
type instance App SequenceSym xss = Sequence xss

sequence :: NP (NP a) xss -> NP (NP a) (Sequence xss)
sequence Nil         = Nil :* Nil
sequence (xs :* xss) = map2 (con2 (:*)) xs (sequence xss)

sequenceSym :: Lam (NP (NP a)) (NP (NP a)) SequenceSym
sequenceSym = Lam sequence

-------------------------------------------------------------------------------
-- Foldr
-------------------------------------------------------------------------------

-- | List right fold
type Foldr :: (a ~> b ~> b) -> b -> [a] -> b
type family Foldr f z xs where
    Foldr f z '[]      = z
    Foldr f z (x : xs) = f @@ x @@ (Foldr f z xs)

type FoldrSym :: (a ~> b ~> b) ~> b ~> [a] ~> b
data FoldrSym f
type instance App FoldrSym f = FoldrSym1 f

type FoldrSym1 :: (a ~> b ~> b) -> b ~> [a] ~> b
data FoldrSym1 f z
type instance App (FoldrSym1 f) z = FoldrSym2 f z

type FoldrSym2 :: (a ~> b ~> b) -> b -> [a] ~> b
data FoldrSym2 f z xs
type instance App (FoldrSym2 f z) xs = Foldr f z xs

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

-- | Zip with
--
-- >>> :kind! ZipWith (Con2 '(,)) [1, 2, 3] ['x', 'y']
-- ZipWith (Con2 '(,)) [1, 2, 3] ['x', 'y'] :: [(Natural, Char)]
-- = ['(1, 'x'), '(2, 'y')]
--
type ZipWith :: (a ~> b ~> c) -> [a] -> [b] -> [c]
type family ZipWith f xs ys where
    ZipWith f '[] ys = '[]
    ZipWith f (x : xs) '[] = '[]
    ZipWith f (x : xs) (y : ys) = f @@ x @@ y : ZipWith f xs ys

type ZipWithSym :: (a ~> b ~> c) ~> [a] ~> [b] ~> [c]
data ZipWithSym f
type instance App ZipWithSym f = ZipWithSym1 f

type ZipWithSym1 :: (a ~> b ~> c) -> [a] ~> [b] ~> [c]
data ZipWithSym1 f xs
type instance App (ZipWithSym1 f) xs = ZipWithSym2 f xs

type ZipWithSym2 :: (a ~> b ~> c) -> [a] -> [b] ~> [c]
data ZipWithSym2 f xs ys
type instance App (ZipWithSym2 f xs) ys = ZipWith f xs ys

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

-------------------------------------------------------------------------------
-- Filter
-------------------------------------------------------------------------------

type Filter :: (a ~> Bool) -> [a] -> [a]
type family Filter p xs where
    Filter f '[] = '[]
    Filter f (x ': xs) = FilterAux (f @@  x) x f xs

type FilterAux :: Bool -> a -> (a ~> Bool) -> [a] -> [a]
type family FilterAux b x p xs where
    FilterAux 'True  x p xs = x ': Filter p xs
    FilterAux 'False x p xs =      Filter p xs

type FilterSym :: (a ~> Bool) ~> [a] ~> [a]
data FilterSym p
type instance App FilterSym p = FilterSym1 p

type FilterSym1  :: (a ~> Bool) -> [a] ~> [a]
data FilterSym1 p xs
type instance App (FilterSym1 p) xs = Filter p xs

filter :: Lam a SBool p -> NP a xs -> NP a (Filter p xs)
filter _ Nil       = Nil
filter p (x :* xs) = case p @@ x of
    STrue   -> x :* filter p xs
    SFalse  ->      filter p xs

filterSym :: Lam2 (a :~> SBool) (NP a) (NP a) FilterSym
filterSym = Lam filterSym1

filterSym1 :: Lam a SBool p -> Lam (NP a) (NP a) (FilterSym1 p)
filterSym1 p = Lam (filter p)
