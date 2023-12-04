{-# LANGUAGE Trustworthy #-}
module DeFun (
    -- * Introduction
    -- $intro
    --

    -- * Implementation
    module DeFun.Core,
    module DeFun.Function,
    module DeFun.Bool,
    module DeFun.List,
    module Data.SOP.NP.DeFun,
    module SBool.DeFun,
) where

import Data.SOP.NP.DeFun
import DeFun.Bool
import DeFun.Core
import DeFun.Function
import DeFun.List
import SBool.DeFun

-- $intro
--
-- This package provides defunctionalization helpers to write
-- type-level computations.
--
-- As [UnsaturatedTypeFamilies](https://github.com/ghc-proposals/ghc-proposals/pull/242) are not yet implemented in the GHC,
-- we cannot define type-level type families.
-- Or we can, but we could only apply them to type-constructors:
--
-- @
-- type BadMap :: (a -> b) -> [a] -> [b]
-- type family BadMap f xs where
--     BadMap f '[]      = '[]
--     BadMap f (x : xs) = f x : BadMap f xs
-- @
--
-- can be only applied to type or data-constructors,
-- i.e. @BadMap Just [1, 2, 3]@ would work, but @BadMap 'Not' [True, False]@
-- will not.
--
-- The trick is to defunctionalize higher-order functions. Instead of taking
-- a function argument, we'll take a defunctionalization /symbol/,
-- and use 'App' type family to apply it:
--
-- @
-- type Map :: (a ~> b) -> [a] -> [b]
-- type family Map f xs where
--     Map f '[]    = '[]
--     Map f (x:xs) = f @@ x : Map f xs
-- @
--
-- where '@@' is the application operator.
--
-- Now we can call @'Map' 'NotSym' [True, False]@ and it will compute.
-- The @'Map' Just [1, 2, 3]@ wont work, we need to convert a constuctor
-- into a symbol @'Map' (Con1 Just) [1, 2, 3]@ works.
--
-- Then, the @Map@ itself can be defunctionalized.
-- We need to define two symbols, as @Map@ takes two arguments:
--
-- @
-- type MapSym :: (a ~> b) ~> [a] ~> [b]
-- data MapSym f
-- type instance App MapSym f = MapSym1 f
--
-- type MapSym1  :: (a ~> b) -> [a] ~> [b]
-- data MapSym1 f xs
-- type instance App (MapSym1 f) xs = Map f xs
-- @
--
-- So far the above is general enough, and is defined in @singletons@
-- (sans using different names) in a similar way.
--
-- Another thing which we also need, is to represent the type-level computation at type level as well.
-- The @singletons@ package uses @Sing@ type-family, that's the whole package of that package.
--
-- However, @Sing@ is quite resticting. For example, one natural "term-level" reflection of lists
-- is 'Data.SOP.NP.NP' type.
--
-- For example given 'Append' type family, we can write a very useful 'append' function:
--
-- @
-- append :: NP a xs -> NP a ys -> NP a (Append xs ys)
-- append Nil       ys = ys
-- append (x :* xs) ys = x :* append xs ys
-- @
--
-- @singletons@ machinery doesn't help us define these.
--
-- Then 'Append' (and 'append') can be used as an argument (to e.g. 'Map') and 'map':
-- we can write something like
--
-- @
-- mapAppend xs yss = map (appendSym @@ xs) yss
-- @
--
-- and GHC will infer the type
--
-- @
-- mapAppend :: NP a xs -> NP (NP a) xss -> NP (NP a) (Map (AppendSym1 xs) xss)
-- @
--
-- to that function.
--
-- Being able to relatively easily write functions like 'mapAppend'
-- is the main motivation for creation of @defun@ package.
--
-- Another example is append to n-ary sums ('NS'):
--
-- @
-- append_NS :: forall xs ys f sing. NP sing xs -> Either (NS f xs) (NS f ys) -> NS f (Append xs ys)
-- append_NS _ (Left xs) = goLeft xs where
--     goLeft :: NS f xs' -> NS f (Append xs' ys)
--     goLeft (Z x) = Z x
--     goLeft (S x) = S (goLeft x)
-- append_NS xs (Right ys0) = goRight xs ys0 where
--     goRight :: NP sing xs' -> NS f ys -> NS f (Append xs' ys)
--     goRight Nil       ys = ys
--     goRight (_ :* xs) ys = S (goRight xs ys)
-- @
--
-- where we use @NP sing@ as an explicit singleton for @xs@,
-- instead of using @SingI@ from @singletons@ or @All@ from @sop-core@.
--
-- In my experience, using explicit "singletons" (especially in a libraries' internal plumbing)
-- is a lot more convenient than trying to create all required @All@ dictionaries
-- (then you would need to write everything twice, 'Type' and 'Constraint' versions: i.e. for 'NP' and 'All', or convert back and forth between @NP (Dict c)@ and @All c@).
--
