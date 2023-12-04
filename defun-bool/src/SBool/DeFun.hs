{-# LANGUAGE Trustworthy #-}
-- | Boolean functions.
--
-- Type families are defined in "Data.Type.Bool" module in @base@ package.
-- Term implementation use 'SBool' from @singleton-bool@ package.
--
module SBool.DeFun (
    -- * Logical and
    LAnd, LAndSym, LAndSym1,
    land, landSym, landSym1,
    -- * Logical or
    LOr, LOrSym, LOrSym1,
    lor, lorSym, lorSym1,
    -- * Logical not
    Not, NotSym,
    not, notSym,
) where

import Data.Singletons.Bool (SBool (..), sboolAnd, sboolNot, sboolOr)
import Data.Type.Bool       (Not, type (&&), type (||))
import Prelude              (Bool)

import DeFun.Bool
import DeFun.Core

-------------------------------------------------------------------------------
-- LAnd
-------------------------------------------------------------------------------

land :: SBool x -> SBool y -> SBool (LAnd x y)
land = sboolAnd

landSym1 :: SBool x -> Lam SBool SBool (LAndSym1 x)
landSym1 x = Lam (land x)

landSym :: Lam2 SBool SBool SBool LAndSym
landSym = Lam landSym1

-------------------------------------------------------------------------------
-- LOr
-------------------------------------------------------------------------------

lor :: SBool x -> SBool y -> SBool (LOr x y)
lor = sboolOr

lorSym1 :: SBool x -> Lam SBool SBool (LOrSym1 x)
lorSym1 x = Lam (lor x)

lorSym :: Lam2 SBool SBool SBool LOrSym
lorSym = Lam lorSym1

-------------------------------------------------------------------------------
-- Not
-------------------------------------------------------------------------------

not :: SBool x -> SBool (Not x)
not = sboolNot

notSym :: Lam SBool SBool NotSym
notSym = Lam not
