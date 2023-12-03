{-# LANGUAGE Trustworthy #-}
-- | Boolean functions.
--
-- Term implementation use 'SBool' from @singleton-bool@ package.
--
module DeFun.Bool (
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

import DeFun.Core

-------------------------------------------------------------------------------
-- LAnd
-------------------------------------------------------------------------------

-- | Logical and. A synonym of 'Data.Type.Bool.&&'
type LAnd :: Bool -> Bool -> Bool
type LAnd x y = x && y

type LAndSym :: Bool ~> Bool ~> Bool
data LAndSym x
type instance App LAndSym x = LAndSym1 x

type LAndSym1 :: Bool -> Bool ~> Bool
data LAndSym1 x y
type instance App (LAndSym1 x) y = LAnd x y

land :: SBool x -> SBool y -> SBool (LAnd x y)
land = sboolAnd

landSym1 :: SBool x -> Lam SBool SBool (LAndSym1 x)
landSym1 x = Lam (land x)

landSym :: Lam2 SBool SBool SBool LAndSym
landSym = Lam landSym1

-------------------------------------------------------------------------------
-- LOr
-------------------------------------------------------------------------------

-- | Logical or. A synonym of 'Data.Type.Bool.||
type LOr :: Bool -> Bool -> Bool
type LOr x y = x || y

type LOrSym :: Bool ~> Bool ~> Bool
data LOrSym x
type instance App LOrSym x = LOrSym1 x

type LOrSym1 :: Bool -> Bool ~> Bool
data LOrSym1 x y
type instance App (LOrSym1 x) y = LOr x y

lor :: SBool x -> SBool y -> SBool (LOr x y)
lor = sboolOr

lorSym1 :: SBool x -> Lam SBool SBool (LOrSym1 x)
lorSym1 x = Lam (lor x)

lorSym :: Lam2 SBool SBool SBool LOrSym
lorSym = Lam lorSym1

-------------------------------------------------------------------------------
-- Not
-------------------------------------------------------------------------------

-- | Logical not.
type NotSym :: Bool ~> Bool
data NotSym x
type instance App NotSym x = Not x

not :: SBool x -> SBool (Not x)
not = sboolNot

notSym :: Lam SBool SBool NotSym
notSym = Lam not
