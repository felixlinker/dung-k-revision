-- | Module: Language.Dung.Logics.Stb
-- Copyright: 2019 Felix Linker
-- License: MIT
-- Maintainer: linkerfelix@gmail.com
--
-- This module implements the stable kernel for argumentation frameworks and
-- AGM-style stable-revision for Dung-Logics.
module Language.Dung.Logics.Stb
    ( kernel
    , krFree
    , (|=)
    , (Language.Dung.Logics.Stb.+)
    , (Language.Dung.Logics.Stb.*)
    ) where

import qualified Language.Dung.AF as AF
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.PartialOrd as PO
import Control.Applicative
import Language.Dung.Logics
import Language.Dung.Logics.Internal

-- | Stable kernel. Maps the AF to a redundancy-free representative of its
-- stable-strong-equivlance-class.
kernel :: (Ord a) => Kernel a
kernel (AF.AF args atts) = AF.AF args $ kernelAtts (AF.AF args atts)

-- | Is the AF free of stable-redundancy?
krFree :: (Ord a) => AF.DungAF a -> Bool
krFree = krFreeWith kernel

-- | Maps an AF to all its non-redundant attacks.
kernelAtts :: (Ord a) => AF.DungAF a -> [(a, a)]
kernelAtts (AF.AF args atts) =
    let selfAttacking = Set.fromList $ Prelude.map fst $ Prelude.filter (uncurry (==)) atts
        kStb = not . ((&&) <$> uncurry (/=) <*> ((`Set.member` selfAttacking) . fst))
    in Prelude.filter kStb atts

-- | Stable models relation. Returns whether the first AF stb-models the second.
(|=) :: (Ord a) => AF.DungAF a -> AF.DungAF a -> Bool
(|=) = modelsWith kernel

-- | Stable expansion. Returns first AF stb-expanded with the second.
(+) :: (Ord a) => AF.DungAF a -> AF.DungAF a -> Maybe (AF.DungAF a)
(+) = expandWith kernel

-- | Stable revision. Returns first AF stb-revised with the second.
(*) :: (Ord a) => AF.DungAF a -> AF.DungAF a -> AF.DungAF a
f * g =
    let g_k = kernel g
        expandWithCandidates = subAFs $ kernel f
        candidates = filter krFree $ map (kernel . (union g_k)) expandWithCandidates
    in head candidates
