module Lib.Stb
    ( kernel
    , krFree
    , expansion
    , revision
    ) where

import qualified Language.Dung.AF as AF
import qualified Data.Set as Set
import qualified Data.List as List
import Control.Applicative
import Lib.DungLogics
import Lib.Internal

kernel :: (Ord a) => AF.DungAF a -> AF.DungAF a
kernel (AF.AF args atts) = AF.AF args $ Lib.Stb.kernelAtts (AF.AF args atts)

krFree :: (Ord a) => AF.DungAF a -> Bool
krFree = krFreeWith kernel

kernelAtts :: (Ord a) => AF.DungAF a -> [(a, a)]
kernelAtts (AF.AF args atts) =
    let selfAttacking = Set.fromList $ Prelude.map fst $ Prelude.filter (uncurry (==)) atts
        kStb = not . and . ([uncurry (/=), (`Set.member` selfAttacking) . fst] <*>) . (:[])
    in Prelude.filter kStb atts

expansion :: (Ord a) => AF.DungAF a -> AF.DungAF a -> Maybe (AF.DungAF a)
expansion = expandWith kernel

revision :: (Ord a) => AF.DungAF a -> AF.DungAF a -> AF.DungAF a
revision f g =
    let g_k = kernel g
        expandWithCandidates = subAFs $ kernel f
        candidates = filter krFree $ map (kernel . (union g_k)) expandWithCandidates
    in head candidates
