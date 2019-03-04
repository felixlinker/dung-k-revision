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
import Control.Applicative
import Language.Dung.Logics
import Language.Dung.Logics.Internal

kernel :: (Ord a) => AF.DungAF a -> AF.DungAF a
kernel (AF.AF args atts) = AF.AF args $ kernelAtts (AF.AF args atts)

krFree :: (Ord a) => AF.DungAF a -> Bool
krFree = krFreeWith kernel

kernelAtts :: (Ord a) => AF.DungAF a -> [(a, a)]
kernelAtts (AF.AF args atts) =
    let selfAttacking = Set.fromList $ Prelude.map fst $ Prelude.filter (uncurry (==)) atts
        kStb = not . ((&&) <$> uncurry (/=) <*> ((`Set.member` selfAttacking) . fst))
    in Prelude.filter kStb atts

(|=) :: (Ord a) => AF.DungAF a -> AF.DungAF a -> Bool
(|=) = modelsWith kernel

(+) :: (Ord a) => AF.DungAF a -> AF.DungAF a -> Maybe (AF.DungAF a)
(+) = expandWith kernel

(*) :: (Ord a) => AF.DungAF a -> AF.DungAF a -> AF.DungAF a
f * g =
    let g_k = kernel g
        expandWithCandidates = subAFs $ kernel f
        candidates = filter krFree $ map (kernel . (union g_k)) expandWithCandidates
    in head candidates
