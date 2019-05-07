-- | Module: Language.Dung.Logics.Internal
-- Copyright: 2019 Felix Linker
-- License: MIT
-- Maintainer: linkerfelix@gmail.com
--
-- Helper module to calculate sub-AFs.
module Language.Dung.Logics.Internal
    ( subAFs
    ) where

import Control.Applicative
import qualified Language.Dung.AF as AF
import qualified Data.Set as Set
import qualified Data.Set.Subsets as Subsets

-- | Maps an AF to all its sub-AFs in subset-descending order, i.e. for a return
-- value of @ x:xs @ it recursively holds that @ x @ is not truly greater than
-- any AF in @ xs @.
subAFs :: (Ord a) => AF.DungAF a -> [AF.DungAF a]
subAFs (AF.AF args atts) =
    let subArgs = Subsets.subsetsDesc args (-1) $ length args
        subAtts = Subsets.subsetsDesc atts (-1) $ length atts
    in subAFs' (Set.fromList atts) subArgs subAtts

subAFs' :: (Ord a) => Set.Set (a, a) -> [[a]] -> [[(a, a)]] -> [AF.DungAF a]
subAFs' _ (_:[]) _ = [AF.AF [] []]
subAFs' baseAtts (_:args:subArgs) [] =
    let atts = filterAttsInArgs (Set.fromList args) baseAtts
        subAtts = Subsets.subsetsDesc (Set.toList atts) (-1) (Set.size atts)
    in subAFs' baseAtts (args:subArgs) subAtts
subAFs' baseAtts (args:subArgs) (atts:subAtts) =
    AF.AF args atts : subAFs' baseAtts (args:subArgs) subAtts

filterAttsInArgs :: (Ord a) => Set.Set a -> Set.Set (a, a) -> Set.Set (a, a)
filterAttsInArgs args = let mem = (`Set.member` args)
    in Set.filter ((||) <$> mem . fst <*> mem . snd)
