module Lib.DungLogics.Internal
    ( subAFs
    ) where

import Control.Applicative
import qualified Language.Dung.AF as AF
import qualified Data.Set as Set
import qualified Data.Set.Subsets as Subsets

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
