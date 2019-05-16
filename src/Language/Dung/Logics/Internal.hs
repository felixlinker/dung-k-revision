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
    in [ AF.AF args' atts'  | args' <- subArgs
                            , atts' <- subAtts
                            , attsInArgs args' atts' ]

attsInArgs :: (Ord a) => [a] -> [(a, a)] -> Bool
attsInArgs args =
    let argsSet = Set.fromList args
        mem = (`Set.member` argsSet)
    in foldl (\red (a1, a2) -> red && mem a1 && mem a2) True
