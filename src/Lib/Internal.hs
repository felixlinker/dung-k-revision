module Lib.Internal
    ( subAFs
    ) where

import qualified Language.Dung.AF as AF
import qualified Data.Set as Set
import qualified Data.Set.Subsets as Subsets

subAFs :: (Ord a) => AF.DungAF a -> [AF.DungAF a]
subAFs (AF.AF [] _) = [AF.AF [] []]
subAFs (AF.AF args atts) =
    foldl1 (++) [ subAttAFs $ AF.AF subArgs atts | subArgs <- Subsets.subsetsDesc args (-1) $ length args ]

subAttAFs :: (Ord a) => AF.DungAF a -> [AF.DungAF a]
subAttAFs (AF.AF args atts) =
    let atts' = filter inArgs atts
    in [ AF.AF args subAtts | subAtts <- Subsets.subsetsDesc atts' (-1) $ length atts' ]
    where
        argsSet = Set.fromList args
        inArgs (a, b) = a `Set.member` argsSet && b `Set.member` argsSet
