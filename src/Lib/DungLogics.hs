module Lib.DungLogics
    ( krFreeWith
    , union
    , expandWith
    ) where

import qualified Language.Dung.AF as AF
import qualified Data.Set as Set
import qualified Data.List as List

type Kernel a = AF.DungAF a -> AF.DungAF a

krFreeWith :: (Ord a) => Kernel a -> AF.DungAF a -> Bool
krFreeWith k (AF.AF args atts) =
    let (AF.AF _ kAtts) = k $ AF.AF args atts
    in (Set.fromList atts) == (Set.fromList kAtts)

union :: (Ord a) => AF.DungAF a -> AF.DungAF a -> AF.DungAF a
union (AF.AF [] _) x = x
union x (AF.AF [] _) = x
union (AF.AF args1 atts1) (AF.AF args2 atts2) =
    AF.AF (List.union args1 args2) (List.union atts1 atts2)

expandWith :: (Ord a) => Kernel a -> AF.DungAF a -> AF.DungAF a -> Maybe (AF.DungAF a)
expandWith k (AF.AF [] _) x = Just $ k x
expandWith k x (AF.AF [] _) = Just $ k x
expandWith k x y =
    let combined = union (k x) (k y)
    in if krFreeWith k combined then Just combined else Nothing
