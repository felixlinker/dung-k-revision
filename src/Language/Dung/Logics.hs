module Language.Dung.Logics
    ( krFreeWith
    , modelsWith
    , union
    , expandWith
    ) where

import qualified Language.Dung.AF as AF
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.PartialOrd as PartialOrd

type Kernel a = AF.DungAF a -> AF.DungAF a

instance Ord a => PartialOrd.PartialOrd (AF.DungAF a) where
    (AF.AF args atts) <= (AF.AF args' atts') =
        (Set.fromList args PartialOrd.<= Set.fromList args') &&
        (Set.fromList atts PartialOrd.<= Set.fromList atts')

krFreeWith :: (Ord a) => Kernel a -> AF.DungAF a -> Bool
krFreeWith k (AF.AF args atts) =
    let (AF.AF _ kAtts) = k $ AF.AF args atts
    in (Set.fromList atts) == (Set.fromList kAtts)

modelsWith :: (Ord a) => Kernel a -> AF.DungAF a -> AF.DungAF a -> Bool
modelsWith k f g = (k g) PartialOrd.<= (k f)

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
