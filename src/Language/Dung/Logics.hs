-- | Module: Language.Dung.Logics
-- Copyright: 2019 Felix Linker
-- License: MIT
-- Maintainer: linkerfelix@gmail.com
--
-- This module implements defines basic types and functions to implement
-- Dung-Logics for argumentation frameworks alongside with AGM-style expansion
-- and revision on them for a given semantics.
-- Details of this field can be found in this paper: <https://www.researchgate.net/publication/323704530_AGM_meets_Abstract_Argumentation_Expansion_and_Revision_for_Dung_Frameworks>
--
-- An example implementation for stable-semantics can be found in
-- "Language.Dung.Logics.Stb".
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

-- | A kernel function. A kernel function should map AFs to
-- strong-equivalence-class-representatives, i.e. if AFs F and G are strongly
-- equivalent, their image of a kernel should be the same.
type Kernel a = AF.DungAF a -> AF.DungAF a

-- | Implements the sub-graph relation for AFs.
instance Ord a => PartialOrd.PartialOrd (AF.DungAF a) where
    (AF.AF args atts) <= (AF.AF args' atts') =
        (Set.fromList args PartialOrd.<= Set.fromList args') &&
        (Set.fromList atts PartialOrd.<= Set.fromList atts')

-- | Checks whether for a given kernel an AF is free of redundancy.
krFreeWith :: (Ord a) => Kernel a -> AF.DungAF a -> Bool
krFreeWith k (AF.AF args atts) =
    let (AF.AF _ kAtts) = k $ AF.AF args atts
    in (Set.fromList atts) == (Set.fromList kAtts)

-- | Checks whether for a given kernel whether the first AF models the second
-- in Dung-Logics.
modelsWith :: (Ord a) => Kernel a -> AF.DungAF a -> AF.DungAF a -> Bool
modelsWith k f g = (k g) PartialOrd.<= (k f)

-- | Implements graph-union on AFs.
union :: (Ord a) => AF.DungAF a -> AF.DungAF a -> AF.DungAF a
union (AF.AF [] _) x = x
union x (AF.AF [] _) = x
union (AF.AF args1 atts1) (AF.AF args2 atts2) =
    AF.AF (List.union args1 args2) (List.union atts1 atts2)

-- | Dung-Logic expansion for a given kernel.
expandWith :: (Ord a) => Kernel a -> AF.DungAF a -> AF.DungAF a -> Maybe (AF.DungAF a)
expandWith k (AF.AF [] _) x = Just $ k x
expandWith k x (AF.AF [] _) = Just $ k x
expandWith k x y =
    let combined = union (k x) (k y)
    in if krFreeWith k combined then Just combined else Nothing
