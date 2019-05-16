module Util
    ( Context
    , CmdContext
    , CmdRet
    , Cmd
    , insertMaybe
    , maybeLast
    , snub
    , rpad
    , concatNewLn
    ) where

import Data.Tuple
import Data.Maybe
import Data.List
import Control.Monad
import qualified Language.Dung.AF as AF
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

type Context a = Map.Map String (AF.DungAF a)
type CmdContext = Context Int
type CmdRet = (CmdContext, String, Bool)
type Cmd = CmdContext -> String -> CmdRet

-- | Inserts a maybe value into a map if it is present.
insertMaybe :: Ord k => k -> Maybe a -> Map.Map k a -> Map.Map k a
insertMaybe _ Nothing = id
insertMaybe k (Just a) = Map.insert k a

-- | Returns the last element of a list or @Nothing@ if empty.
maybeLast :: [a] -> Maybe a
maybeLast = foldl (curry $ Just . snd) Nothing

-- | Remove duplicates from lists.
snub :: Ord a => [a] -> [a]
snub = Set.toList . Set.fromList

-- | Adds elements to the end of the list until it is larger or equal than a
-- given limit.
rpad :: Int -> a -> [a] -> [a]
rpad = rpad' id

rpad' :: ([a] -> [a]) -> Int -> a -> [a] -> [a]
rpad' aggr 0 _ _ = aggr []
rpad' aggr n p [] = rpad' (aggr . (p:)) (n - 1) p []
rpad' aggr n p (x:xs) = rpad' (aggr . (x:)) (n - 1) p xs

-- | Look up a list of keys.
llookup :: Ord k => [k] -> Map.Map k a -> [a]
llookup ks m = mapMaybe (`Map.lookup` m) ks

-- | As @unlines@ but without a trailing '\n'.
concatNewLn :: [String] -> String
concatNewLn = concat . intersperse "\n"
