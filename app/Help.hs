module Help
    ( help
    , helpAll
    , helpStr
    , helps
    ) where

import Control.Monad
import Data.List
import Util
import Parser
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

help' :: Map.Map String HelpDescr -> String
help' = concatNewLn . intersperse ""
    . Map.foldMapWithKey helpStr

-- | Get a help string for all commands provided.
help :: [String] -> String
help = help' . Map.restrictKeys helps . Set.fromList

-- | Get a help string for all commands available.
helpAll :: String
helpAll = help' helps

data HelpDescr = Help   { descr :: String
                        , syntax :: String
                        }

helpStr :: String -> HelpDescr -> [String]
helpStr name (Help descr syntax) =
    let descrLn = spacePad (':':name) ++ descr
        helpTail = if null syntax then [] else [(spacePad "") ++ syntax]
    in [concatNewLn $ descrLn:helpTail]

padBy :: Int
padBy =    let maxLen = foldl max 0 $ map length $ Map.keys helps
            in maxLen + 4 + (maxLen `mod` 4)

spacePad :: String -> String
spacePad = rpad padBy ' '

helps :: Map.Map String HelpDescr
helps = Map.fromList    [ ("quit", Help "exit program" "")
                        , ("q", Help "alias to :quit" "")
                        ,   ( "af"
                            , Help "store an AF described as adjacency matrix"
                                "<IDENTIFIER> := <ATTACKS>* [; <ATTACKS>*]*"
                            )
                        ,   ( "exp"
                            , Help "calculate and store the result of F +^stb G"
                                "<IDENTIFIER> := <IDENTIFIER-F> <IDENTIFIER-G>"
                            )
                        ,   ( "rev"
                            , Help "calculate and store the result of F *^stb G"
                                "<IDENTIFIER> := <IDENTIFIER-F> <IDENTIFIER-G>"
                            )
                        , ("show", Help "show a stored AF" "<IDENTIFIER>")
                        , ("help", Help "show help" "")
                        , ("h", Help "alias for :help" "")
                        ]
