#!/bin/bash

set -x

stack build :hplus

# TWO ARGS
#stack exec -- hplus "arg0:d -> arg1:[Maybe d] -> d" > topDown_tests/1_firstJust.txt
#stack exec -- hplus "arg0:[c] -> arg1:[d] -> [[(c,d)]]" > topDown_tests/13_cartProduct.txt
#stack exec -- hplus "arg0:Maybe d -> arg1:d -> Maybe d" > topDown_tests/14_maybe.txt
#stack exec -- hplus "arg0:d -> arg1:[d] -> ([d], [d])" > topDown_tests/19_splitAtFirst.txt
#stack exec -- hplus "arg0:[Int] -> arg1:(Int,Int) -> Bool" > topDown_tests/29_containsEdge.txt
#stack exec -- hplus "arg0:Bool -> arg1:d -> Maybe d" > topDown_tests/38_test.txt
#stack exec -- hplus "arg0:Int -> arg1:[d] -> [d]" > topDown_tests/39_appendN.txt
#stack exec -- hplus "arg0:Maybe c -> arg1:d -> Either c d" > topDown_tests/24_mbToEither.txt
#stack exec -- hplus "arg0:String -> arg1:Char -> [String]" > topDown_tests/7_splitStr.txt

# # THREE ARGS
stack exec -- hplus "arg0:Int -> arg1:Int -> arg2:[d] -> ([d], [d])" > topDown_tests/36_takeNdropM.txt

# # ONE ARG
#stack exec -- hplus "arg0:[Either c d] -> Either c [d]" > topDown_tests/3_rights.txt
#stack exec -- hplus "arg0:Either c b -> arg1:Either c d -> Either c d" > topDown_tests/11_eitherTriple.txt
#stack exec -- hplus "arg0:[Either c d] -> Either c d" > topDown_tests/12_firstRight.txt
#stack exec -- hplus "arg0:Either c (Either c d) -> Either c d" > topDown_tests/16_mergeEither.txt
#stack exec -- hplus "arg0:[[[d]]] -> [d]" > topDown_tests/17_flatten.txt
#stack exec -- hplus "arg0:[(d,c)] -> d" > topDown_tests/21_firstKey.txt
#stack exec -- hplus "arg0:[d] -> (d, [d])" > topDown_tests/23_head-rest.txt
#stack exec -- hplus "arg0:[Maybe d] -> d" > topDown_tests/32_firstMaybe.txt
#stack exec -- hplus "arg0:Int -> [Int]" > topDown_tests/34_singleList.txt
#stack exec -- hplus "arg0:[d] -> (d,d)" > topDown_tests/42_head-tail.txt

  
