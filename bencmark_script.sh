#!/bin/bash

set -x

stack build :hplus

# TWO ARGS
stack exec -- hplus "arg0:d -> arg1:[Maybe d] -> d" > firstJust.txt
stack exec -- hplus "arg0:[c] -> arg1:[d] -> [[(c,d)]]" > cartProduct.txt
stack exec -- hplus "arg0:Maybe d -> arg1:d -> Maybe d" > maybe.txt
stack exec -- hplus "arg0:d -> arg1:[d] -> ([d], [d])" > splitAtFirst.txt
stack exec -- hplus "arg0:[Int] -> arg1:(Int,Int) -> Bool" > containsEdge.txt
stack exec -- hplus "arg0:Bool -> arg1:d -> Maybe d" > test.txt
stack exec -- hplus "arg0:Int -> arg1:[d] -> [d]" > appendN.txt


# # THREE ARGS
# stack exec -- hplus "arg0:Int -> arg1:Int -> arg2:[d] -> ([d], [d])" > takeNdropM.txt

# # ONE ARG
# stack exec -- hplus "arg0:[Either c d] -> Either c d" > firstRight.txt
# stack exec -- hplus "arg0:Either c (Either c d) -> Either c d" > mergeEither.txt
# stack exec -- hplus "arg0:[[[d]]] -> [d]" > flatten.txt
# stack exec -- hplus "arg0:[(d,c)] -> d" > firstKey.txt
# stack exec -- hplus "arg0:[d] -> (d, [d])" > head-rest.txt
# stack exec -- hplus "arg0:[Maybe d] -> d" > firstMaybe.txt
# stack exec -- hplus "arg0:Int -> [Int]" > singleList.txt
# stack exec -- hplus "arg0:[d] -> (d,d)" > head-tail.txt

