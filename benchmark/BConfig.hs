module BConfig where

import Types.Generate
import Types.Experiments
import Types.Environment
import BTypes


defaultTimeout = 2 * 60 :: Int
defaultQueryFile = "benchmark/suites/allQueries.json"
defaultExperiment = TrackTypesAndTransitions

searchParams = defaultSearchParams
searchParamsHOF = defaultSearchParams{_useHO=True}
searchParamsBaseline = defaultSearchParams{_useRefine=NoRefine}
searchParamsZeroStart = defaultSearchParams{_useRefine=AbstractRefinement}
searchParamsStopEarly = defaultSearchParams{_earlyCut=True,_stopThresh=20}
searchParamsZeroStopEarly = searchParamsStopEarly{_useRefine=AbstractRefinement}
searchParamsZeroQuery = defaultSearchParams{_useRefine=NoGar}
searchParamsILP = defaultSearchParams
