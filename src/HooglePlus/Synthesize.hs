module HooglePlus.Synthesize(synthesize, envToGoal) where

import Database.Environment
import Database.Util
import qualified HooglePlus.Abstraction as Abstraction
import PetriNet.PNSolver
import HooglePlus.Refinement
import Synquid.Error
import Synquid.Logic
import Synquid.Parser
import Synquid.Pretty
import Synquid.Program
import Synquid.Resolver
import Synquid.Type
import Synquid.Util
import Types.Common
import Types.Environment
import Types.Experiments
import Types.Program
import Types.Solver
import Types.Type
import Types.TopDown

import Control.Applicative ((<$>))
import Control.Concurrent.Chan
import Control.Exception
import Control.Lens
import Control.Monad
import Control.Monad.Except
import Control.Monad.Logic
import Control.Monad.Reader
import Control.Monad.State
import Data.Either
import Data.List
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Time.Clock
import Data.Time.Format
import System.Exit
import Text.Parsec.Indent
import Text.Parsec.Pos
import Text.Printf (printf)


updateEnvWithBoundTyVars :: RSchema -> Environment -> (Environment, RType)
updateEnvWithBoundTyVars (Monotype ty) env = (env, ty)
updateEnvWithBoundTyVars (ForallT x ty) env = updateEnvWithBoundTyVars ty (addTypeVar x env)

updateEnvWithSpecArgs :: RType -> Environment -> (Environment, RType)
updateEnvWithSpecArgs ty@(ScalarT _ _) env = (env, ty)
updateEnvWithSpecArgs (FunctionT x tArg tRes) env = updateEnvWithSpecArgs tRes $ addVariable x tArg $ addArgument x tArg env

envToGoal :: Environment -> String -> IO Goal
envToGoal env queryStr = do
  let transformedSig = "goal :: " ++ queryStr ++ "\ngoal = ??"
  let parseResult = flip evalState (initialPos "goal") $ runIndentParserT parseProgram () "" transformedSig
  case parseResult of
    Left parseErr -> putDoc (pretty $ toErrorMessage parseErr) >> putDoc empty >> error "uh oh"
    Right (funcDecl:decl:_) -> case decl of
      Pos _ (SynthesisGoal id uprog) -> do
        let Pos _ (FuncDecl _ sch) = funcDecl
        let goal = Goal id env sch uprog 3 $ initialPos "goal"
        let spec = runExcept $ evalStateT (resolveSchema (gSpec goal)) (initResolverState { _environment = env })
        case spec of
          Right sp -> return $ goal { gEnvironment = env, gSpec = sp }
          Left parseErr -> putDoc (pretty parseErr) >> putDoc empty >> exitFailure

      _ -> error "parse a signature for a none goal declaration"

synthesize :: SearchParams -> Goal -> Chan Message -> IO ()
synthesize searchParams goal messageChan = do
    let env''' = gEnvironment goal
    -- putStrLn $ "environment''': " ++ show (env''' ^. symbols)

    let (env'', monospec) = updateEnvWithBoundTyVars (gSpec goal) env'''
    -- let (env', destinationType) = updateEnvWithSpecArgs monospec env'''
    
    -- putStrLn $ "environment'': " ++ show (env'' ^. symbols)

    let (env', destinationType) = updateEnvWithSpecArgs monospec env''
    let useHO = _useHO searchParams
    let rawSyms = env' ^. symbols
    let hoCands = env' ^. hoCandidates
    
    -- putStrLn $ "environment: " ++ show rawSyms

    env <- do
      let syms = Map.filter (not . isHigherOrder . toMonotype) rawSyms
      return $
          env'
              {_symbols = Map.withoutKeys syms $ Set.fromList hoCands, _hoCandidates = []}

    let args = Monotype destinationType : Map.elems (env ^. arguments)
 
    putStrLn $ "monospec:" ++ show monospec
    putStrLn $ "goal:" ++ show goal
    putStrLn $ "destinationType:" ++ show destinationType

    -- putStrLn $ "environment: " ++ show (env ^. symbols)
    -- get return type
    -- get unified functions of the return type
    -- call DFS on all of those
    result <- dfsTop env messageChan 3 (shape destinationType)

    --putStrLn $ "wow we are here"
    putStrLn $ unlines $ result
    -- putStrLn $ unlines $ take 10 result

    -- putStrLn $ "Length of filtered: " ++ (show $ length $ filtered)
    -- putStrLn $ "wow we are here2"
    -- let filtered2 = filter (\x -> (isInfixOf "(GHC.List.!!" x) || (isInfixOf "(Data.Maybe.Just" x) || (isInfixOf "(GHC.List.last" x) ) filtered 
    -- putStrLn $ "wow we are here3"
    -- putStrLn $ "length: " ++ show (length filtered2)
    -- putStrLn $ unlines $ take 10 filtered
    -- putStrLn $ "here4"
    -- result <- dfs env messageChan 3 ("start", shape monospec)

    -- print the result
    -- putStrLn $ "result:" ++ unlines result

    --print $ cst' ^. components
    return () 


-- start off calling dfs with an empty memoize map
dfsTop :: Environment -> Chan Message -> Int -> SType -> IO [String]
dfsTop env messageChan depth hole = flip evalStateT emptyComps $ do
  -- collect all the component types (which we might use to fill the holes)
  let components = Map.toList (env ^. symbols)

  -- map each hole ?? to a list of component types that unify with the hole
  unifiedFuncs <- getUnifiedFunctions env messageChan components hole :: StateT Comps IO [(Id, SType)]

  lift $ putStrLn $ "unifiedFuns: " ++ (unlines $ map show unifiedFuncs)
  -- lift $ putStrLn $ "argUnifiedFuncs:" ++ show argUnifiedFuncs
  -- recurse, solving each unified component as a goal, solution is a list of programs
  -- the first element of list2 is the list of first argument solutions

  --dfs ... (Id, SType) -> StateT Comps IO [String]

  -- synthesize a list of programs for each top-level function unifying with goal
  -- return the first solution that uses both args (if any)
  let f x = ((isInfixOf "arg0" x) && (isInfixOf "arg1" x)) --  && (not (isInfixOf "@@" x)) && (not (isInfixOf "Nil" x))
  --let f x = (isInfixOf "one" x) && (isInfixOf "zero" x) && (isInfixOf "!!" x) && (not (isInfixOf "@@" x)) && (not (isInfixOf "Nil" x))
  result <- fmap concat $ mapM (fmap (take 1 . filter f) . dfs env messageChan depth) unifiedFuncs :: StateT Comps IO [String]
  -- lift $ print $ length result

  -- st <- get
  -- lift $ putStrLn "Done. Number of dfs calls at each depth: "
  -- lift $ print (st ^. counter)

  return result





  -- synthesize all programs:
  -- fmap concat $ mapM (dfs env messageChan depth) unifiedFuncs :: StateT Comps IO [String]
  
  
  -- (flip evalStateT) Map.empty (dfs env messageChan depth goal)


getUnifiedFunctions :: Environment -> Chan Message -> [(Id, RSchema)] -> SType -> StateT Comps IO [(Id, SType)]
getUnifiedFunctions envv messageChan xs goalType = do

  modify $ set components []
  
  -- lift $ putStrLn $ "allTypes: " ++ show allTypes

  st <- get
  let memoized = st ^. memoize :: Map SType [(Id, SType)]

  case Map.lookup goalType memoized of
    Just cs -> do
      -- lift $ putStrLn $ "already in there: " ++ show goalType
      -- lift $ putStrLn $ "unified components: " ++ show cs
      return cs
    Nothing -> do
      -- lift $ putStrLn $ "not in there yet: " ++ show goalType
      helper envv messageChan xs goalType
      st <- get
      let cs = st ^. components
      modify $ set memoize (Map.insert goalType cs (st ^. memoize))
      return $ st ^. components

  where 
    helper :: Environment -> Chan Message -> [(Id, RSchema)] -> SType -> StateT Comps IO ()
    helper _ _ [] _ = return ()
    -- skip components with @@ or Nil
    helper envv messageChan ( v@(id, schema) : ys) goalType
      | isInfixOf "@@" id = helper envv messageChan ys goalType
      | isInfixOf "Nil" id = helper envv messageChan ys goalType
      | otherwise = do
        
        -- lift $ putStrLn $ "\ngoalType: " ++ show goalType
        -- lift $ putStrLn $ "id: " ++ id
        -- lift $ putStrLn $ "schema: " ++ show schema
        

        -- lift $ putStrLn "not in there yet: " ++ show goalType

        let initSolverState = emptySolverState { _messageChan = messageChan }
        let t1 = shape (lastType (toMonotype schema))
        let t2 = goalType

        -- lift $ putStrLn "thing printed"
        st' <- execStateT (solveTypeConstraint envv t1 t2) initSolverState

        let sub =  st' ^. typeAssignment
        let checkResult = st' ^. isChecked
        -- lift $ putStrLn $ "checkResult: " ++ show checkResult

        let schema' = stypeSubstitute sub (shape $ toMonotype schema)

        st <- get
        if (checkResult) 
          then do
            -- lift $ putStrLn $ "schema': " ++ show schema'
            modify $ set components ((id, schema') : st ^. components) 
          else return ()
        
        helper envv messageChan ys goalType



isGround :: SType -> Bool
isGround (FunctionT id arg0 arg1) = False
isGround _ = True

-- type MyProgram = String
-- env, max depth, components, goal
-- returns list of possible programs
-- memoized
dfs :: Environment -> Chan Message -> Int -> (Id, SType) -> StateT Comps IO [String]
dfs _ _ 0 (id, schema) = do -- stop if depth is 0
  if (isGround schema) then return ["(" ++ id ++ ")"] else return []
dfs env messageChan depth (id, schema) = do
  -- check if schema is ground
  if (isGround schema) 
  -- then return ["isground at depth='" ++ show depth ++ "'" ++ id]
  then return ["(" ++ id ++ ")"]
  else do -- return []

    st <- get

    let memoized = st ^. memoizeDfs :: Map (Int, SType) [String]

    case Map.lookup (depth, schema) memoized of
      Just progs -> return progs
      Nothing    -> do

        -- add 1 to "number of times dfs called at this depth" counter
        -- modify $ set counter (Map.insertWith (+) depth 1 (st ^. counter))

        -- when (sum (st ^. counter) `mod` 1000 == 999) (lift $ putStr id >> print (st ^. counter))

        if (sum (st ^. counter) > 100000000) then
          -- once we hit 1000000 just say the solution is "stop"
          return ["stop"]
        else do

          -- collect all the argument types (the holes ?? we need to fill)
          let args = allArgTypes schema
          -- lift $ putStrLn $ "schema: " ++ show schema
          -- lift $ putStrLn $ "args: " ++ show args
          -- -- collect all the component types (which we might use to fill the holes)
          let components = Map.toList (env ^. symbols)

          -- putStrLn $ "args:" ++ show args
          -- putStrLn $ "depth:" ++ show depth


          -- clear the list of components

          -- map each hole ?? to a list of component types that unify with the hole
          argUnifiedFuncs <- mapM (getUnifiedFunctions env messageChan components) args :: StateT Comps IO [[(Id, SType)]]
          -- putStrLn $ "argUnifiedFuncs:" ++ show argUnifiedFuncs
          -- recurse, solving each unified component as a goal, solution is a list of programs
          -- the first element of list2 is the list of first argument solutions
          list2 <- mapM (fmap concat . mapM (dfs env messageChan (depth - 1))) argUnifiedFuncs :: StateT Comps IO [[String]]
          -- putStrLn $ "list2: " ++ show list2
          -- each arg hole is a list of programs
          -- take cartesian product of args and prepend our func name
          -- to get the list of resulting programs solving our original goal
          -- the first element of list3 is a list of programs that fit as first argument
          let list3 = sequence list2 :: [[String]]
          -- let formatFn args = "(depth='" ++ show depth ++ "'" ++ intercalate " " (id:args) ++ ")" -- takes ["(a)","(b)"] to "(f (a) (b))"
          let formatFn args = "(" ++ intercalate " " (id:args) ++ ")" -- takes ["(a)","(b)"] to "(f (a) (b))"
          let list4 = map formatFn list3

          modify $ set memoizeDfs (Map.insert (depth, schema) list4 (st ^. memoizeDfs))
          return list4

  -- print $ typeOf list
  -- each iteration of GUF returns IO [(Id, SType)]

  -- let (arg : argss) = args
  -- let blah = getUnifiedFunctions env messageChan components arg
  -- print $ typeOf blah

  -- dfs env messageChan (depth - 1) 
  -- list2 

  -- list2 :: [(Id, SType) -> IO [String]]
  -- let list2 = mapM (fmap (dfs env messageChan (depth - 1))) argUnifiedFuncs
  -- let list2 = mapM (mapM (dfs env messageChan (depth - 1))) argUnifiedFuncs
  -- let list2 = mapM (map (dfs env messageChan (depth - 1))) argUnifiedFuncs
  -- let list2 = fmap (fmap (dfs env messageChan (depth - 1))) argUnifiedFuncs
  -- let list2 = fmap (mapM (dfs env messageChan (depth - 1))) argUnifiedFuncs
  -- let list2 = fmap (map (dfs env messageChan (depth - 1))) argUnifiedFuncs
  -- let list2 = map (fmap (dfs env messageChan (depth - 1))) argUnifiedFuncs
  -- let list2 = map (mapM (dfs env messageChan (depth - 1))) argUnifiedFuncs
  --let list2 = map (map (dfs env messageChan (depth - 1))) argUnifiedFuncs
  -- list2 <- fmap (dfs env messageChan (depth - 1))) argUnifiedFuncs


  -- list2 :: [ [ IO [String] ] ]
  -- let list2  = map (map (dfs env messageChan (depth - 1))) argUnifiedFuncs'

  -- blah :: [IO [String]]
  -- let (blah : blahs) = list2

  -- print $ typeOf blah
  --  list3 :: [[String]]
  -- list3 <- mapM sequence list2
  -- putStrLn $ "list3: " ++ show list3
  -- fmap putStrLn list2
  -- print $ typeOf  list2

  -- print $ typeOf list2

  -- turn results of dfs into list of programs

  -- list3 ::      -- [[IO [String]]]
  -- list3 <- sequence list2
  -- let list4 = map (\a -> id ++ " (" ++ a ++ ")") $ map concat list3
  --return []
  -- return list4

  --- 

  
  -- check other candidates
  -- let otherCandidates = checkOtherCandidates (id, schema)
  -- dfs otherCandidates (depth - 1) acc

-- checkOtherCandidates :: (Id, SType) -> IO ()
-- checkOtherCandidates (id, schema) = do
--   get args
--   getUnifiedFunctions env [(id, schema)]


-- iterateOverEnv :: [(Id, RSchema)] -> [String]
-- iterateOverEnv [] = []
-- iterateOverEnv ( (id, schema) : xs) = id : iterateOverEnv xs

-- getUnifiedFunctions :: Environment -> [(Id, RSchema)] -> RType -> [(Id, RSchema)]
-- getUnifiedFunctions _ [] _ = []
-- getUnifiedFunctions envv ( v@(id, schema) : xs) goalType = do
--     let initSolverState = emptySolverState
--     let t1 = shape (lastType (toMonotype schema))
--     let t2 = shape goalType
--             -- putStrLn $ "t1: " ++ show t1
--             -- putStrLn $ "t2: " ++ show t2
--             -- getUnifiedFunctions env xs goalType
--     st' <- execStateT (solveTypeConstraint envv t1 t2) initSolverState
--     -- getUnifiedFunctions env xs goalType
--     let substitution =  st' ^. typeAssignment
--     let checkResult = st' ^. isChecked

--     if (checkResult) then v : getUnifiedFunctions envv xs goalType
--                     else getUnifiedFunctions envv xs goalType

-- getArgTypes :: [(Id, RSchema)] -> [RSchema]
-- getArgTypes [] = []
-- getArgTypes ( (_, schema) : xs) = schema : getArgTypes xs

-- toBlah :: RSchema -> RType
-- toBlah (Monotype v) = shape v 
-- toBlah (ForallT _ v) = toBlah v 
-- toBlah PredSig v = toBlah v




       -- if useHO -- add higher order query arguments
        --     then do
    --------------------------
    -- IGNORE PARTS BELOW THIS
    --------------------------
                -- let args = env' ^. arguments
                -- let hoArgs = Map.filter (isFunctionType . toMonotype) args
                -- let hoFuns = map (\(k, v) -> (k ++ hoPostfix, toFunType v)) (Map.toList hoArgs)
                -- return $
                --     env'
                --         { _symbols = rawSyms `Map.union` Map.fromList hoFuns
                --         , _hoCandidates = hoCands ++ map fst hoFuns
                --         }
    --------------------------
    -- IGNORE PARTS ABOVE THIS
    --------------------------
            -- else do
            --     let syms = Map.filter (not . isHigherOrder . toMonotype) rawSyms
            --     return $
            --         env'
            --             {_symbols = Map.withoutKeys syms $ Set.fromList hoCands, _hoCandidates = []}

    --putStrLn $ "Component number: " ++ show (Map.size $ allSymbols env)
    --putStrLn $ "Hello world"
 
  -- lift $ putStrLn $ "goalType: " ++ show goalType
  
  -- if (not $ isGround goalType) -- sometimes it returns (a -> b) - need to make sure we do just one of those
  --   then do
  --     -- return []
  --     let allTypes = allArgTypes goalType ++ [lastType goalType]
  --     lift $ putStrLn $ "allTypes: " ++ show allTypes
  --     -- unifiedFuncs <- mapM (getUnifiedFunctions envv messageChan xs) args
  --     fmap concat $ mapM (getUnifiedFunctions envv messageChan xs) allTypes
  --   else do
  --     modify $ set components []
  --     st <- get
  --     let memoized = st ^. memoize :: Map SType [(Id, SType)]
  --     case Map.lookup goalType memoized of
  --       Just cs -> do
  --         -- lift $ putStrLn $ "already in there: " ++ show goalType
  --         return cs
  --       Nothing -> do
  --         -- lift $ putStrLn $ "not in there yet: " ++ show goalType
  --         helper envv messageChan xs goalType
  --         st <- get
  --         let cs = st ^. components
  --         modify $ set memoize (Map.insert goalType cs (st ^. memoize))
  --         return $ st ^. components



------------------------
{- 
    * i've realized we can't do mapping stuff because substituation might be different
    * and also it's printing out weird things for destination type:
          schema: (a -> ((a -> ByteString) -> ByteString))
          args: [a,(a -> ByteString)]
      this means that the return type is still a function, so im not sure how getUnified is 
      working with that
-}
------------------------



   --print $ args
    --print $ Monotype destinationType
  -- start with all the datatypes defined in the components, first level abstraction

    --------------------------
    -- trying code Zheng gave us 
    --------------------------

    -- let rs2 = _refineStrategy searchParams
    -- let initSolverState =
    --         emptySolverState
    --             { _searchParams = searchParams
    --             , _abstractionCover =
    --                   case rs2 of
    --                       SypetClone -> Abstraction.firstLvAbs env (Map.elems (allSymbols env))
    --                       TyGar0 -> emptySolverState ^. abstractionCover
    --                       TyGarQ -> Abstraction.specificAbstractionFromTypes env args
    --                       NoGar -> Abstraction.specificAbstractionFromTypes env args
    --                       NoGar0 -> emptySolverState ^. abstractionCover
    --             , _messageChan = messageChan
    --             }

    {-
    case trial of
      AnyT -> putStrLn $ "AnyT"
      BotT -> putStrLn $ "botT"
      ScalarT b@(TypeVarT _ id) _ -> putStrLn $ "isBound: " ++ show (isBound env id)
      _    -> putStrLn $ "otherwise"

    putStrLn $ "trial: " ++ show trial
    putStrLn $ "trial2: " ++ show trial2
    -}
    {-
    -- make an empty solver state to use in evalState
    let initSolverState = emptySolverState { _messageChan = messageChan }


    -- used trial just to get one type for testing (not real code)
    let t1 = shape destinationType
    let t2 = shape (ScalarT IntT ftrue)

    putStrLn $ "t1: " ++ show t1
    putStrLn $ "t2: " ++ show t2

    putStrLn $ "here1" 

    st' <- execStateT (solveTypeConstraint env t1 t2) initSolverState
    putStrLn $ "here2" 


    let args2 = allArgTypes destinationType
    putStrLn $ "args: " ++ show (getArgTypes (Map.toList (env ^. arguments)))

    let substitution =  st' ^. typeAssignment
    let checkResult = st' ^. isChecked

    putStrLn $ "sub: " ++ show substitution
    putStrLn $ "checked: " ++ show checkResult

    let ( (id, schema) : xs) = (Map.toList (env ^. symbols))
    let blah = lastType (toMonotype schema)
    putStrLn $ "blah: " ++ show (shape blah)
    
    st' <- execStateT (solveTypeConstraint env t1 (shape blah)) initSolverState
    -}
    

    -- -}
    --let cons’ = stypeSubstitution substitution (shape $ toMonotype cons)

    --------------------------
    -- trying code Zheng gave us 
    --------------------------

    -- this is code we don't want I think below
    {-
    let rs = _refineStrategy searchParams
    let is =
            emptySolverState
                { _searchParams = searchParams
                , _abstractionCover =
                      case rs of
                          SypetClone -> Abstraction.firstLvAbs env (Map.elems (allSymbols env))
                          TyGar0 -> emptySolverState ^. abstractionCover
                          TyGarQ -> Abstraction.specificAbstractionFromTypes env args
                          NoGar -> Abstraction.specificAbstractionFromTypes env args
                          NoGar0 -> emptySolverState ^. abstractionCover
                , _messageChan = messageChan
                }
    catch
        (evalStateT (runPNSolver env destinationType) is)
        (\e ->
             writeChan messageChan (MesgLog 0 "error" (show e)) >>
             writeChan messageChan (MesgClose (CSError e)))
    -}


    --print $ iterateOverEnv (Map.toList (env ^. symbols)) 

    --let stc = solveTypeConstraint env trial trial2
    -- putStrLn $ show  (pretty stc)
    
    -- putStrLn $ "st': " ++ show st'

    --{-

    -- this is code we don't want I think above
    -- let initCompState = emptyComps

    -- dfsTop env messageChan 3 ("start", shape destinationType)

    -- cst' <- execStateT (getUnifiedFunctions env (Map.toList (env ^. symbols)) destinationType messageChan) initCompState
