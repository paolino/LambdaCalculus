{-# LANGUAGE LambdaCase #-}

{- | Native conformance driver for capture-avoiding substitution.

Builds the frozen 282-case zoo, independently canonicalizes
production 'beta' results to de Bruijn form, and compares them
with pinned oracle normal forms. A deliberate output mutation is
rejected before any clean summary is trusted.
-}
module Main (main) where

import Data.Char (ord)
import Data.List (elemIndex, intercalate, nub)
import Lambda
    ( Expr (..)
    , Tactic (Normal)
    , application
    , beta
    , freevars
    , withFreshes
    )
import System.Environment (getArgs)
import System.Exit (die, exitFailure, exitSuccess)

data Case = Case
    { caseId :: String
    , category :: String
    , captureExpected :: Bool
    , expression :: Expr Char
    }

data Oracle = Oracle
    { oracleInput :: String
    , oracleNormal :: String
    }

data Ty
    = Base
    | Arr Ty Ty
    deriving (Eq, Ord, Show)

data Generated
    = GVar Int
    | GLam Ty Generated
    | GApp Generated Generated
    deriving (Eq, Ord, Show)

data DB
    = DBVar Int
    | DBLam DB
    | DBApp DB DB
    deriving (Eq, Ord, Show)

data Verdict
    = Agreement
    | Mismatch
    | Indeterminate String
    deriving (Eq, Show)

data BinderCheck = BinderCheck
    { binderCheckId :: String
    , binderTerm :: Expr Char
    , binderExpect :: Expr Char -> Either String ()
    }

v :: Char -> Expr Char
v = T

lm :: Char -> Expr Char -> Expr Char
lm = (:\)

ap :: Expr Char -> Expr Char -> Expr Char
ap = (:#)

varNames :: [Char]
varNames = nub $ "xyzwnmlkij" ++ ['a' .. 'z']

boot :: [(String, Expr Char)]
boot =
    [ ("FALSE", falseTerm varNames)
    , ("TRUE", trueTerm varNames)
    , ("AND", andTerm varNames)
    , ("OR", orTerm varNames)
    , ("ID", idTerm varNames)
    , ("ZERO", zeroTerm varNames)
    , ("SUCC", succTerm varNames)
    , ("PLUS", plusTerm varNames)
    ]

falseTerm :: [Char] -> Expr Char
falseTerm (x : y : _) = lm x $ lm y $ v y
falseTerm _ = error "falseTerm: insufficient names"

trueTerm :: [Char] -> Expr Char
trueTerm (x : y : _) = lm x $ lm y $ v x
trueTerm _ = error "trueTerm: insufficient names"

andTerm :: [Char] -> Expr Char
andTerm (x : y : _) = lm x $ lm y $ ap (ap (v x) (v y)) (v x)
andTerm _ = error "andTerm: insufficient names"

orTerm :: [Char] -> Expr Char
orTerm (x : y : _) = lm x $ lm y $ ap (ap (v x) (v x)) (v y)
orTerm _ = error "orTerm: insufficient names"

idTerm :: [Char] -> Expr Char
idTerm (x : _) = lm x $ v x
idTerm _ = error "idTerm: insufficient names"

zeroTerm :: [Char] -> Expr Char
zeroTerm = falseTerm

succTerm :: [Char] -> Expr Char
succTerm (x : z : w : _) =
    lm x $ lm z $ lm w $ ap (v z) $ ap (ap (v x) (v z)) (v w)
succTerm _ = error "succTerm: insufficient names"

plusTerm :: [Char] -> Expr Char
plusTerm (x : y : z : w : _) =
    lm x $ lm y $ lm z $ lm w $
        ap (ap (v x) (v z)) $ ap (ap (v y) (v z)) (v w)
plusTerm _ = error "plusTerm: insufficient names"

shippedExamples :: [Case]
shippedExamples =
    [ Case "example-0-plus-0" "shipped-example" False $
        ap (ap (plusTerm varNames) (zeroTerm varNames)) (zeroTerm varNames)
    , Case "example-succ-0" "shipped-example" False $
        ap (succTerm varNames) (zeroTerm varNames)
    , Case "example-1-plus-1" "shipped-example" False $
        ap
            (ap
                (plusTerm varNames)
                (ap (succTerm varNames) (zeroTerm varNames)))
            (ap (succTerm varNames) (zeroTerm varNames))
    , Case "example-true-and-false" "shipped-example" False $
        ap (ap (andTerm varNames) (trueTerm varNames)) (falseTerm varNames)
    , Case "example-false-or-false" "shipped-example" False $
        ap (ap (orTerm varNames) (falseTerm varNames)) (falseTerm varNames)
    , Case "example-id-true" "shipped-example" False $
        ap (idTerm varNames) (trueTerm varNames)
    ]

bootApplications :: [Case]
bootApplications =
    [ Case
        ("boot-" ++ lower leftName ++ "-applied-to-" ++ lower rightName)
        "boot-pair"
        False
        (ap left right)
    | (leftName, left) <- boot
    , (rightName, right) <- boot
    ]
  where
    lower = map $ \c ->
        if 'A' <= c && c <= 'Z'
            then toEnum (fromEnum c + 32)
            else c

captureCases :: [Case]
captureCases =
    [ Case "capture-textbook-open" "capture" True $
        ap (lm 'x' $ lm 'y' $ v 'x') (v 'y')
    , Case "capture-textbook-closed-over" "capture" True $
        lm 'y' $ ap (lm 'x' $ lm 'y' $ v 'x') (v 'y')
    , Case "capture-nested-two-open" "capture" True $
        ap
            (lm 'x' $ lm 'y' $ lm 'z' $ ap (ap (v 'x') (v 'y')) (v 'z'))
            (ap (v 'y') (v 'z'))
    , Case "capture-nested-two-closed-over" "capture" True $
        lm 'y' $ lm 'z' $
            ap
                (lm 'x' $ lm 'y' $ lm 'z' $ ap (ap (v 'x') (v 'y')) (v 'z'))
                (ap (v 'y') (v 'z'))
    , Case "capture-three-conflicts-open" "capture" True $
        ap
            (lm 'x' $ lm 'y' $ lm 'z' $ lm 'w' $
                ap (ap (ap (v 'x') (v 'y')) (v 'z')) (v 'w'))
            (ap (ap (v 'y') (v 'z')) (v 'w'))
    , Case "capture-shadowed-conflicts-harmless" "capture" True $
        ap (lm 'x' $ lm 'y' $ lm 'y' $ v 'x') (v 'y')
    , Case "capture-argument-under-depth" "capture" True $
        lm 'a' $ lm 'b' $
            ap (lm 'x' $ lm 'a' $ ap (v 'x') (v 'b')) (v 'a')
    , Case "shadow-target-stops-substitution" "shadowing" False $
        ap (lm 'x' $ lm 'x' $ v 'x') (v 'y')
    , Case "shadow-three-identical-binders" "shadowing" False $
        lm 'x' $ lm 'x' $ lm 'x' $ v 'x'
    , Case "shadow-outer-reference" "shadowing" False $
        lm 'x' $ ap (lm 'y' $ lm 'x' $ v 'y') (v 'x')
    , Case "capture-no-conflict-control" "capture-control" False $
        ap (lm 'x' $ lm 'z' $ ap (v 'x') (v 'z')) (v 'y')
    , Case "capture-free-in-function-position" "capture" True $
        ap (lm 'x' $ lm 'y' $ ap (v 'y') (v 'x')) (v 'y')
    ]

smallTypes :: [Ty]
smallTypes =
    [ Base
    , Arr Base Base
    , Arr Base $ Arr Base Base
    , Arr (Arr Base Base) Base
    , Arr (Arr Base Base) $ Arr Base Base
    ]

generate :: [Ty] -> Ty -> Int -> [Generated]
generate context wanted size = variables ++ abstractions ++ applications
  where
    variables
        | size == 1 =
            [ GVar index
            | (index, actual) <- zip [0 ..] context
            , actual == wanted
            ]
        | otherwise = []
    abstractions = case wanted of
        Arr argument result
            | size >= 2 ->
                GLam argument
                    <$> generate (argument : context) result (size - 1)
        _ -> []
    applications
        | size >= 3 =
            [ GApp function argument
            | argumentType <- smallTypes
            , functionSize <- [1 .. size - 2]
            , let argumentSize = size - 1 - functionSize
            , function <-
                generate context (Arr argumentType wanted) functionSize
            , argument <- generate context argumentType argumentSize
            ]
        | otherwise = []

generatedTerms :: [Generated]
generatedTerms =
    take 200 $
        nub
            [ term
            | size <- [2 .. 11]
            , wanted <- smallTypes
            , term <- generate [] wanted size
            ]

binderNames :: [Char]
binderNames = ['a' .. 'z']

generatedToExpr :: Generated -> Expr Char
generatedToExpr = go []
  where
    go environment = \case
        GVar index -> v $ environment !! index
        GLam _ body ->
            let name = binderNames !! length environment
             in lm name $ go (name : environment) body
        GApp function argument ->
            ap (go environment function) (go environment argument)

generatedCases :: [Case]
generatedCases =
    [ Case
        ("generated-stlc-" ++ pad 3 index)
        "systematic-closed-stlc"
        False
        (generatedToExpr term)
    | (index, term) <- zip [1 :: Int ..] generatedTerms
    ]
  where
    pad width n = replicate (width - length shown) '0' ++ shown
      where
        shown = show n

zoo :: [Case]
zoo =
    shippedExamples
        ++ bootApplications
        ++ captureCases
        ++ generatedCases

-- | Named-binder checks for sibling freshness. De Bruijn cannot see
-- this class: duplicate generated binders remain alpha-equivalent.
binderChecks :: [BinderCheck]
binderChecks =
    [ BinderCheck "capture-sibling-two-open" siblingTwoOpen twoSiblingApp
    , BinderCheck "capture-sibling-two-redex" siblingTwoRedex twoSiblingRedex
    , BinderCheck "capture-sibling-three-open" siblingThreeOpen threeSiblingApp
    , BinderCheck "capture-sibling-under-lambda" siblingUnderLambda twoSiblingUnderLambda
    ]

siblingTwoOpen :: Expr Char
siblingTwoOpen =
    (T 'f' :# ('y' :\ T 'x')) :# ('y' :\ T 'x')

siblingTwoRedex :: Expr Char
siblingTwoRedex =
    ('y' :\ T 'x') :# ('y' :\ T 'x')

siblingThreeOpen :: Expr Char
siblingThreeOpen =
    ((T 'f' :# ('y' :\ T 'x')) :# ('y' :\ T 'x')) :# ('y' :\ T 'x')

siblingUnderLambda :: Expr Char
siblingUnderLambda =
    'z' :\ (('y' :\ T 'x') :# ('y' :\ T 'x'))

twoSiblingApp :: Expr Char -> Either String ()
twoSiblingApp = \case
    (T 'f' :# (left :\ T 'y')) :# (right :\ T 'y')
        | left /= right -> Right ()
        | otherwise ->
            Left $ "duplicate generated binder: " ++ [left]
    other -> Left $ "unexpected normal form: " ++ show other

twoSiblingRedex :: Expr Char -> Either String ()
twoSiblingRedex = \case
    (left :\ T 'y') :# (right :\ T 'y')
        | left /= right -> Right ()
        | otherwise ->
            Left $ "duplicate generated binder: " ++ [left]
    other -> Left $ "unexpected normal form: " ++ show other

threeSiblingApp :: Expr Char -> Either String ()
threeSiblingApp = \case
    ((T 'f' :# (a :\ T 'y')) :# (b :\ T 'y')) :# (c :\ T 'y')
        | length (nub [a, b, c]) == 3 -> Right ()
        | otherwise ->
            Left $ "duplicate generated binder: " ++ [a, b, c]
    other -> Left $ "unexpected normal form: " ++ show other

twoSiblingUnderLambda :: Expr Char -> Either String ()
twoSiblingUnderLambda = \case
    _ :\ ((left :\ T 'y') :# (right :\ T 'y'))
        | left /= right -> Right ()
        | otherwise ->
            Left $ "duplicate generated binder: " ++ [left]
    other -> Left $ "unexpected normal form: " ++ show other

seedDuplicate :: Expr Char
seedDuplicate =
    (T 'f' :# ('a' :\ T 'y')) :# ('a' :\ T 'y')

toDB :: Expr Char -> DB
toDB = go []
  where
    go environment = \case
        T name -> case elemIndex name environment of
            Just index -> DBVar index
            Nothing -> DBVar $ length environment + ord name
        name :\ body -> DBLam $ go (name : environment) body
        function :# argument ->
            DBApp (go environment function) (go environment argument)

renderDB :: DB -> String
renderDB = \case
    DBVar index -> "v" ++ show index
    DBLam body -> "l(" ++ renderDB body ++ ")"
    DBApp function argument ->
        "a(" ++ renderDB function ++ ")(" ++ renderDB argument ++ ")"

conflictCount :: Expr Char -> Int
conflictCount = \case
    T _ -> 0
    _ :\ body -> conflictCount body
    (name :\ body) :# argument ->
        substCount name argument body
            + conflictCount body
            + conflictCount argument
    function :# argument ->
        conflictCount function + conflictCount argument

substCount :: Char -> Expr Char -> Expr Char -> Int
substCount target replacement = walk
  where
    forbidden = freevars replacement
    walk = \case
        T _ -> 0
        name :\ body
            | name == target -> 0
            | otherwise ->
                (if name `elem` forbidden then 1 else 0) + walk body
        function :# argument -> walk function + walk argument

splitTab :: String -> [String]
splitTab xs = case break (== '\t') xs of
    (field, '\t' : rest) -> field : splitTab rest
    (field, _) -> [field]

loadOracle :: FilePath -> IO [(String, Oracle)]
loadOracle path = do
    raw <- readFile path
    let rows = filter (not . null) (lines raw)
    parsed <- mapM parseRow rows
    let ids = map fst parsed
    if length (nub ids) /= length ids
        then die "oracle identifiers are not unique"
        else return parsed
  where
    parseRow line = case splitTab line of
        identifier : inputDb : normalDb : _ ->
            return (identifier, Oracle inputDb normalDb)
        _ -> die $ "malformed oracle row: " ++ line

lookupOracle :: String -> [(String, Oracle)] -> Maybe Oracle
lookupOracle identifier = lookup identifier

productionNF :: Expr Char -> String
productionNF term =
    renderDB . toDB $ withFreshes varNames (beta Normal term)

judge
    :: Maybe String
    -> [(String, Oracle)]
    -> Case
    -> (String, Verdict, String, String, Int)
judge mutated oracleRow case_ =
    case lookupOracle (caseId case_) oracleRow of
        Nothing ->
            ( caseId case_
            , Indeterminate "missing-oracle-row"
            , produced
            , ""
            , conflicts
            )
        Just o
            | inputDb /= oracleInput o ->
                ( caseId case_
                , Indeterminate "input-bridge-roundtrip-mismatch"
                , produced
                , oracleNormal o
                , conflicts
                )
            | captureExpected case_ && conflicts == 0 ->
                ( caseId case_
                , Indeterminate
                    "declared-capture-case-did-not-exercise-conflict-path"
                , produced
                , oracleNormal o
                , conflicts
                )
            | produced == oracleNormal o ->
                (caseId case_, Agreement, produced, oracleNormal o, conflicts)
            | otherwise ->
                (caseId case_, Mismatch, produced, oracleNormal o, conflicts)
  where
    inputDb = renderDB (toDB (expression case_))
    conflicts = conflictCount (expression case_)
    produced = case mutated of
        Just dummy
            | dummy == caseId case_ -> "v999999"
        _ -> productionNF (expression case_)

parseArgs :: [String] -> IO (Maybe String)
parseArgs = \case
    [] -> return Nothing
    ["--match", pattern] -> return (Just pattern)
    _ -> die "usage: conformance [--match CASE-ID]"

matchById :: Maybe String -> (a -> String) -> [a] -> [a]
matchById Nothing _ xs = xs
matchById (Just needle) getId xs = filter ((== needle) . getId) xs

countVerdict :: Verdict -> [Verdict] -> Int
countVerdict wanted = length . filter (== wanted)

summarize :: String -> [Verdict] -> String
summarize label verdicts =
    intercalate "\t"
        [ label
        , "cases=" ++ show (length verdicts)
        , "agreements=" ++ show (countVerdict Agreement verdicts)
        , "mismatches=" ++ show (countVerdict Mismatch verdicts)
        , "indeterminate="
            ++ show (length [v | v@(Indeterminate _) <- verdicts])
        ]

printFailures :: [(String, Verdict, String, String, Int)] -> IO ()
printFailures rows =
    mapM_ printOne
        [ row
        | row@(_, verdict, _, _, _) <- rows
        , verdict /= Agreement
        ]
  where
    printOne (identifier, verdict, produced, expected, conflicts) =
        putStrLn $
            intercalate "\t"
                [ "fail"
                , identifier
                , showVerdict verdict
                , "conflicts=" ++ show conflicts
                , produced
                , expected
                ]
    showVerdict = \case
        Agreement -> "agreement"
        Mismatch -> "mismatch"
        Indeterminate reason -> "indeterminate:" ++ reason

expectedZooSize :: Int
expectedZooSize = 282

mutationCaseId :: [Case] -> String
mutationCaseId selected =
    if any ((== "example-id-true") . caseId) selected
        then "example-id-true"
        else caseId (head selected)

runBinderCheck :: BinderCheck -> (String, Verdict, String, String, Int)
runBinderCheck check =
    case binderExpect check result of
        Right () ->
            (binderCheckId check, Agreement, shown, "distinct-sibling-binders", 0)
        Left reason ->
            (binderCheckId check, Mismatch, shown, reason, 0)
  where
    result = withFreshes varNames $
        application 'x' (binderTerm check) (T 'y')
    shown = show result

main :: IO ()
main = do
    pattern <- getArgs >>= parseArgs
    oracleRows <- loadOracle "test/oracle-normal-forms.tsv"
    let zooSelected = matchById pattern caseId zoo
        binderSelected = matchById pattern binderCheckId binderChecks
    if null zooSelected && null binderSelected
        then die "no cases matched"
        else return ()
    case pattern of
        Nothing -> checkCorpus oracleRows
        Just _ -> return ()
    case twoSiblingApp seedDuplicate of
        Left _ ->
            putStrLn "binder-seed: rejected duplicate generated binders"
        Right () -> do
            putStrLn "binder-seed: vacuous"
            exitFailure
    zooOk <-
        if null zooSelected
            then return True
            else runZoo oracleRows zooSelected
    let binderRows = map runBinderCheck binderSelected
        binderVerdicts = [v | (_, v, _, _, _) <- binderRows]
    putStrLn $ summarize "binder" binderVerdicts
    printFailures binderRows
    let binderOk =
            countVerdict Agreement binderVerdicts == length binderVerdicts
                && not (null binderSelected)
                || null binderSelected
    if zooOk && binderOk then exitSuccess else exitFailure

runZoo
    :: [(String, Oracle)]
    -> [Case]
    -> IO Bool
runZoo oracleRows selected = do
    let mutatedId = mutationCaseId selected
        mutatedRows = map (judge (Just mutatedId) oracleRows) selected
        mutatedVerdicts = [v | (_, v, _, _, _) <- mutatedRows]
        cleanRows = map (judge Nothing oracleRows) selected
        cleanVerdicts = [v | (_, v, _, _, _) <- cleanRows]
    if countVerdict Mismatch mutatedVerdicts < 1
        then do
            putStrLn "negative-control: vacuous"
            putStrLn $ summarize "mutated" mutatedVerdicts
            return False
        else do
            putStrLn $
                "negative-control: rejected mutated_case="
                    ++ mutatedId
                    ++ " mutated_nf=v999999"
            putStrLn $ summarize "clean" cleanVerdicts
            printFailures cleanRows
            return $
                countVerdict Agreement cleanVerdicts == length cleanVerdicts
                    && null [v | v@(Indeterminate _) <- cleanVerdicts]

checkCorpus :: [(String, Oracle)] -> IO ()
checkCorpus oracleRows = do
    let zooIds = map caseId zoo
        oracleIds = map fst oracleRows
    if length zoo /= expectedZooSize
        then die $ "zoo size " ++ show (length zoo) ++ " /= 282"
        else return ()
    if length oracleIds /= expectedZooSize
        then
            die $
                "oracle size "
                    ++ show (length oracleIds)
                    ++ " /= 282"
        else return ()
    if zooIds /= oracleIds
        then die "zoo identifiers disagree with oracle identifiers"
        else return ()
