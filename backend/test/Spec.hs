module Main (main) where

import BNFGenerator (bnfParser, generateHaskellCode, validate)
import Control.Monad (unless)
import Data.List (isSuffixOf, sort)
import Instances (ParseResult (Error, Result), parse)
import System.Directory (listDirectory)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import Test.DocTest (doctest)

data RegressionCase = RegressionCase
    { caseName :: String
    , inputPath :: FilePath
    , expectedDirectory :: FilePath
    }

regressionCases :: [RegressionCase]
regressionCases =
    [ RegressionCase "simple grammar" "examples/input/simple.bnf" "examples/expected_output/simple"
    , RegressionCase "modifiers and parameters" "examples/input/modifiers.bnf" "examples/expected_output/modifiers"
    , RegressionCase "validation cleanup" "examples/input/validation.bnf" "examples/expected_output/validation"
    ]

main :: IO ()
main = do
    putStrLn "Running parser-combinator doctests..."
    doctest ["-isrc", "src/Instances.hs", "src/Parser.hs"]

    putStrLn "Running BNF generation regression tests..."
    results <- mapM runRegressionCase regressionCases
    warningsMatch <- checkValidationWarnings
    unless (and results && warningsMatch) exitFailure
    putStrLn "All parser tests passed."

runRegressionCase :: RegressionCase -> IO Bool
runRegressionCase testCase = do
    source <- readFile (inputPath testCase)
    case parse bnfParser source of
        Error parseError -> do
            putStrLn $ "FAIL: " ++ caseName testCase ++ " could not be parsed: " ++ show parseError
            pure False
        Result _ grammar -> do
            expectedFiles <- expectedHaskellFiles (expectedDirectory testCase)
            expectedOutputs <- mapM readFile expectedFiles
            let actual = generateHaskellCode grammar
                passed = actual `elem` expectedOutputs
            putStrLn $ status passed ++ ": " ++ caseName testCase
            pure passed

checkValidationWarnings :: IO Bool
checkValidationWarnings = do
    source <- readFile "examples/input/validation.bnf"
    let expected =
            [ "Duplicate rule: duplicated"
            , "Left recursion in: expr"
            , "Left recursion in: term"
            , "Left recursion in: factor"
            ]
    case parse bnfParser source of
        Error parseError -> do
            putStrLn $ "FAIL: validation fixture could not be parsed: " ++ show parseError
            pure False
        Result _ grammar -> do
            let passed = validate grammar == expected
            putStrLn $ status passed ++ ": validation warnings"
            pure passed

expectedHaskellFiles :: FilePath -> IO [FilePath]
expectedHaskellFiles directory = do
    entries <- sort <$> listDirectory directory
    pure [directory </> entry | entry <- entries, ".hs" `isSuffixOf` entry]

status :: Bool -> String
status True = "PASS"
status False = "FAIL"
