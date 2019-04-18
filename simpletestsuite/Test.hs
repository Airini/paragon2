module Main where

import RunCommand (runCommandStrWait)
import KompTest

import Data.List
import Data.List.Extra (splitOn, breakOnEnd)
import Data.Maybe
import System.Directory
import System.Environment
import System.Exit
import System.IO
import System.Process
import System.FilePath
import System.Posix.Files
import Control.Exception
import Control.Monad
import System.Console.GetOpt

data Flag = JavaC
  deriving (Eq)

options :: [OptDescr Flag]
options =
  [ Option [] ["javac"] (NoArg JavaC) "Run javac compiler on generated .java files"
  ]

cmd c = do
  putStrLn c
  (out,err,code) <- runCommandStrWait c ""
  putStrLn out
  putStrLn err

getAll g = do
    pwd  <- getCurrentDirectory
    dirs <- getDirectoryContents (pwd </> ptog)
    files <- forM dirs $ \dir -> do
      f1 <- getDirectoryContents (pwd </> ptog </> dir)
      f2 <- liftM sort (filterM (\x -> return (isSuffixOf ".para" x)) f1)
      mapM (\x -> return $ ptog </> dir </> x) f2
    return (foldl (++) [] files)
  where ptog = testsuiteDir </> g

getAllIssues = do
  pwd <- getCurrentDirectory
  isList <- getDirectoryContents (pwd </> testsuiteDir </> "issues")
  filIssues <- filterM (\x -> return (isPrefixOf "issue" x)) isList
  return $ sort filIssues

paracViaCabal = "cabal new-run parac --"
cabalOut      = "Up to date\n"
testsuiteDir  = "simpletestsuite"
paraRT        = "examples/lib/paragonRT.jar"
paraPI        = "lib"

noCabalPrefix s = fromMaybe s (stripPrefix cabalOut s)
noCabalO = concat . splitOn cabalOut

isAsExpected str1' str2 = do
    if null str2 then do isEmpty str1 else
      if str1 == str2 then do
        putStrLn "ok"
        return 0
      else do
        putStrLn $ color 1 "Output does not match expected output"
        putStrLn $ color 1 "Expected output"
        putStrLn $ color 2 $ str2
        putStrLn $ color 1 "Received output"
        putStrLn $ color 3 $ str1
        return 1
  where str1 = noCabalO str1'

isEmpty str = do
  if null str then do
        putStrLn "ok"
        return 0 
    else do
        putStrLn $ color 1 "Output should be empty but isn't! Output was:"
        putStrLn $ color 3 $ str
        return 1
        
isNonEmpty str = do
  if null str then do
        putStrLn $ color 1 "Output should be non-empty but was!"
        return 1
    else do
        putStrLn "ok"
        return 0

dropLast n l = reverse (drop n (reverse l))


uglyErrorCheck e = do
  if e == "ExitSuccess" then do return 0 else do
    putStrLn $ color 1 "Error in compilation of parac."
    error "Test aborted."


compileParagon = do
  (o,e,c) <- runCommandStrWait "cabal new-build" ""
  putStrLn $ o ++ e
  uglyErrorCheck (show c)


testParagon runJavaC = do
  
  putStrLn $ color 4 "\nCompiling paragon"
  compileParagon
  
  putStrLn $ color 4 "Testing valid programs"
  allGood <- getAll "good"
  nfaultsG <- forM allGood $ \program -> do
    putStr $ (snd $ breakOnEnd "good/" program) ++ "... "
    (out,err,code) <- runCommandStrWait (paracViaCabal ++ " --oldskool -p " ++ paraPI ++ " " ++ program) ""
    fault <- isAsExpected (err ++ out) ""
    if (fault == 1)
      then return fault
      else 
        if runJavaC
          then do
            (jout,jerr,_) <- runCommandStrWait
              ("javac -cp " ++ paraRT ++ " " ++ replaceExtension program "java") ""
            isEmpty (jerr ++ jout)
          else return fault
 
  putStrLn $ color 4 "\nTesting invalid programs"
  allBad <- getAll "bad"
  nfaultsB <- forM allBad $ \program -> do
    putStr $ (snd $ breakOnEnd "bad/" program) ++ "... "
    (out,err,_) <- runCommandStrWait (paracViaCabal ++ " --oldskool -p " ++ paraPI ++ " " ++ program) ""
    (eout,_,_) <- runCommandStrWait ("cat " ++ (replaceExtension program "exp")) ""
    fault <- isAsExpected (err ++ out) eout
    return fault
  
  putStrLn $ color 4 "\nTesting issues"
  allIssues <- getAllIssues
  nfaultsI <- forM allIssues $ \issue -> do
    let relptoIssue = testsuiteDir </> "issues" </> issue
    putStr $ issue ++ "... "
    (clist,_,_) <- runCommandStrWait ("cat " ++ relptoIssue </> "compile") ""
    (eout,_,_) <- runCommandStrWait ("cat " ++ relptoIssue </> "expected") ""
    totalOut <- forM (lines clist) $ \file -> do
      (out,err,_) <- runCommandStrWait 
                     (paracViaCabal ++ " --oldskool -p " ++ paraPI ++ ":" ++ relptoIssue ++
                      " " ++ relptoIssue </> file) ""
      if runJavaC && null eout && (null . noCabalO) (err ++ out)
        then do
          (jout,jerr,_) <- runCommandStrWait
            ("javac -cp " ++ paraRT ++ " " ++ relptoIssue </> replaceExtension file "java") ""
          return (jerr ++ jout)
        else return (err ++ out)
    fault <- isAsExpected (foldl (++) [] totalOut) eout
    return fault

  putStr "\nTest ended with "
  let
    nfaultsT = (sum nfaultsG) + (sum nfaultsB)
    in if nfaultsT == 1 then
         putStr $ (show nfaultsT) ++ " failure and "
       else
         putStr $ (show nfaultsT) ++ " failures and "
  let 
    totalI = sum nfaultsI
    in if totalI == 1 then
         putStrLn $ (show totalI) ++ " open issue."
       else
         putStrLn $ (show totalI) ++ " open issues."

main = do
  args <- getArgs
  let header = "Usage: ./Test [OPTIONS]"
  case getOpt RequireOrder options args of
    (o,[],[])  -> testParagon (JavaC `elem` o)
    (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
