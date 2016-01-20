module Main where

import Distribution.TestSuite

import Prelude hiding ((+), (-), (/))
import Graphics.Solidhs

sph = Sphere 4 (1,2,3)

test_plus = do
  x <- return $ sph + sph
  putStrLn $ show x
  return $ case x of
      Union [a, b]  -> Finished Pass
      _              -> Finished $ Fail $ show x

test_plusplus = do
  x <- return $ sph + sph + sph
  putStrLn $ show x
  return $ case x of
      Union [a,b,c] -> Finished Pass
      _             -> Finished $ Fail $ show x

test_minus = do
  x <- return $ sph - sph
  putStrLn $ show x
  return $ case x of
      Diff [a, b]  -> Finished Pass
      _             -> Finished $ Fail $ show x

test_minusminus = do
  x <- return $ sph - sph - sph
  putStrLn $ show x
  return $ case x of
      Diff [a,b,c] -> Finished Pass
      _             -> Finished $ Fail $ show x

test_slash = do
  x <- return $ sph / sph
  putStrLn $ show x
  return $ case x of
      Intersection [a, b]  -> Finished Pass
      _              -> Finished $ Fail $ show x

test_slashslash = do
  x <- return $ sph / sph / sph
  putStrLn $ show x
  return $ case x of
      Intersection [a,b,c] -> Finished Pass
      _             -> Finished $ Fail $ show x

tests :: IO [Test]
tests = return [
   Test $ TestInstance {run = test_plus, name = "test unions"}
  ,Test $ TestInstance {run = test_plusplus, name = "test unions"}
  ,Test $ TestInstance {run = test_minus, name = "test diffs"}
  ,Test $ TestInstance {run = test_minusminus, name = "test diffs"}
  ,Test $ TestInstance {run = test_slash, name = "test interssssss"}
  ,Test $ TestInstance {run = test_slashslash, name = "test interssssss"}]

mayfail f (Finished Pass) = return ()
mayfail f (Finished (Fail t)) = f t

main = do
  ts <- tests
  mapM_ (\ (Test x) -> run x >>= mayfail
               (\ err -> putStrLn $ name x ++ " FAILED.\nGot " ++ err )) ts
