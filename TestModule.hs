module Main where
import Control.Monad
import qualified Control.Exception as E
import System.IO (hSetBuffering, stdout, BufferMode (NoBuffering))
handler (E.ErrorCall s) = putStrLn $ "*** Exception: "
incomplete1 0 = [undefined]
incomplete1 n = n:(incomplete1 (n-1))
incomplete2 0 = undefined
incomplete2 n = n:(incomplete2 (n-1))
incomplete3 n 0 = undefined:reverse [0..n-1]
incomplete3 n m = n:incomplete3 (n-1) (m-1)
enumFromTo' a n = enumFromTo a (a + n)
case1 c' e' l' = case l' of x':xs' -> c' x' xs'; [] -> e'
codeList :: [[Int] -> [Int]]
codeList = [
  id,
  (\xX -> (xX)),
  (undefined:: ([Int] -> [Int])),
  (\u -> ((undefined:: [Int]))),
  ((\rRZ -> ((\b -> (rRZ)))) (undefined:: [Int])),
  (((id head) []) ((undefined:: (Bool -> [Int])) True)),
  (\g -> (g)),
  (\cC -> (cC)),
  tail,
  id]
main = do
  hSetBuffering stdout NoBuffering
  forM_ codeList $ \code -> do
    forM_ [0..5] $ \x -> do
      E.catch (print $ code $ incomplete1 x) handler
    forM_ [0..5] $ \x -> do
      E.catch (print $ code $ incomplete2 x) handler
    forM_ [0..5] $ \x -> forM_ [0..x] $ \y -> do
      E.catch (print $ code $ incomplete3 x y) handler
    putStrLn "===="

