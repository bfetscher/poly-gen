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
  \a -> (!!) (seq a (seq a (seq a ([]::[] (Bool -> [] Int))))) ((!!) a (id (undefined::Int))) ((\b -> (!!) ([]::[] (Bool -> Bool)) (undefined::Int) b) (seq a (seq a (\b -> seq b True) (seq a (head (undefined::[] ([] ([] Int)))))))),
  foldr (\a -> (undefined::(([] Int) -> [] Int) -> ([] Int) -> [] Int)) ((undefined::([] (Int -> [] Int)) -> ([] Int) -> [] Int) ((\a -> seq a (undefined::[] (Int -> [] Int))) (id (undefined::Int)))) (case1 (\a -> seq a tail) (undefined::[] Int) (id ([]::[] Int))),
  (undefined::Bool -> ([] Int) -> [] Int) ((\a -> seq a (seq a (\b -> seq b False)) ((+1) (seq a (undefined::Int)))) ((\a -> \b -> seq b (seq a ([]::[] Bool))) (\a -> id 0))),
  (undefined::([] Int) -> [] Int),
  tail,
  id (\a -> a),
  tail,
  \a -> case1 (\b -> (undefined::([] ([] (Bool -> Bool))) -> [] Int)) a ([]::[] ([] (Bool -> Bool))),
  tail,
  \a -> a]
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

