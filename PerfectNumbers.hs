module PerfectNumbers (classify, Classification(..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

classify :: Int -> Maybe Classification
classify i
  | i < 0 = Nothing
  | i == 1 = Just Deficient
  | sumToSelf (lsFactors i) 0 i == i = Just Perfect
  | sumToSelf (lsFactors i) 0 i > i = Just Abundant
  | sumToSelf (lsFactors i) 0 i < i = Just Deficient
  | otherwise = Nothing


sumToSelf :: [Int] -> Int -> Int -> Int
sumToSelf [] storage val = storage
sumToSelf [x] storage val
  | storage == val = storage
  | otherwise = sumToSelf [] (storage+x) val
sumToSelf (x:xs) storage val
  | storage == val = storage
  | otherwise = sumToSelf xs (storage+x) val


lsFactors :: Int -> [Int]
lsFactors a = [1] ++ (init (lsFactors' 2 a)) where
    lsFactors' :: Int -> Int -> [Int]
    lsFactors' start num
        | start<num && num `mod` start == 0 = [start] ++ (lsFactors' (start+1) num)
        | start<num && num `mod` start /= 0 = lsFactors' (start+1) num
        | otherwise = [num]

main = do
    mapM (putStrLn.show) (lsFactors 28)
    -- let answer = classify 28
    -- case answer of
    --     Just x -> (putStrLn . show) x
    --     _ -> putStrLn ""


