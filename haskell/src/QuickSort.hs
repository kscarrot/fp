module QuickSort where

-- 快速排序算法实现
-- 基本版本：使用列表推导式
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (基准:剩余) = 
    quicksort [元素 | 元素 <- 剩余, 元素 <= 基准] ++ [基准] ++ quicksort [元素 | 元素 <- 剩余, 元素 > 基准]

-- 优化版本：使用filter函数，更清晰易读
quicksort' :: Ord a => [a] -> [a]
quicksort' [] = []
quicksort' (基准:剩余) = 
    quicksort' (filter (<= 基准) 剩余) ++ [基准] ++ quicksort' (filter (> 基准) 剩余)

-- 三路快速排序：处理重复元素更高效
三路快速排序 :: Ord a => [a] -> [a]
三路快速排序 [] = []
三路快速排序 (基准:剩余) = 
    三路快速排序 小于 ++ 等于 ++ 三路快速排序 大于
  where
    小于 = [元素 | 元素 <- 剩余, 元素 < 基准]
    等于 = [元素 | 元素 <- 剩余, 元素 == 基准] ++ [基准]
    大于 = [元素 | 元素 <- 剩余, 元素 > 基准]

-- 使用foldr的版本
quicksortFoldr :: Ord a => [a] -> [a]
quicksortFoldr [] = []
quicksortFoldr (基准:剩余) = 
    quicksortFoldr (foldr (\元素 累计 -> if 元素 <= 基准 then 元素:累计 else 累计) [] 剩余) 
    ++ [基准] 
    ++ quicksortFoldr (foldr (\元素 累计 -> if 元素 > 基准 then 元素:累计 else 累计) [] 剩余)

-- 测试函数
testQuicksort :: IO ()
testQuicksort = do
    let 测试列表 :: [Integer]
        测试列表 = [3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5]
    putStrLn "原始列表:"
    print 测试列表
    putStrLn "\n基本快速排序结果:"
    print $ quicksort 测试列表
    putStrLn "\n优化快速排序结果:"
    print $ quicksort' 测试列表
    putStrLn "\n三路快速排序结果:"
    print $ 三路快速排序 测试列表
    putStrLn "\n使用foldr的快速排序结果:"
    print $ quicksortFoldr 测试列表 