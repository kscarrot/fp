module Tree where

-- 这是一个二叉树的代数数据类型定义:
-- 1. Tree a 是一个类型构造器,其中 a 是类型参数,表示节点中存储的数据类型
-- 2. Empty 表示空树,是Tree的一个构造器
-- 3. Node a (Tree a) (Tree a) 表示非空节点,包含:
--    - 一个值 a
--    - 左子树 (Tree a)
--    - 右子树 (Tree a)
data Tree a = Empty | Node a (Tree a) (Tree a)



-- 创建一个示例二叉树:
--       1
--      / \
--     2   3
--    / \
--   4   5
示例树 :: Tree Int
示例树 = Node 1 
            (Node 2 
                (Node 4 Empty Empty)
                (Node 5 Empty Empty))
            (Node 3 Empty Empty)

-- 满二叉树:
--       1
--      / \
--     2   3
--    / \
--   4   5
满二叉树 :: Tree Int
满二叉树 = Node 1 
                (Node 2 
                    (Node 4 Empty Empty)
                    (Node 5 Empty Empty))
                (Node 3 Empty Empty)

-- 完全二叉树:
--       1
--      / \
--     2   3
--    / \ / \
--   4  5 6  7
完全二叉树 :: Tree Int
完全二叉树 = Node 1 
                (Node 2 
                    (Node 4 Empty Empty)
                    (Node 5 Empty Empty))
                (Node 3 (Node 6 Empty Empty) (Node 7 Empty Empty))


-- 前序遍历: 根 -> 左 -> 右
前序遍历 :: Tree a -> [a]
前序遍历 Empty = []
前序遍历 (Node x 左 右) = x : 前序遍历 左 ++ 前序遍历 右

-- 中序遍历: 左 -> 根 -> 右
中序遍历 :: Tree a -> [a]
中序遍历 Empty = []
中序遍历 (Node x 左 右) = 中序遍历 左 ++ [x] ++ 中序遍历 右

-- 后序遍历: 左 -> 右 -> 根
后序遍历 :: Tree a -> [a]
后序遍历 Empty = []
后序遍历 (Node x 左 右) = 后序遍历 左 ++ 后序遍历 右 ++ [x]

打印树 :: Show a => Tree a -> IO ()
打印树 Empty = putStrLn "空树"
打印树 (Node x 左 右) = do
    putStrLn ("节点: " ++ show x)
    putStrLn "左子树:"
    打印树 左
    putStrLn "右子树:"
    打印树 右

层序遍历 :: Tree a -> [[a]]
层序遍历 t = 遍历层 [t]
  where
    遍历层 [] = []
    遍历层 当前层 = let
        -- 步骤1: 获取当前层的所有节点值
        当前层的值 = [值 | Node 值 _ _ <- 当前层]
        -- 步骤2: 收集下一层的所有子树
        下一层 = [子树 | Node _ 左子树 右子树 <- 当前层, 子树 <- [左子树, 右子树]]
        -- 步骤3: 递归处理下一层，并过滤掉空列表
        in if null 当前层的值 then [] else 当前层的值 : 遍历层 下一层


打印层序 :: Show a => Tree a -> IO ()
打印层序 t = print (层序遍历 t)

-- 测试遍历函数
main :: IO ()
main = do
    putStrLn "树结构:"
    打印树 示例树
    
    putStrLn "前序遍历结果:"
    print (前序遍历 示例树)  -- 输出: [1,2,4,5,3]
    putStrLn "中序遍历结果:"
    print (中序遍历 示例树)   -- 输出: [4,2,5,1,3]
    putStrLn "后序遍历结果:"
    print (后序遍历 示例树) -- 输出: [4,5,2,3,1]
    putStrLn "层序遍历结果:"
    打印层序 示例树


