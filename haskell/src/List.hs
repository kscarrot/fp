module List where

-- 这是一个数组的代数数据类型定义:
-- 1. Array a 是一个类型构造器,其中 a 是类型参数,表示数组中存储的数据类型
-- 2. Empty 表示空数组,是Array的一个构造器
-- 3. Cons a (Array a) 表示非空数组,包含:
--    - 一个值 a
--    - 剩余数组 (Array a)
data Array a = Empty | Cons a (Array a)

-- 创建一个示例数组: [1,2,3,4]
示例数组 :: Array Int
示例数组 = Cons 1 (Cons 2 (Cons 3 (Cons 4 Empty)))

-- 空数组
空数组 :: Array Int
空数组 = Empty

-- 单元素数组
单元素数组 :: Array Int
单元素数组 = Cons 1 Empty
