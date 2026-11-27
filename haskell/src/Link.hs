module Link where

-- 这是一个链表的代数数据类型定义:
-- 1. Link a 是一个类型构造器,其中 a 是类型参数,表示节点中存储的数据类型
-- 2. Empty 表示空链表,是Link的一个构造器
-- 3. Node a (Link a) 表示非空节点,包含:
--    - 一个值 a
--    - 下一个节点的引用 (Link a)
data Link a = Empty | Node a (Link a)

-- 创建一个示例链表: 1 -> 2 -> 3 -> 4
示例链表 :: Link Int
示例链表 = Node 1 (Node 2 (Node 3 (Node 4 Empty)))

-- 空链表
空链表 :: Link Int
空链表 = Empty

-- 单节点链表
单节点链表 :: Link Int
单节点链表 = Node 1 Empty
