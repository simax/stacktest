# Haskell test app
module Main where


-- To build on save:
-- Note: Case sensitive project name
-- stack exec -- ghcid -c "stack ghci StackTest"

-- To run repl: 
-- stack ghci (from project root) 

-- Definition for singly-linked list:
data ListNode a = ListNode { val :: a
                           , next :: ListNode a
                           } | Nil deriving Show


-- mergeTwoLinkedLists :: ListNode Int -> ListNode Int -> ListNode Int
-- mergeTwoLinkedLists l1 Nil = l1
-- mergeTwoLinkedLists Nil l2 = l2
-- mergeTwoLinkedLists l1 l2 =    
--     ListNode { val = val', next = buildList l1' l2' } 
--     where 
--         buildList l Nil =  l
--         buildList Nil l =  l
--         buildList l1 l2 = mergeTwoLinkedLists l1 l2   
--         (val', l1', l2') = if (val l1) <= (val l2) then ((val l1), (next l1), l2) else ((val l2), l1, (next l2))    

-- testMerge = 
--     putStrLn $ show $ mergeTwoLinkedLists (ListNode { val = 1, 
--                                                         next = ListNode { val = 1, 
--                                                         next = ListNode { val = 2, 
--                                                         next = ListNode { val = 4, next = Nil } } } }) 
--                                             (ListNode { val = 0, 
--                                                         next = ListNode { val = 3, 
--                                                         next = ListNode { val = 5, next = Nil } } })


-- reverseNodesInKGroups l 1 = l
reverseNodesInKGroups l k = 
    forLoop 1 (partitions l k) l
    -- moveToIndex l 1 4 k 
    where 
        forLoop n numIters sorted = if n < numIters then forLoop (n + 1) numIters (moveToIndex l n numIters k) else sorted       
        -- sortChunk chunk = chunk 
        -- getChunk l n i k = if (x < k) then getChunk (moveToIndex l n i k) (n + 1) i k 
        moveToIndex l n i k = if (n < i) then moveToIndex (next l) (n + 1) i k else buildPartition l 1 k
        buildPartition l n k = if (n < k) then ListNode { val = (val l), next = buildPartition (next l) (n+1) k } else ListNode { val = (val l), next = Nil } 

        -- l' = if (val l) <= val (next l) then ListNode { val = val (next l), next = (next (next l)) } else l 
        -- (val', l') = if (val l) <= val (next l) then (val (next l), (next l)) else (val l, l) 
        partitions l k = ceiling $ fromIntegral (listSize l) / fromIntegral k  
        listSize Nil = 0 
        listSize l = 1 + (listSize (next l))


-- test = take 10 [ x * y | x <- [1..], x > 10, x /= 21, odd x, y <- [-100..0], y `mod` 3 == 0]

main :: IO ()
main = 
    -- putStrLn $ show $ listSize $ (ListNode {val=1, next = ListNode {val=2, next = ListNode {val=3, next = Nil}}})
    let lst = ListNode {val=1, next = ListNode {val=2, next = ListNode {val=3, next = ListNode {val=4, next = ListNode {val=5, next = ListNode {val=6, next = ListNode {val=7, next = ListNode {val=8, next = ListNode {val=9, next = ListNode {val=10, next = ListNode {val=11, next = Nil}}}}}}}}}}} in
    putStrLn $ show $ reverseNodesInKGroups lst 3 
    -- putStrLn $ show $ test