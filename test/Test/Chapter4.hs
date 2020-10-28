{-# OPTIONS_GHC -Wno-type-defaults #-}

{-# LANGUAGE TypeApplications #-}

module Test.Chapter4
    ( chapter4
    ) where

import Test.Hspec (Spec, describe, it, shouldBe)

import Chapter4


chapter4 :: Spec
chapter4 = describe "Chapter4" $ do
    chapter4normal
    chapter4advanced

chapter4normal :: Spec
chapter4normal = describe "Chapter4Normal" $ do
    describe "Task2: Functor for Secret" $ do
        let trap = Trap "it's a trap"
        it "doen't affect trap" $
            fmap @(Secret String) @Bool not trap `shouldBe` trap
        it "change reward, same type" $
            fmap @(Secret String) @Bool not (Reward False) `shouldBe` Reward True
        it "change reward, other type" $
            fmap @(Secret String) @Int even (Reward 5) `shouldBe` Reward False
        it "change reward, other type" $
            fmap @(Secret String) @Int even (Reward 4) `shouldBe` Reward True
    describe "Task4: Applicative for Secret" $ do
        let trap :: Secret String Int
            trap = Trap "it's a trap"
        it "pure int" $
            pure @(Secret String) "x" `shouldBe` Reward "x"
        it "pure bool" $
            pure @(Secret String) False `shouldBe` Reward False
        it "trap <*> reward" $
            Trap "it's a trap" <*> Reward 42 `shouldBe` trap
        it "trap <*> trap" $
            Trap "it's a trap" <*> Trap "42" `shouldBe` trap
        it "reward <*> trap" $
            Reward not <*> Trap 42 `shouldBe` Trap 42
        it "reward <*> reward - same type" $
            Reward not <*> Reward True `shouldBe` (Reward False :: Secret String Bool)
        it "reward <*> reward" $
            Reward odd <*> Reward 42 `shouldBe` (Reward False :: Secret String Bool)
    describe "Task6: Monad for Secret" $ do
        it "Trap" $ (Trap "aaar" >>= halfSecret) `shouldBe` Trap "aaar"
        it "Reward even" $ (Reward 42 >>= halfSecret) `shouldBe` Reward 21
        it "Reward odd" $ (Reward 11 >>= halfSecret) `shouldBe` Trap "it's a trap"

chapter4advanced :: Spec
chapter4advanced = describe "Chapter4Advanced" $ do
    describe "Task 8*: Before the Final Boss" $ do
        it "Nothing - Nothing" $ andM Nothing Nothing `shouldBe` Nothing
        it "Nothing - Just" $ andM Nothing (Just True) `shouldBe` Nothing
        it "Just True - Nothing" $ andM (Just True) Nothing `shouldBe` Nothing
        it "Just False - Nothing" $ andM (Just False) Nothing `shouldBe` Just False
        it "Just - Just : False" $ andM (Just True) (Just False) `shouldBe` Just False
        it "Just - Just : True" $ andM (Just True) (Just True) `shouldBe` Just True
    describe "Task 9: Binary tree" $ do
        it "Create tree" $ createTree testList `shouldBe` testBTree
        it "Reverse tree" $ reverseTree testBTree `shouldBe` testReverseTree
        it "Flatten tree to List" $ flattenTree testBTree `shouldBe` testList 

halfSecret :: Int -> Secret String Int
halfSecret n
    | even n = Reward (div n 2)
    | otherwise = Trap "it's a trap"

testList :: List Int
testList = (Cons 0 (Cons 1 (Cons 2 (Cons 3 (Cons 4 (Cons 5 (Cons 6 (Cons 7 (Cons 8 (Cons 9 (Cons 10 Empty)))))))))))

testBTree :: BTree Int
testBTree = Node (Node (Node (Node Nil 0 Nil) 1 Nil) 2 (Node (Node Nil 3 Nil) 4 Nil)) 5 (Node (Node (Node Nil 6 Nil) 7 Nil) 8 (Node (Node Nil 9 Nil) 10 Nil))

testReverseTree :: BTree Int
testReverseTree = Node (Node (Node Nil 10 (Node Nil 9 Nil)) 8 (Node Nil 7 (Node Nil 6 Nil))) 5 (Node (Node Nil 4 (Node Nil 3 Nil)) 2 (Node Nil 1 (Node Nil 0 Nil)))