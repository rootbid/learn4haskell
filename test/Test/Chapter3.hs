module Test.Chapter3
    ( chapter3
    ) where

import Test.Hspec (Spec, describe, it, shouldBe)

import Chapter3


chapter3 :: Spec
chapter3 = describe "Chapter3" $ do
    chapter3normal
    describe "Chapter3Advanced" $ it "" $ True `shouldBe` True

chapter3normal :: Spec
chapter3normal = describe "Chapter3Normal" $ do
    describe "Task4: ADTs" $ do
        it "build castle" $ buildCastle testCity1 `shouldBe` testBuildCastle
        it "build house" $ buildHouse testHouses testCity1 `shouldBe` testBuildHouses
        it "build walls: >= 10 population" $ buildWalls testCity2 `shouldBe` testBuildWalls
        it "build walls: < 10 population" $ buildWalls testCity3 `shouldBe` testCity3
        
    describe "Task 8: Weekdays" $ do
        it "isWeekend - 1" $ isWeekend Sunday `shouldBe` True
        it "isWeekend - 2" $ isWeekend Monday `shouldBe` False
        it "nextDay - 1" $ nextDay Sunday `shouldBe` Monday
        it "nextDay - 2" $ nextDay Wednesday `shouldBe` Thursday
        it "daysToParty - 1" $ daysToParty Friday `shouldBe` 0
        it "daysToParty - 1" $ daysToParty Sunday `shouldBe` 5
    
    describe "Task9: Final task" $ do
        it "knight vs monster-match1" $ battle kk mm `shouldBe` "Monster wins"
        it "knight vs monster-match2" $ battle kk' mm' `shouldBe` "Knight wins"
        it "knight vs monster-match3" $ battle kk'' mm'' `shouldBe` "Knight wins"
        it "knight vs monster-match4" $ battle kk''' mm''' `shouldBe` "Monster wins"

testCity1 :: City
testCity1 = City
    { castle    = Nothing
    , wall      = Wall False
    , community = Library
    , housing   = Housing 0 2 3 5
    }

testBuildCastle :: City
testBuildCastle = City
    { castle    = Just (Castle "NewCastle")
    , wall      = Wall False
    , community = Library
    , housing   = Housing 0 2 3 5
    }

testBuildHouses :: City
testBuildHouses = City
    { castle    = Nothing
    , wall      = Wall False
    , community = Library
    , housing   = Housing 5 5 3 5
    }

testCity2 :: City
testCity2 = City
    { castle    = Just (Castle "SmallCastle")
    , wall      = Wall False
    , community = Library
    , housing   = Housing 0 2 3 5
    }

testBuildWalls :: City
testBuildWalls = City
    { castle    = Just (Castle "SmallCastle")
    , wall      = Wall True
    , community = Library
    , housing   = Housing 0 2 3 5
    }

testCity3 :: City
testCity3 = City
    { castle    = Just (Castle "SmallCastle")
    , wall      = Wall False
    , community = Library
    , housing   = Housing 0 2 3 3
    }

testHouses :: Housing
testHouses = Housing 5 3 0 0

kk :: Knight'
kk = K
    { kHealth = Health 10
    , kAttack = Attack 15
    , kDefense = Defense 5
    , kActions = KAttack
    }

mm :: Monster'
mm = M
    { mHealth = Health 25
    , mAttack = Attack 20
    , mActions = MAttack
    }

kk' :: Knight'
kk' = K
    { kHealth = Health 30
    , kAttack = Attack 35
    , kDefense = Defense 5
    , kActions = KAttack
    }

mm' :: Monster'
mm' = M
    { mHealth = Health 25
    , mAttack = Attack 20
    , mActions = MAttack
    }

kk'' :: Knight'
kk'' = K
    { kHealth = Health 45
    , kAttack = Attack 15
    , kDefense = Defense 5
    , kActions = KAttack
    }

mm'' :: Monster'
mm'' = M
    { mHealth = Health 25
    , mAttack = Attack 20
    , mActions = MAttack
    }

kk''' :: Knight'
kk''' = K
    { kHealth = Health 45
    , kAttack = Attack 15
    , kDefense = Defense 5
    , kActions = KAttack
    }

mm''':: Monster'
mm''' = M
    { mHealth = Health 50
    , mAttack = Attack 40
    , mActions = MAttack
    }