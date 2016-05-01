import Test.Hspec
import Lambda

test = hspec specs
--------------------------------------------
zoo = [self,id_,zero,suc,false,true,and_,not_]

runI f = runBeta <*> f $ [1..]
a ==:== b = runI a =:= runI b 
infixr 1 ==:== 
specs = do
    describe "reduce id" $ do
        it "beta reduce id on the zoo" $ and
            [   and [id_ %# f ==:== f | f <- zoo]
            ]
    describe "bools" $ do
        it "checks and truth table" $ and
            [    and_ %# true %# true ==:==  true
            ,    and_ %# true %# false ==:==  false
            ,    and_ %# false %# true ==:==  false
            ,    and_ %# false %# false ==:==  false
            ]
 
 
 
        it "checks or truth table" $ and
            [    or_ %# true %# true ==:==  true
            ,    or_ %# true %# false ==:==  true
            ,    or_ %# false %# true ==:==  true
            ,    or_ %# false %# false ==:==  false
            ]
    describe "numbers" $ do
        it "checks 2 + 2 = 4" $ and
            [   plus %# two %# two ==:== suc %# three
            ,   plus %# three %# three ==:== plus %# (suc %# three) %# two
            ,   plus %# three %# three ==:== plus %# two %# (suc %# three)
            ]

