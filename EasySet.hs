
----- appealing fresh names set ----------------------------------------------
data V = X | Y | Z | W | J | U  | H | R | G | S | N | M | K | Q | F | D | O | B  deriving (Show,Enum,Eq,Bounded)
 
one = suc %# zero
two = suc %# one
three = suc %# two
four = suc %# three

runV f = runBeta <*> f $ [X ..]
runVs f = withFreshes [X ..] . betas $ f [X ..]
