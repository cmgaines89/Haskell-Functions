--Corinne Gaines

--function to make a swap pass
swapPass(list) =
	if null list || null(tail list) then
		list
	else
		if head list > head(tail list) then
			head (tail list) : swapPass(head list : tail(tail list))
		else
			head list : swapPass(tail list)

--function to make outer level passes
passes(list, n) = 
	if n == 0 then
		list
	else
		passes(swapPass(list), n-1)

--bubble sort
bubSort(list) = 
	passes(list, length list)

--determines the smallest prime factor
smallestPrimeFactor :: Integer -> Integer
smallestPrimeFactor i =
	if 143 `mod` i == 0 then i
	else smallestPrimeFactor (i + 1)

--spanstring
spanString :: (Char, [Char]) -> [Char]
spanString (a, b) = 
	if null b then []
	else
	if head b == a then []
		else head b : spanString (a, tail b)

--triples
triples :: [(a, b)] -> [(a, b, a)]
triples n = 
	if null n then []
	else (fst (head n), snd (head n), fst (head n)) : triples (tail n)

--type this = (Char, [Integer]) -> (Integer, Char)
typeB :: (Char, [Integer]) -> (Integer, Char)
typeB (a, [b]) = (b, a)

--D.) type this = (a, b) -> (b, b)
typeD:: (a, b) -> (b, b)
typeD (a, b) = (b, b)

--afters
afters (_, []) = []
afters (key, (x : y) = if x = key 
	then x : (afters (key, y))
	else afters (key, y)

--neverFollows
neverFollows :: Eq a => (a, a, [a]) -> Bool
neverFollows (x, y, lst) =
	if tail lst == [] then True
	else if head lst == x && head (tail lst) == y then False
	else neverFollows (x, y, tail lst)

--Ord
greater :: Ord a => (a, [a]) -> [a]
greater (e, lst) =
	if lst == [] then lst
	else if head lst > e then head lst : greater(e,	tail lst)
	else greater (e, tail lst)

--inclist
inclist :: [Integer] -> Integer -> [Integer]
inclist lst a =
	if lst == [] then lst
	else map(a+)lst

--square sum
sqsum :: [Integer] -> Integer
sqsum lst =
	foldr (+) 0 (map(^2)lst)

--single clause factorial
fact n =
	if n == 1 then 1
	else n * fact (n-1)

--multiple clause factorial
fact n =
	if n == 1 then 1
	else n * fact (n-1)

fact 1 = 1
fact n = n * fact (n-1)

--reverse list
rev [] = []
rev (h : t) = rev t ++ [h]

rev1 x =
	if null x then []
	else rev (tail x) ++ [head x]
