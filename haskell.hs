--Corinne Gaines
--COSC 350

--bubble sort

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

--Question 1:
smallestPrimeFactor :: Integer -> Integer
smallestPrimeFactor i =
	if 143 `mod` i == 0 then i
	else smallestPrimeFactor (i + 1)

--Question 2:
spanString :: (Char, [Char]) -> [Char]
spanString (a, b) = 
	if null b then []
	else
	if head b == a then []
		else head b : spanString (a, tail b)

--Question 3
triples :: [(a, b)] -> [(a, b, a)]
triples n = 
	if null n then []
	else (fst (head n), snd (head n), fst (head n)) : triples (tail n)

--Question 4
--A.) type this = [(Integer, Float)]
--typeA :: [(Integer, Float)]
--typeA [(a, b)] = [(a, b), (a, b)]

--B.) type this = (Char, [Integer]) -> (Integer, Char)
typeB :: (Char, [Integer]) -> (Integer, Char)
typeB (a, [b]) = (b, a)

--C.) data that = see Float | say Integer
--    type this = (Bool, [that])

--D.) type this = (a, b) -> (b, b)
typeD:: (a, b) -> (b, b)
typeD (a, b) = (b, b)

--Question 2 afters
afters (_, []) = []
afters (key, (x : y) = if x = key 
	then x : (afters (key, y))
	else afters (key, y)

--Question 1
neverFollows :: Eq a => (a, a, [a]) -> Bool
neverFollows (x, y, lst) =
	if tail lst == [] then True
	else if head lst == x && head (tail lst) == y then False
	else neverFollows (x, y, tail lst)

--Question 2
greater :: Ord a => (a, [a]) -> [a]
greater (e, lst) =
	if lst == [] then lst
	else if head lst > e then head lst : greater(e,	tail lst)
	else greater (e, tail lst)

--Question 4
inclist :: [Integer] -> Integer -> [Integer]
inclist lst a =
	if lst == [] then lst
	else map(a+)lst

--Question 5
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