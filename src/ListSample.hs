{-# LANGUAGE DataKinds #-}
module ListSample where

-- List and its related inportant functions
-- cons(:) can construct a list
cons_empty_int = [] :: [Int]
cons_empty     = []
cons_int1 = [1]
cons_int2 = 1 :[]
                    
-- pattern match of list used in function definition
match0 a = print a

match1 a@[] = print a
match1 (n:ns) = print (n:ns)

match2 a@[] = print a
match2 a@(_:ns) = print ns

match3 a@[] = print a
match3 a@(n:_) = print n

match4 a@[] = print a
match4 a@(_:_) = print a

match5 a@[] = print a
match5 a = print a

-- pattern match with case of syntax
mattch4 a = case a of
  a@[] -> print a
  a@(n:_) -> print a


-- head: Use head if the argument is really has a head.
head_empty_list1 = head []
head_list1 = head [1]
head_list2 = head (1:[])
head_list3 = head $ 1:[]

-- tail: Use tail if the argument is really has a tail.
tail_empty_list = tail []
tail_empty_list1 = tail [1]
tail_empty_list2 = tail (1:[])
tail_empty_list3 = tail $ 1:[]


--- map id over a list
map_id = map (\x -> x)
map_id_0 = map_id []
map_id_1 = map_id [1]
map_id_2 = map_id [1,2]

map_id0 = map id []
map_id1 = map id [1]
map_id2 = map id [1,2]


-- [1..] is [1,2,3...]
map_inf = map id [1..]
mapn k  = take k $ map_inf
mapn10 = mapn 10

-- define 2 times with lambda 
x2 = \x -> 2 * x

-- map 2times over a list
map_2times0 = map (\x -> 2 * x) []
map_2times1 = map (\x -> 2 * x) [1]
map_2times2 = map x2 [1]

map_2times_inf = map (\x -> 2 * x) [1..]
map_2times_inf_n n = take n map_2times_inf

-- 2 times can be expressed in diffrent ways
times2_0 = \x -> 2 * x
times2_1 = (*) 2
times2_2 = (2 *)
times2_3 = (* 2)

-- they all can work with map
map_times2_0 = map times2_0
map_times2_1 = map times2_1
map_times2_2 = map times2_2
map_times2_3 = map times2_3

-- filter
filter_even   = filter (\x -> mod x 2 == 0)
filter_prime  = filter isPrime [1..]
  where
    isPrime = undefined

-- meaningless filter 1
filter_True0 = filter (\x -> True) [1..]
filter_True1 = filter (\_ -> True) [1..]
filter_True2 = filter (const True) [1..] -- const:: a -> b -> a return first argument and ignore seecond argument

-- meaningless filter 2
filter_False0 = filter (\x -> False) [1..]
filter_False1 = filter (\_ -> False) [1..]
filter_Flase2 = filter (const False) [1..]

-- map after map
mmap_2xShow1 = (map (\s -> show s)) (map x2 [1..10]) -- \s -> show s == show (eta-conversion)
mmap_2xShow2 = (map show) (map x2 [1..10])
mmap_2xShow3 = (map show .  map x2) [1..10]
mmap_2xShow4 = map show .  map x2 $  [1..10]
mmap_2xShow5 = map (show . x2) [1..10]  -- much shorter than the above examples

-- data constructor :: field name can be a function. P is for Person
data P = P{name::String, birthYear::Int} deriving(Show, Eq) -- check :type for name and birthYear on REPL

dataMatch1 (P n y) = print (n, y)
dataMatch2 t      = print (name t, birthYear t)

dataMatch3 (P "" _) = print "No Name"
dataMatch3 (P n _ ) = print n

dataMatch4 t@P{birthYear = 100} = print "hundred"
dataMatch4 t@P{birthYear = _} = print (t)

tls = [P "name1" 2000, P "name2" 2001]                                
dmap1 = map (\(P n y) -> n) tls
dmap2 = map (\(P n _) -> n) tls
dmap3 = map (\t -> name t) tls              
dmap4 = map name tls              
dmap5 = map (\P{name = n} -> n) tls

-- filter and filter :: super meaningless filter
ff_0 = filter (const False) . filter (const True)


isEven = \x -> x `mod` 2 == 0
-- map and filter
mf_0 = filter isEven . map x2
mf_1 = filter isEven . map (\x -> x2 x + 1)

-- foldr and foldl'
--
--                +----------------------- function  
--                |          +------------ initial value
--                |          |     +------ input list
--                |          |     |                
-- foldr :: (a -> b -> b) -> b -> [a] -> b
-- foldl :: (b -> a -> b) -> b -> [a] -> b

frl0 = foldr (:) [] [1..10]  -- (:) :: a -> [a]-> [a]
fll0 = foldl (flip (:)) [] [1..10]

fr0 = foldr (+) 0 -- [Integer] -> Integer  -- easy
fl0 = foldl (+) 0 -- [Integer] -> Integer  -- easy
fr1 = foldr (-) 0 -- [Integer] -> Integer  -- difficult 
fl1 = foldl (-) 0 -- [Integer] -> Integer  -- difficult

-- foldr and higher order function
-- f :: Integer -> String -> String
f0 a b = show a ++ " " ++ b
fr_0 = foldr f0 "" [1..10]

-- f1 :: Integer -> Integer -> Integer
f1 a b = a * b
fr_1 = foldr f1 1 [1..10]

-- f2 :: P -> Integer -> Integer
f2 p n = max (birthYear p) n
fr_2 = foldr f2 0 ps
ps = [P "a" 1900, P "b" 1920, P "c" 1980, P "d" 1930]

-- f3 :: Integer -> T -> T
data T a = L | T a (T a) (T a) deriving(Show, Eq)
f3 a t =  T a t L
fr_3 = foldr f3 L [1..5] -- construct unbalanced tree

f3' a t = T a L t
fr_3' = foldr f3' L [1..5]

-- f4 :: (Integer -> Integer) -> Integer -> Integer
f4 f i = f i
fr_4 = foldr f4 1 [(1 +), (*2)] -- input can be functions

-- f5 :: P -> [P] -> [P]
f5 p ps = if f p then p:ps else ps
  where
    f p = if birthYear p > 1929 then True else False -- return True iff p is born after 1929
fr_5 = foldr f5 [] ps

-- f6 :: Integer -> (Integer -> Integer) -> (Integer-> Integer)
f6 i f = f . (+ i)           -- 
fr_6 = foldr f6 id [1,2,3,4] -- fr_6 returns a function!

f6' i f = f . (* i)           -- 
fr_6' = foldr f6' id [1,2,3,4] -- fr_6 returns a function!

-- f7 :: (Integer -> Integer) -> T (Integer -> Integer) -> T (Integer -> Integer)
f7 f t = T f t L
fr_7 = foldr f7 L [(*1), (*2), (*3)]

instance Functor T where
  fmap f L = L
  fmap f (T a t1 t2) = T (f a) (fmap f t1) (fmap f t2)

--f::((Integer -> Integer) -> Integer
fr_7'  = fmap (\f -> f 1) fr_7


-- zip
zip0 = zip [] []
zip10 = zip [1] [] :: [(Int, Int)]
zip01 = zip [] [1] :: [(Int, Int)]
zip11 = zip [1] [1]
zip12 = zip [1] [1,2]
zip21 = zip [2,1] [1]

zipnn = zip [1..] [2..]

-- unzip
unzip0 0 = unzip [(a,a+1)| a<-[1..]]


