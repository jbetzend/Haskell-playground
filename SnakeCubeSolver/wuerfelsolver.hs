-- ######################################################
-- #							#
-- #	Snake Cube Solver				#
-- #	programmed in Hasekll by Jonas Betzendahl	#
-- #		jbetzend@techfak.uni-bielefeld.de	#
-- #							#
-- #	Short intro:					#
-- #	http://en.wikipedia.org/wiki/Snake_cube		#
-- #							#
-- #	License:	Public Domain (CC0), 2012	#
-- #							#
-- ######################################################

module Main
where

import System.Environment
import Control.Parallel

-- 	## TODOS:	1. Sort all possible results by human-reachableness
--			2. Add High-Performance magic and/or parralelisation.
--			3. Make program recognize size automatically.

-- 	## USAGE:	If you want to solve your own cube, define it as a list of Vecs, write it down like the examples below (as a list of Vecs),
-- 			start this program up in ghci and then call "solve [your cube here] [dimension]" where [dimenson] is the size of you cube (a 3x3x3 cube has a size of 3)

type Vec 	= (Integer, Char)		-- I could have used standard vectors here but I found this to be way more understandable and also easier to compute.
type Vecs	= [Vec]

type Point 	= (Integer, Integer, Integer)	-- A point in 3D space is still a triplet of Integer values. 
type Points	= [Point]

type Container 	= (Vecs, Points)
type Containers = [Container]

-- {{{ // solving functions //

solve :: Vecs -> Integer -> [Vecs]
solve all@(x:y:xs) d
	| not (isvalidentry all d)		= error "Please enter a valid list of Vecs that make a cube."
	| otherwise 				= allsolutions [x:y:[]] (xs) d
	where 
		allsolutions :: [Vecs] -> Vecs -> Integer -> [Vecs]
		allsolutions xs [] _ = xs
		allsolutions xs (y:ys) d = allsolutions (head (filter (valid d) [ p ++ [q]| p <- xs, q <- possiblenexts (last p) (y) ])) (ys) (d)

		-- Parameter for better understanding:
		-- Containers:	All pathways up to this point and their respective cloud of points, so we don't have to generate those over and over.
		-- Vecs:	All Vecs that are yet to be modeled into the paths.
		-- Integer:	The length of one side of the cube to be (e.g. 3,4,5)
		-- [Vecs]:	All possible valid solutions (minus symmetries).	

		--allsols :: Containers -> Vecs -> Integer -> [Vecs]
		--allsols xs [] _ = getpaths xs
		--allsols xs (y:ys) d = allsols (givevalids [((fst cont) ++ g, bs ++ makenewpoints (last cont) g) | cont <- xs, g <- posnexts (snd cont) (y)]) (ys) (d)	

		valid :: Integer -> Vecs -> Bool
		valid d xs = validpoints d (generatepoints xs [(0,0,0)])
		
		--where
		--	givevalids :: Integer -> Containers -> Containers
		--	givevalids dim xs = filter (validpoints dim) [snd c | c <- xs]
 
-		--	getpaths :: Containers -> [Vecs]
		--	getpaths []	    = []
		--	getpaths ((a,_):xs) = a:(getpaths xs) 

							-- "Holy list comprehension, batman! What is this monstrosity?"
							--
							-- Well, it goes a little something like this:
							-- You have a bunch of lists of vecs, representing all possible valid solutions up to this point. (xs)
							-- We also have all Vecs that still need to be deployed in a list. (y:ys)
							-- 
							-- Now, for each element in xs, compute the next possible vecs and add the ones that add to valid paths.
							-- Use all of this to compute the next step. Break if you have no Vecs left that can be next.

-- // Takes two Vecs, the first being the last vector (to determine possible axes) and the second being the current one (to determine magnitude)
-- // While programming, I found this the more viable than rotation matrices out of reasons of readabillity and computing speed. 

-- possiblenexts :: Vecs -> Vec -> Vecs	
-- possiblenexts xs (a,_)
-- 	| (hasyz xs)	=
--	| (hasy 

possiblenexts (_,'x') (a,_) = [(a,'y'),((-a),'y'),(a,'z'),((-a),'z')] 
possiblenexts (_,'y') (a,_) = [(a,'x'),((-a),'x'),(a,'z'),((-a),'z')] 
possiblenexts (_,'z') (a,_) = [(a,'y'),((-a),'y'),(a,'x'),((-a),'x')] 

-- // Generates all points in 3D Space that are hit if you follow the given list of Vecs, starting at the last entry of the given list of points.
-- // Also some dirty hacks that no haskell programmer should be proud of.

generatepoints :: Vecs -> Points -> Points
generatepoints xs [] = generatepoints xs [(0,0,0)] 
generatepoints [] ys = ys
generatepoints (x:xs) ys = generatepoints xs (ys ++ (newpoints (last ys) x))
		where
			-- Welcome to HaskellMagic!
			newpoints :: Point -> Vec -> Points
			newpoints (a,b,c) (0,e)	= []
			newpoints (a,b,c) (d,e)
                                | (e == 'x')    = ((a+(f d)),b,c):(newpoints ((a+(f d)),b,c) ((d-(f d)),e))
                                | (e == 'y')    = (a,(b+(f d)),c):(newpoints (a,(b+(f d)),c) ((d-(f d)),e))
                                | (e == 'z')    = (a,b,(c+(f d))):(newpoints (a,b,(c+(f d))) ((d-(f d)),e))
				where 
					f d | (d >= 0) = 1 | (d < 0) = -1 
			
-- }}}

-- {{{ // checking functions //

-- Takes a set of points (generates by "generatepoints", see above) and returns True if
-- these points still could belong to a valid cube, when they grow up.
validpoints :: Integer -> Points -> Bool
validpoints d [a]						= True
validpoints d all@(x:y:xs)
	| (elem x (y:xs))					= False				-- No element is allowed to appear twice.
	| (distxyz all >= d)					= False				-- The distance between the two points that are the farthest apart may only be so large. 
	| otherwise 						= validpoints (y:xs) d		-- Recursive call of this function to the rest of the list
	where
		-- Gives the biggest distance between the farthest points on all three axes.  
		distxyz :: Points -> Integer
		distxyz xs = maximum [(minmax (xlist xs)), (minmax (ylist xs)), (minmax (zlist xs))]

		minmax hs = (maximum hs) - (minimum hs)

		-- List all first / second / thrid elements out of a list of triplets.
		xlist xs = ([tripfst l | l <- xs ])
		ylist xs = ([tripsnd m | m <- xs ])
		zlist xs = ([triptrd n | n <- xs ])

		-- Read the first, second or thrird element of a triplet.
		tripfst (a,b,c) = a
		tripsnd (a,b,c) = b
		triptrd (a,b,c) = c

-- // Checks the validity of a given Vec, belonging to a NxNxN cube, where N is also given to the function.

isvalidvector :: Vec -> Integer -> Bool
isvalidvector (a, b) d
	| (abs(a) > (d-1))					= False		-- Not a valid vector in our sense if it's longer than two.
	| ((b /= 'x') && (b /= 'y') && (b /= 'z'))		= False		-- Needs to be on the x,y or z axis
	| otherwise 						= True		-- If none of the above, this is a valid vector.

-- // Check a whole list of Vecs to wether or not they add up to a structure that still can become a valid cube.

isvalidentry :: Vecs -> Integer -> Bool
isvalidentry w d
	| (sum([abs(fst vs) | vs <- w]) /= ((d^3)-1)) 		= False 	-- The (-1) is necessary to compensate for the fact that we are dealing in "distance from starting point".
	| otherwise 						= checkvecs w d
	where
		checkvecs ws d
			| (ws == [])				= True
			| (isvalidvector (head ws) d)		= checkvecs (tail ws) d
			| otherwise				= False
-- }}}

-- {{{ // Predefined cubes //

saschas_cube = [(2,'x'), (-2,'y'), (2,'x'), (-2,'y'), (1,'x'), (-1,'y'), (1,'x'), (-2,'y'), (2,'x'), (-1,'y'), (1,'x'), (-2,'y'), (1,'x'), (-2,'y'), (1,'x'), (-1,'y'), (2,'x')]

daniels_cube = [(2,'x'), (1,'y'), (2,'x'), (1,'y'), (1,'x'), (3,'y'), (1,'x'), (2,'y'), (1,'x'), (2,'y'), (1,'x'), (2,'y'), (1,'x'), (1,'y'), (1,'x'), (1,'y'), (1,'x'), (1,'y'), (1,'x'), (1,'y'), (2,'x'), (2,'y'), (1,'x'), (1,'y'), (1,'x'), (1,'y'), (1,'x'), (2,'y'), (3,'x'), (1,'y'), (1,'x'), (1,'y'), (3,'x'), (1,'y'), (2,'x'), (1,'y'), (1,'x'), (1,'y'), (1,'x'), (1,'y'), (1,'x'), (1,'y'), (1,'x'), (1,'y'), (3,'x'), (1,'y')]

-- }}}

givestufftoprint :: String -> [Vecs]
givestufftoprint s
	| (s == "d")	= (solve daniels_cube 4)
	| (s == "s")	= (solve saschas_cube 3)
	| otherwise 	= error "Invalid command line arguments. Please hand 'd' or 's'."

main :: IO()
main =	do
		args <- getArgs				-- getting command line arguments	
		putStr "Solving cube ... "

		let a = (givestufftoprint (head args))
		print a
