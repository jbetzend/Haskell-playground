-- ######################################################
-- #							#
-- #	== WichtelWizard ==				#
-- # 							#
-- #	A small haskell programm to manage a round of	#
-- #	secret santa without all participants having	#
-- #	to be at one place all at one.			#
-- #							#
-- #	All participants recieve an e-mail with a 	#
-- #	short message and the person they are supposed	#
-- #	to get a present for.				#
-- #							#
-- #	Written by Jonas Betzendahl, 2012		#
-- #	Copyright CC0 / Public Domain			#
-- #							#
-- ######################################################

{-# LANGUAGE OverloadedStrings #-}

module SecretSanta
where

import Network.Mail.SMTP
import System.Random
import Data.Text
import Data.Text.Lazy
import System.IO.Unsafe

-- A secret santa is a Name and an e-mail-adress, so two strings.
type Participant = (String, String)

textPack = Data.Text.pack
lazyPack = Data.Text.Lazy.pack

p_head = Prelude.head
p_drop = Prelude.drop 

-- Define your mail server and port here. These should be constant for all mails.
host 	= "exaple mail server"
port	= 25

-- All Participants still need to be hardcoded. Right here.
herp = ("Herp Derpington", "Herp@tld.com")
derpina = ("Derpina McHerpsome", "derpina.mcherpsome@tld.com")

-- A list of all Participants.
secretsantas = [herp, derpina]

-- Gives a random element of a given list.
-- WATCH OUT: Uses unsafePerformIO
randomElement :: [a] -> a
randomElement as = p_head $ p_drop x as 
	where 
	x = unsafePerformIO $ randomRIO (0, (Prelude.length as) - 1)

-- Randomizes a given list using randomElement from above.
shuffleList :: (Eq a) => [a] -> [a]
shuffleList [] = []
shuffleList xs = a:(shuffleList $ Prelude.filter (/=a) xs) 
	where
	a = randomElement xs 

-- Takes a list of Participants (should be shuffled) and matches them.
-- The last element gets the first one, the first the second and so on...
output :: [Participant] -> IO()
output all@(a:as) = do
		let l = Prelude.last as
		putStrLn $ "Sending mail to " ++ (fst l)
		shootMail l a
		output' all
		where
		output' (a:b:cs)
			| (cs == []) = do shootMail a b
			| otherwise = do
				putStrLn $ "Sending mail to " ++ (fst a) 
				shootMail a b
				output' (b:cs)
		
		output' _ = error "Something went wrong!"

-- Writes an e-mail from the (still hardcoded) from adress to wichteler, informing
-- him/her that he/she is supposed to get a present for wichtelee.
shootMail :: Participant -> Participant -> IO()
shootMail wichteler wichtelee = do			
				let from    = Address (Just "Santa-O-Matic 9000") "email@domain.com"
				let to      = [Address (Just $ textPack $ fst wichteler) (textPack $ snd wichteler)]
				let subject = "[automated] Your secret santa"
				let text    = Data.Text.Lazy.concat [ "Hello ", (lazyPack $ fst wichteler), "!\n\nDo not be alarmed! I am merely an automated e-mail from a\nprogram that allows to pair up people for a round of secret santa\nwithout them all having to be in one plave.\nThe random number generator\nhas been shaken well and determined that you are supposed to\nget a present for ", (lazyPack $ fst wichtelee), ".\nI wish you the best of lick and a lot of fun while playing!\n\nHappy holidays and a great start into the new year!\nYour Santa-O-Matic-9000 mail"]

				let body    = plainTextPart text
				let mail    = simpleMail from to [] [] subject [body]
 
				sendMail host port mail


main :: IO()
main = 	do

	putStrLn "Shaking random-number generator and sending e-mails ..."

	let verteilung = shuffleList secretsantas
	output verteilung

	putStrLn " all done!"
