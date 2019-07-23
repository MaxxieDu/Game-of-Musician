{-
    File:           Proj1.hs
    Author:     Zeyu Du
    ID:             952331
    Subject:    COMP90048 Declarative Programming
    Purpose:   A program for  a two-player logical guessing game of musician.
    Detail :      The program performs the role of the performer and guesses 
                      the correct 3-pitch chord while receiving feedback compared with 
                      the target chord. The next guess is based on current Gamestate. 
-}

module Proj1 (Pitch, toPitch, feedback, GameState, initialGuess, nextGuess) where

import Data.List

{- Define the type of data Pitch , enumerate all the data value, and use the show 
    show function to present Pitch as a  two-character string.
-}
data Pitch = A1|A2|A3|B1|B2|B3|C1|C2|C3|D1|D2|D3|E1|E2|E3|F1|F2|F3|G1|G2|G3
    deriving(Read,Eq)
instance Show Pitch where 
        show a | a==A1 = "A1"|a==A2 = "A2"|a==A3 ="A3"
                     | a==B1 = "B1"|a==B2 = "B2"|a==B3 = "B3"
                     | a==C1 = "C1"|a==C2 = "C2"|a==C3 = "C3" 
                     | a==D1 = "D1"|a==D2 ="D2"|a==D3 = "D3"
                     | a==E1 = "E1" |a==E2 = "E2"  |a==E3 = "E3"
                     | a==F1 = "F1" |a==F2 = "F2"  |a==F3 = "F3"
                     | a==G1 = "G1"|a==G2 ="G2"|a==G3 = "G3"
                  
-- The toPitch function convert a  valid Pitch into a string and
-- an invalid input to Nothing.
toPitch :: String -> Maybe Pitch
toPtich [ ] = Nothing
toPitch s  | (elem (head s) ['A'..'G']) &&( elem (read (tail s) :: Int) [1,2,3] )
                    && length s==2 = Just ((read s)::Pitch)
                 | otherwise = Nothing

-- Gamestate stores a list of potential chords. Each chord is a list of 3 pitches.
data GameState = GameState [[Pitch]]
        deriving (Show, Eq)

--An initial guess is provided , and also the initial Gamestate.
--It takes no input argument.
initialGuess :: ([Pitch], GameState)
initialGuess = ([A1,B1,C2], GameState (initialGameState 3 pitchlist) ) 
   where pitchlist =[A1,A2,A3,B1,B2,B3,
                                C1,C2,C3,D1,D2,D3,
                                E1,E2,E3,F1,F2,F3,G1,G2,G3]

--This function returns a list of all the possible chords as initial Gamestate.
initialGameState :: Int -> [Pitch] -> [[Pitch]]
initialGameState 0 _                = [[]]
initialGameState _ []                = []
initialGameState  r xs@(x:xs') = [x:ys | ys <- initialGameState (r-1 )xs'] ++ 
                                                       initialGameState r xs'

--A function of converting a chord(a list of 3 pitches) into a list of strings.
pitchtoString :: [Pitch] -> [String] 
pitchtoString [a,b,c] = [show a, show b, show c] 

--This function compare the target pitches with guess pitches.
--It returns the number of identical pitches. 
comparePitch :: [Pitch] -> [Pitch] -> Int
comparePitch [] _ = 0
comparePitch _ [] = 0
comparePitch [x,y,z] (g:gs) | x==g|| y ==g || z == g 
                                                 =1+ comparePitch [x,y,z] gs
                                              | otherwise = 0 + comparePitch [x,y,z] gs

-- The feedback function takes target chord and guess chord as input.
-- The output is a triple with numbers of correct pitchs, notes and octaves
feedback :: [Pitch]-> [Pitch] -> (Int,Int,Int)
feedback target guess = (pitch, note, octave)
            where  
                 num              = length (pitchtoString guess)             
                 pitch             = comparePitch target guess
                 note              = num - length (targetnote \\ guessnote) - pitch
                 octave           = num - length (targetoctave \\ guessoctave) - pitch
                 guessnote    = map (\x -> [x !! 0]) (pitchtoString guess)
                 targetnote    = map (\x -> [x !! 0]) (pitchtoString target)
                 guessoctave = map (\x -> [x !! 1]) (pitchtoString guess)
                 targetoctave = map (\x -> [x !! 1]) (pitchtoString target)


--Define a function to see if the feedback score is the same.
compareScore :: (Int, Int, Int) -> (Int, Int, Int) -> Bool
compareScore (a, b, c) (x, y, z) = a==x && b==y && c==z

{- This function delete all the chords that its feedback with previous guess 
    does not match current score from list of chords.
-}
deleteChord :: [[Pitch]] -> [Pitch] -> (Int, Int, Int) -> [[Pitch]]
deleteChord (x:xs) previousg score
      | compareScore (feedback x previousg) score =
                            x : deleteChord xs previousg score
      | otherwise = deleteChord xs previousg score
    

{- This function takes input as a pair of the previous guess and game state, and 
    the feedback to this guess as a triple of correct pitches, notes, and octaves, 
    and returns a pair of the next guess and new game state consisting of updated chords.
-}
nextGuess :: ([Pitch], GameState) -> (Int, Int, Int) -> ([Pitch], GameState)
nextGuess (pg, GameState old) score = (nextg, newGameState) where
        newGameState = GameState updatedChords
        updatedChords = deleteChord old pg score
        nextg = updatedChords !! 0















