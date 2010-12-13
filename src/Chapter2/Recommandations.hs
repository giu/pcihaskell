-- |This module implements functions for calculating a similarity score which are mentioned in the Chapter 2 of 'Programming Collective Intelligence'
module Recommendation 
        ( getEuclideanScores
        , getPearsonScores
        , getJaccardScores
        ) where

import Data.List
import Data.Function

type UserName   =   String
type ItemName   =   String
type Rating     =   Double

data User       =   User {
                        getName :: UserName,
                        getItemRatings :: [ItemRating]
                    } deriving (Show)

data ItemRating =   ItemRating {
                        getItemName :: ItemName,
                        getRating :: Rating
                    } deriving (Show)

data Sums       =   Sums {
                        getSum1 :: Double,
                        getSum2 :: Double,
                        getSqrSum1 :: Double,
                        getSqrSum2 :: Double,
                        getProdSum :: Double
                    } deriving (Show)

-- |Returns the ratings of all the mutually rated movies of User1 (u1) and User2 (u2)
getMutuallyRatings :: User -> User -> [(Rating, Rating)]
getMutuallyRatings u1 u2    = foldl (\acc x -> case find (\y -> getItemName y == getItemName x) (getItemRatings u1) of 
                                                                                                Just t -> (getRating t, getRating x) : acc 
                                                                                                Nothing -> acc) [] (getItemRatings u2)

-- |Generates a list of users with the according similarity score. The latter is calculated using a score function
generateSimilaritiesList :: [User] -> (User -> User -> Double) -> [(UserName, UserName, Double)]
generateSimilaritiesList [] _ = []
generateSimilaritiesList (x:xs) scoreFunc = foldl (\acc cur -> (getName x, getName cur, scoreFunc x cur) : acc) [] (xs) ++ generateSimilaritiesList xs scoreFunc

-- |Score function that calculates the similarity score between two users by using the Euclidean distance
scoreEuclidean :: User -> User -> Double
scoreEuclidean u1 u2  = 1 / (1 + sqrt (sum[(x-y)^2 | (x,y) <- (getMutuallyRatings u1 u2)]))

-- |Score function that calculates the similarity score between two users by using the Pearson product-moment correlation coefficient 
scorePearson :: User -> User -> Double
scorePearson u1 u2      =   if den == 0 
                                then 0
                                else (((getProdSum sums) - ((getSum1 sums) * (getSum2 sums) / n)) / den)
                            where   sums    = foldl (\sms (x,y) -> Sums (getSum1 sms + x) (getSum2 sms + y) (getSqrSum1 sms + (x^2)) (getSqrSum2 sms + (y^2)) (getProdSum sms + (x*y))) (Sums 0 0 0 0 0) ratings
                                    n       = fromIntegral (length ratings)
                                    ratings = getMutuallyRatings u1 u2
                                    cal     = ((getSqrSum1 sums) - ((getSum1 sums)^2) / n) * ((getSqrSum2 sums) - ((getSum2 sums)^2) / n)
                                    den     = if (cal >= 0) then sqrt cal else 0

-- |Score function that calculates the similarity score between two users by using the Jaccard coefficient
scoreJaccard :: User -> User -> Double
scoreJaccard u1 u2 =    inters / unio
                        where   inters = (fromIntegral . length) $ getMutuallyRatings u1 u2
                                unio = (fromIntegral . length)  $ unionBy (\a b -> (getItemName a) == (getItemName b)) (getItemRatings u1) (getItemRatings u2)

-- |Reverses the sort order
sortDesc :: (Ord c) => (a, b, c) -> (d, e, c) -> Ordering
sortDesc (_,_,x) (_,_,y)    |   x > y = LT
                            |   x < y = GT
                            |   otherwise = EQ

-- |Generates a list with the similarity scores between all users
getScores :: (User -> User -> Double) -> [(UserName, UserName, Double)]
getScores scoreFunc = sortBy sortDesc (generateSimilaritiesList critics scoreFunc)

-- |Generates a list with the similarity scores between all users by using the Euclidean distance
getEuclideanScores :: [(UserName, UserName, Double)]
getEuclideanScores = getScores scoreEuclidean

-- |Generates a list with the similarity scores between all users by using the Pearson product-moment correlation coefficient 
getPearsonScores :: [(UserName, UserName, Double)]
getPearsonScores = getScores scorePearson

-- |Generates a list with the similarity scores between all users by using the Jaccard coefficient
getJaccardScores :: [(UserName, UserName, Double)]
getJaccardScores = getScores scoreJaccard


-- |Sample list of users and the appertaining movie ratings
critics = 
                [
                    User "Lisa Rose" [
                        ItemRating "Lady in the Water" 2.5,
                        ItemRating "Snakes on a Plane" 3.5,
                        ItemRating "Just My Luck" 3,
                        ItemRating "Superman Returns" 3.5,
                        ItemRating "You, Me and Dupree" 2.5,
                        ItemRating "The Night Listener" 3
                    ],

                    User "Gene Seymour" [
                        ItemRating "Lady in the Water" 3,
                        ItemRating "Snakes on a Plane" 3.5,
                        ItemRating "Just My Luck" 1.5,
                        ItemRating "Superman Returns" 5,
                        ItemRating "You, Me and Dupree" 3.5,
                        ItemRating "The Night Listener" 3
                    ],

                    User "Michael Phillips" [
                        ItemRating "Lady in the Water" 2.5,
                        ItemRating "Snakes on a Plane" 3,
                        ItemRating "Superman Returns" 3.5,
                        ItemRating "The Night Listener" 4
                    ],

                    User "Claudia Puig" [
                        ItemRating "Snakes on a Plane" 3.5,
                        ItemRating "Just My Luck" 3,
                        ItemRating "Superman Returns" 4,
                        ItemRating "You, Me and Dupree" 2.5,
                        ItemRating "The Night Listener" 4.5
                    ],

                    User "Mick LaSalle" [
                        ItemRating "Lady in the Water" 3,
                        ItemRating "Snakes on a Plane" 4,
                        ItemRating "Just My Luck" 2,
                        ItemRating "Superman Returns" 3,
                        ItemRating "You, Me and Dupree" 2,
                        ItemRating "The Night Listener" 3
                    ],

                    User "Jack Matthews" [
                        ItemRating "Lady in the Water" 3,
                        ItemRating "Snakes on a Plane" 4,
                        ItemRating "Superman Returns" 5,
                        ItemRating "You, Me and Dupree" 3.5,
                        ItemRating "The Night Listener" 3
                    ],

                    User "Toby" [
                        ItemRating "Snakes on a Plane" 4.5,
                        ItemRating "Superman Returns" 4,
                        ItemRating "You, Me and Dupree" 1
                    ]
                ]
