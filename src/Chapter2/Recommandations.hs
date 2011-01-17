-- |This module implements functions for calculating similarity scores
module Recommendations 
        ( generateCompleteScoreList
        , generateSpecificElementScoreList
        , scoreEuclidean
        , scorePearson
        , scoreJaccard
        , getSampleElements
        ) where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.List

-- |Algebraic data type representing a collection of various sums, that are needed by the score function that uses the Pearson coefficient. 
data Sums       =   Sums {
                        sum1 :: Double,
                        sum2 :: Double,
                        sqrSum1 :: Double,
                        sqrSum2 :: Double,
                        prodSum :: Double
                    } deriving (Show)

data NamePair   =   NamePair {
                        name1 :: String,
                        name2 :: String
                    }

instance Eq NamePair where
    (NamePair xn1 xn2) == (NamePair yn1 yn2)  = ((xn1,xn2) == (yn1,yn2)) || ((xn1,xn2) == (yn2,yn1))

instance Show NamePair where
    show np = "<" ++ name1 np ++ " / " ++ name2 np ++ ">"

-- |Transforms the dataset in a way that it can either be used for user-based filtering (similarity between users) or for item-based filtering (similarity between items)
transform :: M.Map String (M.Map String Double) -> M.Map String (M.Map String Double)
transform elmnts =  foldr (\(itmName, elmName, rating) -> M.insertWith (\newVal oldVal -> M.union newVal oldVal) itmName (M.fromList [(elmName, rating)])) itemsUnique reversedList -- transform the dataset
                    where   itemsUnique = foldr (\currMp acc -> M.union acc (M.fromList [(obj, M.empty) | obj <- M.keys currMp])) M.empty (M.elems elmnts) -- get a map of all the objects in the dataset
                            reversedList = foldr (\(elmName, objList) acc -> [(itmName,elmName,rating) | (itmName, rating) <- M.toList objList] ++ acc) [] (M.toList elmnts) -- flatten the map
                                

-- |Generates a map with all the ratings both parties share
getMutuallyRatings :: M.Map String Double -> M.Map String Double -> M.Map String (Double, Double)
getMutuallyRatings xs ys    =   M.intersectionWithKey getItemRatings xs ys
                                where getItemRatings k rx ry = (rx,ry)

-- |Score function that calculates the similarity score by using the Euclidean distance
scoreEuclidean :: M.Map String Double -> M.Map String Double -> Double
scoreEuclidean xs ys  =   1 / (1 + sqrt (ratingsSum))
                        where ratingsSum = M.fold (\(a,b) acc -> (+) acc $ (a-b)^2) 0 $ getMutuallyRatings xs ys

-- |Score function that calculates the similarity score by using the Jaccard coefficient
scoreJaccard :: M.Map String Double -> M.Map String Double -> Double
scoreJaccard xs ys =    inters / unio
                        where   inters = (fromIntegral . M.size) $ getMutuallyRatings xs ys
                                uni =   if M.size xs > M.size ys then M.union xs ys -- From the Data.Map documentation: 'Hedge-union is more efficient on (bigset `union` smallset).' So, lets make it more efficient
                                        else M.union ys xs
                                unio = (fromIntegral . M.size) $ uni

-- |Score function that calculates the similarity score by using the Pearson product-moment correlation coefficient 
scorePearson :: M.Map String Double -> M.Map String Double -> Double
scorePearson x y =  if den == 0 
                        then 0
                        else (((prodSum sums) - ((sum1 sums) * (sum2 sums) / n)) / den)

                    where   sums    = M.fold (\(a,b) sms -> Sums (sum1 sms + a) (sum2 sms + b) (sqrSum1 sms + (a^2)) (sqrSum2 sms + (b^2)) (prodSum sms + (a*b))) (Sums 0 0 0 0 0) ratings
                            n       = (fromIntegral . M.size) ratings
                            ratings = getMutuallyRatings x y
                            cal     = ((sqrSum1 sums) - ((sum1 sums)^2) / n) * ((sqrSum2 sums) - ((sum2 sums)^2) / n)
                            den     = if cal >= 0 then sqrt cal else 0

-- |Generates a map with the calculated similarity score between each elements (cartesian product). The similarity score is calculated using a defined score function. The map is sorted ascending by the similarity score.
generateCompleteScoreList :: M.Map String (M.Map String Double) -> (M.Map String Double -> M.Map String Double -> Double) -> M.Map Double [NamePair]
generateCompleteScoreList  elmnts scoreFunc =    M.fromListWith (\newVal oldVal -> union newVal oldVal) [(scoreFunc xis yis, [NamePair x y]) | (x,xis) <- lst, (y,yis) <- lst, x /= y]
                                                        where lst = M.toList elmnts

-- |Generates a map with the calculated similarity score between the element with the specified name and all others. The similarity score is calculated using a defined score function. The map is sorted ascending by the similarity score.
generateSpecificElementScoreList :: String -> M.Map String (M.Map String Double) -> (M.Map String Double -> M.Map String Double -> Double) -> M.Map Double [NamePair]
generateSpecificElementScoreList name elmnts scoreFunc =    case M.lookup name elmnts of
                                                                Just itms -> M.fromListWith (\newVal oldVal -> union newVal oldVal) [(scoreFunc itms xis, [NamePair name x]) | (x,xis) <- M.toList elmnts, x /= name]
                                                                Nothing -> M.empty

getSampleElements = M.fromList [
                        ("Lisa Rose", M.fromList [
                            ("Lady in the Water",2.5),
                            ("Snakes on a Plane",3.5),
                            ("Just My Luck",3),
                            ("Superman Returns",3.5),
                            ("You, Me and Dupree",2.5),
                            ("The Night Listener",3)
                        ]),

                        ("Gene Seymour", M.fromList [
                            ("Lady in the Water",3),
                            ("Snakes on a Plane",3.5),
                            ("Just My Luck",1.5),
                            ("Superman Returns",5),
                            ("You, Me and Dupree",3.5),
                            ("The Night Listener",3)
                        ]),

                        ("Michael Phillips", M.fromList [
                            ("Lady in the Water",2.5),
                            ("Snakes on a Plane",3),
                            ("Superman Returns",3.5),
                            ("The Night Listener",4)
                        ]),
     
                        ("Claudia Puig", M.fromList [
                            ("Snakes on a Plane",3.5),
                            ("Just My Luck",3),
                            ("Superman Returns",4),
                            ("You, Me and Dupree",2.5),
                            ("The Night Listener",4.5)
                        ]),

                        ("Mick LaSalle", M.fromList [
                            ("Lady in the Water",3),
                            ("Snakes on a Plane",4),
                            ("Just My Luck",2),
                            ("Superman Returns",3),
                            ("You, Me and Dupree",2),
                            ("The Night Listener",3)
                        ]),

                        ("Jack Matthews", M.fromList [
                            ("Lady in the Water",3),
                            ("Snakes on a Plane",4),
                            ("Superman Returns",5),
                            ("You, Me and Dupree",3.5),
                            ("The Night Listener",3)
                        ]),

                        ("Toby", M.fromList [
                            ("Snakes on a Plane",4.5),
                            ("Superman Returns",4),
                            ("You, Me and Dupree",1)
                        ])
                    ]