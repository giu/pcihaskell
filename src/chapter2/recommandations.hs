{- 
    Programming Collective Intelligence
    Chapter 2: Making Recommendations
-}
module Recommendation (getEuclideanScores, getPearsonScores) where
    import Data.List
    import Data.Function

    type Name       =   String
    type MovieName  =   String
    type Rating     =   Double

    data Person     =   Person {
                            getName :: Name,
                            getMovieRatings :: [MovieRating]
                         } deriving (Show)

    data MovieRating =  MovieRating {
                            getMovieName :: MovieName,
                            getRating :: Rating
                        } deriving (Show)

    data Sums       =   Sums {
                            getSum1 :: Double,
                            getSum2 :: Double,
                            getSqrSum1 :: Double,
                            getSqrSum2 :: Double,
                            getProdSum :: Double
                        } deriving (Show)

    {-Sample list of persons and appertaining movie ratings-}
    critics = 
                    [
                        Person "Lisa Rose" [
                            MovieRating "Lady in the Water" 2.5,
                            MovieRating "Snakes on a Plane" 3.5,
                            MovieRating "Just My Luck" 3,
                            MovieRating "Superman Returns" 3.5,
                            MovieRating "You, Me and Dupree" 2.5,
                            MovieRating "The Night Listener" 3
                        ],

                        Person "Gene Seymour" [
                            MovieRating "Lady in the Water" 3,
                            MovieRating "Snakes on a Plane" 3.5,
                            MovieRating "Just My Luck" 1.5,
                            MovieRating "Superman Returns" 5,
                            MovieRating "You, Me and Dupree" 3.5,
                            MovieRating "The Night Listener" 3
                        ],

                        Person "Michael Phillips" [
                            MovieRating "Lady in the Water" 2.5,
                            MovieRating "Snakes on a Plane" 3,
                            MovieRating "Superman Returns" 3.5,
                            MovieRating "The Night Listener" 4
                        ],

                        Person "Claudia Puig" [
                            MovieRating "Snakes on a Plane" 3.5,
                            MovieRating "Just My Luck" 3,
                            MovieRating "Superman Returns" 4,
                            MovieRating "You, Me and Dupree" 2.5,
                            MovieRating "The Night Listener" 4.5
                        ],

                        Person "Mick LaSalle" [
                            MovieRating "Lady in the Water" 3,
                            MovieRating "Snakes on a Plane" 4,
                            MovieRating "Just My Luck" 2,
                            MovieRating "Superman Returns" 3,
                            MovieRating "You, Me and Dupree" 2,
                            MovieRating "The Night Listener" 3
                        ],

                        Person "Jack Matthews" [
                            MovieRating "Lady in the Water" 3,
                            MovieRating "Snakes on a Plane" 4,
                            MovieRating "Superman Returns" 5,
                            MovieRating "You, Me and Dupree" 3.5,
                            MovieRating "The Night Listener" 3
                        ],

                        Person "Toby" [
                            MovieRating "Snakes on a Plane" 4.5,
                            MovieRating "Superman Returns" 4,
                            MovieRating "You, Me and Dupree" 1
                        ]
                    ]

    {- Returns the ratings of all the mutually rated movies of person 1 (p1) and person 2 (p2) -}
    getMutuallyRatings          :: Person -> Person -> [(Rating, Rating)]
    getMutuallyRatings p1 p2    = foldl (\acc x -> case find (\y -> getMovieName y == getMovieName x) (getMovieRatings p1) of 
                                                                                                    Just t -> (getRating t, getRating x) : acc 
                                                                                                    Nothing -> acc) [] (getMovieRatings p2)

    {-Generates a list of persons with the according similarity score. The latter is calculated using a score function-}
    generateSimilaritiesList                    ::  [Person] -> ([(Rating, Rating)] -> Double) -> [(Name, Name, Double)]
    generateSimilaritiesList [] _               =   []
    generateSimilaritiesList (x:xs) scoreFunc   =   foldl (\acc cur -> (getName x, getName cur, scoreFunc (getMutuallyRatings x cur)) : acc) [] (xs) ++ generateSimilaritiesList xs scoreFunc



    {-Uses the Euclidean Distance for the calculation of a similarity score-}
    scoreEuclidean          :: [(Rating, Rating)] -> Double
    scoreEuclidean ratings  = 1 / (1 + sqrt (sum[(x-y)^2 | (x,y) <- ratings]))

    {-Uses the Pearson product-moment correlation coefficient for the calculation of a similarity score-}
    scorePearson            :: [(Rating, Rating)] -> Double
    scorePearson ratings    |   null ratings = 0
                            |   otherwise = if den == 0 
                                                then 0
                                                else (((getProdSum sums) - ((getSum1 sums) * (getSum2 sums) / n)) / den)
                                where   sums    = foldl (\sms (x,y) -> Sums (getSum1 sms + x) (getSum2 sms + y) (getSqrSum1 sms + (x^2)) (getSqrSum2 sms + (y^2)) (getProdSum sms + (x*y))) (Sums 0 0 0 0 0) ratings
                                        n   = fromIntegral (length ratings)
                                        cal = ((getSqrSum1 sums) - ((getSum1 sums)^2) / n) * ((getSqrSum2 sums) - ((getSum2 sums)^2) / n)
                                        den = if (cal >= 0) then sqrt cal else 0

    {- Reverse sort order -}
    sortDesc (_,_,x) (_,_,y)    |   x > y = LT
                                |   x < y = GT
                                |   otherwise = EQ

    getScores           :: ([(Rating, Rating)] -> Double) -> [(Name, Name, Double)]
    getScores scoreFunc = sortBy sortDesc (generateSimilaritiesList critics scoreFunc)

    getEuclideanScores :: [(Name, Name, Double)]
    getEuclideanScores = getScores scoreEuclidean

    getPearsonScores :: [(Name, Name, Double)]
    getPearsonScores = getScores scorePearson
