module Cluster where

import Data.List
import Data.Maybe
import Debug.Trace

--Points are an x coordinate, a y coordinate, and an integer label.
type Point = (Double, Double, Int)

type Center = Point

--A cluster is a center and a list of points.
type Cluster = (Center, [Point])

-- All undefined values and functions should be completed. Your code will compile and test
-- -- (with the -- test flag) even if some functions are left undefined.
--
-- --                                       Milestone
--

--Given a list of elements and an integer k, return k evenly spaced elements of the list.
--As a first attempt, simply take the first k elements. Fix this after you finish the rest of the
--project.

getKElems :: Int -> [a] -> [a]
getKElems 1 (x:_) = [x] --if k == 1 then return head of list
getKElems k lst= [ val | (val, ind) <- zippedLst, ind `elem` intIndexMap]
  where
    zippedLst = zip lst [1..]
    indexMap = indexesLst 1.0 (fromIntegral (length lst)) k
    intIndexMap = map floor indexMap

indexesLst :: Double -> Double -> Int -> [Double]
indexesLst start end step =
   map (\ i -> start + fromIntegral i * inc) [(0::Int) .. (step - 1)]
     where
       inc = (end - start) / fromIntegral (step - 1)


--Example: getKElems 3 [1..6] = [1,3,6]

--Return the Euclidean distance between two points. You may work in squared distances if you
--prefer.
eucDist :: Point -> Point -> Double
eucDist (x1, y1, _) (x2, y2, _) = sqrt ((x1 - x2) ^ 2 + (y1 - y2) ^ 2)

--Example: eucDist (0,0,10) (1,1,10) < eucDist (0,0,10) (0,2,10)

--Return the Manhattan distance between two points: the distance between the points based on
--strictly horizontal and vertical movement, as if on a city grid.
manhatDist :: Point -> Point -> Double
manhatDist (x1, y1, _) (x2, y2, _) = abs (x1 - x2) + abs (y1 - y2)

--Example: manhatDist (0,0,10) (1,1,10) == manhatDist (0,0,10) (0,2, 10)

--Return the Chebyshev distance between two points: the maximum between the x-distance and the
--y-distance, as if diagonal movement was free, like for a King in chess.
chebyDist :: Point -> Point -> Double
chebyDist (x1, y1, _) (x2, y2, _) = max (abs (x2 - x1)) (abs (y2 - y1))

--Example: chebyDist (0,0,10) (0,5,10) == chebyDist (0,0,10) (5,5,10)

--Return the traffic-aware Manhattan distance: count horizontal distances twice as much as vertical.
trafficDist :: Point -> Point -> Double
trafficDist (x1, y1, _) (x2, y2, _) = 2 * abs (x1 - x2) + abs (y1 - y2)

--Example: trafficDist (0,0,10) (0,10,10) == trafficDist (0,0,10) (5,0,10)

--Return the township-ware Manhattan distance. The label of the point is taken to be the township
--the point is in.  A penalty factor of 2 is applied to the distance between points in different
--townships.
townshipDist :: Point -> Point -> Double
townshipDist (x1, y1, town1) (x2, y2, town2) =
  if town1 == town2
    then manDist
    else 2 * manDist
  where
    manDist = manhatDist (x1, y1, town1) (x2, y2, town2)

--Example: townshipDist (0,0,10) (1,1,20) == 2*townshipDist (0,0,10) (1,1,10)

--Given a list of doubles, compute their average. You may need fromIntegral.
average :: [Double] -> Double
average lst = sum lst / fromIntegral (length lst)

--Example:  average [0,5,10] = 5.0

--Given a ranking function and a list of elements, return the element with the minimum rank.
minimize :: (a -> Double) -> [a] -> a
minimize f = foldl1 (\x y -> if f x < f y then x else y)

--Example: minimize (fromIntegral . length) ["aaaa", "b"] = "b"

--Given a bucketing function, a list of buckets, and a list of items to be placed in buckets,
--and returns the list of bucket, items-in-buckets pairs.
--Go take your old buildCorpus function, copy it, and turn it into a HOF (higher-order function).
-- Bucket is a HOF, because it takes a function as input. You can write it using recursion, other
-- HOFs, or list comprehensions as you choose.
bucket :: Eq b => (a -> b) -> [b] -> [a] -> [(b, [a])]
bucket f buckets items = [(bucket, classifyItemBucket f bucket items) | bucket <- buckets]
  where
    classifyItemBucket :: Eq b => (a -> b) -> b -> [a] -> [a]
    classifyItemBucket f bucket items = [item | item <- items, f item == bucket]

--Example:  bucket length [1..3] ["Hi","my","job","is","fun","!"]
--[(1,["!"]),(2,["Hi","my","is"]),(3,["job","fun"])]
--

--Full project!

--Given a metric, a list of centers, and a point, return the center closest to the point.
--Hint: you've already written a higher-order function to do this.
assignPoint :: (Point -> Center -> Double) -> [Center] -> Point -> Center
assignPoint f centers point = minimize (f point) centers

--Examples: assignPoint eucDist  [(0,0,-1),(5,7,-1)] (5,0,100) = (0.0,0.0,-1)
--          assignPoint trafficDist  [(0,0,-1),(5,7,-1)] (5,0,100) = (5.0,7.0,-1)

--Given a metric, a list of centers, and a list of point, return the clusters, where each point is
--assigned to the nearest center.
--Hint: you've already written a higher-order function to do this.
assignPoints :: (Point -> Center -> Double) -> [Center] -> [Point] -> [Cluster]
assignPoints f centers points = bucket (assignPoint f centers) centers points

--Examples:
--let testClusters = assignPoints trafficDist [(1,1,-1),(2,2,-1)] tenPoints
--
--[(c, length ps) | (c,ps) <- testClusters]
--[((1.0,1.0,-1),1),((2.0,2.0,-1),9)]
--
--testClusters
--[((1.0,1.0,-1),[(1.0,7.0,700)]),
-- ((2.0,2.0,-1),[(7.0,1.0,100),(7.0,3.0,200),(8.0,1.0,300),(8.0,2.0,400),(7.5,3.5,500),
--                (2.0,7.0,600),(3.0,7.0,800),(2.0,8.0,900),(2.0,6.0,1000)])]

--Given a metric and a cluster, return the mean of the points in the cluster.
--The existing center is NOT a point, and should be ignored.
--The label should be the label of the closest point to the new center.
--Since you can't take the mean of an empty list of points, return Nothing in that case.
findMean :: (Point -> Center -> Double) -> Cluster -> Maybe Center
findMean _ (_, []) = Nothing
findMean f (_, points) = Just (avgXs, avgYs, newLabel)
  where
    (sumXs, sumYs, count) = foldl (\(x1, y1 , ct) (x2, y2, _) -> (x1 + x2, y1 + y2, ct + 1))
                              (0, 0, 0) points
    avgXs = sumXs / count
    avgYs = sumYs / count
    (_, _ , newLabel) = foldl (\(x1, y1, lbl) (x2, y2, nLbl) -> if f (x1, y1, lbl) (avgXs, avgYs, lbl) < f (x2, y2, nLbl) (avgXs, avgYs, nLbl) then (x1, y1 , lbl) else (x2, y2, nLbl)) (0, 0, 0) points
    --newMean = (sumXs/count, sumYs/count, count)


--Example: findMean eucDist ((3,3,0), [(0,0,0), (10,10,0), (2,2,1)]) = Just (4.0,4.0,1)

--Given a metric and a list of clusters, relocate all the centers to the mean of their clusters. Be
--sure to return only the valid centers. If any cluster is empty, simply remove it.
moveCenters :: (Point -> Center -> Double) -> [Cluster] -> [Center]
moveCenters f clusters = mapMaybe (findMean f) (filter (not . null) clusters)
--Example:  moveCenters trafficDist testClusters  = [(1.0,7.0,700),(5.166666666666667,4.277777777777778,200)]

--Given a metric, k, and a list of clusters, first move the centers, and then reassign the points
--to the new centers.
--Note that clusters can become empty and disappear. For full credit, replace missing clusters as
--described on the website.
--
improveClusters :: (Point -> Center -> Double) -> Int -> [Cluster] -> [Cluster]
improveClusters f k [(_, [])] = []
improveClusters f k clusters = let points = [ point | (_, pntList) <- clusters, point <- pntList]
                                   potentialClusters = assignPoints f (moveCenters f clusters) points
                                in  if length potentialClusters == k
                                    then potentialClusters 
                                    else improveEmptyClusters f k clusters

improveEmptyClusters :: (Point -> Center -> Double) -> Int -> [Cluster] -> [Cluster]
improveEmptyClusters f k clusters = assignPoints f [cen, last pts] pts ++ remaining
  where
    maxPts = maximum [length pts | (_, pts) <- clusters]
    (c@(cen, pts): _) = [ (ctr, pts) | (ctr, pts) <- clusters, length pts == maxPts]
    remaining = filter (/= c) clusters
                              
--improveClusters :: (Point -> Center -> Double) -> Int -> [Cluster] -> [Cluster]
--improveClusters f k clusters = assignPoints f newCenters points
--  where
--    newCenters = moveCenters f clusters
--    points = [ point | (_, pntList) <- clusters, point <- pntList]


--Example: let newClusters = improveClusters trafficDist 2 testClusters
--[(c, length ps) | (c,ps) <- newClusters]
--[((1.0,7.0,700),5),((5.166666666666667,4.277777777777778,200),5)]

--iterationLimit should be used by kMeans to limit how many times improveClusters can be called.
--Remember variables are not mutable.
iterationLimit = 100

--Given a metric, k, and a list of points, create the initial clusters and repeatedly
--improve them, until they stop changing.
kMeans :: (Point -> Center -> Double) -> Int -> [Point] -> [Cluster]
kMeans f k points = makeClusters f k 0 initCluster
  where
    initCluster = assignPoints f (getKElems k points) points

makeClusters :: (Point -> Center -> Double) -> Int -> Int -> [Cluster] -> [Cluster]
makeClusters _ _ 100 clusters = clusters
makeClusters f k count clusters =
  if newClusters /= clusters
  then makeClusters f k (count + 1) newClusters
  else clusters
    where newClusters = improveClusters f k clusters















