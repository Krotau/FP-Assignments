-- Authors: Bram van den Berg (s1062047) and Tim Smeets (s1065376)
-- Created at Friday September 4th, 2020.

module Shapes
where

data Shape
  =  Circle Double            -- radius
  |  Square Double            -- length
  |  Rectangle Double Double  -- length and width
  deriving (Show)

-- testing data and references for explanation.
var1 = Circle 3.0
var2 = Square 2.0
var3 = Rectangle 3.0 4.0

-- printing info for in the console.
showShape :: Shape -> String
showShape (Circle r)       =  "circle of radius " ++ show r
showShape (Square l)       =  "square of length " ++ show l
showShape (Rectangle l w)  =  "rectangle of length " ++ show l ++ " and width " ++ show w

-- area        :: Shape -> Double
area :: Shape -> Double
area(Circle r) = pi * r*r -- this is the equation for calculating the surface area of a cirlce.
area(Square l) = l * l -- the surface area of a square is the length times eachother.
area(Rectangle l w) = l * w -- the surface area of a rectangle is its length times its width.

-- perimeter   :: Shape -> Double
-- A perimeter is the sides of shape summed together.
perimeter :: Shape -> Double
perimeter(Circle r) = 2 * pi * r -- 2pi r is the equation for the perimeter of a circle
perimeter(Square l) = 4 * l -- the perimeter of a square is the length of a side times 4 (a square has 4 sides.)
perimeter(Rectangle l w) = 2 * l + 2 * w -- Since it is a rectangle, the length and width each have a pair on the opposite side. A rectangle has 4 sides.

-- center       :: Shape -> (Double, Double)  -- x- and y-coordinates
-- for reference to the shape examples used in the explanation, see var1, var2 and var3.
center :: Shape -> (Double, Double)
center(Circle r) = (0, 0) -- (r, r) is also possible, depends where the circle is drawn on the graph, ex. the origin is at (r,r) or (0, 0).
center(Square l) = (l/2, l/2) -- The square starts at (0,0) and ends at (2,2) so the center is halfway it length to the right and up.
center(Rectangle l w) = (w/2, l/2) -- The rectangle start at (0,0) and ends at (4,3) so the center is halfway its length and width at (2;1,5)

-- boundingBox  :: Shape -> (Double, Double)  -- width and height
-- A boundingbox is the biggest possible rectangle the encapsulates an entie shape.
boundingBox :: Shape -> (Double, Double)
boundingBox(Circle r) = (r*2, r*2) -- r is the radius, so the diameter is 2 times r, which is also the perfect fit for the boundingbox sides around a circle.
boundingBox(Square l) = (l, l) -- a boundingbox around a square is itself.
boundingBox(Rectangle l w) = (w, l) -- a boundingbox around a rectangle is itself.
