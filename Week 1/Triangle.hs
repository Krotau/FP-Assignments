-- Authors: Bram van den Berg (s1062047) and Tim Smeets (s1065376)
-- Created at Friday September 4th, 2020.


-- spaces and stars code for triangle and christmas tree
oneSpace :: Int -> String
oneSpace 0 = ""
oneSpace n = " " ++ (oneSpace(n-1))

oneAsteriks :: Int -> String
oneAsteriks 0 = "\n"
oneAsteriks n = "*" ++ (oneAsteriks(n-1))

-- Default values:
defaultStartingDepth = 1
defaultOffset = 0

-- Normal Triangle code
-- Maximum_depth is the recursion limit, current_depth is the current recusrion depth and offset is used when drawing the christmas tree so it aligns correctly.
drawTriangle :: Int -> Int -> Int -> String
drawTriangle maximum_depth current_depth offset = 
    if current_depth <= maximum_depth 
        then oneSpace (maximum_depth-current_depth+offset) ++ oneAsteriks(current_depth*2-1) ++ drawTriangle maximum_depth (current_depth+1) offset
        else ""

-- Triangle calls the a helper function with some default parameters and the given height of the triangle as n. n will be the maximum_depth in the drawTriangle function.
triangle :: Int -> String
triangle n = drawTriangle n defaultStartingDepth defaultOffset

-- Christmas Tree Code
-- This function draws a triangle recursively, starting at minimal depth up to the maximum depth. Using an offset the drawn triangles will allign correctly.
drawChristmasTree :: Int -> Int -> String
drawChristmasTree max_depth curr_depth =
    if curr_depth <= max_depth
        then drawTriangle curr_depth 1 (max_depth-curr_depth) ++ drawChristmasTree max_depth (curr_depth+1)
        else ""

christmasTree :: Int -> String
christmasTree n = drawChristmasTree n defaultStartingDepth

