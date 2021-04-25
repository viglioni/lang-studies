
# Table of Contents

1.  [sum the numbers between two integers recursively](#org3d4c474)
2.  [define a square function](#org7474fb5)
3.  [sum squares between two integers](#orgb30b90f)
4.  [higher order sum](#org8999cd5)
5.  [Define the square sum in terms of higherOrderSum](#org9843dac)
6.  [Define the sum between two values in terms of higherOrderSum](#org7f966c4)
7.  [Generalise over the function provided by sumInts](#org4596fc9)
8.  [Define a factorial method using the generalized func](#orgc06e129)



<a id="org3d4c474"></a>

# sum the numbers between two integers recursively

-   Example:
    -   sumInts 0 1 = 1
    -   sumInts 1 3 = 6

    :{
    sumInts :: Int -> Int -> Int
    sumInts a b =
      if a == b
        then b
        else (+ a) $ (sumInts (a + 1) b)
    :}
    
    map (\[a,b] -> sumInts a b) [[0, 1] , [1, 3], [1,5], [2,10]]         

    [1,6,15,54]


<a id="org7474fb5"></a>

# define a square function

    square :: Int -> Int
    square x = x*x
    
    map square [1..9]

    [1,4,9,16,25,36,49,64,81]


<a id="orgb30b90f"></a>

# sum squares between two integers

    :{
    sumSquares :: Int -> Int -> Int
    sumSquares a b =
      if a == b
        then b
        else (square a +) $ (sumSquares (a + 1) b)
    :}
    
    map (sumSquares 1) [1..10]

    [1,3,8,18,35,61,98,148,213,295]


<a id="org8999cd5"></a>

# higher order sum

-   Define a higher order sum function which accepts an (Int -> Int) function to apply to all integers between two values.
-   Again this should look similar to the sumInts and sumSquares functions
-   `higherOrderSum \:: (Int -> Int) -> Int -> Int -> Int`

Applies a function to each element and sum them:

    :{
    higherOrderSum :: (Int -> Int) -> Int -> Int -> Int
    higherOrderSum func a b =
      if a == b
        then b
        else (func a +) $ (higherOrderSum func (a + 1) b)
    :}
    
    map (\fn -> higherOrderSum fn 1 10) [(+ 1), (* 2), square]

    [64,100,295]


<a id="org9843dac"></a>

# Define the square sum in terms of higherOrderSum

-   **~hoSumSquares:** Int -> Int -> Int~

    hoSumSquares :: Int -> Int -> Int
    hoSumSquares = higherOrderSum square
    
    map (hoSumSquares 0) [1..10]

    [1,3,8,18,35,61,98,148,213,295]


<a id="org7f966c4"></a>

# Define the sum between two values in terms of higherOrderSum

-   Note there is no parameter on the function definition
-   Try to use a lambda if possible
-   **~hoSumInts:** Int -> Int -> Int~

    hoSumInts :: Int -> Int -> Int
    hoSumInts = higherOrderSum (+ 0)
    
    map (hoSumInts 0) [1..10]

    [1,3,6,10,15,21,28,36,45,55]


<a id="org4596fc9"></a>

# Generalise over the function provided by sumInts

-   **That is, apply ~fn:** Int -> Int -> Int~ between a and b

    :{
    applyToRange :: (Int -> Int -> Int) -> Int -> Int -> Int
    applyToRange func a b =
      if a == b 
        then b
        else (func a) $ (applyToRange func (a + 1) b)
    :}
    
    map (\fn -> applyToRange fn 2 10) [(+) , (*)]

    [54,3628800]


<a id="orgc06e129"></a>

# Define a factorial method using the generalized func

-   **~hoFactorial:** Int -> Integral~

    hoFactorial :: Int -> Integral
    hoFactorial = applyToRange (*) 1
    
    map hoFactorial [1..15]

    [1,2,6,24,120,720,5040,40320,362880,3628800,39916800,479001600,6227020800,87178291200,1307674368000]

