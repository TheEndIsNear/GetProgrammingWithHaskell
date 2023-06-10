sumSquareOrSquareSum x y = (\sumSquare squareSum ->
  if sumSquare > squareSum
     then sumSquare
     else squareSum) (x^2 + y^2) (x+y)^2

doubleDoubles x = (\dubs -> dubs*2) (x*2)

--overwrite x = let x = 2
--  in
--    let x = 3
--      in
--        let x = 4
--          in
--          x

overwrite x = (\x -> (\x -> (\x -> x) 4) 3) 2 
