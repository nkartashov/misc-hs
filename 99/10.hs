pack [] = []
pack l = helper [] l
  where
    helper [] (x:xs) = helper [[x]] xs
    helper res [] = reverse res
    helper ((x:xs):xxs) (y:ys)
      | x == y = helper ((y:x:xs):xxs) ys
      | otherwise = helper ([y]:(x:xs):xxs) ys

encode :: (Eq a) => [a] -> [(Int, a)]
encode = map helper . pack
  where 
    helper l@(x:_) = (length l, x)
