module Playground (
    result
)
where

numbers :: [Integer]
numbers = [1, 2, 3, 4, 5, 6]

result :: Integer -> [Integer]
result y = filter (\x -> x < y) numbers