module Exercises where


foldlA f a bs = foldr (\b -> \g -> (\a -> g (f a b))) id bs a

foldlB f a bs = foldr (\a b -> f b a) a bs

foldlC f = flip $ foldr (\a b g -> b (f g a)) id

foldlD = foldr . flip
