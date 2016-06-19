module PowerSet where


import Set



powerSet :: (Ord a) => Set a -> Set (Set a)
powerSet empty = singleton empty

--10mins 
---Powerset Set s = x union y
