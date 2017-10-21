

-- The c function takes an Int and an array of Strings and return an int 
-- as well as printing Strings
foreign import ccall "my_func" myFunc :: Int -> IO Int
