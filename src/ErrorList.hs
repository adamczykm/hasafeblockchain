module ErrorList where

-- TODO: possibly add serializeing for errors, for advanced "debugging"

-- data type representing all of our pure code
data Error = UknownError
           | DivisionByZero
           | SomeError String
           deriving(Eq,Show)
