module Logging where 

type Message = String
data Log = Ok | Fail String

instance Monoid Log where 
  mempty = Ok
  mappend Ok Ok = Ok
  mappend Ok (Fail m) = Fail m
  mappend (Fail m) Ok = Fail m
  mappend (Fail m1) (Fail m2) = Fail (m1++", "++m2)

instance Show Log where 
  show Ok = "Ok"
  show (Fail m) = "Fail " ++ show m

(<|>) :: Log -> Log -> Log
(<|>) Ok _ = Ok
(<|>) _ Ok = Ok
(<|>) f g  = mappend f g

mkLog :: Bool -> Message -> Log
mkLog cond msg = if cond then Ok else Fail msg

main :: IO()
main = do
  let e1 = Ok 
  let e2 = Ok
  let e3 = Fail "error 1"
  let e4 = Fail "error 2"
  putStrLn "begin test: "
  putStrLn $ show $ mconcat [e1, e2]
  putStrLn $ show $ e1 <|> e2
  putStrLn $ show $ e1 <|> e2 <|> e3
  putStrLn $ show $ mconcat [e1, e2, e3]
  putStrLn $ show $ mconcat [e1, e2, e3, e4]
  putStrLn "done."
