{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
import Language.FaCT.Inline

[fact|secret uint32 add(secret uint32 x, secret uint32 y) {
  return x + y;
}
|]
[fact|secret uint32 sub(secret uint32 x, secret uint32 y) {
  return x - y;
}
|]


main :: IO ()
main = do
  putStrLn "hello!"
  res1 <- add 3 55
  putStrLn $ show res1
  res2 <- sub 55 3
  putStrLn $ show res2
  putStrLn "bye!"
