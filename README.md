Once you install [FaCT](https://github.com/PLSysSec/FaCT), you can use it inline in your Haskell project:

```haskell
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
```

This is a small protype and should not be used in production code.
