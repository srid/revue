# Haskell

Here's some Haskell:

```haskell
import System
frontend :: (StaticWidget x (), Widget x ())
frontend = (head', body)
  where
    head' = el "title" $ text "Hello"
    body = do
      el "h1" $ text "Hello"
      el "p" $ text "Work in progress"
```
