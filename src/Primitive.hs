module Primitive where

import Syntax
import Control.Monad

primitives = [
  ("__b+", VPrimFun $ \[VInt x, VInt y] -> VInt $ x + y), 
  ("__b-", VPrimFun $ \[VInt x, VInt y] -> VInt $ x - y), 
  ("__b*", VPrimFun $ \[VInt x, VInt y] -> VInt $ x * y), 
  ("__b/", VPrimFun $ \[VInt x, VInt y] -> VInt $ x `div` y),
  ("__b%", VPrimFun $ \[VInt x, VInt y] -> VInt $ x `mod` y),   
  ("__b==", VPrimFun $ \args -> case args of
      [VBool x, VBool y] -> VBool $ x == y
      [VInt x, VInt y] -> VBool $ x == y),
  ("__b!=", VPrimFun $ \args -> case args of
      [VBool x, VBool y] -> VBool $ x == y
      [VInt x, VInt y] -> VBool $ x == y),
  ("__b<", VPrimFun $ \[VInt x, VInt y] -> VBool $ x < y), 
  ("__b<=", VPrimFun $ \[VInt x, VInt y] -> VBool $ x <= y), 
  ("__b>", VPrimFun $ \[VInt x, VInt y] -> VBool $ x > y), 
  ("__b>=", VPrimFun $ \[VInt x, VInt y] -> VBool $ x >= y),

  ("__b&&", VPrimFun $ \[VBool x, VBool y] -> VBool $ x && y), 
  ("__b||", VPrimFun $ \[VBool x, VBool y] -> VBool $ x || y),

  ("print", VPrimFunIO $ \args -> mapM_ (putStr . show) args >> return VVoid),
  ("println", VPrimFunIO $ \args -> mapM_ (putStr . show) args >> putStrLn "" >> return VVoid)
  ]
