module MiniC_Program (
    sum_n
) where

import MiniC

sum_n :: Integer -> Pgm                           --  'sum.imp'
sum_n n = Pgm ids stmts  where
    ids   = ["n", "sum"]                            --  int n, sum
    stmts = mkStmts                                 --
          [ "n" := Int n                            --  n = $n
          , "sum" := Int 0                          --  sum = 0
          , While (Not (Var "n" :<= (Int 0)))       --  while (!(n <= 0)) {
               (StmtsBlock (mkStmts                 --
                 [ "sum" := (Var "sum" :+ Var "n")  --    sum = sum + n
                 , "n" := (Var "n" :+ Negate 1)     --    n = n + -1
                 ]))                                --  }
          ]
