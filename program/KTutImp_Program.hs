----------------------------------------------------------------------
--  Sample programs to test syntax and semantics.
--
--  ./Test  -m KTutImp -e 'mapM print $ map eval_imp […]'
--      (where … = sum_imp, divide_imp, div0_imp)
--  (Eventually Test will be able find all of the `x :: Pgm` here
--  and evaluate them all for you.)

module KTutImp_Program
    ( sum_imp, divide_imp, div0_imp
    , sum_imp_io                        -- needs IO
    ) where

import KTutImp

----------------------------------------------------------------------

sum_imp :: Integer -> Pgm                           --  'sum.imp'
sum_imp n = Pgm ids stmts  where
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

divide_imp :: Pgm
divide_imp = Pgm ids stmts  where
    ids   = ["a", "b", "r"]                         --  int a, b, r
    stmts = mkStmts                                 --
          [ "a" := Int 100                          --  a = 100
          , "b" := Int 3                            --  b = 3
          , "r" := (Var "a" :/ Var "b")             --  r = a / b
          ]

div0_imp :: Pgm
div0_imp = Pgm ids stmts  where
    ids   = ["r"]
    stmts = mkStmts [ "r" := (Int 42 :/ Int 0) ]

---------------------------------------------------------------------

sum_imp_io :: Pgm                                   --  'sum.imp'
sum_imp_io = Pgm ids stmts  where
    ids   = ["n", "sum"]                            --  int n, sum
    stmts = mkStmts                                 --
          [ "n" := Read                             --  n = read
          , "sum" := Int 0                          --  sum = 0
          , While (Not (Var "n" :<= (Int 0)))       --  while (!(n <= 0)) {
               (StmtsBlock (mkStmts                 --
                 [ "sum" := (Var "sum" :+ Var "n")  --    sum = sum + n
                 , "n" := (Var "n" :+ Negate 1)     --    n = n + -1
                 ]))                                --  }
          , Print (Var "sum")                       --  print(sum)
          ]

