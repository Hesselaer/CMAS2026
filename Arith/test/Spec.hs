import Test.HUnit
import Control.Monad (unless)
import Data.Set as Set
import System.Exit (exitFailure)

import Syntax.Abs
import Semantics

emptyEnv :: Env
emptyEnv = Env []

data ArithTestCase = ArithTestCase
  { testName :: String
  , program :: Exp
  , testEnv :: Env
  , denotationRes :: Maybe Double
  , bigStepRes :: Set Double
  , smallStepRes :: Set (Env, Exp)
  } deriving (Show)

instance Testable ArithTestCase where
  test tc = testName tc ~: TestList $
    [ "Denotational" ~: (denotationRes tc) ~=? (denotation (testEnv tc) (program tc))
    , "Big-step"     ~: (bigStepRes tc)    ~=? (bigStep (testEnv tc) (program tc))
    , "Small-step"   ~: (smallStepRes tc)  ~=? (smallStep (testEnv tc, program tc))
    ]

tests :: [ArithTestCase]
tests =
  [ ArithTestCase "Numeral" (ENum 5) emptyEnv
    (Just 5)  o1  empty
  , ArithTestCase "Plus" (EPlus (ENum 5) (ENum 4)) emptyEnv
    (Just 9)  o2  (fromList [(emptyEnv, ENum 9)])
  , ArithTestCase "Mul" (ETimes (ENum 5) (ENum 4)) emptyEnv
    (Just 20) o3 (fromList [(emptyEnv, ENum 20)])
  , ArithTestCase "Plus-mul" (ETimes (EPlus (ENum 6) (ENum 4)) (EPlus (ENum 6) (ENum 1))) emptyEnv
    (Just 70) o4 (fromList [ (emptyEnv, (ETimes (ENum 10) (EPlus (ENum 6) (ENum 1))))
                           , (emptyEnv, (ETimes (EPlus (ENum 6) (ENum 4)) (ENum 7)))
                           ])
  , ArithTestCase "Variables" (EPlus (ETimes (ENum 5) (EVar (Ident "x"))) (EVar (Ident "y"))) e1
    (Just 21)
    o5
    (fromList [ (e1, EPlus (ETimes (ENum 5) (ENum 4)) (EVar (Ident "y")))
              , (e1, EPlus (ETimes (ENum 5) (EVar (Ident "x"))) (ENum 1))
              ])
  , ArithTestCase "Div zero" (EDiv (ENum 5) (ENum 0)) emptyEnv
    Nothing  Set.empty  Set.empty
  , ArithTestCase "Div" (EDiv (ENum 9) (ENum 3)) emptyEnv
    (Just 3) (singleton 3) (singleton (emptyEnv, ENum 3))
  , ArithTestCase "Div numerator steps"
    (EDiv (EPlus (ENum 3) (ENum 5)) (ENum 2)) emptyEnv
    (Just 4) (singleton 4)
    (singleton (emptyEnv, EDiv (ENum 8) (ENum 2)))
  , ArithTestCase "Div denominator steps"
    (EDiv (ENum 8) (EPlus (ENum 1) (ENum 1))) emptyEnv
    (Just 4) (singleton 4)
    (singleton (emptyEnv, EDiv (ENum 8) (ENum 2)))
  , ArithTestCase "Div denominator reduces to zero"
    (EDiv (ENum 8) (EPlus (ENum 2) (ENum (-2)))) emptyEnv
    Nothing Set.empty
    (singleton (emptyEnv, EDiv (ENum 8) (ENum 0)))
  , ArithTestCase "Nested division by zero"
    (EPlus (ENum 1) (EDiv (ENum 5) (ENum 0))) emptyEnv
    Nothing Set.empty Set.empty
  , ArithTestCase "Variable denominator is zero"
    (EDiv (EVar (Ident "x")) (EVar (Ident "y"))) e2
    Nothing Set.empty
    (fromList
      [ (e2, EDiv (ENum 4) (EVar (Ident "y")))
      , (e2, EDiv (EVar (Ident "x")) (ENum 0))
      ])
  , ArithTestCase "Fractional and negative operands"
    (EPlus (EDiv (ENum 1.5) (ENum 0.5))
           (ETimes (ENum (-2)) (ENum 1.5))) emptyEnv
    (Just 0) (singleton 0)
    (fromList
      [ (emptyEnv,
          EPlus (ENum 3) (ETimes (ENum (-2)) (ENum 1.5)))
      , (emptyEnv,
          EPlus (EDiv (ENum 1.5) (ENum 0.5)) (ENum (-3)))
      ])
  ]
  where
    e1 :: Env
    e1 = Env [Assign (Ident "x") 4, Assign (Ident "y") 1]

    e2 :: Env
    e2 = Env [Assign (Ident "x") 4, Assign (Ident "y") 0]

    o1, o2, o3, o4, o5 :: Set Double
    o1 = singleton 5
    o2 = singleton 9
    o3 = singleton 20
    o4 = singleton 70
    o5 = singleton 21

main :: IO ()
main = do
  counts <- runTestTT $ test tests
  unless (errors counts == 0 && failures counts == 0) exitFailure
