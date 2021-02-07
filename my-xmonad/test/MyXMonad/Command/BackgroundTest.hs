module MyXMonad.Command.BackgroundTest where

import           Test.Tasty (testGroup)

import Test.Tasty.HUnit (testCase, (@?=))

test_module = testGroup "MyXMonad.Command.Background" [
  testShow
  ]

testShow = testGroup "show" [
    testCase "foo" $ False @?= True 
  ]
