{- |
Module      : Test.Hspec.Benri
Copyright   : (c) 2022 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3

Provides \convenient\ functions for writing hspec tests
-}
module Test.Hspec.Benri (
  -- * expect a monadic value
  endsThen,

  -- * expect a monadic @Maybe@
  endsJust,
  endsJust_,
  endsNothing,

  -- * expect a monadic @Either@
  endsLeft,
  endsLeft_,
  endsRight,
  endsRight_,
) where

import Data.Maybe (isJust)
import Test.Hspec (Expectation, HasCallStack, shouldBe, shouldSatisfy)


{- |
 @action \`endsRight\` @expected@ sets the expectation that @action@
 returns @Right@ @expected@.
-}
endsRight :: (HasCallStack, Show a, Eq a, Show b, Eq b) => IO (Either a b) -> b -> Expectation
action `endsRight` expected = action >>= (`shouldBe` Right expected)


{- |
 @action \`endsLeft\` @expected@ sets the expectation that @action@
 returns @Left@ @expected@.
-}
endsLeft ::
  (HasCallStack, Show a, Eq a, Show b, Eq b) => IO (Either a b) -> a -> Expectation
action `endsLeft` expected = action >>= (`shouldBe` Left expected)


{- |
 @action \`endsRight_\` sets the expectation that @action@
 returns @Right _@.
-}
endsRight_ :: (Show a, Show b) => IO (Either a b) -> IO ()
endsRight_ action = endsThen action $ either (const False) (const True)


{- |
 @action \`endsLeft_\` sets the expectation that @action@
 returns @Left _@.
-}
endsLeft_ :: (Show a, Show b) => IO (Either a b) -> IO ()
endsLeft_ action = endsThen action $ either (const True) (const False)


{- |
 @action \`endsJust\`  @expected@ sets the expectation that @action@
 returns @Just@ @expected@.
-}
endsJust ::
  (HasCallStack, Show a, Eq a) => IO (Maybe a) -> a -> Expectation
action `endsJust` expected = action >>= (`shouldBe` Just expected)


{- |
 @action \`endsNothing\` expected@ sets the expectation that @action@
 returns @Nothing@.
-}
endsNothing :: (Show a, Eq a) => IO (Maybe a) -> IO ()
endsNothing action = action >>= (`shouldBe` Nothing)


{- |
 @action \`endsJust_\` sets the expectation that @action@
 returns @Just _@.
-}
endsJust_ :: (Show a) => IO (Maybe a) -> IO ()
endsJust_ action = endsThen action isJust


{- |
 @action \`endsThen\` expected@ sets the expectation that @action@
 returns @expected@.
-}
endsThen :: (Show a) => IO a -> (a -> Bool) -> IO ()
endsThen action p = action >>= (`shouldSatisfy` p)


infix 1 `endsLeft`, `endsRight`, `endsThen`, `endsJust`
