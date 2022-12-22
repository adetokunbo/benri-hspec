{- |
Module      : Test.Hspec.Benri
Copyright   : (c) 2022 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3

Provides \convenient\ functions for writing hspec tests
-}
module Test.Hspec.Benri (
  -- * monadic version of 'shouldSatisfy'
  endsThen,

  -- * expect a @Maybe@ from a monad
  endsJust,
  endsJust_,
  endsNothing,

  -- * expect an @Either@ from a monad
  endsLeft,
  endsLeft_,
  endsRight,
  endsRight_,
) where

import Data.Maybe (isJust)
import Test.Hspec (Expectation, HasCallStack, shouldBe, shouldSatisfy)


{- $setup
 >>> import Text.Read (readEither, readMaybe)
 >>> import Data.Maybe (isNothing, isJust)
-}


{- |
 @action \`endsRight\` expected@ sets the expectation that @action@
 returns @Right expected@.

==== __Example__

>>> pure (readEither "1" :: Either String Int) `endsRight` 1
-}
endsRight :: (HasCallStack, Show a, Eq a, Show b, Eq b) => IO (Either a b) -> b -> Expectation
action `endsRight` expected = action >>= (`shouldBe` Right expected)


{- |
 @action \`endsLeft\` expected@ sets the expectation that @action@
 returns @Left expected@.

==== __Example__

>>> pure (readEither "not an int" :: Either String Int) `endsLeft` "Prelude.read: no parse"
-}
endsLeft ::
  (HasCallStack, Show a, Eq a, Show b, Eq b) => IO (Either a b) -> a -> Expectation
action `endsLeft` expected = action >>= (`shouldBe` Left expected)


{- |
 @endsRight_ action@ sets the expectation that @action@ returns @Right b@.

==== __Example__

>>> endsRight_ $ pure (readEither "1" :: Either String Int)
-}
endsRight_ :: (Show a, Show b) => IO (Either a b) -> IO ()
endsRight_ action = endsThen action $ either (const False) (const True)


{- |
 @endsLeft_ action@ sets the expectation that @action@ returns @Left a@.

==== __Example__

>>> endsLeft_ $ pure (readEither "not an int" :: Either String Int)
-}
endsLeft_ :: (Show a, Show b) => IO (Either a b) -> IO ()
endsLeft_ action = endsThen action $ either (const True) (const False)


{- |
 @action \`endsJust\`  expected@ sets the expectation that @action@
 returns @Just expected@.

==== __Example__

>>> pure (readMaybe "1" :: Maybe Int) `endsJust` 1
-}
endsJust ::
  (HasCallStack, Show a, Eq a) => IO (Maybe a) -> a -> Expectation
action `endsJust` expected = action >>= (`shouldBe` Just expected)


{- |
 @endsNothing action@ sets the expectation that @action@
 returns @Nothing@.

==== __Example__

>>> endsNothing $ pure (readMaybe "not an int" :: Maybe Int)
-}
endsNothing :: (Show a, Eq a) => IO (Maybe a) -> IO ()
endsNothing action = action >>= (`shouldBe` Nothing)


{- |
 @endsJust_ action@ sets the expectation that @action@
 returns @Just a@.

==== __Example__

>>> endsJust_ $ pure (readMaybe "1" :: Maybe Int)
-}
endsJust_ :: (Show a) => IO (Maybe a) -> IO ()
endsJust_ action = endsThen action isJust


{- |
 @action \`endsThen\` expected@ sets the expectation that @action@
 returns that satifies the predicate @p@.

==== __Example__

>>> pure (readMaybe "1" :: Maybe Int) `endsThen` isJust
>>> pure (readMaybe "not a number" :: Maybe Int) `endsThen` isNothing
-}
endsThen :: (Show a) => IO a -> (a -> Bool) -> IO ()
endsThen action p = action >>= (`shouldSatisfy` p)


infix 1 `endsLeft`, `endsRight`, `endsThen`, `endsJust`
