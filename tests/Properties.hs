import Control.Applicative
import Control.Monad
import Data.Maybe ( isJust )
import Data.Void ( Void )

import Test.Framework
import Test.Framework.Providers.SmallCheck
import Test.SmallCheck.Series

import Sylvia.Model
import Sylvia.Text.Parser
import Sylvia.Text.PrettyPrint

instance Serial Void where
    series _ = []
    coseries _ _ = []

instance Serial a => Serial (Inc a) where
    series = cons0 O \/ cons1IgnoreDepth S
      where
        cons1IgnoreDepth c d = [c z | z <- series d]

    coseries rs d = [ \i -> case i of
                        O   -> z
                        S x -> f x
                    | z <- alts0 rs d
                    , f <- alts1 rs d
                    ]

instance Serial a => Serial (Exp a) where
    series = cons1 Ref \/ cons1 Lam \/ cons2 App
    coseries rs d = [ \e -> case e of
                        Ref x   -> f x
                        Lam e'  -> g e'
                        App a b -> h a b
                    | f <- alts1 rs d
                    , g <- alts1 rs d
                    , h <- alts2 rs d
                    ]

-- | Converting an expression to a string, then back again should result
-- in the same expression.
prop_parse_pprint :: Exp Void -> Bool
prop_parse_pprint orig = isJust $ do
    new <- eitherToMaybe . parseExp . pprintExp $ orig
    guard $ orig == new
  where
    eitherToMaybe = either (const Nothing) Just

prop_subst_match :: Int -> Int -> Bool
prop_subst_match x y = (subst x . match x) y == y

prop_shiftDown_shiftUp :: N Int -> Bool
prop_shiftDown_shiftUp (N x) = (shiftDown . shiftUp) x == x

main :: IO ()
main = defaultMain $ map (plusTestOptions options)
    [ testProperty "parse.pprint" prop_parse_pprint
    , testProperty "subst.match" prop_subst_match
    , testProperty "shiftDown.shiftUp" prop_shiftDown_shiftUp
    ]

options :: TestOptions
options = TestOptions
    { topt_maximum_test_depth = Just 5
    , topt_timeout = Nothing
    , topt_seed = Nothing
    , topt_maximum_generated_tests = Nothing
    , topt_maximum_unsuitable_generated_tests = Nothing
    , topt_maximum_test_size = Nothing
    }
