import Test.Hspec
import CarteSpec as CS

main :: IO ()
--main = putStrLn "Test suite not yet implemented"
main = hspec $ do
    -- carte
    CS.carteAllCoordsInBoundsSpec