import Graphics.UI.Gtk
import Control.Monad (void)
import Data.Text (unpack)

normalize :: Double -> Double
normalize x = x - (2 * pi) * fromIntegral (floor (x / (2 * pi)))

factorial :: Integer -> Integer
factorial n = product [1..n]

sinTaylor :: Double -> Double -> (Double, Integer)
sinTaylor x e = go x e 0 0
    where
      go x e n s
        | abs term < e = (s, n)
        | otherwise = go x e (n + 1) (s + term)
        where
          term = ((-1) ** fromIntegral n) * (x ** fromIntegral (2 * n + 1)) / fromIntegral (factorial (2* n + 1))

cosTaylor :: Double -> Double -> (Double, Integer)
cosTaylor x e = go x e 0 0
    where
      go x e n s
        | abs term < e = (s, n)
        | otherwise = go x e (n + 1) (s + term)
        where
          term = ((-1) ** fromIntegral n) * (x ** fromIntegral (2 * n)) / fromIntegral (factorial (2* n))

main :: IO ()
main = do
    _ <- initGUI

    window <- windowNew
    set window [windowTitle := "Трансцендентные функции",
                   windowDefaultWidth := 400,
                   windowDefaultHeight := 300]

    vbox   <- vBoxNew True 3
    hbox   <- hBoxNew True 4

    buttonCalc <- buttonNewWithLabel "Вычислить"
    
    fieldX       <- entryNew
    fieldEpsilon <- entryNew
  
    labelResult <- labelNew $ Just ""
    labelEnterX <- labelNew $ Just "x:"
    labelEnterE <- labelNew $ Just "e:"

    void $ onClicked buttonCalc $ do
        xText       <- entryGetText fieldX
        epsilonText <- entryGetText fieldEpsilon

        let xVal    = read (unpack xText) :: Double
            x       = normalize xVal :: Double
            epsilon = read (unpack epsilonText) :: Double
            (sinValue, sinTerms) = sinTaylor x epsilon
            (cosValue, cosTerms) = cosTaylor x epsilon
            result = "sin(" ++ show xVal ++ ") = " ++ show sinValue ++ " (" ++ show sinTerms ++ " членов суммы)\n" ++
              "cos(" ++ show xVal ++ ") = " ++ show cosValue ++ " (" ++ show cosTerms ++ " членов суммы)"
        labelSetText labelResult result
        
    boxPackStart vbox hbox         PackNatural 0
    boxPackStart hbox labelEnterX  PackNatural 0
    boxPackStart hbox fieldX       PackNatural 0
    boxPackStart hbox labelEnterE  PackNatural 0
    boxPackStart hbox fieldEpsilon PackNatural 0
    boxPackStart vbox buttonCalc   PackNatural 0
    boxPackStart vbox labelResult  PackNatural 0
    

    containerAdd window vbox

    void $ onDestroy window mainQuit

    widgetShowAll window
    mainGUI
