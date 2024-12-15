import qualified Graphics.UI.Gtk as Gtk
import Control.Monad (void)

sum1WithFloat :: Float -> Float
sum1WithFloat delta = sum (replicate 1000000 delta) + 1

sum2WithFloat :: Float -> Float
sum2WithFloat delta = 1 + sum (replicate 1000000 delta)

sum1WithDouble :: Double -> Double
sum1WithDouble delta = sum (replicate 1000000 delta) + 1

sum2WithDouble :: Double -> Double
sum2WithDouble delta = 1 + sum (replicate 1000000 delta)

relativeError :: (RealFrac a) => a -> a -> a
relativeError trueValue computedValue = abs ((trueValue - computedValue) / trueValue)

main :: IO ()
main = do
    _ <- Gtk.initGUI

    window <- Gtk.windowNew
    Gtk.set window [Gtk.windowTitle Gtk.:= "Погрешность суммирования",
                   Gtk.windowDefaultWidth Gtk.:= 400,
                   Gtk.windowDefaultHeight Gtk.:= 300]

    hbox1     <- Gtk.hBoxNew True 2
    vbox1_1   <- Gtk.vBoxNew True 3
    vbox1_2   <- Gtk.vBoxNew True 3
    hbox1_1_1 <- Gtk.hBoxNew True 3
    hbox1_1_2 <- Gtk.hBoxNew True 3
    hbox1_2_1 <- Gtk.hBoxNew True 3
    hbox1_2_2 <- Gtk.hBoxNew True 3

    buttonSum1Float  <- Gtk.buttonNewWithLabel "Cумма 1"
    buttonSum2Float  <- Gtk.buttonNewWithLabel "Сумма 2"
    buttonSum1Double <- Gtk.buttonNewWithLabel "Сумма 1"
    buttonSum2Double <- Gtk.buttonNewWithLabel "Сумма 2"
    
    sum1FloatField  <- Gtk.entryNew
    sum2FloatField  <- Gtk.entryNew
    sum1DoubleField <- Gtk.entryNew
    sum2DoubleField <- Gtk.entryNew

    labelTypeFloat  <- Gtk.labelNew $ Just "Тип float"
    labelTypeDouble <- Gtk.labelNew $ Just "Тип double"
    labelErrorSum1Float  <- Gtk.labelNew $ Just "Погрешность суммы 1"
    labelErrorSum2Float  <- Gtk.labelNew $ Just "Погрешность суммы 2"
    labelErrorSum1Double <- Gtk.labelNew $ Just "Погрешность суммы 1"
    labelErrorSum2Double <- Gtk.labelNew $ Just "Погрешность суммы 2"    

    void $ Gtk.onClicked buttonSum1Float $ do
        let delta = 1e-6 :: Float
            s1Float = sum1WithFloat delta
            relativeErrorResult = relativeError (1000000 * delta + 1) s1Float
        Gtk.entrySetText sum1FloatField (show relativeErrorResult)

    void $ Gtk.onClicked buttonSum2Float $ do
        let delta = 1e-6 :: Float
            s2Float = sum2WithFloat delta
            relativeErrorResult = relativeError (1 + 1000000 * delta) s2Float
        Gtk.entrySetText sum2FloatField (show relativeErrorResult)

    void $ Gtk.onClicked buttonSum1Double $ do
        let delta = 1e-6 :: Double
            s1Double = sum1WithDouble delta
            relativeErrorResult = relativeError (1000000 * delta + 1) s1Double
        Gtk.entrySetText sum1DoubleField (show relativeErrorResult)

    void $ Gtk.onClicked buttonSum2Double $ do
        let delta = 1e-6 :: Double
            s2Double = sum2WithDouble delta
            relativeErrorResult = relativeError (1 + 1000000 * delta) s2Double
        Gtk.entrySetText sum2DoubleField (show relativeErrorResult)        

    Gtk.boxPackStart hbox1     vbox1_1              Gtk.PackNatural 0
    Gtk.boxPackStart hbox1     vbox1_2              Gtk.PackNatural 0
    Gtk.boxPackStart vbox1_1   labelTypeFloat       Gtk.PackNatural 0
    Gtk.boxPackStart vbox1_1   hbox1_1_1            Gtk.PackNatural 0
    Gtk.boxPackStart vbox1_1   hbox1_1_2            Gtk.PackNatural 0
    Gtk.boxPackStart vbox1_2   labelTypeDouble      Gtk.PackNatural 0
    Gtk.boxPackStart vbox1_2   hbox1_2_1            Gtk.PackNatural 0
    Gtk.boxPackStart vbox1_2   hbox1_2_2            Gtk.PackNatural 0
    Gtk.boxPackStart hbox1_1_1 labelErrorSum1Float  Gtk.PackNatural 0
    Gtk.boxPackStart hbox1_1_1 sum1FloatField       Gtk.PackNatural 0
    Gtk.boxPackStart hbox1_1_1 buttonSum1Float      Gtk.PackNatural 0
    Gtk.boxPackStart hbox1_1_2 labelErrorSum2Float  Gtk.PackNatural 0
    Gtk.boxPackStart hbox1_1_2 sum2FloatField       Gtk.PackNatural 0
    Gtk.boxPackStart hbox1_1_2 buttonSum2Float      Gtk.PackNatural 0
    Gtk.boxPackStart hbox1_2_1 labelErrorSum1Double Gtk.PackNatural 0
    Gtk.boxPackStart hbox1_2_1 sum1DoubleField      Gtk.PackNatural 0
    Gtk.boxPackStart hbox1_2_1 buttonSum1Double     Gtk.PackNatural 0
    Gtk.boxPackStart hbox1_2_2 labelErrorSum2Double Gtk.PackNatural 0
    Gtk.boxPackStart hbox1_2_2 sum2DoubleField      Gtk.PackNatural 0    
    Gtk.boxPackStart hbox1_2_2 buttonSum2Double     Gtk.PackNatural 0

    Gtk.containerAdd window hbox1
    Gtk.widgetShowAll window

    void $ Gtk.onDestroy window Gtk.mainQuit

    Gtk.mainGUI
