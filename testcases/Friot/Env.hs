module Env where
import Basic

temprature :: Int -> Signal Int
temprature port = Signal 0

motion :: Int -> Signal Bool
motion port = Signal True

sound :: Int -> Signal Int
sound port = Signal 0

brightness :: Int -> Signal Int
brightness port = Signal 0

accelerator :: Int -> Signal (Float, Float, Float)
accelerator port = Signal (1,2,3)