module Env where
import Basic

temprature :: Int -> Signal Int
temprature port = Signal 0

motion :: Int -> Signal Bool
motion port = Signal True

sound :: Int -> Signal Int
sound port = Signal 0

button :: Int -> Signal Bool
button port = Signal True