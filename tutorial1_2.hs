{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent.Async (async, wait)
import Network.HTTP.Simple (httpLBS, getResponseBody, parseRequest)
import GHC.Generics
import Data.Aeson 
import Data.Text (Text)
import Text.Printf (printf)

-- Estrutura de dados para representar a resposta da API
data Weather = Weather
  { cityName :: Text
  , temperature :: Double
  } deriving (Show, Generic)

instance FromJSON Weather where
  parseJSON = withObject "Weather" $ \obj -> do
    cityName <- obj .: "name"
    main <- obj .: "main"
    temperature <- main .: "temp"
    return Weather
      { cityName = cityName
      , temperature = temperature
      }

--Funcao que retorna a chave para acesso na API
getAPIKey :: String
getAPIKey = "6d2df5c7a6cc3477e695cd0b25765ac1"

-- Função principal
main :: IO ()
main = do
  let cities = ["Rio de Janeiro", "London", "New York", "Tokyo"]
  let apiUrls = map (\city -> "https://api.openweathermap.org/data/2.5/weather?appid="++ getAPIKey ++"&q=" ++ city) cities

  asyncTasks <- mapM (async . fetchTemperature) apiUrls

  responses <- mapM wait asyncTasks

  mapM_ printWeather responses


-- Função para fazer requisição assíncrona à API e obter a temperatura
fetchTemperature :: String -> IO Weather
fetchTemperature url = do
  request <- parseRequest url
  response <- httpLBS request
  case decode (getResponseBody response) of
    Just weather -> return weather
    Nothing -> error "Falha ao tentar decodificar os dados"

-- Função para imprimir a temperatura de uma cidade
printWeather :: Weather -> IO ()
printWeather (Weather cityName temp) = 
  putStrLn $ "Temperatura em " ++ show cityName ++ ": " ++ printf "%.2f" (temp - 273.15) ++ "°C"
