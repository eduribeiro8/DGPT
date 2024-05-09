module Main

-- Definição do tipo de fonte de energia
data FonteEnergia = Combustao | Eletrica

-- Definição do tipo Veiculo parametrizado pelo tipo de fonte de energia
data Veiculo : FonteEnergia -> Type where
  -- Construtor para veículos movidos a combustão
  CarroC : (combustivel : Double) -> Veiculo Combustao
  -- Construtor para veículos elétricos
  CarroE : (bateria : Double) -> Veiculo Eletrica
  -- Construtor para ônibus movidos a combustão
  Onibus : (combustivel : Double) -> Veiculo Combustao

-- Função para abastecer veículos movidos a combustão
abastecer : Veiculo Combustao -> Veiculo Combustao
abastecer (CarroC _) = CarroC 100.0
abastecer (Onibus _) = Onibus 200.0

-- Função para recarregar veículos elétricos
recarregar : Veiculo Eletrica -> Veiculo Eletrica
recarregar (CarroE _) = CarroE 100

-- Função para calcular a autonomia de veículos movidos a combustão
autonomiaC : Veiculo Combustao -> Double
autonomiaC (CarroC x) = x * 15.0
autonomiaC (Onibus x) = x * 8.0

-- Função para calcular a autonomia de veículos elétricos
autonomiaE : Veiculo Eletrica -> Double
autonomiaE (CarroE x) = x * 9.5

-- Função para calcular a autonomia de qualquer tipo de veículo
autonomia : {a : FonteEnergia} -> Veiculo a -> Double
autonomia {a = Combustao} x = autonomiaC x
autonomia {a = Eletrica} x = autonomiaE x

-- Função que simula uma viagem com um determinado veículo, levando em consideração 
-- a distância do destino e imprime o estado final do veículo após a viagem ou uma
-- mensagem de erro caso a viagem nao seja possivel.
simularViagem : {a : FonteEnergia} -> Veiculo a -> Double -> IO ()
simularViagem {a = Combustao} v dist = 
  case v of
    (CarroC x) => 
      if autonomia v >= dist
       then print $ "O veiculo chegara ao destino com combustivel = " ++ show (x - (dist / 15.0)) ++ "L"
       else print "A distancia da viagem e maior que a autonomia do veiculo!"

    (Onibus x) => 
      if autonomia v >= dist 
        then print $ "O veiculo chegara ao destino com combustivel = " ++ show (x - (dist / 8.0)) ++ "L"
        else print "A distancia da viagem e maior que a autonomia do veiculo!"

simularViagem {a = Eletrica} v dist = 
  case v of
    (CarroE x) => 
      if autonomia v >= dist 
        then print $ "O veiculo chegara ao destino com a bateria = " ++ show (x - (dist / 9.5)) ++ "%"
        else print "A distancia da viagem e maior que a autonomia do veiculo!"


-- Função principal
main : IO ()
main = do 
  putStrLn "oi"
