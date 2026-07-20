module Main where

import Labs2025
import Tarefa2 (efetuaJogada)
import Magic

-- Mapa de teste principal para todos os testes
mapaTestePadrao :: Mapa
mapaTestePadrao = [
    [Ar,   Ar,    Ar,    Ar,    Ar,    Ar],    
    [Ar,   Ar,    Ar,    Ar,    Ar,    Ar],    
    [Ar,   Ar,    Ar,    Ar,    Ar,    Ar],   
    [Terra,Terra, Terra, Terra, Agua,  Agua],  
    [Terra,Terra, Terra, Terra, Agua,  Agua]   
    ]

-- Funções auxiliares para criar minhocas em diferentes estados
criarMinhoca :: Posicao -> Minhoca; criarMinhoca pos = Minhoca (Just pos) (Viva 100) 2 2 2 2 2
criarMinhocaSemMunicao :: Posicao -> Minhoca; criarMinhocaSemMunicao pos = Minhoca (Just pos) (Viva 100) 0 0 0 0 0
criarMinhocaMorta :: Posicao -> Minhoca; criarMinhocaMorta pos = Minhoca (Just pos) Morta 2 2 2 2 2

-- Altera o terreno de forma segura para usar na criação dos estados de teste.
alteraTerrenoNoMapa :: Posicao -> Terreno -> Mapa -> Mapa
alteraTerrenoNoMapa (x,y) terrNovo mapa =
    case splitAt y mapa of
      (mapaAntes, linha:mapaResto) ->
        case splitAt x linha of
          (linhaAntes, _:linhaResto) -> let linhaNova = linhaAntes ++ [terrNovo] ++ linhaResto in mapaAntes ++ [linhaNova] ++ mapaResto
          _ -> mapa
      _ -> mapa

-- LISTA DE TESTES

testesT2 :: [(NumMinhoca, Jogada, Estado)]
testesT2 =
  [
    -- Teste 0: Movimento válido. Salto para cima (Norte) a partir do chão.
    let estado = Estado mapaTestePadrao [] [criarMinhoca (2,2)]
    in (0, Move Norte, estado)

    -- Teste 1: Movimento válido. Salto na diagonal (Nordeste) a partir do chão.
  , let estado = Estado mapaTestePadrao [] [criarMinhoca (2,2)]
    in (0, Move Nordeste, estado)

    -- Teste 2  : Movimento válido. Salto na diagonal (Noroeste) a partir do chão.
  , let estado = Estado mapaTestePadrao [] [criarMinhoca (2,2)]
    in (0, Move Noroeste, estado)

    -- Teste 3  : Movimento inválido. Tentar mover quando a minhoca está no ar.
  , let estado = Estado mapaTestePadrao [] [criarMinhoca (1,1)]
    in (0, Move Norte, estado)

    -- Teste 4: Movimento inválido. Tentar mover para os lados (Este) a partir do chão.
  , let estado = Estado mapaTestePadrao [] [criarMinhoca (2,2)]
    in (0, Move Este, estado)

    -- Teste 5: Movimento para Água. Minhoca deve morrer e ficar na nova posição.
  , let estado = Estado mapaTestePadrao [] [criarMinhoca (2,3)]
    in (0, Move Sudeste, estado)

    -- Teste 6: Movimento para fora do mapa. Minhoca deve morrer e perder a sua posição.
  , let estado = Estado mapaTestePadrao [] [criarMinhoca (0,3)]
    in (0, Move Oeste, estado)

    -- Teste 7: Movimento inválido. Posição de destino está ocupada por outra minhoca.
  , let estado = Estado mapaTestePadrao [] [criarMinhoca (2,1), criarMinhoca (2,2)]
    in (0, Move Oeste, estado)

    -- Teste 8 : Movimento inválido. Posição de destino está ocupada por um barril.
  , let estado = Estado mapaTestePadrao [Barril (2,1) False] [criarMinhoca (2,2)]
    in (0, Move Oeste, estado)

    -- Teste 9: Movimento inválido. Uma minhoca morta não se pode mover.
  , let estado = Estado mapaTestePadrao [] [criarMinhocaMorta (2,2)]
    in (0, Move Norte, estado)

    -- Teste 10: Disparo Bazuca. Cria um objeto Disparo no alvo e gasta 1 munição.
  , let estado = Estado mapaTestePadrao [] [criarMinhoca (2,2)]
    in (0, Dispara Bazuca Este, estado)

    -- Teste 11: Disparo inválido. Minhoca não tem munição suficiente.
  , let estado = Estado mapaTestePadrao [] [criarMinhocaSemMunicao (2,2)]
    in (0, Dispara Bazuca Este, estado)

    -- Teste 12: Disparo Jetpack. Move a minhoca para a posição de destino.
  , let estado = Estado mapaTestePadrao [] [criarMinhoca (2,2)]
    in (0, Dispara Jetpack Nordeste, estado)

    -- Teste 13: Disparo Jetpack inválido. Posição de destino está ocupada.
  , let estado = Estado mapaTestePadrao [] [criarMinhoca (2,2), criarMinhoca (2,3)]
    in (0, Dispara Jetpack Nordeste, estado)

    -- Teste 14: Disparo Escavadora. Destrói Terra, altera o mapa e move a minhoca.
  , let estado = Estado mapaTestePadrao [] [criarMinhoca (2,2)]
    in (0, Dispara Escavadora Sul, estado)

    -- Teste 15: Disparo Escavadora. Apenas move a minhoca se o alvo já for Ar.
  , let estado = Estado mapaTestePadrao [] [criarMinhoca (2,2)]
    in (0, Dispara Escavadora Norte, estado)

    -- Teste 16: Disparo Mina. Coloca a mina na posição de destino se estiver livre.
  , let estado = Estado mapaTestePadrao [] [criarMinhoca (2,2)]
    in (0, Dispara Mina Nordeste, estado)

    -- Teste 17: Disparo Mina. Coloca na posição da minhoca se o alvo for inválido (Terra).
  , let estado = Estado mapaTestePadrao [] [criarMinhoca (2,2)]
    in (0, Dispara Mina Este, estado)

    -- Teste 18 : Disparo Dinamite. Coloca a dinamite no alvo se estiver livre.
  , let estado = Estado mapaTestePadrao [] [criarMinhoca (2,2)]
    in (0, Dispara Dinamite Nordeste, estado)

    -- Teste 19: Disparo Dinamite. Coloca na posição da minhoca se o alvo for inválido (Terra).
  , let estado = Estado mapaTestePadrao [] [criarMinhoca (2,2)]
    in (0, Dispara Dinamite Este, estado)

    -- Teste 20: Disparo inválido. Minhoca já tem um disparo da mesma arma ativo.
  , let disparoAtivo = Disparo (5,5) Este Bazuca Nothing 0
        estado = Estado mapaTestePadrao [disparoAtivo] [criarMinhoca (2,2)]
    in (0, Dispara Bazuca Norte, estado)
  ]

-- FUNÇÃO MAIN E FEEDBACK

main :: IO (); main = runFeedback =<< dataTarefa2

dataTarefa2 :: IO TaskData
dataTarefa2 = do
    let inputs = testesT2
    outputs <- mapM (runTest . uncurry3 efetuaJogada) inputs
    return $ T2 inputs outputs
  where
    uncurry3 f (x,y,z) = f x y z