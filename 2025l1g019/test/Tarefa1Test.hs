module Main where

import Labs2025
import Tarefa1 (validaEstado)
import Magic

-- Mapa simples para a maioria dos testes
mapaBase :: Mapa
mapaBase = [[Ar, Ar, Ar, Ar],
            [Ar, Ar, Ar, Ar],
            [Terra, Terra, Pedra, Ar],
            [Terra, Terra, Pedra, Agua]]

-- Minhoca base saudável no 'Ar'
minhocaBase :: Minhoca
minhocaBase = Minhoca (Just (1,1)) (Viva 100) 1 1 1 1 1

-- Estado base válido com uma minhoca
estadoBase :: Estado
estadoBase = Estado mapaBase [] [minhocaBase]

-- Definir aqui os testes
testesT1 :: [Estado]
testesT1 =
  [
    -- Teste 0: Estado base (VÁLIDO)
    estadoBase,

    -- Teste 1: Estado de exemplo
    Estado
      [[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar],
       [Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar],
       [Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar],
       [Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar],
       [Terra,Terra,Terra,Terra,Terra,Ar,Ar,Ar,Ar,Ar],
       [Terra,Terra,Terra,Terra,Terra,Pedra,Pedra,Agua,Agua,Agua]]
      [Barril (3,0) False,
       Disparo (3,3) Oeste Bazuca Nothing 0]
      [Minhoca (Just (3,4)) (Viva 100) 1 1 1 1 1,
       Minhoca (Just (2,0)) (Viva 100) 1 1 1 1 1],

    -- Teste 2: Mapa vazio (INVÁLIDO)
    Estado [] [] [],

    -- Teste 3: Mapa não-retangular (INVÁLIDO)
    Estado [[Ar, Ar], [Ar]] [] [],

    -- Teste 4: Minhoca fora do mapa (INVÁLIDO)
    estadoBase { minhocasEstado = [minhocaBase { posicaoMinhoca = Just (10, 10) }] },

    -- Teste 5: Duas minhocas sobrepostas (INVÁLIDO)
    estadoBase { minhocasEstado = [minhocaBase, minhocaBase] },

    -- Teste 6: Minhoca sobreposta a barril (INVÁLIDO)
    estadoBase { objetosEstado = [Barril (1,1) False], minhocasEstado = [minhocaBase] },

    -- Teste 7: Minhoca viva em Água (INVÁLIDO)
    estadoBase { minhocasEstado = [minhocaBase { posicaoMinhoca = Just (3, 3) }] },

    -- Teste 8: Minhoca morta em Água (VÁLIDO)
    estadoBase { minhocasEstado = [minhocaBase { posicaoMinhoca = Just (3, 3), vidaMinhoca = Morta }] },

    -- Teste 9: Minhoca viva sem posição (INVÁLIDO)
    estadoBase { minhocasEstado = [minhocaBase { posicaoMinhoca = Nothing }] },

    -- Teste 10: Minhoca morta sem posição (VÁLIDO)
    estadoBase { minhocasEstado = [minhocaBase { posicaoMinhoca = Nothing, vidaMinhoca = Morta }] },

    -- Teste 11: Minhoca com vida > 100 (INVÁLIDO)
    estadoBase { minhocasEstado = [minhocaBase { vidaMinhoca = Viva 101 }] },

    -- Teste 12: Minhoca com munição negativa (INVÁLIDO)
    estadoBase { minhocasEstado = [minhocaBase { jetpackMinhoca = -1 }] },

    -- Teste 13: Objeto Barril fora do mapa (INVÁLIDO)
    estadoBase { objetosEstado = [Barril (10, 10) False] },

    -- Teste 14: Objeto Barril em terreno opaco (INVÁLIDO)
    estadoBase { objetosEstado = [Barril (2, 2) False] },

    -- Teste 15: Disparos duplicados (INVÁLIDO)
    estadoBase { objetosEstado = [Disparo (1,0) Norte Bazuca Nothing 0, Disparo (0,1) Este Bazuca Nothing 0] },

    -- Teste 16: Dinamite com tempo 'Nothing' (INVÁLIDO)
    estadoBase { objetosEstado = [Disparo (1,0) Norte Dinamite Nothing 0] },

    -- Teste 17: Dinamite com tempo > 4 (INVÁLIDO)
    estadoBase { objetosEstado = [Disparo (1,0) Norte Dinamite (Just 5) 0] },

    -- Teste 18: Dinamite com tempo válido (VÁLIDO)
    estadoBase { objetosEstado = [Disparo (1,0) Norte Dinamite (Just 4) 0] },

    -- Teste 19: Mina com tempo > 2 (INVÁLIDO)
    estadoBase { objetosEstado = [Disparo (1,0) Norte Mina (Just 3) 0] },

    -- Teste 20: Mina com tempo 'Nothing' (VÁLIDO)
    estadoBase { objetosEstado = [Disparo (1,0) Norte Mina Nothing 0] },

    -- Teste 21: Mina sobreposta a barril (INVÁLIDO)
    estadoBase { objetosEstado = [Barril (1,0) False, Disparo (1,0) Norte Mina Nothing 0] },

    -- Teste 22: Bazuca sobreposta a barril (VÁLIDO)
    estadoBase { objetosEstado = [Barril (1,0) False, Disparo (1,0) Norte Bazuca Nothing 0] },

    -- Teste 23: Exceção Bazuca - Perfura Pedra (VÁLIDO)
    estadoBase { objetosEstado = [Disparo (2,2) Sul Bazuca Nothing 0] }
  ]

dataTarefa1 :: IO TaskData
dataTarefa1 = do
    let ins = testesT1
    outs <- mapM (runTest . validaEstado) ins
    return $ T1 ins outs

main :: IO ()
main = runFeedback =<< dataTarefa1