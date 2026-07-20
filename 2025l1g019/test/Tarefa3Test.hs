module Main where

import Labs2025
import Tarefa3
import Magic  -- módulo fornecido pelo sistema de feedback

-- Testes para a Tarefa 3

-- Mapa de exemplo
mapaExemplo :: Mapa
mapaExemplo =
  [[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
  ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
  ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
  ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
  ,[Terra,Terra,Terra,Terra,Terra,Ar,Ar,Ar,Ar,Ar]
  ,[Terra,Terra,Terra,Terra,Terra,Pedra,Pedra,Agua,Agua,Agua]
  ]

-- Teste 0: Minhoca no ar a cair
minhocaNoAr :: Minhoca
minhocaNoAr = Minhoca (Just (2,5)) (Viva 100) 1 1 1 1 1
estadoMinhocaCai :: Estado
estadoMinhocaCai = Estado mapaExemplo [] [minhocaNoAr]

-- Teste 1: Minhoca cai na água e morre
minhocaNaAgua :: Minhoca
minhocaNaAgua = Minhoca (Just (4,7)) (Viva 100) 1 1 1 1 1
estadoMinhocaMorreAgua :: Estado
estadoMinhocaMorreAgua = Estado mapaExemplo [] [minhocaNaAgua]

-- Teste 2: Bazuca e depois explode
bazucaMovendo :: Objeto
bazucaMovendo = Disparo (3,5) Oeste Bazuca Nothing 0
minhocaAlvo :: Minhoca
minhocaAlvo = Minhoca (Just (3,3)) (Viva 100) 1 1 1 1 1
estadoBazucaAvanca :: Estado
estadoBazucaAvanca = Estado mapaExemplo [bazucaMovendo] [minhocaAlvo]

-- Teste 3: Barril no ar explode
barrilNoAr :: Objeto
barrilNoAr = Barril (2,5) False
minhocaPertoBarril :: Minhoca
minhocaPertoBarril = Minhoca (Just (2,3)) (Viva 100) 1 1 1 1 1
estadoBarrilExplode :: Estado
estadoBarrilExplode = Estado mapaExemplo [barrilNoAr] [minhocaPertoBarril]

-- Teste 4: Mina fica ativa quando minhoca inimiga se aproxima
minaInativa :: Objeto
minaInativa = Disparo (4,5) Norte Mina Nothing 0
minhocaLonge :: Minhoca
minhocaLonge = Minhoca (Just (2,2)) (Viva 100) 1 1 1 1 1
minhocaPerto :: Minhoca
minhocaPerto = Minhoca (Just (4,6)) (Viva 100) 1 1 1 1 1
estadoMinaAtiva :: Estado
estadoMinaAtiva = Estado mapaExemplo [minaInativa] [minhocaLonge, minhocaPerto]

-- Teste 5: Mina com o tempo explode
minaComTempo :: Objeto
minaComTempo = Disparo (3,3) Norte Mina (Just 0) 0
minhocaVitima :: Minhoca
minhocaVitima = Minhoca (Just (3,4)) (Viva 50) 1 1 1 1 1
estadoMinaExplode :: Estado
estadoMinaExplode = Estado mapaExemplo [minaComTempo] [minhocaVitima]

-- Teste 6: Dinamite em parábola
dinamiteMovendo :: Objeto
dinamiteMovendo = Disparo (2,5) Este Dinamite (Just 3) 0
estadoDinamiteParabola :: Estado
estadoDinamiteParabola = Estado mapaExemplo [dinamiteMovendo] []

-- Teste 7: Dinamite explode após a contagem
dinamiteProntaExplodir :: Objeto
dinamiteProntaExplodir = Disparo (3,3) Norte Dinamite (Just 0) 0
minhocasProximas :: [Minhoca]
minhocasProximas =
  [ Minhoca (Just (3,1)) (Viva 100) 1 1 1 1 1
  , Minhoca (Just (2, 4)) (Viva 100) 1 1 1 1 1
  ]
estadoDinamiteExplode :: Estado
estadoDinamiteExplode = Estado mapaExemplo [dinamiteProntaExplodir] minhocasProximas

-- Teste 8: Múltiplos objetos e minhocas
barrilNormal :: Objeto
barrilNormal = Barril (3,2) False
bazucaAtiva :: Objeto
bazucaAtiva = Disparo (3,8) Oeste Bazuca Nothing 0
minhocaChao, minhocaAr :: Minhoca
minhocaChao = Minhoca (Just (4,8)) (Viva 80) 1 1 1 1 1
minhocaAr = Minhoca (Just (1,5)) (Viva 60) 1 1 1 1 1
estadoComplexo :: Estado
estadoComplexo = Estado mapaExemplo [barrilNormal,bazucaAtiva] [minhocaChao,minhocaAr]

-- Teste 9: Explosão destrói a Terra
terrenoDestrutivel :: Mapa
terrenoDestrutivel =
  [[Ar,Ar,Ar,Ar,Ar]
  ,[Ar,Terra,Terra,Terra,Ar]
  ,[Ar,Terra,Terra,Terra,Ar]
  ,[Ar,Terra,Terra,Terra,Ar]
  ,[Ar,Ar,Ar,Ar,Ar]
  ]
bazucaCentro :: Objeto
bazucaCentro = Disparo (2,2) Norte Bazuca Nothing 0
estadoDestruirTerra :: Estado
estadoDestruirTerra = Estado terrenoDestrutivel [bazucaCentro] []

-- Teste 10: Mina cai (gravidade)
minaNoAr :: Objeto
minaNoAr = Disparo (2,5) Norte Mina Nothing 0
estadoMinaCai :: Estado
estadoMinaCai = Estado mapaExemplo [minaNoAr] []

-- Teste 11: Minhoca cai e morre (sai do mapa)
minhocaBeira :: Minhoca
minhocaBeira = Minhoca (Just (5,9)) (Viva 100) 1 1 1 1 1
mapaComBuraco :: Mapa
mapaComBuraco =
  [[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
  ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
  ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
  ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
  ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
  ,[Terra,Terra,Terra,Terra,Terra,Ar,Ar,Ar,Ar,Terra]
  ,[Terra,Terra,Terra,Terra,Terra,Pedra,Pedra,Agua,Agua,Ar]
  ]
estadoMinhocaSaiMapa :: Estado
estadoMinhocaSaiMapa = Estado mapaComBuraco [] [minhocaBeira]

-- Teste 12: Barril prestes a explodir
barrilExplodindo :: Objeto
barrilExplodindo = Barril (3,5) True
estadoBarrilProntoExplodir :: Estado
estadoBarrilProntoExplodir = Estado mapaExemplo [barrilExplodindo] []

-- Teste 13: Dinamite no chão
dinamiteChao :: Objeto
dinamiteChao = Disparo (3,3) Este Dinamite (Just 2) 0
estadoDinamiteChao :: Estado
estadoDinamiteChao = Estado mapaExemplo [dinamiteChao] []

-- Teste 14: Múltiplas explosões simultâneas
bazuca1, bazuca2 :: Objeto
bazuca1 = Disparo (2,2) Norte Bazuca Nothing 0
bazuca2 = Disparo (2,7) Norte Bazuca Nothing 0
minhocaCentral :: Minhoca
minhocaCentral = Minhoca (Just (2,5)) (Viva 100) 1 1 1 1 1
estadoMultiplasExplosoes :: Estado
estadoMultiplasExplosoes = Estado mapaExemplo [bazuca1,bazuca2] [minhocaCentral]

testesT3 :: [Estado]
testesT3 =
  [ estadoMinhocaCai
  , estadoMinhocaMorreAgua
  , estadoBazucaAvanca
  , estadoBarrilExplode
  , estadoMinaAtiva
  , estadoMinaExplode
  , estadoDinamiteParabola
  , estadoDinamiteExplode
  , estadoComplexo
  , estadoDestruirTerra
  , estadoMinaCai
  , estadoMinhocaSaiMapa
  , estadoBarrilProntoExplodir
  , estadoDinamiteChao
  , estadoMultiplasExplosoes
  ]

dataTarefa3 :: IO TaskData
dataTarefa3 = do
  let ins = testesT3
  outs <- mapM (return . Right . avancaEstado) ins 
  return $ T3 ins outs

main :: IO ()
main = runFeedback =<< dataTarefa3