module Main where

import Labs2025
import Tarefa4
import Magic -- Módulo de feedback do professor

-- =========================================================================
--  1. DEFINIÇÃO DE MAPAS (Cenários)
-- =========================================================================

-- Mapa A: Chão simples (para testes de tiro e movimento livre)
mapaLimpo :: Mapa
mapaLimpo = 
    replicate 5 (replicate 10 Ar) ++ 
    [replicate 10 Terra] -- Chão na linha 5

-- Mapa B: Parede de Terra (para testar "Farm de Pontos")
mapaComParede :: Mapa
mapaComParede = [
    [Ar,   Ar,    Ar,    Ar,    Ar,    Ar,    Ar,    Ar,    Ar,    Ar,    Ar],    
    [Ar,   Ar,    Ar,    Ar,    Ar,    Ar,    Ar,    Ar,    Ar,    Ar,    Ar],    
    [Ar,   Ar,    Ar,    Ar,    Ar,    Ar,    Ar,    Ar,    Ar,    Ar,    Ar],   
    [Ar,   Ar,    Ar,    Ar,    Ar,    Ar,    Ar,    Ar,    Ar,    Ar,    Ar],
    [Ar,   Ar,    Ar,    Ar,    Ar, Terra,    Ar,    Ar,    Ar,    Ar,    Ar],  
    [Terra,Terra,Terra, Terra, Terra, Terra, Terra,Terra, Terra, Terra, Terra]   
    ]
   -- Linha 5 toda terra (parede à frente)

-- Mapa C: Obstáculo de Pedra (para testar "Saltar Obstáculos")
mapaComPedra :: Mapa
mapaComPedra = [
    [Ar,   Ar,    Ar,    Ar,    Ar,    Ar,    Ar,    Ar,    Ar,    Ar,    Ar],    
    [Ar,   Ar,    Ar,    Ar,    Ar,    Ar,    Ar,    Ar,    Ar,    Ar,    Ar],    
    [Ar,   Ar,    Ar,    Ar,    Ar,    Ar,    Ar,    Ar,    Ar,    Ar,    Ar],   
    [Ar,   Ar,    Ar,    Ar,    Ar,    Ar,    Ar,    Ar,    Ar,    Ar,    Ar],
    [Ar,   Ar,    Ar,    Ar,    Ar, Pedra,    Ar,    Ar,    Ar,    Ar,    Ar],  
    [Terra,Terra,Terra, Terra, Terra, Terra, Terra,Terra, Terra, Terra, Terra]   
    ]
-- =========================================================================
--  2. CASOS DE TESTE (Unit Tests for Bot Logic)
-- =========================================================================

-- TESTE 1: PRIORIDADE MÁXIMA - O ASSASSINO
-- Situação: Inimigo alinhado, sem obstáculos.
-- Expectativa: Dispara Bazuca Este (Matar).
testeAssassino :: Estado
testeAssassino = Estado mapaLimpo [] [bot, inimigo]
  where
    bot     = Minhoca (Just (4, 1)) (Viva 100) 10 10 10 10 10
    inimigo = Minhoca (Just (4, 8)) (Viva 100) 10 10 10 10 10

-- TESTE 2: FARM DE PONTOS (RICO)
-- Situação: Inimigo longe, mas uma parede de Terra bloqueia o caminho imediato.
-- Expectativa: Dispara Bazuca Este (Destruir parede para ganhar 10pts e abrir caminho).
testeEscavadora :: Estado
testeEscavadora = Estado mapaComParede [] [bot, inimigoLonge]
  where
    bot          = Minhoca (Just (4, 2)) (Viva 100) 10 10 10 10 10
    inimigoLonge = Minhoca (Just (4, 9)) (Viva 100) 10 10 10 10 10
    -- Nota: O bot está na linha 5, a próxima posição (5,3) é Terra.

-- TESTE 3: TREPADOR (ANTI-STUCK)
-- Situação: O caminho para o inimigo está bloqueado por uma PEDRA (indestrutível).
-- Expectativa: Move Norte (Saltar/Trepar) em vez de ficar preso a andar contra a pedra.
testeTrepador :: Estado
testeTrepador = Estado mapaComPedra [] [bot, inimigoLonge]
  where
    bot          = Minhoca (Just (4, 2)) (Viva 100) 10 10 10 10 10
    inimigoLonge = Minhoca (Just (4, 9)) (Viva 100) 10 10 10 10 10
    -- Nota: Pedra na posição (4,3).


-- TESTE 4: SEGURANÇA (SUICÍDIO)
-- Situação: Inimigo está "colado" ao bot (distância <= 2).
-- Expectativa: Move (Foge/Reposiciona). NÃO PODE DISPARAR BAZUCA (morreria com a explosão).
testeSuicidio :: Estado
testeSuicidio = Estado mapaLimpo [] [bot, inimigoPerto]
  where
    bot          = Minhoca (Just (4, 2)) (Viva 100) 10 10 10 10 10
    inimigoPerto = Minhoca (Just (4, 3)) (Viva 100) 10 10 10 10 10



-- TESTE 5: PERSEGUIÇÃO PURA
-- Situação: Inimigo longe e diagonal.
-- Expectativa: Move na direção correta (Norte ou Este).
testePerseguicao :: Estado
testePerseguicao = Estado mapaLimpo [] [bot, inimigoAlto]
  where
    bot         = Minhoca (Just (4, 2)) (Viva 100) 10 10 10 10 10
    inimigoAlto = Minhoca (Just (1, 5)) (Viva 100) 10 10 10 10 10

-- =========================================================================
--  3. EXECUÇÃO
-- =========================================================================

testesTarefa4 :: [Estado]
testesTarefa4 = 
    [ testeAssassino      -- T1: Dispara Bazuca
    , testeEscavadora     -- T2: Dispara Bazuca (na Terra)
    , testeTrepador       -- T3: Move Norte (Salta a Pedra)
    , testeSuicidio       -- T5: Move (Não Dispara)
    , testePerseguicao    -- T7: Move Norte (Sobe para o inimigo)
    ]

dataTarefa4 :: IO TaskData
dataTarefa4 = do
    let ins = testesTarefa4
    outs <- mapM (runTest . tatica) ins
    return $ T4 ins outs

main :: IO ()
main = runFeedback =<< dataTarefa4