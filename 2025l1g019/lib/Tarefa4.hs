module Tarefa4 where

import Data.List (find, sortOn)
import Data.Maybe (isJust, fromJust, listToMaybe)
import Data.Either (partitionEithers)
import Labs2025
import Tarefa2 (efetuaJogada, temMunicao)
import Tarefa3 (avancaMinhoca, avancaObjeto, aplicaDanos) 

type Danos = [(Posicao, Int)]

--  1. FUNÇÕES ESTRUTURAIS 

-- | Gera a lista de 100 jogadas avançando o tempo entre elas
tatica :: Estado -> [(NumMinhoca, Jogada)]
tatica e = reverse . snd $ foldl avancaTatica (e, []) [0..99]

avancaTatica :: (Estado, [(NumMinhoca, Jogada)]) -> Ticks -> (Estado, [(NumMinhoca, Jogada)])
avancaTatica (e, acc) t = (avancaJogada j e, j : acc)
    where j = jogadaTatica t e

-- | Simula uma jogada e o respetivo avanço de tempo nas minhocas e objetos
avancaJogada :: (NumMinhoca, Jogada) -> Estado -> Estado
avancaJogada (i, j) e@(Estado _ objs ms) = foldr aplicaDanos eFinal danos
    where
        eJogada@(Estado map' objs' ms') = efetuaJogada i j e
        msFinais = map (avancaMinhocaJogada eJogada) (zip3 [0..] ms ms')
        (objsFinais, danos) = partitionEithers $ map (avancaObjetoJogada (eJogada { minhocasEstado = msFinais }) objs) (zip [0..] objs')
        eFinal = Estado map' objsFinais msFinais

avancaMinhocaJogada :: Estado -> (NumMinhoca, Minhoca, Minhoca) -> Minhoca
avancaMinhocaJogada e (i, mAntiga, mNova) 
    | posicaoMinhoca mAntiga == posicaoMinhoca mNova = avancaMinhoca e i mNova
    | otherwise = mNova

avancaObjetoJogada :: Estado -> [Objeto] -> (NumObjeto, Objeto) -> Either Objeto Danos
avancaObjetoJogada e objetos (i, o') = if o' `elem` objetos 
    then avancaObjeto e i o' 
    else Left o'

-- =========================================================================
--  2. A MENTE
-- =========================================================================

jogadaTatica :: Ticks -> Estado -> (NumMinhoca, Jogada)
jogadaTatica t e@(Estado _ _ ms) = 
    case minhocaDaVez of
        Just (i, m) -> (i, cerebroBot m i e)
        Nothing     -> (0, Move Norte)
    where
        total = length ms
        n = if total == 0 then 1 else total
        iVez = t `mod` n
        minhocaDaVez = 
            let mCandidata = ms !! iVez
            in if isViva mCandidata then Just (iVez, mCandidata)
               else find (\(_, m) -> isViva m) (zip [0..] ms)
        isViva m = isJust (posicaoMinhoca m)

cerebroBot :: Minhoca -> Int -> Estado -> Jogada
cerebroBot m i est@(Estado mapa _ ms)
    -- 1. SEGURANÇA
    | distParaAlvo <= 2 = acaoFugir 

    -- 2. ATAQUE: Se houver inimigo na mira, dispara bazuca
    | bazucaMinhoca m > 0 && isJust alvoInimigo = Dispara Bazuca (fromJust alvoInimigo)

    -- 3. OBSTÁCULO: Salta Pedras ou Amigos
    | terrenoFrente == Pedra || temAmigoFrente = 
        case dirMovimento of
            Este  -> Move Nordeste
            Oeste -> Move Noroeste
            _     -> Move Norte

    -- 4. TERRA: Dispara se estiver longe, ou salta se estiver perto
    | temMunicao m Bazuca && isJust terraLonge = Dispara Bazuca (fromJust terraLonge)
    | terrenoFrente == Terra = Move (saltoDirecional dirMovimento)
    
    | otherwise = acaoMovimento
    where
        posM = fromJust (posicaoMinhoca m)
        alvoInimigo = procuraInimigoNaMira posM i est 
        (acaoMovimento, dirMovimento) = calculaMovimento posM i ms
        posFrente = somaPos posM dirMovimento
        
        temAmigoFrente = any (\(idx, mO) -> idx /= i && posicaoMinhoca mO == Just posFrente) (zip [0..] ms)
        acaoFugir = if dirMovimento == Este then Move Oeste else Move Este

        terraLonge = 
            let p3 = somaPos (somaPos (somaPos posM dirMovimento) dirMovimento) dirMovimento
            in if isPosicaoValida p3 mapa && obterTerreno p3 mapa == Terra then Just dirMovimento else Nothing

        inimigosDist = sortOn fst [ (abs(l1-l2) + abs(c1-c2), mi) 
                                   | (idx, mi) <- zip [0..] ms, idx /= i
                                   , isJust (posicaoMinhoca mi)
                                   , let (l1,c1) = posM, let (l2,c2) = fromJust (posicaoMinhoca mi) ]
        distParaAlvo = case inimigosDist of { [] -> 999; ((d,_):_) -> d }
        terrenoFrente = if not (isPosicaoValida posFrente mapa) then Pedra else obterTerreno posFrente mapa
        saltoDirecional Este = Nordeste; saltoDirecional Oeste = Noroeste; saltoDirecional _ = Norte


-- =========================================================================
--  3. FUNÇÕES TÁTICAS E UTILITÁRIOS
-- =========================================================================

procuraInimigoNaMira :: Posicao -> Int -> Estado -> Maybe Direcao
procuraInimigoNaMira posM meuId (Estado mapa _ ms) = listToMaybe possiveis
    where
        possiveis = [ d | (i, mI) <- zip [0..] ms
                        , i /= meuId
                        , isJust (posicaoMinhoca mI)
                        , let posI = fromJust (posicaoMinhoca mI)
                        , let (dist, dir) = calculaTiro posM posI
                        , isJust dir
                        , let d = fromJust dir
                        , dist > 2 && semBloqueio posM posI ms mapa ]

calculaMovimento :: Posicao -> Int -> [Minhoca] -> (Jogada, Direcao)
calculaMovimento (l, c) meuId ms =
    case inimigosOrdenados of
        [] -> (Move Norte, Norte)
        ((_, mPerto):_) -> 
            let (lA, cA) = fromJust (posicaoMinhoca mPerto)
            in if abs (lA - l) > abs (cA - c) 
               then (if lA > l then Move Sul else Move Norte, if lA > l then Sul else Norte)
               else (if cA > c then Move Este else Move Oeste, if cA > c then Este else Oeste)
    where
        inimigos = [(abs(l-l2)+abs(c-c2), m) | (i, m) <- zip [0..] ms, i /= meuId, isJust (posicaoMinhoca m), let (l2,c2) = fromJust (posicaoMinhoca m)]
        inimigosOrdenados = sortOn fst inimigos

calculaTiro :: Posicao -> Posicao -> (Int, Maybe Direcao)
calculaTiro (l1, c1) (l2, c2)
    | l1 == l2 = (abs (c1 - c2), if c2 > c1 then Just Este else Just Oeste)
    | c1 == c2 = (abs (l1 - l2), if l2 > l1 then Just Sul else Just Norte)
    | otherwise = (0, Nothing)

semBloqueio :: Posicao -> Posicao -> [Minhoca] -> Mapa -> Bool
semBloqueio (l1, c1) (l2, c2) ms mapa = not (any bloqueiaW ms) && not (any bloqueiaMap traj)
    where
        traj = if l1 == l2 then [ (l1, c) | c <- [(min c1 c2)+1 .. (max c1 c2)-1] ]
                           else [ (l, c1) | l <- [(min l1 l2)+1 .. (max l1 l2)-1] ]
        bloqueiaW m = case posicaoMinhoca m of { Just p -> p `elem` traj; _ -> False }
        bloqueiaMap p = obterTerreno p mapa `elem` [Pedra, Terra]

somaPos :: Posicao -> Direcao -> Posicao
somaPos (l,c) Norte = (l-1, c); somaPos (l,c) Sul = (l+1, c)
somaPos (l,c) Este = (l, c+1); somaPos (l,c) Oeste = (l, c-1); somaPos p _ = p 

isPosicaoValida :: Posicao -> Mapa -> Bool
isPosicaoValida (l,c) m = l >= 0 && l < length m && c >= 0 && c < length (head m)


obterTerreno :: Posicao -> Mapa -> Terreno
obterTerreno p m = (m !! fst p) !! snd p