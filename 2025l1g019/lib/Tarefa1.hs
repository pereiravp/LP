module Tarefa1 where

import Labs2025
import Data.List (nub) 

-- Função principal que valida um estado completo do jogo.
validaEstado :: Estado -> Bool
validaEstado estado =
    validaMapa (mapaEstado estado) &&
    validaObjetos estado &&
    validaMinhocas estado

-- Valida o Mapa (não vazio, retangular).
validaMapa :: Mapa -> Bool
validaMapa [] = False
validaMapa (l:ls) = not (null l) && all (\linha -> length linha == length l) ls

-- Valida todos os objetos no estado.
validaObjetos :: Estado -> Bool
validaObjetos e = all (objetoValido e) (objetosEstado e) && disparosUnicos (objetosEstado e)
  where
    objetoValido :: Estado -> Objeto -> Bool
    objetoValido est obj = case obj of
        Barril pos _ ->
            posicaoValida (mapaEstado est) pos &&
            not (terrenoOpaco (mapaEstado est) pos) &&
            not (posicaoOcupadaPorOutroBarril obj (objetosEstado est)) &&
            not (posicaoOcupadaPorMinhoca pos (minhocasEstado est))

   
        Disparo pos _ tipo tempo dono ->
            tipo `notElem` [Jetpack, Escavadora] && 
            tempoValido tipo tempo &&              
            dono >= 0 && dono < length (minhocasEstado est) && 
            posicaoValida (mapaEstado est) pos &&
          
            (not (terrenoOpaco (mapaEstado est) pos) || tipo == Bazuca) &&
            case tipo of
                 Mina -> not (posicaoOcupadaPorBarril pos (objetosEstado est))
                 Dinamite -> not (posicaoOcupadaPorBarril pos (objetosEstado est))
                 _ -> True

    tempoValido Bazuca Nothing = True
    tempoValido Mina Nothing = True
    tempoValido Mina (Just t) = t >= 0 && t <= 2
    tempoValido Dinamite (Just t) = t >= 0 && t <= 4
    tempoValido _ _ = False

    disparosUnicos objs =
        let disparos = [(dono, tipo) | Disparo _ _ tipo _ dono <- objs]
        in length disparos == length (nub disparos)

-- Valida todas as minhocas. (Função corrigida)
validaMinhocas :: Estado -> Bool
validaMinhocas e =
    not (posicoesDuplicadas (minhocasEstado e)) &&
    all (minhocaValida e) (minhocasEstado e)
  where
    posicoesDuplicadas :: [Minhoca] -> Bool
    posicoesDuplicadas ms =
        let ps = [p | m <- ms, Just p <- [posicaoMinhoca m]]
        in length (nub ps) /= length ps

    minhocaValida est m = case posicaoMinhoca m of
        Nothing -> vidaMinhoca m == Morta 
        Just pos ->
            posicaoValida (mapaEstado est) pos &&
            not (posicaoOcupadaPorBarril pos (objetosEstado est)) &&
            case terrenoNaPosicao (mapaEstado est) pos of
                Agua -> vidaMinhoca m == Morta 
                _    -> validaVida (vidaMinhoca m) && validaMunicoes m

    validaVida (Viva v) = v > 0 && v <= 100 
    validaVida Morta = True

    validaMunicoes m = all (>= 0) [jetpackMinhoca m, escavadoraMinhoca m, bazucaMinhoca m, minaMinhoca m, dinamiteMinhoca m]

-- Funções Auxiliares Genéricas

-- Verifica se uma posição está dentro dos limites do mapa.
posicaoValida :: Mapa -> Posicao -> Bool
posicaoValida m (x,y)
    | null m || null (head m) = False 
    | y < 0 || y >= length m = False
    | x < 0 || x >= length (head m) = False
    | otherwise = True

-- Verifica se o terreno numa dada posição é opaco (Terra ou Pedra).
terrenoOpaco :: Mapa -> Posicao -> Bool
terrenoOpaco m p
    | not (posicaoValida m p) = True 
    | otherwise = case terrenoNaPosicao m p of
        Pedra -> True
        Terra -> True
        _     -> False

-- Retorna o tipo de terreno numa dada posição. (Assume posição válida)
terrenoNaPosicao :: Mapa -> Posicao -> Terreno
terrenoNaPosicao m (x,y) = (m !! y) !! x

-- Verifica se uma posição está ocupada por um barril.
posicaoOcupadaPorBarril :: Posicao -> [Objeto] -> Bool
posicaoOcupadaPorBarril pos objs = any (\o -> case o of Barril p _ -> p == pos; _ -> False) objs

-- Verifica se uma posição está ocupada por um barril *diferente* do barril atual.
posicaoOcupadaPorOutroBarril :: Objeto -> [Objeto] -> Bool
posicaoOcupadaPorOutroBarril meuBarril objs = any (\o -> case o of Barril p _ -> p == posicaoBarril meuBarril && o /= meuBarril; _ -> False) objs

-- Verifica se uma posição está ocupada por *qualquer* minhoca.
posicaoOcupadaPorMinhoca :: Posicao -> [Minhoca] -> Bool
posicaoOcupadaPorMinhoca pos ms = any (\m -> posicaoMinhoca m == Just pos) ms

-- Verifica se uma posição está ocupada por *outra* minhoca (excluindo a própria).
posicaoOcupadaPorOutraMinhoca :: Minhoca -> [Minhoca] -> Bool
posicaoOcupadaPorOutraMinhoca minhaMinhoca ms = any (\m -> posicaoMinhoca m == posicaoMinhoca minhaMinhoca && m /= minhaMinhoca) ms