{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
{-|
Module      : Tarefa0_2025
Description : Funções auxiliares.

Módulo que define funções auxiliares que serão úteis na resolução do trabalho prático de LI1/LP1 em 2025/26.
|-}

module Tarefa0_2025 where
import Labs2025

import Data.List (find) 

-- Retorna a quantidade de munições disponíveis de uma minhoca para uma dada arma.
encontraQuantidadeArmaMinhoca :: TipoArma -> Minhoca -> Int
encontraQuantidadeArmaMinhoca Jetpack  m = jetpackMinhoca m
encontraQuantidadeArmaMinhoca Escavadora m = escavadoraMinhoca m
encontraQuantidadeArmaMinhoca Bazuca   m = bazucaMinhoca m
encontraQuantidadeArmaMinhoca Mina     m = minaMinhoca m
encontraQuantidadeArmaMinhoca Dinamite m = dinamiteMinhoca m

-- Atualiza a quantidade de munições disponíveis de uma minhoca para uma dada arma.
atualizaQuantidadeArmaMinhoca :: TipoArma -> Minhoca -> Int -> Minhoca
atualizaQuantidadeArmaMinhoca Jetpack  m n = m { jetpackMinhoca = n }
atualizaQuantidadeArmaMinhoca Escavadora m n = m { escavadoraMinhoca = n }
atualizaQuantidadeArmaMinhoca Bazuca   m n = m { bazucaMinhoca = n }
atualizaQuantidadeArmaMinhoca Mina     m n = m { minaMinhoca = n }
atualizaQuantidadeArmaMinhoca Dinamite m n = m { dinamiteMinhoca = n }

-- Verifica se um tipo de terreno é destrutível, i.e., pode ser destruído por explosões.
eTerrenoDestrutivel :: Terreno -> Bool
eTerrenoDestrutivel Terra = True
eTerrenoDestrutivel _     = False

-- Verifica se um tipo de terreno é opaco, i.e., não permite que objetos ou minhocas se encontrem por cima dele.
eTerrenoOpaco :: Terreno -> Bool
eTerrenoOpaco Terra = True
eTerrenoOpaco Pedra = True
eTerrenoOpaco _     = False

-- Verifica se uma posição está dentro dos limites do mapa.
posDentroMapa :: Posicao -> Mapa -> Bool
posDentroMapa (l, c) m =
    let numLinhas = length m
        numColunas = if numLinhas > 0 then length (head m) else 0
    in l >= 0 && l < numLinhas && c >= 0 && c < numColunas

-- Retorna o Terreno numa dada Posição
obterTerreno :: Posicao -> Mapa -> Terreno
obterTerreno (l,c) m = (m !! l) !! c

-- Verifica se uma posição do mapa está livre, i.e., pode ser ocupada por um objeto ou minhoca.
ePosicaoMapaLivre :: Posicao -> Mapa -> Bool
ePosicaoMapaLivre p m =posDentroMapa p m && not (eTerrenoOpaco (obterTerreno p m))

-- Funções auxiliares para ePosicaoEstadoLivre
ocupadoPorBarril :: Posicao -> [Objeto] -> Bool
ocupadoPorBarril _ []=False
ocupadoPorBarril p (obj:resto)
                       |eEsteBarril obj p =True
                       |otherwise=ocupadoPorBarril p resto

-- Função auxiliar de ocupadoPorBarril
eEsteBarril::Objeto->Posicao->Bool
eEsteBarril (Barril po _) p =p==po
eEsteBarril _ _            =False

ocupadoPorMinhoca :: Posicao -> [Minhoca] -> Bool
ocupadoPorMinhoca _ [] =False
ocupadoPorMinhoca p (m:resto)
                  |posicaoMinhoca m == Just p = True
                  |otherwise=ocupadoPorMinhoca p resto

-- Verifica se uma posição do estado está livre, i.e., pode ser ocupada por um objeto ou minhoca.
ePosicaoEstadoLivre :: Posicao -> Estado -> Bool
ePosicaoEstadoLivre p est =
    ePosicaoMapaLivre p (mapaEstado est) &&
    not (ocupadoPorBarril p (objetosEstado est)) &&
    not (ocupadoPorMinhoca p (minhocasEstado est))

-- Verifica se numa lista de objetos já existe um disparo feito para uma dada arma por uma dada minhoca.
minhocaTemDisparo :: TipoArma -> NumMinhoca -> [Objeto] -> Bool
minhocaTemDisparo tipoArma numMinhoca objs =
    case find isDisparo objs of
        Just _ -> True
        Nothing -> False
    where
        isDisparo o = case o of
            (Disparo _ _ tipo _ dono) -> tipo == tipoArma && dono == numMinhoca
            _ -> False

-- Destrói uma dada posição no mapa (tipicamente como consequência de uma explosão).
destroiPosicao :: Posicao -> Mapa -> Mapa
destroiPosicao (l, c) m =
    if not (posDentroMapa (l, c) m) || not (eTerrenoDestrutivel (obterTerreno (l,c) m))
    then m
    else
        let linhaModificada = take c (m !! l) ++ [Ar] ++ drop (c + 1) (m !! l)
        in take l m ++ [linhaModificada] ++ drop (l + 1) m

-- Adiciona um novo objeto a um estado.
adicionaObjeto :: Objeto -> Estado -> Estado
adicionaObjeto obj est =
    est { objetosEstado = obj : objetosEstado est }