{-|
Module      : Tarefa0_geral
Description : Funções auxiliares gerais.

Módulo que define funções genéricas sobre listas e matrizes.
-}

module Tarefa0_geral where

-- Tipos de dados
type Matriz a = [[a]]
type Posicao = (Int,Int)
type Dimensao = (Int,Int)
data Direcao = Norte | Nordeste | Este | Sudeste | Sul | Sudoeste | Oeste | Noroeste
    deriving (Eq,Ord,Show,Read,Enum)

-- Funções não-recursivas.
eIndiceListaValido :: Int -> [a] -> Bool
eIndiceListaValido i l = i >= 0 && i < length l

dimensaoMatriz :: Matriz a -> Dimensao
dimensaoMatriz [] = (0,0)
dimensaoMatriz (h:t)
    | null h    = (0,0)
    | otherwise = (length (h:t), length h)

ePosicaoMatrizValida :: Posicao -> Matriz a -> Bool
ePosicaoMatrizValida (l, c) m =
    let (numLinhas, numColunas) = dimensaoMatriz m
    in l >= 0 && l < numLinhas && c >= 0 && c < numColunas

movePosicao :: Direcao -> Posicao -> Posicao
movePosicao Norte (l, c)     = (l-1, c)
movePosicao Nordeste (l, c)  = (l-1, c+1)
movePosicao Este (l, c)      = (l, c+1)
movePosicao Sudeste (l, c)   = (l+1, c+1)
movePosicao Sul (l, c)       = (l+1, c)
movePosicao Sudoeste (l, c)  = (l+1, c-1)
movePosicao Oeste (l, c)     = (l, c-1)
movePosicao Noroeste (l, c)  = (l-1, c-1)

movePosicaoJanela :: Dimensao -> Direcao -> Posicao -> Posicao
movePosicaoJanela (numL, numC) dir pos =
    let (nl, nc) = movePosicao dir pos
    in if nl >= 0 && nc >= 0 && nl < numL && nc < numC
       then (nl, nc)
       else pos

origemAoCentro :: Dimensao -> Posicao -> Posicao
origemAoCentro (numL, numC) (l, c) =
    let centroL = numL `div` 2
        centroC = numC `div` 2
    in (c - centroC, centroL - l)

rodaPosicaoDirecao :: (Posicao,Direcao) -> (Posicao,Direcao)
rodaPosicaoDirecao (pos, dir) = (pos, novaDir)
  where
    novaDir = case dir of
        Norte    -> Nordeste; Nordeste -> Este; Este     -> Sudeste; Sudeste  -> Sul
        Sul      -> Sudoeste; Sudoeste -> Oeste; Oeste    -> Noroeste; Noroeste -> Norte

-- Funções recursivas.
encontraIndiceLista :: Int -> [a] -> Maybe a
encontraIndiceLista _ [] = Nothing
encontraIndiceLista 0 (h:_) = Just h
encontraIndiceLista i (_:t)
    | i < 0     = Nothing
    | otherwise = encontraIndiceLista (i-1) t

atualizaIndiceLista :: Int -> a -> [a] -> [a]
atualizaIndiceLista _ _ [] = []
atualizaIndiceLista 0 novo (_:t) = novo : t
atualizaIndiceLista i novo (h:t)
    | i < 0     = (h:t)
    | otherwise = h : (atualizaIndiceLista (i-1) novo t)

encontraPosicaoMatriz :: Posicao -> Matriz a -> Maybe a
encontraPosicaoMatriz (l, c) m
    | l < 0 || c < 0 = Nothing
    | otherwise      = encontraLinha l m
  where
    encontraLinha :: Int -> Matriz a -> Maybe a
    encontraLinha _ [] = Nothing
    encontraLinha 0 (h:_) = encontraColuna c h
    encontraLinha n (_:t) = encontraLinha (n-1) t

    encontraColuna :: Int -> [a] -> Maybe a
    encontraColuna _ [] = Nothing
    encontraColuna 0 (h:_) = Just h
    encontraColuna n (_:t) = encontraColuna (n-1) t

atualizaPosicaoMatriz :: Posicao -> a -> Matriz a -> Matriz a
atualizaPosicaoMatriz (l, c) novoEl m
    | not (ePosicaoMatrizValida (l,c) m) = m
    | otherwise      = atualizaLinha l m
  where
    atualizaLinha _ [] = []
    atualizaLinha 0 (h:t) =
        let linhaNova = atualizaIndiceLista c novoEl h
        in linhaNova : t
    atualizaLinha n (h:t) = h : atualizaLinha (n-1) t

moveDirecoesPosicao :: [Direcao] -> Posicao -> Posicao
moveDirecoesPosicao ds pos = foldl (flip movePosicao) pos ds

moveDirecaoPosicoes :: Direcao -> [Posicao] -> [Posicao]
moveDirecaoPosicoes _ [] = []
moveDirecaoPosicoes d (p:ps) =
    (movePosicao d p) : (moveDirecaoPosicoes d ps)

eMatrizValida :: Matriz a -> Bool
eMatrizValida [] = True
eMatrizValida (h:t) = all (\linha -> length linha == length h) t