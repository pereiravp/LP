{-|
Module      : Labs2025
Description : Tipos de dados do jogo.

Módulo com os tipos de dados que vão ser utilizados para modelar as tarefas do trabalho prático de LI1\/LP1 em 2025\/26.
-}
module Labs2025
    ( module Labs2025
    , module Tarefa0_geral
    ) where

import Tarefa0_geral

-- | Um tipo de terreno do mapa.
data Terreno
    -- | Terreno vazio.
    = Ar
    -- | Terreno que afoga minhocas.
    | Agua
    -- | Terreno opaco e destrutivel.
    | Terra
    -- | Terreno opaco e indestrutivel.
    | Pedra
    deriving (Eq,Ord,Show,Read,Enum)

-- | O mapa do jogo é uma matriz de terrenos.
-- Nota: Como é um 'type', não tem construtor. É apenas uma lista de listas.
type Mapa = Matriz Terreno

-- | Os diversos tipos de arma disponíveis para uma minhoca.
data TipoArma = Jetpack | Escavadora | Bazuca | Mina | Dinamite
    deriving (Eq,Ord,Show,Read,Enum)

-- | O estado de saúde de uma minhoca.
data VidaMinhoca
    -- | Está viva com um número inteiros de pontos de vida.
    = Viva Int
    -- | Está morta.
    | Morta
    deriving (Eq,Ord,Show,Read)

-- | O estado completo de uma minhoca.
data Minhoca = Minhoca
    -- | Uma posição no mapa. Opcional porque a minhoca pode ter saído do mapa.
    { posicaoMinhoca :: Maybe Posicao
    -- | O estado de saúde da minhoca.
    , vidaMinhoca :: VidaMinhoca
    -- | Munições de @Jetpack@.
    , jetpackMinhoca :: Int
    -- | Munições de @Escavadora@.
    , escavadoraMinhoca :: Int
    -- | Munições de @Bazuca@.
    , bazucaMinhoca :: Int
    -- | Munições de @Mina@.
    , minaMinhoca :: Int
    -- | Munições de @Dinamite@.
    , dinamiteMinhoca :: Int
    }
    deriving (Eq,Ord,Show,Read)

-- | Um tick é a unidade de tempo do jogo.
type Ticks = Int

-- | O índice de uma minhoca na lista de minhocas.
type NumMinhoca = Int

-- | O índice de um objeto na lista de objetos.
type NumObjeto = Int

-- | Um objeto colocado no mapa.
data Objeto
    -- | Um disparo de uma arma.
    = Disparo
        -- | A posição do disparo no mapa.
        { posicaoDisparo :: Posicao
        -- | A direção do disparo.
        , direcaoDisparo :: Direcao
        -- | O tipo de arma do disparo.
        , tipoDisparo :: TipoArma
        -- | O tempo até o disparo explodir. Opcional porque nem todos os disparos de todas as armas têm um tempo pré-definido para explodir.
        , tempoDisparo :: Maybe Ticks
        -- | A minhoca que efetuou o disparo.
        , donoDisparo :: NumMinhoca
        }
    -- | Um barril de pólvora.
    | Barril
        -- | A posição do barril no mapa.
        { posicaoBarril :: Posicao
        -- | Se o barril está prestes a explodir ou não.
        , explodeBarril :: Bool
        }
    deriving (Eq,Ord,Show,Read)

-- | Estado do jogo.
data Estado = Estado
    -- | O mapa atual.
    { mapaEstado :: Mapa
    -- | Uma lista com os objetos presentes no mapa.
    , objetosEstado :: [Objeto]
    -- | Uma lista com as minhocas no jogo.
    , minhocasEstado :: [Minhoca]
    }
    deriving (Eq,Ord,Show,Read)

-- | Uma jogada que uma minhoca pode efetuar.
data Jogada
    -- | Disparar uma arma numa dada direção.
    = Dispara TipoArma Direcao
    -- | Mover-se numa dada direção.
    | Move Direcao
    deriving (Eq,Ord,Show,Read)