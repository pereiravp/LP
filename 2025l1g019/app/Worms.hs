module Worms where

import Graphics.Gloss 
import Labs2025
import Data.List (elemIndices)

-- * Tipos de Dados Principais

-- | Define os diferentes estados/ecrãs do jogo.
data Modo 
    = Menu              -- ^ Menu principal
    | SelecionarMapa    -- ^ Ecrã de escolha de nível
    | PaginaComandos    -- ^ Ecrã de ajuda
    | InserirNomes      -- ^ Configuração de jogadores
    | Jogando           -- ^ Jogo a decorrer
    | Pausa             -- ^ Menu de pausa
    | Finalizado Resultado -- ^ Ecrã de vitória/empate
    deriving (Eq, Show)

-- | Resultado final da partida.
data Resultado 
    = Vitoria Int       -- ^ Índice da minhoca vencedora
    | EmpateEntre [Int] -- ^ Lista de índices em caso de empate
    deriving (Eq, Show)

-- | Opções de navegação do Menu Principal.
data OpcaoMenu = OpcaoJogar | OpcaoBots | OpcaoComandos | OpcaoSair
    deriving (Eq, Show, Enum)

-- | Configurações para a IA (Bot).
data ConfigBot = ConfigBot
    { jogadorHumano :: Int      -- ^ Índice do jogador humano
    , minhocasBots  :: [Int]    -- ^ Índices controlados pelo CPU
    , delayBotTurno :: Float    -- ^ Tempo de espera para ação do bot
    } deriving (Eq, Show)

-- | Armazena todos os assets gráficos carregados.
data Imagens = Imagens
    { imgWorm, imgGrassLeft, imgGrassMid, imgGrassRight :: Picture
    , imgSlice01, imgSlice02, imgSlice03, imgSlice04, imgSlice05, imgSlice06, imgSlice07 :: Picture
    , imgBazuca, imgMissil, imgHelmet, imgFundo, imgLogo :: Picture
    , imgEscavadora, imgDinamite, imgBarril, imgMina, imgJetpack :: Picture
    , imgWin :: Picture 
    , imgCruz :: Picture 
    , imgMapa1 :: Picture
    , imgMapa2 :: Picture
    }

-- | Armas e ferramentas disponíveis.
data ArmaSelecionada = SemArma | ComBazuca | ComMina | ComDinamite | ComEscavadora | ComJetpack
    deriving (Eq, Show, Enum)

-- | Representação de um explosivo ativo: (Posição, Tipo, Timer).
type Explosivo = (Point, ArmaSelecionada, Float)

-- | Estado global da aplicação (Gloss).
data Worms = Worms
    { wEstado          :: Estado           -- ^ Estado físico do jogo (Labs2025)
    , wMinhocaAtiva    :: Int              -- ^ Índice da minhoca atual
    , wImagens         :: Imagens          -- ^ Sprites
    , wModo            :: Modo             -- ^ Ecrã atual
    , wOpcaoSelect     :: OpcaoMenu        -- ^ Seleção no menu
    , wTempo           :: Float            -- ^ Contador de tempo geral
    , wMira            :: Float            -- ^ Ângulo da mira
    , wArmaSelecionada :: ArmaSelecionada  -- ^ Arma equipada
    , wTempoTurno      :: Float            -- ^ Tempo restante do turno
    , wMensagem        :: Maybe (String, Float) -- ^ Mensagens de feedback no ecrã
    , wProjetil        :: Maybe (Point, Vector) -- ^ Projétil ativo (Pos, Vel)
    , wDirecao         :: Direcao          -- ^ Direção da minhoca ativa
    , wTirosPorTurno   :: Int              -- ^ Contador de ações no turno
    , wNomes           :: [String]         -- ^ Nomes dos jogadores
    , wPontuacoes      :: [Int]            -- ^ Pontuação acumulada
    , wNomeBuffer      :: String           -- ^ Buffer para input de texto
    , wTempoJetpack    :: Float            -- ^ Combustível do Jetpack
    , wSpacePressed    :: Bool             -- ^ Estado da tecla Espaço
    , wDelayJetpack    :: Float            -- ^ Cooldown do movimento do Jetpack
    , wExplosivos      :: [Explosivo]      -- ^ Lista de explosivos no mapa
    , wDelayEscavadora :: Float            -- ^ Cooldown da escavadora
    , wMapaVisual      :: [[Int]]          -- ^ Mapa para renderização (tiles)
    , wConfigBot       :: Maybe ConfigBot  -- ^ Configuração de IA (se ativa)
    , wBotDelay        :: Float            -- ^ Timer interno do Bot
    , wBotProximaJogada :: Maybe Jogada    -- ^ Próxima ação planeada pelo Bot
    , wMapaIndex       :: Int              -- ^ ID do mapa selecionado
    }

-- * Definição de Níveis

-- | Devolve a matriz de inteiros correspondente ao ID do mapa.
getMapa :: Int -> [[Int]]
getMapa 1 = -- Mapa Original
    [ [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
    , [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
    , [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
    , [0,0,0,0,0,0,0,0,2,2,2,0,0,0,0,0,0,0,0,0,0,2,2,2,0,0,0,0,0,0,0]
    , [0,0,0,0,0,0,0,0,8,8,8,0,0,0,0,0,0,0,0,0,0,8,8,8,0,0,0,0,0,0,0]
    , [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0] 
    , [0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,2,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
    , [0,0,0,2,0,0,0,0,0,0,0,0,0,2,4,4,4,2,0,0,0,0,0,0,0,0,0,2,0,0,0] 
    , [5,7,0,4,2,0,0,0,0,0,0,0,2,4,4,4,4,4,2,0,0,0,0,0,0,0,2,4,0,7,5]
    , [0,0,0,4,4,2,0,0,0,0,0,2,4,4,4,4,4,4,4,2,0,0,0,0,0,2,4,4,0,0,0] 
    , [0,0,0,4,4,4,2,2,2,2,2,4,4,4,4,4,4,4,4,4,2,2,2,2,2,4,4,4,0,0,0] 
    , [0,0,0,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,0,0,0] 
    , [0,0,0,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,0,0,0] 
    , [0,0,0,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,0,0,0] 
    ]

getMapa 2 = -- Mapa Ilhas Flutuantes
    [ [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
    , [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
    , [2,2,2,2,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,2,2,2,2,2,0]
    , [4,4,4,4,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4,4,4,4,4,4,0]
    , [0,0,0,0,0,0,0,0,0,0,2,2,2,2,2,2,2,2,2,0,0,0,0,0,0,0,0,0,0,0,0]
    , [0,0,0,0,0,0,0,0,0,0,4,4,4,4,4,4,4,4,4,0,0,0,0,0,0,0,0,0,0,0,0]
    , [0,0,0,0,0,0,0,0,0,0,8,8,8,8,8,8,8,8,8,0,0,0,0,0,0,0,0,0,0,0,0]
    , [0,0,0,2,2,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,2,2,0,0,0,0,0]
    , [0,0,2,4,4,4,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,4,4,4,2,0,0,0,0]
    , [0,0,4,4,4,4,4,0,0,0,0,0,2,2,2,2,2,0,0,0,0,0,4,4,4,4,4,0,0,0,0]
    , [0,0,0,0,0,0,0,0,0,0,0,2,4,4,4,4,4,2,0,0,0,0,0,0,0,0,0,0,0,0,0]
    , [2,2,2,2,2,2,2,2,0,0,2,4,4,4,4,4,4,4,2,0,0,2,2,2,2,2,2,2,2,2,2]
    , [4,4,4,4,4,4,4,4,0,0,4,4,4,4,4,4,4,4,4,0,0,4,4,4,4,4,4,4,4,4,4]
    , [8,8,8,8,8,8,8,8,0,0,4,4,4,4,4,4,4,4,4,0,0,8,8,8,8,8,8,8,8,8,8]
    ]
getMapa _ = getMapa 1 

-- * Inicialização

-- | Gera o estado inicial do jogo com base no mapa escolhido.
estadoInicial :: Imagens -> Int -> Worms
estadoInicial imgs mapaId = 
    let mapaV = getMapa mapaId
        mapaF = map (map (\n -> if n > 0 then Terra else Ar)) mapaV
        
        -- Define posições iniciais conforme o mapa
        (posWorms, posBarris) = case mapaId of
            1 -> ( [(2,9), (2,22), (7,1), (7,29)] 
                 , [(5,15), (8,25)]               
                 )
            2 -> ( [(1,2), (1,26), (6,4), (6,24)] 
                 , [(3,14), (8,14)] 
                 )
            _ -> ([(2,9), (2,22), (7,1), (7,29)], [])

        listaMinhocas = [ Minhoca (Just p) (Viva 100) 3 3 5 3 2 | p <- posWorms ]
        listaBarris   = [ Barril p False | p <- posBarris ]

    in Worms
    { wEstado = Estado mapaF listaBarris listaMinhocas
    , wMinhocaAtiva = 0
    , wImagens = imgs
    , wModo = Menu
    , wOpcaoSelect = OpcaoJogar
    , wTempo = 0.0
    , wMira = 0.0
    , wArmaSelecionada = ComBazuca
    , wTempoTurno = 15.0
    , wMensagem = Nothing
    , wProjetil = Nothing
    , wDirecao = Este
    , wTirosPorTurno = 0
    , wNomes = []
    , wPontuacoes = [0,0,0,0]
    , wNomeBuffer = ""
    , wTempoJetpack    = 3.0
    , wSpacePressed    = False
    , wDelayJetpack    = 0.0
    , wExplosivos      = []
    , wDelayEscavadora = 0.0
    , wMapaVisual      = mapaV
    , wConfigBot       = Nothing
    , wBotDelay        = 0.0
    , wBotProximaJogada = Nothing
    , wMapaIndex       = mapaId 
    }

-- * Funções Utilitárias

-- | Reinicia o jogo mantendo as imagens carregadas.
reiniciarJogo :: Worms -> [String] -> Worms
reiniciarJogo wAntigo novosNomes = 
    let novoW = estadoInicial (wImagens wAntigo) (wMapaIndex wAntigo)
    in novoW { wNomes = novosNomes, wModo = Jogando }

-- | Adiciona uma mensagem temporária ao ecrã.
adicionaMensagem :: String -> Float -> Worms -> Worms
adicionaMensagem texto duracao w = w { wMensagem = Just (texto, duracao) }

-- | Calcula o índice da próxima minhoca viva.
proximaMinhocaViva :: Worms -> Int
proximaMinhocaViva w = 
    let ms = minhocasEstado (wEstado w)
        total = length ms
        ativo = wMinhocaAtiva w
        buscar i = 
            let idx = (ativo + i) `mod` total
                m = ms !! idx
            in if i > total then ativo 
               else case vidaMinhoca m of { Viva _ -> idx; Morta -> buscar (i + 1) }
    in buscar 1

-- | Verifica se existem condições de vitória ou empate.
verificaVitoria :: Worms -> Maybe Resultado
verificaVitoria w = 
    let ms = minhocasEstado (wEstado w)
        vivas = filter estaViva (zip [0..] ms)
        jogoAcabou = length vivas <= 1
        pontos = wPontuacoes w
        maxPontos = maximum pontos
        vencedores = elemIndices maxPontos pontos
    in if jogoAcabou
       then case vencedores of
            [unico] -> Just (Vitoria unico)
            varios  -> Just (EmpateEntre varios)
       else Nothing
  where
    estaViva (_, m) = case vidaMinhoca m of { Viva _ -> True; Morta -> False }

-- | Verifica se é o turno de um Bot.
ehTurnoDoBOT :: Worms -> Bool
ehTurnoDoBOT w = case wConfigBot w of
    Nothing -> False
    Just cfg -> wMinhocaAtiva w `elem` minhocasBots cfg

-- | Configura o jogo para o modo Bot (1 Humano vs 3 Bots).
iniciarModoBOT :: Worms -> Worms
iniciarModoBOT w = 
    let wLimpo = estadoInicial (wImagens w) (wMapaIndex w)
    in wLimpo 
    { wConfigBot = Just ConfigBot 
        { jogadorHumano = 0           
        , minhocasBots = [1, 2, 3]    
        , delayBotTurno = 1.0         
        }
    , wNomes = ["VOCÊ", "BOT 1", "BOT 2", "BOT 3"]
    , wModo = Jogando
    , wBotDelay = 0.0
    , wBotProximaJogada = Nothing
    , wMensagem = Just ("Modo BOT: Voce vs 3", 3.0)
    }