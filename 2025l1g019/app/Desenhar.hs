module Desenhar where

import Graphics.Gloss
import Labs2025
import Worms
import Data.List (intercalate)

-- * Constantes de Renderização

tamanhoBloco :: Float
tamanhoBloco = 51.0

offsetX :: Float
offsetX = -750

offsetY :: Float
offsetY = 300

escalaMap :: Float
escalaMap = 0.80

escalaWorm :: Float
escalaWorm = 0.5 

escalaArma :: Float
escalaArma = 0.4

-- | Converte coordenadas de matriz (Linha, Coluna) para píxeis Gloss (X, Y).
coordToGloss :: (Int, Int) -> (Float, Float)
coordToGloss (l, c) = (offsetX + fromIntegral c * tamanhoBloco, offsetY - fromIntegral l * tamanhoBloco)

-- * Auxiliares de Mapa (Visual)

getVal :: Int -> Int -> [[Int]] -> Int
getVal l c m 
    | l < 0 || l >= length m = 0 
    | c < 0 || c >= length (head m) = 0 
    | otherwise = (m !! l) !! c

temArEmCima :: Int -> Int -> [[Int]] -> Bool
temArEmCima l c m = getVal (l - 1) c m == 0

temArEsquerda :: Int -> Int -> [[Int]] -> Bool
temArEsquerda l c m = getVal l (c - 1) m == 0

temArDireita :: Int -> Int -> [[Int]] -> Bool
temArDireita l c m = getVal l (c + 1) m == 0

-- | Desenha uma barra de vida proporcional.
desenhaBarraVida :: Int -> Float -> Float -> Picture
desenhaBarraVida hp largura altura = Pictures
    [ Color black $ rectangleSolid (largura + 2) (altura + 2)
    , Color red $ rectangleSolid largura altura
    , Translate (-(largura * (1 - pct)) / 2) 0 $ Color green $ rectangleSolid (largura * pct) altura
    ]
  where pct = fromIntegral hp / 100.0

-- * Função Principal de Desenho

-- | Renderiza o estado atual do jogo para um 'Picture'.
desenha :: Worms -> Picture
desenha w = case wModo w of
    Menu             -> desenhaMenu (wImagens w) (wOpcaoSelect w)
    SelecionarMapa   -> desenhaSelecaoMapa (wImagens w) (wMapaIndex w)
    PaginaComandos   -> desenhaComandos (wImagens w)
    InserirNomes     -> desenhaInserirNomes w
    Jogando          -> desenhaJogo w
    Pausa            -> desenhaPausa w
    Finalizado res   -> desenhaFim w res

-- * Menus e UI

desenhaMenu :: Imagens -> OpcaoMenu -> Picture
desenhaMenu imgs opcao = Pictures
    [ Translate 0 0 $ imgFundo imgs
    , Translate 0 80 $ Scale 0.8 0.8 $ imgLogo imgs
    , desenhaBotao "JOGAR"    (opcao == OpcaoJogar)    (-40)
    , desenhaBotao "BOTS"     (opcao == OpcaoBots)     (-120)
    , desenhaBotao "COMANDOS" (opcao == OpcaoComandos) (-200)
    , desenhaBotao "SAIR"     (opcao == OpcaoSair)     (-280)
    ]

desenhaBotao :: String -> Bool -> Float -> Picture
desenhaBotao texto selecionado y = Translate 0 y $ Scale escala escala $ Pictures
    [ Color (makeColorI 0 0 0 180) $ rectangleSolid 320 70
    , Color cor $ rectangleSolid 300 60 
    , Color white $ rectangleWire 300 60
    , Translate (-calcOffset texto) (-10) $ Scale 0.3 0.3 $ Color white $ Text texto
    ]
  where 
    escala = if selecionado then 1.1 else 1.0
    cor = if selecionado then makeColorI 100 200 100 255 else makeColorI 80 80 80 255
    calcOffset s = fromIntegral (length s) * 10.0

desenhaSelecaoMapa :: Imagens -> Int -> Picture
desenhaSelecaoMapa imgs selecionado = Pictures
    [ Translate 0 0 $ imgFundo imgs
    , Color (makeColorI 0 0 0 200) $ rectangleSolid 1000 600
    , Translate (-200) 200 $ Scale 0.5 0.5 $ Color white $ Text "ESCOLHA O MAPA"
    
    , Translate (-200) 0 $ Pictures 
        [ if selecionado == 1 then Color yellow $ rectangleSolid 220 160 else blank 
        , Scale 0.2 0.2 $ imgMapa1 imgs 
        , Translate (-60) (-120) $ Scale 0.2 0.2 $ Color white $ Text "MAPA 1" 
        ]

    , Translate 200 0 $ Pictures 
        [ if selecionado == 2 then Color yellow $ rectangleSolid 220 160 else blank 
        , Scale 0.2 0.2 $ imgMapa2 imgs 
        , Translate (-60) (-120) $ Scale 0.2 0.2 $ Color white $ Text "MAPA 2" 
        ]
        
    , Translate (-280) (-250) $ Scale 0.2 0.2 $ Color yellow $ Text "Setas: Escolher | Enter: Confirmar"
    ]

desenhaComandos :: Imagens -> Picture
desenhaComandos imgs = Pictures
    [ Translate 0 0 $ imgFundo imgs
    , Color (makeColorI 0 0 0 230) $ rectangleSolid 1100 650 
    , Color white $ rectangleWire 1100 650
    , Translate (-300) 250 $ Scale 0.6 0.6 $ Color green $ Text "COMO JOGAR"
    
    , Translate (-350) 150 $ Scale 0.25 0.25 $ Color yellow $ Text "MOVIMENTO"
    , Translate (-350) 100 $ Scale 0.18 0.18 $ Color white $ Text "A / D : Andar para os lados"
    , Translate (-350) 60  $ Scale 0.18 0.18 $ Color white $ Text "W : Saltar para cima"
    , Translate (-350) 20  $ Scale 0.18 0.18 $ Color white $ Text "Q / E : Saltar na diagonal"
    
    , Translate (-350) (-50) $ Scale 0.25 0.25 $ Color yellow $ Text "COMBATE"
    , Translate (-350) (-100) $ Scale 0.18 0.18 $ Color white $ Text "Setas Cima/Baixo : Apontar Mira"
    , Translate (-350) (-140) $ Scale 0.18 0.18 $ Color white $ Text "ESPACAO (Manter) : Carregar Forca"
    , Translate (-350) (-180) $ Scale 0.18 0.18 $ Color white $ Text "ESPACAO (Soltar) : Disparar!"
    
    , Translate 150 150 $ Scale 0.25 0.25 $ Color yellow $ Text "ARSENAL"
    , Translate 150 100 $ Scale 0.18 0.18 $ Color white $ Text "1. BAZUCA : Disparo padrao explosivo"
    , Translate 150 60  $ Scale 0.18 0.18 $ Color white $ Text "2. MINA : Planta no chao (3s para armar)"
    , Translate 150 20  $ Scale 0.18 0.18 $ Color white $ Text "3. DINAMITE : Explode apos 3 segundos"
    , Translate 150 (-20) $ Scale 0.18 0.18 $ Color white $ Text "4. ESCAVADORA : Remove terra a frente"
    , Translate 150 (-60) $ Scale 0.18 0.18 $ Color white $ Text "5. JETPACK : Voar (Setas para mover)"

    , Translate (-200) (-250) $ Scale 0.2 0.2 $ Color green $ Text "TECLA P: Passar Turno Manualmente"
    , Translate (-250) (-300) $ Scale 0.25 0.25 $ Color yellow $ Text "[ENTER] VOLTAR AO MENU"
    ]

desenhaInserirNomes :: Worms -> Picture
desenhaInserirNomes w = Pictures
    [ Translate 0 0 $ imgFundo (wImagens w)
    , Color (makeColorI 0 0 0 220) $ rectangleSolid 800 400
    , Color white $ rectangleWire 800 400
    , Translate (-200) 100 $ Scale 0.4 0.4 $ Color green $ Text "QUEM VAI JOGAR?"
    , Translate (-300) 0 $ Scale 0.3 0.3 $ Color white $ Text ("Nome da Minhoca " ++ show (length (wNomes w) + 1) ++ ":")
    , Translate (-300) (-80) $ Scale 0.3 0.3 $ Color yellow $ Text ((wNomeBuffer w) ++ "_")
    , Translate (-250) (-150) $ Scale 0.15 0.15 $ Color white $ Text "[ENTER] CONFIRMAR   [BACKSPACE] APAGAR"
    ]

desenhaTecla :: String -> Float -> Float -> Picture
desenhaTecla s x y = Translate x y $ Pictures
    [ Color (makeColorI 50 50 50 255) $ rectangleSolid (largura) 50
    , Color white $ rectangleWire (largura) 50
    , Translate offset (-10) $ Scale 0.2 0.2 $ Color white $ Text s
    ]
  where 
    largura = if length s > 1 then 120 else 50
    offset = if length s > 1 then -40 else -10

desenhaLabel :: String -> Float -> Float -> Picture
desenhaLabel s x y = Translate (x + 50) (y - 10) $ Scale 0.12 0.12 $ Color white $ Text s

desenhaPausa :: Worms -> Picture
desenhaPausa w = Pictures
    [ desenhaJogo w
    , Color (makeColorI 0 0 0 200) $ rectangleSolid 1250 750
    , Scale 0.5 0.5 $ Color white $ Text "PAUSA"
    ]

desenhaFim :: Worms -> Resultado -> Picture
desenhaFim w resultado = Pictures
    [ Translate 0 0 $ imgFundo (wImagens w)
    , Color (makeColorI 0 0 0 220) $ rectangleSolid 1400 800
    , Translate 0 250 $ Scale 0.6 0.6 $ imgWin (wImagens w)
    , desenhaTextoVencedor resultado
    , Translate (-200) 0 $ Pictures (desenhaTabelaPontos (wNomes w) (wPontuacoes w))
    , Translate 0 (-250) $ Scale 0.2 0.2 $ Color white $ Text "Pressione ENTER para voltar ao Menu"
    , Translate 0 (-290) $ Scale 0.15 0.15 $ Color green $ Text "Onde pode iniciar um novo jogo!"
    ]
  where
    safeGetNome i = if i < length (wNomes w) then wNomes w !! i else "Minhoca " ++ show (i+1)

    desenhaTextoVencedor (Vitoria idx) = 
        Translate 0 120 $ Scale 0.4 0.4 $ Color yellow $ Text ("VENCEDOR: " ++ safeGetNome idx)
    
    desenhaTextoVencedor (EmpateEntre idxs) = 
        let nomesEmpatados = map safeGetNome idxs
            texto = "EMPATE: " ++ intercalate " & " nomesEmpatados
        in Translate (-300) 120 $ Scale 0.3 0.3 $ Color yellow $ Text texto

    desenhaTabelaPontos nomes pontos = 
        [ Translate 0 (fromIntegral i * (-50)) $ Pictures
            [ Translate 0 0 $ Scale 0.25 0.25 $ Color (corPorIndice i) $ Text (nome ++ ":")
            , Translate 300 0 $ Scale 0.25 0.25 $ Color white $ Text (show pts ++ " Pts")
            ]
        | (i, (nome, pts)) <- zip [0..] (zip (padNomes nomes) pontos)
        ]
    
    padNomes ns = ns ++ ["M"++show i | i <- [(length ns + 1)..4]]
    
    corPorIndice 0 = red
    corPorIndice 1 = blue
    corPorIndice 2 = green
    corPorIndice 3 = yellow
    corPorIndice _ = white

-- * Renderização do Jogo

desenhaJogo :: Worms -> Picture
desenhaJogo w = Pictures
    [ Translate 0 0 $ imgFundo (wImagens w)
    , desenhaMapa (wMapaVisual w) (wImagens w)
    , desenhaObjetos (objetosEstado (wEstado w)) (wImagens w)
    , desenhaExplosivos (wExplosivos w) (wImagens w)
    , desenhaMinhocas (minhocasEstado (wEstado w)) (wMinhocaAtiva w) (wMira w) (wArmaSelecionada w) (wDirecao w) (wImagens w) (wNomes w)
    , desenhaProjetil (wProjetil w) (wArmaSelecionada w) (wImagens w)
    , desenhaHUD w
    , desenhaMensagens w
    ]

desenhaMapa :: [[Int]] -> Imagens -> Picture
desenhaMapa mapa imgs = Pictures
    [ Translate x y $ desenhaBloco l c valor mapa imgs
    | (l, linha) <- zip [0..] mapa, (c, valor) <- zip [0..] linha, valor > 0
    , let (x, y) = coordToGloss (l, c) ]

-- | Desenha um bloco individual do mapa (com lógica de tiles adjacentes).
desenhaBloco :: Int -> Int -> Int -> [[Int]] -> Imagens -> Picture
desenhaBloco l c tipo mapa imgs = 
    let sprite = Scale escalaMap escalaMap
    in case tipo of
        1 -> let topo = temArEmCima l c mapa
                 esq  = temArEsquerda l c mapa
                 dir  = temArDireita l c mapa
             in if topo then
                 if esq && not dir then sprite (imgGrassLeft imgs)
                 else if not esq && dir then sprite (imgGrassRight imgs)
                 else sprite (imgGrassMid imgs)
             else case (esq, dir) of
                     (True, False) -> sprite (imgSlice01 imgs)
                     (False, True) -> sprite (imgSlice03 imgs)
                     _             -> sprite (imgSlice02 imgs)
        
        2 -> sprite (imgSlice02 imgs)
        3 -> sprite (imgSlice04 imgs)
        4 -> sprite (imgSlice06 imgs)
        5 -> sprite (imgGrassLeft imgs)
        6 -> sprite (imgGrassMid imgs)
        7 -> sprite (imgGrassRight imgs)
        8 -> sprite (imgSlice07 imgs)
        _ -> blank

desenhaExplosivos :: [Explosivo] -> Imagens -> Picture
desenhaExplosivos lista imgs = Pictures [ desenhaExp e imgs | e <- lista ]

desenhaExp :: Explosivo -> Imagens -> Picture
desenhaExp ((x, y), tipo, timer) imgs = 
    case tipo of
        ComMina     -> Translate x y $ Scale 0.25 0.25 $ imgMina imgs
        ComDinamite -> Pictures
            [ Translate x y $ Scale 0.25 0.25 $ imgDinamite imgs
            , Translate (x - 10) (y + 20) $ Scale 0.1 0.1 $ Color yellow $ Text (show (round timer :: Int))
            ]
        _ -> blank

desenhaProjetil :: Maybe (Point, Vector) -> ArmaSelecionada -> Imagens -> Picture
desenhaProjetil Nothing _ _ = blank
desenhaProjetil (Just ((x, y), (vx, vy))) arma imgs = 
    case arma of
        ComMina -> Translate x y $ Scale 0.25 0.25 $ imgMina imgs
        ComDinamite -> Translate x y $ Rotate (x * 5) $ Scale 0.25 0.25 $ imgDinamite imgs
        _ -> let anguloRad = atan2 vy vx
                 anguloGraus = anguloRad * 180 / pi
             in Translate x y $ Rotate (-anguloGraus) $ Scale 0.3 0.3 $ imgMissil imgs

desenhaHUD :: Worms -> Picture
desenhaHUD w = Pictures
    [ desenhaBarraInferior w
    , desenhaInfoTurno w
    , desenhaPainelMinhocas w
    ]

desenhaPainelMinhocas :: Worms -> Picture
desenhaPainelMinhocas w = Translate (-700) 400 $ Pictures 
    [ Color (makeColorI 0 0 0 180) $ rectangleSolid 300 150
    , Color white $ rectangleWire 300 150
    , Pictures [ 
        let nome = if i < length (wNomes w) then wNomes w !! i else "Minhoca " ++ show (i+1)
            pontos = if i < length (wPontuacoes w) then wPontuacoes w !! i else 0
            minhoca = minhocasEstado (wEstado w) !! i
        in desenhaLinhaMinhoca nome pontos minhoca i (i == wMinhocaAtiva w) 
        | i <- [0..3] 
      ]
    ]

desenhaLinhaMinhoca :: String -> Int -> Minhoca -> Int -> Bool -> Picture
desenhaLinhaMinhoca nome pontos m index isAtiva = 
    let y = 45 - (fromIntegral index * 30)
        hp = case vidaMinhoca m of { Viva v -> v; Morta -> 0 }
        corNome = if isAtiva then yellow else white
    in Translate (-140) y $ Pictures
        [ Translate 0 0 $ Scale 0.12 0.12 $ Color corNome $ Text (take 8 nome)
        , Translate 100 5 $ desenhaBarraVida hp 60 8
        , Translate 160 0 $ Scale 0.12 0.12 $ Color white $ Text ("Pts: " ++ show pontos)
        ]

desenhaBarraInferior :: Worms -> Picture
desenhaBarraInferior w = Translate 0 (-480) $ Pictures
    [ Color (makeColorI 50 50 50 200) $ rectangleSolid 800 100
    , Color white $ rectangleWire 800 100
    , desenhaSlotArma w ComBazuca     (Just Bazuca)   "1" (-300) (imgBazuca (wImagens w))
    , desenhaSlotArma w ComMina       (Just Mina)     "2" (-150) (imgMina (wImagens w))
    , desenhaSlotArma w ComDinamite   (Just Dinamite) "3" (0)    (imgDinamite (wImagens w))
    , desenhaSlotArma w ComEscavadora Nothing         "4" (150)  (imgEscavadora (wImagens w))
    , desenhaSlotArma w ComJetpack    Nothing         "5" (300)  (imgJetpack (wImagens w))
    ]

desenhaSlotArma :: Worms -> ArmaSelecionada -> Maybe TipoArma -> String -> Float -> Picture -> Picture
desenhaSlotArma w armaSel maybeTipoArma tecla x img =
    let idx = wMinhocaAtiva w
        minhoca = minhocasEstado (wEstado w) !! idx
        (qtdTexto, mostraQtd) = 
            if armaSel == ComJetpack 
            then (show (round (wTempoJetpack w) :: Int) ++ "s", True)
            else if armaSel == ComDinamite
            then 
                let dinAtiva = filter (\(_, t, _) -> t == ComDinamite) (wExplosivos w)
                in case dinAtiva of
                    ((_, _, t):_) -> (show (round t :: Int) ++ "s", True) 
                    []            -> ("3s", True) 
            else case maybeTipoArma of
                Just Bazuca -> (show (2 - wTirosPorTurno w), True)
                Just Mina   -> (show (minaMinhoca minhoca), True)
                _ -> ("", False)
        selecionada = wArmaSelecionada w == armaSel
        corBorda = if selecionada then green else white
    in Translate x 0 $ Pictures
        [ if selecionada then Color (makeColorI 100 200 100 100) $ rectangleSolid 80 80 else blank
        , Color corBorda $ rectangleWire 80 80
        , Scale 0.25 0.25 $ img
        , Translate (-15) (-35) $ Scale 0.15 0.15 $ Color white $ Text tecla
        , if mostraQtd then Translate 10 (-35) $ Scale 0.12 0.12 $ Color white $ Text ("x" ++ qtdTexto) else blank
        ]

desenhaInfoMinhocaHUD :: Worms -> Picture
desenhaInfoMinhocaHUD w = 
    let idx = wMinhocaAtiva w
        minhoca = minhocasEstado (wEstado w) !! idx
        nome = if null (wNomes w) then "M" ++ show (idx + 1) else (wNomes w) !! idx
        hp = case vidaMinhoca minhoca of { Viva v -> v; Morta -> 0 }
    in Pictures
        [ Translate 0 20 $ Scale 0.15 0.15 $ Color white $ Text nome
        , Translate 0 (-10) $ desenhaBarraVida hp 80 10
        ]

desenhaInfoTurno :: Worms -> Picture
desenhaInfoTurno w = Translate 700 400 $ Pictures
    [ Color (makeColorI 0 0 0 180) $ rectangleSolid 220 80
    , Translate 0 20 $ Scale 0.15 0.15 $ Color white $ Text "TURNO"
    , Translate 0 (-10) $ Scale 0.2 0.2 $ Color white $ Text (show (round (wTempoTurno w) :: Int) ++ "s")
    ]

desenhaMensagens :: Worms -> Picture
desenhaMensagens w = case wMensagem w of
    Nothing -> blank
    Just (texto, _) -> Translate 0 250 $ Pictures
        [ Color (makeColorI 0 0 0 200) $ rectangleSolid 400 60
        , Color white $ rectangleWire 400 60
        , Translate (calcOffset texto) (-8) $ Scale 0.2 0.2 $ Color yellow $ Text texto
        ]
  where calcOffset s = fromIntegral (-length s) * 5

desenhaObjetos :: [Objeto] -> Imagens -> Picture
desenhaObjetos objs imgs = Pictures [ desenhaObjeto obj imgs | obj <- objs ]

desenhaObjeto :: Objeto -> Imagens -> Picture
desenhaObjeto (Barril pos _) imgs = 
    let (x, y) = coordToGloss pos 
    in Translate x y $ Scale 0.2 0.2 $ imgBarril imgs
desenhaObjeto _ _ = blank 

desenhaMinhocas :: [Minhoca] -> Int -> Float -> ArmaSelecionada -> Direcao -> Imagens -> [String] -> Picture
desenhaMinhocas ms indiceAtivo mira arma dirGeral imgs nomes = Pictures
    [ desenhaMinhoca m i (i == indiceAtivo) mira arma dirGeral imgs (if null nomes then "M"++show(i+1) else nomes!!i) 
    | (i, m) <- zip [0..] ms ]

-- | Desenha uma minhoca individual, incluindo acessórios e barra de vida.
desenhaMinhoca :: Minhoca -> Int -> Bool -> Float -> ArmaSelecionada -> Direcao -> Imagens -> String -> Picture
desenhaMinhoca m _ ehAtiva mira arma dirGeral imgs nomeStr = case posicaoMinhoca m of
    Nothing -> blank
    Just pos ->
        let (x, y) = coordToGloss pos
        in case vidaMinhoca m of
            -- Ajuste da Cruz para Y-10 para alinhar com o terreno
            Morta -> Translate x (y - 10) $ Scale 0.25 0.25 $ imgCruz imgs
            
            Viva vida ->
                let
                    hp = vida
                    barraVida = Translate 0 45 $ desenhaBarraVida hp 50 10
                    
                    (scaleX, _) = if dirGeral == Oeste 
                                  then (-escalaWorm, -mira) 
                                  else (escalaWorm, mira)
                    
                    boneco = Scale scaleX escalaWorm $ imgWorm imgs
                    
                    acessorios = if ehAtiva then Pictures
                        [ let escalaCapacete = if dirGeral == Oeste then -escalaArma else escalaArma
                          in Translate 0 15 $ Scale escalaCapacete escalaArma $ imgHelmet imgs
                        , case arma of
                            ComBazuca -> 
                                let sX = if dirGeral == Oeste then -escalaArma else escalaArma
                                    rot = if dirGeral == Oeste then mira else -mira
                                in Translate (if dirGeral == Oeste then -10 else 10) (-5) 
                                   $ Rotate rot $ Scale sX escalaArma $ imgBazuca imgs
                            
                            ComEscavadora ->
                                Translate (if dirGeral == Oeste then -15 else 15) (-5) 
                                $ Scale (if dirGeral == Oeste then -0.2 else 0.2) 0.2 $ imgEscavadora imgs
                            ComJetpack ->
                                Translate (if dirGeral == Oeste then 5 else -5) 0 
                                $ Scale (if dirGeral == Oeste then -escalaArma else escalaArma) escalaArma $ imgJetpack imgs
                            ComMina ->
                                 Translate (if dirGeral == Oeste then -10 else 10) (-5)
                                 $ Scale 0.2 0.2 $ imgMina imgs
                            ComDinamite ->
                                 Translate (if dirGeral == Oeste then -10 else 10) (-5)
                                 $ Scale 0.2 0.2 $ imgDinamite imgs
                            _ -> blank
                        ] else blank
                    nomePic = Translate 0 (-40) $ Scale 0.1 0.1 $ Color white $ Text nomeStr
                in Translate x y $ Pictures [boneco, acessorios, barraVida, nomePic]

desenhaMira :: Float -> Picture
desenhaMira angulo = Translate 15 0 $ Rotate (-angulo) $ Pictures 
    [ Color red $ Line [(0, 0), (40, 0)]
    , Translate 40 0 $ Color red $ circleSolid 3
    ]