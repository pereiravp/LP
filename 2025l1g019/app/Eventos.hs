module Eventos (reageEventos, usaJetpack) where

import Graphics.Gloss.Interface.Pure.Game
import Worms
import Labs2025
import Tarefa2 (efetuaJogada, temMunicao)
import Desenhar (coordToGloss)
import Data.Maybe (isJust)

-- * Gestão Principal de Eventos

-- | Distribui os eventos do teclado para a função correta dependendo do modo atual.
reageEventos :: Event -> Worms -> Worms
reageEventos event w = case wModo w of
    Menu           -> reageMenu event w
    SelecionarMapa -> reageSelecionarMapa event w
    PaginaComandos -> reageComandos event w 
    InserirNomes   -> reageInserirNomes event w
    Jogando        -> reageJogo event w
    Pausa          -> reagePausa event w
    Finalizado _   -> reageFim event w

-- * Eventos de Menu

-- | Navegação no Menu Principal.
reageMenu :: Event -> Worms -> Worms
reageMenu (EventKey (SpecialKey KeyEnter) Down _ _) w = 
    case wOpcaoSelect w of
        OpcaoJogar    -> w { wModo = SelecionarMapa, wMapaIndex = 1 } 
        OpcaoBots     -> iniciarModoBOT w
        OpcaoComandos -> w { wModo = PaginaComandos }
        OpcaoSair     -> error "Saindo..."
reageMenu (EventKey (SpecialKey KeyUp) Down _ _) w = w { wOpcaoSelect = opcaoAnterior (wOpcaoSelect w) }
reageMenu (EventKey (SpecialKey KeyDown) Down _ _) w = w { wOpcaoSelect = opcaoSeguinte (wOpcaoSelect w) }
reageMenu _ w = w

-- | Seleção do Mapa (Esquerda/Direita).
reageSelecionarMapa :: Event -> Worms -> Worms
reageSelecionarMapa (EventKey (SpecialKey KeyLeft) Down _ _) w = w { wMapaIndex = 1 }
reageSelecionarMapa (EventKey (SpecialKey KeyRight) Down _ _) w = w { wMapaIndex = 2 }
reageSelecionarMapa (EventKey (SpecialKey KeyEnter) Down _ _) w = 
    w { wModo = InserirNomes, wNomes = [], wNomeBuffer = "" }
reageSelecionarMapa (EventKey (SpecialKey KeyBackspace) Down _ _) w = w { wModo = Menu }
reageSelecionarMapa _ w = w

reageComandos :: Event -> Worms -> Worms
reageComandos (EventKey (SpecialKey KeyBackspace) Down _ _) w = w { wModo = Menu } 
reageComandos (EventKey (SpecialKey KeyEnter) Down _ _) w = w { wModo = Menu }
reageComandos _ w = w

-- * Inserção de Nomes

apagarCarater :: Worms -> Worms
apagarCarater w = 
    let buffer = wNomeBuffer w
    in if null buffer 
       then w 
       else w { wNomeBuffer = init buffer }

reageInserirNomes :: Event -> Worms -> Worms
reageInserirNomes (EventKey (SpecialKey KeyEnter) Down _ _) w =
    let nomeAtual = wNomeBuffer w
        nomeFinal = if null nomeAtual then "Minhoca " ++ show (length (wNomes w) + 1) else nomeAtual
        novosNomes = wNomes w ++ [nomeFinal]
    in if length novosNomes == 4 
       then reiniciarJogo w novosNomes
       else w { wNomes = novosNomes, wNomeBuffer = "" }

reageInserirNomes (EventKey (SpecialKey KeyBackspace) Down _ _) w = apagarCarater w
reageInserirNomes (EventKey (SpecialKey KeyDelete)    Down _ _) w = apagarCarater w
reageInserirNomes (EventKey (Char '\b')               Down _ _) w = apagarCarater w
reageInserirNomes (EventKey (Char '\DEL')             Down _ _) w = apagarCarater w

reageInserirNomes (EventKey (Char c) Down _ _) w =
    let buffer = wNomeBuffer w
    in if length buffer < 10 && c >= ' '
       then w { wNomeBuffer = buffer ++ [c] } 
       else w

reageInserirNomes _ w = w

-- * Pausa e Fim de Jogo

reagePausa :: Event -> Worms -> Worms
reagePausa (EventKey (SpecialKey KeyBackspace) Down _ _) w = w { wModo = Jogando }
reagePausa (EventKey (Char 'm') Down _ _) w = w { wModo = Menu }
reagePausa _ w = w

reageFim :: Event -> Worms -> Worms
reageFim (EventKey (SpecialKey KeyEnter) Down _ _) w = w { wModo = Menu }
reageFim _ w = w

-- * Navegação Auxiliar

opcaoSeguinte :: OpcaoMenu -> OpcaoMenu
opcaoSeguinte OpcaoJogar    = OpcaoBots
opcaoSeguinte OpcaoBots     = OpcaoComandos
opcaoSeguinte OpcaoComandos = OpcaoSair
opcaoSeguinte OpcaoSair     = OpcaoJogar

opcaoAnterior :: OpcaoMenu -> OpcaoMenu
opcaoAnterior OpcaoJogar    = OpcaoSair
opcaoAnterior OpcaoBots     = OpcaoJogar
opcaoAnterior OpcaoComandos = OpcaoBots
opcaoAnterior OpcaoSair     = OpcaoComandos

-- * Lógica In-Game

reageJogo :: Event -> Worms -> Worms
reageJogo event w 
    | ehTurnoDoBOT w = w 
    | otherwise = processarInputJogador event w

-- | Processa input apenas se a minhoca estiver viva.
processarInputJogador :: Event -> Worms -> Worms
processarInputJogador event w =
    let idx = wMinhocaAtiva w
        minhoca = minhocasEstado (wEstado w) !! idx
    in case vidaMinhoca minhoca of
        Morta -> w -- Ignora input se morta
        Viva _ -> processarInputVivo event w

processarInputVivo :: Event -> Worms -> Worms
processarInputVivo (EventKey (Char 'p') Down _ _) w = passarTurnoManual w

processarInputVivo (EventKey (SpecialKey KeySpace) Down _ _) w = 
    case wArmaSelecionada w of
        ComJetpack -> w { wSpacePressed = True } 
        _          -> dispararArma w 

processarInputVivo (EventKey (SpecialKey KeySpace) Up _ _) w = 
    w { wSpacePressed = False } 

processarInputVivo (EventKey (SpecialKey KeyLeft) Down _ _) w = 
    case wArmaSelecionada w of
        ComJetpack -> if wSpacePressed w && wTempoJetpack w > 0 then usaJetpack w (0, -1) else w
        _          -> (executaJogada (Move Oeste) w) { wDirecao = Oeste }

processarInputVivo (EventKey (SpecialKey KeyRight) Down _ _) w = 
    case wArmaSelecionada w of
        ComJetpack -> if wSpacePressed w && wTempoJetpack w > 0 then usaJetpack w (0, 1) else w
        _          -> (executaJogada (Move Este) w) { wDirecao = Este }

processarInputVivo (EventKey (Char 'w') Down _ _) w = executaJogada (Move Norte) w
processarInputVivo (EventKey (Char 'q') Down _ _) w = (executaJogada (Move Noroeste) w) { wDirecao = Oeste }
processarInputVivo (EventKey (Char 'e') Down _ _) w = (executaJogada (Move Nordeste) w) { wDirecao = Este }

processarInputVivo (EventKey (SpecialKey KeyUp) Down _ _) w   = w { wMira = min 90 (wMira w + 5) }
processarInputVivo (EventKey (SpecialKey KeyDown) Down _ _) w = w { wMira = max (-90) (wMira w - 5) }

processarInputVivo (EventKey (Char c) Down _ _) w
    | isJust (wProjetil w) = w
    | c == '1' = w { wArmaSelecionada = ComBazuca }
    | c == '2' = w { wArmaSelecionada = ComMina }
    | c == '3' = w { wArmaSelecionada = ComDinamite }
    | c == '4' = w { wArmaSelecionada = ComEscavadora }
    | c == '5' = w { wArmaSelecionada = ComJetpack }
    | otherwise = w

processarInputVivo (EventKey (SpecialKey KeyBackspace) Down _ _) w = w { wModo = Pausa }
processarInputVivo _ w = w

-- * Execução de Ações

executaJogada :: Jogada -> Worms -> Worms
executaJogada jogada w = 
    let idx = wMinhocaAtiva w
        estadoAtual = wEstado w
        novoEstado = efetuaJogada idx jogada estadoAtual
    in if novoEstado /= estadoAtual 
       then w { wEstado = novoEstado, wMensagem = Just ("Movimento!", 1.0) }
       else w { wMensagem = Just ("Bloqueado", 0.5) }

usaJetpack :: Worms -> (Int, Int) -> Worms
usaJetpack w (dl, dc) =
    let idx = wMinhocaAtiva w
        (Estado m o minhos) = wEstado w
        minhoca = minhos !! idx
    in case posicaoMinhoca minhoca of
        Nothing -> w
        Just (l, c) -> 
            let novoL = l + dl
                novoC = c + dc
                dentro = novoL >= 0 && novoL < length m && novoC >= 0 && novoC < length (head m)
                terrenoLivre = dentro && ((m !! novoL) !! novoC `elem` [Ar, Agua])
                novaMinhoca = if terrenoLivre 
                              then minhoca { posicaoMinhoca = Just (novoL, novoC) } 
                              else minhoca
                novasMinhos = take idx minhos ++ [novaMinhoca] ++ drop (idx + 1) minhos
            in if terrenoLivre 
               then w { wEstado = Estado m o novasMinhos, wDirecao = if dc < 0 then Oeste else if dc > 0 then Este else wDirecao w }
               else w { wMensagem = Just ("Colisão!", 0.5) }

usaEscavadora :: Worms -> Worms
usaEscavadora w =
    if wDelayEscavadora w > 0 then w
    else
        let idx = wMinhocaAtiva w
            (Estado mapa o minhos) = wEstado w
            minhoca = minhos !! idx
        in case posicaoMinhoca minhoca of
            Nothing -> w
            Just (l, c) -> 
                let (targetL, targetC) = case wDirecao w of 
                        Oeste -> (l, c - 1)
                        Este  -> (l, c + 1)
                        _     -> (l, c)
                    
                    mapaVis = wMapaVisual w
                    dentro = targetL >= 0 && targetL < length mapa && targetC >= 0 && targetC < length (head mapa)
                    
                    tipoBloco = if dentro then (mapaVis !! targetL) !! targetC else 0
                    
                    ehTerraValida = dentro && tipoBloco /= 0 && tipoBloco /= 8
                
                in if ehTerraValida
                   then 
                       let novoMapa = atualizaMatriz mapa targetL targetC Ar
                           novoMapaVisual = atualizaMatriz mapaVis targetL targetC 0
                           
                           ptsAntigos = wPontuacoes w
                           ptsAtuais = ptsAntigos !! idx
                           novosPts = take idx ptsAntigos ++ [ptsAtuais + 10] ++ drop (idx + 1) ptsAntigos
                           
                       in w { wEstado = Estado novoMapa o minhos
                            , wMapaVisual = novoMapaVisual
                            , wMensagem = Just ("Escavou! +10 pts", 0.5)
                            , wDelayEscavadora = 0.3
                            , wPontuacoes = novosPts 
                            }
                   else if dentro && tipoBloco == 8
                        then w { wMensagem = Just ("Duro demais!", 0.5) }
                        else w { wMensagem = Just ("Nada para escavar!", 0.5) }

atualizaMatriz :: [[a]] -> Int -> Int -> a -> [[a]]
atualizaMatriz mat l c val 
    | l < 0 || l >= length mat = mat
    | otherwise = take l mat ++ [take c (mat !! l) ++ [val] ++ drop (c + 1) (mat !! l)] ++ drop (l + 1) mat

dispararArma :: Worms -> Worms
dispararArma w = 
    if wTirosPorTurno w >= 2 then w { wMensagem = Just ("Max 2 tiros por turno!", 1.5) }
    else
        case wArmaSelecionada w of
            ComEscavadora -> usaEscavadora w
            _ -> 
                let estado = wEstado w
                    idx = wMinhocaAtiva w
                    minhoca = (minhocasEstado estado) !! idx
                    (l, c) = case posicaoMinhoca minhoca of { Just p -> p; Nothing -> (0,0) }
                    (centroX, centroY) = coordToGloss (l, c)
                    
                    anguloGraus = wMira w
                    anguloRad = anguloGraus * (pi / 180)
                    fatorDirecao = if wDirecao w == Oeste then -1 else 1
                    
                    (forca, nomeArma, tipoArma) = case wArmaSelecionada w of
                        ComBazuca   -> (22.0, "FOGO!", Bazuca)
                        ComMina     -> (0.0,  "Mina Plantada", Mina)
                        ComDinamite -> (15.0,  "Dinamite!", Dinamite)
                        _           -> (0.0,  "", Bazuca)
                    
                    vx = forca * cos anguloRad * fatorDirecao
                    vy = forca * sin anguloRad
                    spawnX = centroX + (40.0 * cos anguloRad * fatorDirecao)
                    spawnY = centroY + (40.0 * sin anguloRad)

                in if wArmaSelecionada w `elem` [ComBazuca, ComMina, ComDinamite]
                   then if temMunicao minhoca tipoArma
                        then 
                            let estadoSemMunicao = gastarMunicaoNoEstado idx tipoArma estado
                            in w { wProjetil = Just ((spawnX, spawnY), (vx, vy)) 
                                 , wMensagem = Just (nomeArma, 1.0)
                                 , wEstado = estadoSemMunicao 
                                 , wTirosPorTurno = wTirosPorTurno w + 1
                                 }
                        else w { wMensagem = Just ("Sem Municao!", 1.0) }
                   else w { wMensagem = Just ("Ferramenta ativa!", 1.0) }

gastarMunicaoNoEstado :: Int -> TipoArma -> Estado -> Estado
gastarMunicaoNoEstado idx arma (Estado m o minhocas) =
    let updateM minho = case arma of
            Bazuca -> minho { bazucaMinhoca = bazucaMinhoca minho - 1 }
            Mina   -> minho { minaMinhoca = minaMinhoca minho - 1 }
            Dinamite -> minho { dinamiteMinhoca = dinamiteMinhoca minho - 1 }
            _ -> minho
        novasMinhocas = take idx minhocas ++ [updateM (minhocas !! idx)] ++ drop (idx + 1) minhocas
    in Estado m o novasMinhocas

passarTurnoManual :: Worms -> Worms
passarTurnoManual w = 
    let proxima = proximaMinhocaViva w
    in w { wMinhocaAtiva = proxima
         , wTempoTurno = 15.0
         , wMira = 0
         , wArmaSelecionada = ComBazuca
         , wMensagem = Just ("Turno Passado!", 2.0)
         , wProjetil = Nothing 
         , wDirecao = Este
         , wTirosPorTurno = 0
         , wTempoJetpack = 3.0
         , wSpacePressed = False
         , wDelayJetpack = 0.0
         , wDelayEscavadora = 0.0
         }