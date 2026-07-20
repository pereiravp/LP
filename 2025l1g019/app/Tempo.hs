module Tempo where

import Graphics.Gloss
import Labs2025
import Worms
import Desenhar (offsetX, offsetY, coordToGloss)
import Tarefa3 (avancaEstado, aplicaDanos, calculaExplosao) 
import Tarefa4 (cerebroBot)
import Tarefa2 (efetuaJogada, temMunicao)
import Eventos (usaJetpack)
import Data.Maybe (isJust, isNothing)

-- | Intervalo de atualização da física.
tickFisica :: Float
tickFisica = 0.05 

-- * Atualização Principal

-- | Função principal de atualização do estado em função do tempo.
-- Gere timers, física de projéteis, explosivos e lógica de turnos.
reageTempo :: Float -> Worms -> Worms
reageTempo dt w
    | wModo w == Jogando = 
        let 
            -- Atualiza mensagem
            wMsg = case wMensagem w of
                Just (txt, t) -> if t - dt <= 0 then w { wMensagem = Nothing } else w { wMensagem = Just (txt, t - dt) }
                Nothing -> w

            -- Atualiza Bot
            wBot = if ehTurnoDoBOT wMsg
                   then atualizaBOTCompleto dt wMsg
                   else wMsg

            tiroNoAr = isJust (wProjetil wBot)
            
            -- Verifica estado da minhoca ativa
            estadoAtual = wEstado wBot
            minhocaAtivaObj = minhocasEstado estadoAtual !! wMinhocaAtiva wBot
            minhocaMorreu = case vidaMinhoca minhocaAtivaObj of { Morta -> True; _ -> False }

            novoTempoTurno = if tiroNoAr 
                             then wTempoTurno wBot 
                             else wTempoTurno wBot - dt
            
            -- Passagem de turno automática
            devePassar = (novoTempoTurno <= 0 && not tiroNoAr) || minhocaMorreu

            wTurno = if devePassar then passarTurnoAutomatico wBot else wBot { wTempoTurno = novoTempoTurno }
            
            -- Verificação de fim de jogo
            wFinal = case verificaVitoria wTurno of { Just res -> wTurno { wModo = Finalizado res }; Nothing -> wTurno }
            
            -- Lógica do Jetpack
            ehVooAtivo = wArmaSelecionada wFinal == ComJetpack && wSpacePressed wFinal && wTempoJetpack wFinal > 0
            novoFuel = if ehVooAtivo then max 0 (wTempoJetpack wFinal - dt) else wTempoJetpack wFinal
            wComFuel = wFinal { wTempoJetpack = novoFuel }
            
            novoDelay = wDelayJetpack wComFuel + dt
            podeMover = novoDelay > 0.25 

            wVoo = if ehVooAtivo && podeMover
                   then (usaJetpack wComFuel (-1, 0)) { wDelayJetpack = 0 }
                   else wComFuel { wDelayJetpack = novoDelay }

            wEscav = wVoo { wDelayEscavadora = max 0 (wDelayEscavadora wVoo - dt) }

            tNovo = wTempo wEscav + dt
        in if tNovo > tickFisica
           then 
               let 
                   wProj = moveProjetilSuave wEscav tNovo
                   wExp = atualizaExplosivos wProj tNovo
                   wSync = sincronizaMapas wExp
                   pararGravidade = ehVooAtivo || (wDelayEscavadora wSync > 0)

                   estadoComGravidade = if pararGravidade
                                        then wEstado wSync 
                                        else avancaEstado (wEstado wSync)
                   
               in wSync { wEstado = estadoComGravidade, wTempo = 0 }
           else wEscav { wTempo = tNovo }
    | otherwise = w

-- * Lógica do Bot

-- | Gere o delay e a execução da jogada do Bot.
atualizaBOTCompleto :: Float -> Worms -> Worms
atualizaBOTCompleto dt w = 
    let delayAtual = wBotDelay w
        novoDelay = delayAtual + dt
    in if novoDelay < 1.0
       then w { wBotDelay = novoDelay }
       else 
           let idx = wMinhocaAtiva w
               estado = wEstado w
               minhoca = minhocasEstado estado !! idx
               
               wNovo = case vidaMinhoca minhoca of
                   Morta -> w 
                   Viva _ -> 
                       let jogadaOriginal = cerebroBot minhoca idx estado
                       in executarJogadaBOT jogadaOriginal idx w
                       
           in wNovo { wBotDelay = 0.0, wBotProximaJogada = Nothing }

-- | Aplica a jogada decidida pelo Bot ao estado do jogo.
executarJogadaBOT :: Jogada -> Int -> Worms -> Worms
executarJogadaBOT jogada idx w = 
    case jogada of
        Move _ -> 
            let estadoAtual = wEstado w
                jogadaSegura = validarJogadaSegura jogada idx estadoAtual
                estadoNovo = efetuaJogada idx jogadaSegura estadoAtual
            in w { wEstado = estadoNovo }
        
        Dispara tipoArma dir ->
            let estadoAtual = wEstado w
                minhoca = minhocasEstado estadoAtual !! idx
            in if not (temMunicao minhoca tipoArma)
               then executarJogadaBOT (Move dir) idx w
               else
                   case tipoArma of
                       Bazuca -> dispararBazucaBOT dir idx w
                       Mina -> let estadoNovo = efetuaJogada idx jogada estadoAtual in w { wEstado = estadoNovo }
                       Dinamite -> let estadoNovo = efetuaJogada idx jogada estadoAtual in w { wEstado = estadoNovo }
                       Jetpack -> usarJetpackBOT dir idx w
                       Escavadora -> escavarBOT dir idx w

dispararBazucaBOT :: Direcao -> Int -> Worms -> Worms
dispararBazucaBOT dir idx w =
    let estado = wEstado w
        minhoca = minhocasEstado estado !! idx
        (l, c) = case posicaoMinhoca minhoca of { Just p -> p; Nothing -> (0,0) }
        (centroX, centroY) = coordToGloss (l, c)
        (vx, vy) = calcularVelocidadePorDirecao dir 20.0
        estadoSemMunicao = gastarMunicaoEstado idx Bazuca estado
    in w { wProjetil = Just ((centroX, centroY), (vx, vy))
         , wEstado = estadoSemMunicao
         , wArmaSelecionada = ComBazuca
         , wMensagem = Just ("BOT disparou Bazuca!", 2.0)
         }

usarJetpackBOT :: Direcao -> Int -> Worms -> Worms
usarJetpackBOT dir idx w =
    let estado = wEstado w
        (Estado mapa objs minhos) = estado
        minhoca = minhos !! idx
    in case posicaoMinhoca minhoca of
        Nothing -> w
        Just (l, c) ->
            let (dl, dc) = case dir of { Norte -> (-1,0); Sul -> (1,0); Este -> (0,1); Oeste -> (0,-1); Nordeste -> (-1,1); Noroeste -> (-1,-1); Sudeste -> (1,1); Sudoeste -> (1,-1) }
                novoL = l + dl; novoC = c + dc
                dentro = novoL >= 0 && novoL < length mapa && novoC >= 0 && novoC < length (head mapa)
                novaMinhoca = if dentro then minhoca { posicaoMinhoca = Just (novoL, novoC), jetpackMinhoca = jetpackMinhoca minhoca - 1 } else minhoca
                novasMinhos = take idx minhos ++ [novaMinhoca] ++ drop (idx + 1) minhos
            in w { wEstado = Estado mapa objs novasMinhos, wMensagem = Just ("BOT usou Jetpack!", 2.0) }

escavarBOT :: Direcao -> Int -> Worms -> Worms
escavarBOT dir idx w =
    let estadoAtual = wEstado w
        estadoNovo = efetuaJogada idx (Dispara Escavadora dir) estadoAtual
    in w { wEstado = estadoNovo, wMensagem = Just ("BOT escavou!", 2.0) }

-- * Funções Auxiliares de Física e Estado

calcularVelocidadePorDirecao :: Direcao -> Float -> (Float, Float)
calcularVelocidadePorDirecao dir forca = case dir of
    Norte -> (0, forca); Sul -> (0, -forca); Este -> (forca, 0); Oeste -> (-forca, 0)
    Nordeste -> (forca * 0.7, forca * 0.7); Noroeste -> (-forca * 0.7, forca * 0.7)
    Sudeste -> (forca * 0.7, -forca * 0.7); Sudoeste -> (-forca * 0.7, -forca * 0.7)

gastarMunicaoEstado :: Int -> TipoArma -> Estado -> Estado
gastarMunicaoEstado idx arma (Estado m o minhos) =
    let updateM minho = case arma of
            Bazuca -> minho { bazucaMinhoca = max 0 (bazucaMinhoca minho - 1) }
            _ -> minho
        novasMinhos = take idx minhos ++ [updateM (minhos !! idx)] ++ drop (idx + 1) minhos
    in Estado m o novasMinhos

validarJogadaSegura :: Jogada -> Int -> Estado -> Jogada
validarJogadaSegura jogada _ _ = jogada

passarTurnoAutomatico :: Worms -> Worms
passarTurnoAutomatico w = 
    let proxima = proximaMinhocaViva w
        ehProximoBOT = case wConfigBot w of { Nothing -> False; Just cfg -> proxima `elem` minhocasBots cfg }
        msgTurno = if ehProximoBOT then "Turno do BOT!" else "Seu Turno!"
    in w { wMinhocaAtiva = proxima, wTempoTurno = 15.0, wMira = 0, wArmaSelecionada = ComBazuca
         , wMensagem = Just (msgTurno, 2.0), wProjetil = Nothing, wDirecao = Este, wTirosPorTurno = 0
         , wTempoJetpack = 3.0, wSpacePressed = False, wDelayJetpack = 0, wDelayEscavadora = 0
         , wBotDelay = 0.0, wBotProximaJogada = Nothing }

sincronizaMapas :: Worms -> Worms
sincronizaMapas w = 
    let (Estado mapaFisico _ _) = wEstado w
        mapaVisual = wMapaVisual w
        novoVisual = zipWith (\linhaF linhaV -> zipWith (\blocoF blocoV -> if blocoF == Ar then 0 else blocoV) linhaF linhaV) mapaFisico mapaVisual
    in w { wMapaVisual = novoVisual }

-- | Remove pedras (ID 8) da lista de danos para que sejam indestrutíveis.
filtraDanosPedra :: [[Int]] -> [(Posicao, Int)] -> [(Posicao, Int)]
filtraDanosPedra mapaVis danos =
    filter (\((l, c), _) -> 
        if l >= 0 && l < length mapaVis && c >= 0 && c < length (head mapaVis)
        then (mapaVis !! l) !! c /= 8 
        else True 
    ) danos

contaTerrasDestruidas :: Mapa -> [(Posicao, Int)] -> Int
contaTerrasDestruidas mapa danos =
    let terras = filter (\((l, c), _) -> 
            l >= 0 && l < length mapa && c >= 0 && c < length (head mapa) && 
            (mapa !! l) !! c == Terra
         ) danos
    in length terras * 10

-- * Gestão de Explosivos

atualizaExplosivos :: Worms -> Float -> Worms
atualizaExplosivos w dt =
    let explosivosAtuais = wExplosivos w
        (novosExplosivos, explodiuAlgo, estadoFinal, ptsGanhos) = foldl (processaExplosivo (wMapaVisual w) dt) ([], False, wEstado w, 0) explosivosAtuais
        novasPontuacoes = if ptsGanhos > 0 then atualizaScore (wPontuacoes w) (wMinhocaAtiva w) ptsGanhos else wPontuacoes w
        msg = if ptsGanhos > 0 then Just ("BOOM! +" ++ show ptsGanhos, 1.5) else if explodiuAlgo then Just ("KABOOM!", 1.0) else wMensagem w
    in w { wExplosivos = novosExplosivos, wEstado = estadoFinal, wPontuacoes = novasPontuacoes, wMensagem = msg }

-- | Processa a lógica de cada explosivo (Mina/Dinamite): gravidade, timer e detonação.
processaExplosivo :: [[Int]] -> Float -> ([Explosivo], Bool, Estado, Int) -> Explosivo -> ([Explosivo], Bool, Estado, Int)
processaExplosivo mapaVis dt (lista, explodiu, est, pts) (pos@(x,y), tipo, timer) =
    let 
        (l, c) = coordToGrid pos 
        
        -- Verifica bloco abaixo para gravidade
        blocoAbaixo = if l+1 < length mapaVis && l+1 >= 0 && c >= 0 && c < length (head mapaVis) 
                      then (mapaVis !! (l+1)) !! c 
                      else 0
        
        estaNoAr = blocoAbaixo == 0
        
        -- Aplica gravidade (queda suave de 10px)
        novaY = if (tipo == ComMina || tipo == ComDinamite) && estaNoAr 
                then y - 10.0 
                else y
        
        novaPos = (x, novaY)
        saiuDoMapa = novaY < -600 

        novoTimer = timer - dt
        
        deveExplodir = not saiuDoMapa && case tipo of 
            ComDinamite -> novoTimer <= 0
            ComMina     -> novoTimer <= 0 && not estaNoAr && inimigoPerto novaPos est (-1)
            _           -> False
            
    in if saiuDoMapa 
       then (lista, explodiu, est, pts) 
       else if deveExplodir
       then 
            let mapaOriginal = mapaEstado est
                danosArea = calculaExplosao mapaOriginal (l, c) 8
                danosFiltrados = filtraDanosPedra mapaVis danosArea
                ptsTerra = contaTerrasDestruidas mapaOriginal danosFiltrados
                estNovo = aplicaDanos danosFiltrados est
                ptsDano = calculaPontosGanhos est estNovo (-1)
            in (lista, True, estNovo, pts + ptsDano + ptsTerra)
       else 
            (lista ++ [(novaPos, tipo, novoTimer)], explodiu, est, pts)

-- * Movimento de Projéteis

-- | Move o projétil ativo com verificação de colisão contínua (anti-tunneling).
moveProjetilSuave :: Worms -> Float -> Worms
moveProjetilSuave w _ = 
    case wProjetil w of
        Nothing -> w
        Just (posAntiga, (vx, vy)) ->
            let 
                novoVy = vy - 0.7
                novoX = fst posAntiga + vx
                novoY = snd posAntiga + novoVy
                
                -- Ponto intermédio para verificar colisão a meio do frame
                meioX = fst posAntiga + (vx * 0.5)
                meioY = snd posAntiga + (novoVy * 0.5)

                (colidiuMeio, hitIDMeio) = verificaColisaoVisual (meioX, meioY) w
                (colidiuFim, hitIDFim) = verificaColisaoVisual (novoX, novoY) w
                
                (colidiu, _, xReal, yReal) = 
                    if colidiuMeio then (True, hitIDMeio, meioX, meioY)
                    else if colidiuFim then (True, hitIDFim, novoX, novoY)
                    else (False, Nothing, novoX, novoY)

                (l, c) = coordToGrid (xReal, yReal)
                posVisualSegura = coordToGloss (l - 1, c)

            in if colidiu
               then 
                   case wArmaSelecionada w of
                        ComBazuca -> 
                            let estA = wEstado w
                                mapaOriginal = mapaEstado estA
                                dA = calculaExplosao mapaOriginal (l, c) 6
                                daFiltrados = filtraDanosPedra (wMapaVisual w) dA
                                ptsTerra = contaTerrasDestruidas mapaOriginal daFiltrados
                                estNovo = aplicaDanos daFiltrados estA
                                ptsDano = calculaPontosGanhos estA estNovo (wMinhocaAtiva w)
                                totalPts = ptsTerra + ptsDano
                                nScores = atualizaScore (wPontuacoes w) (wMinhocaAtiva w) totalPts
                            in w { wEstado = estNovo, wProjetil = Nothing, wPontuacoes = nScores }
                        
                        ComMina -> 
                            if isNothing hitIDMeio && isNothing hitIDFim 
                            then w { wProjetil = Nothing, wMensagem = Just ("Saiu do Mapa!", 1.0) }
                            else w { wProjetil = Nothing, wExplosivos = wExplosivos w ++ [(posVisualSegura, ComMina, 3.0)] }
                        
                        ComDinamite -> 
                            if isNothing hitIDMeio && isNothing hitIDFim
                            then w { wProjetil = Nothing, wMensagem = Just ("Saiu do Mapa!", 1.0) }
                            else w { wProjetil = Nothing, wExplosivos = wExplosivos w ++ [(posVisualSegura, ComDinamite, 3.0)] }
                        
                        _ -> w { wProjetil = Nothing }
               else 
                   w { wProjetil = Just ((xReal, yReal), (vx, novoVy)) }

-- | Verifica se um ponto colide com terreno ou sai do mapa.
verificaColisaoVisual :: Point -> Worms -> (Bool, Maybe Int)
verificaColisaoVisual (x, y) w = 
    let mapaVis = wMapaVisual w
        mapaFis = mapaEstado (wEstado w)
        (l, c) = coordToGrid (x, y)
        dentro = l >= 0 && l < length mapaVis && c >= 0 && c < length (head mapaVis)
        bateu = dentro && ((mapaFis !! l) !! c /= Ar)
        blocoID = if dentro then Just ((mapaVis !! l) !! c) else Nothing
        saiuMapa = y < -500 || x < -900 || x > 900
    in (bateu || saiuMapa, blocoID)

coordToGrid :: (Float, Float) -> (Int, Int)
coordToGrid (x, y) = (floor ((offsetY - y) / 51.0), floor ((x - offsetX) / 51.0))

calculaPontosGanhos :: Estado -> Estado -> Int -> Int
calculaPontosGanhos (Estado _ _ msAnt) (Estado _ _ msNova) atiradorIdx =
    let pares = zip3 [0..] msAnt msNova
        getVida m = case vidaMinhoca m of { Viva v -> v; Morta -> 0 }
    in sum [ max 0 (getVida mOld - getVida mNew) | (i, mOld, mNew) <- pares, i /= atiradorIdx ]

atualizaScore :: [Int] -> Int -> Int -> [Int]
atualizaScore p i pts = take i p ++ [(p !! i) + pts] ++ drop (i + 1) p

inimigoPerto :: Point -> Estado -> Int -> Bool
inimigoPerto posMina (Estado _ _ minhos) minhaIdx =
    let (lMina, cMina) = coordToGrid posMina
    in any (\(i, m) -> 
            case posicaoMinhoca m of 
                Nothing -> False
                Just (lMinhoca, cMinhoca) ->
                    i /= minhaIdx && 
                    abs (lMinhoca - lMina) <= 3 && 
                    abs (cMinhoca - cMina) <= 3
        ) (zip [0..] minhos)

posicaoEhPedra :: Worms -> (Int, Int) -> Bool
posicaoEhPedra w (l, c) =
    let m = wMapaVisual w
    in l >= 0 && l < length m && c >= 0 && c < length (head m) && (m !! l !! c == 8)