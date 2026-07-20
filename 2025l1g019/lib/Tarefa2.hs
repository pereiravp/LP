module Tarefa2 where

import Labs2025
import Data.List (sortBy)

-- Função principal que processa uma jogada
efetuaJogada :: NumMinhoca -> Jogada -> Estado -> Estado
efetuaJogada idx jogada estado
    | idx < 0 || idx >= length (minhocasEstado estado) = estado
    | isMorta minhocaAtual = estado
    | otherwise = case jogada of
        Move dir -> efetuaMovimento idx dir estado
        Dispara arma dir -> efetuaDisparo idx arma dir estado
  where
    minhocaAtual = minhocasEstado estado !! idx

-- MOVIMENTAÇÃO

efetuaMovimento :: NumMinhoca -> Direcao -> Estado -> Estado
efetuaMovimento idx dir estado =
  case posicaoMinhoca (minhocasEstado estado !! idx) of
    Nothing -> estado
    Just posAtual ->
      let
        mapa = mapaEstado estado
        posAlvo = somaPosicao posAtual dir
        statusAtual = getMinhocaStatus posAtual estado
        terrenoAlvo = terrenoNaPosicao mapa posAlvo
      in
        if statusAtual == NoAr
        then estado
        else
          if not (posicaoValida mapa posAlvo)
          then mataMinhoca idx Nothing Morta estado
          else if terrenoAlvo == Agua
               then mataMinhoca idx (Just posAlvo) Morta estado
               else if not (posicaoLivreParaMover posAlvo idx estado)
                    then estado
                    else atualizaPosicaoMinhoca idx posAlvo estado

posicaoLivreParaMover :: Posicao -> NumMinhoca -> Estado -> Bool
posicaoLivreParaMover pos minhocaIdx estado =
    not (terrenoOpaco (mapaEstado estado) pos) &&
    not (posicaoOcupadaPorBarril pos (objetosEstado estado)) &&
    not (posicaoOcupadaPorOutraMinhoca pos minhocaIdx (minhocasEstado estado))

-- DISPAROS

efetuaDisparo :: NumMinhoca -> TipoArma -> Direcao -> Estado -> Estado
efetuaDisparo idx tipoArma dir estado =
  let minhocaAtual = minhocasEstado estado !! idx
  in if not (temMunicao minhocaAtual tipoArma) || jaTemDisparoAtivo idx tipoArma (objetosEstado estado)
     then estado
     else
       case posicaoMinhoca minhocaAtual of
         Nothing -> estado 
         Just posOrigem ->
           let
             mapa = mapaEstado estado
             posAlvo = somaPosicao posOrigem dir
             estadoComMunicaoGasta = gastaMunicao idx tipoArma estado
           in case tipoArma of
               Jetpack ->
                   if posicaoValida mapa posAlvo && posicaoLivreParaMover posAlvo idx estado
                   then atualizaPosicaoMinhoca idx posAlvo estadoComMunicaoGasta
                   else estado
               Escavadora ->
                   if not (posicaoValida mapa posAlvo) then estado
                   else let terrAlvo = terrenoNaPosicao mapa posAlvo
                        in if terrAlvo == Terra && not (posicaoOcupadaPorBarril posAlvo (objetosEstado estado)) && not (posicaoOcupadaPorQualquerMinhoca posAlvo (minhocasEstado estado))
                           then atualizaPosicaoMinhoca idx posAlvo (alteraTerreno posAlvo Ar estadoComMunicaoGasta)
                           else if posicaoLivreParaMover posAlvo idx estado
                                then atualizaPosicaoMinhoca idx posAlvo estadoComMunicaoGasta
                                else estado
               Bazuca -> criaObjeto (Disparo posAlvo dir Bazuca Nothing idx) estadoComMunicaoGasta
               Mina -> let posFinal = if posicaoLivreParaColocarObjeto posAlvo estado then posAlvo else posOrigem
                       in criaObjeto (Disparo posFinal dir Mina Nothing idx) estadoComMunicaoGasta
               Dinamite -> let posFinal = if posicaoLivreParaColocarObjeto posAlvo estado then posAlvo else posOrigem
                           in criaObjeto (Disparo posFinal dir Dinamite (Just 4) idx) estadoComMunicaoGasta

posicaoLivreParaColocarObjeto :: Posicao -> Estado -> Bool
posicaoLivreParaColocarObjeto pos e =
    posicaoValida (mapaEstado e) pos &&
    not (terrenoOpaco (mapaEstado e) pos) &&
    not (posicaoOcupadaPorBarril pos (objetosEstado e)) &&
    not (posicaoOcupadaPorQualquerMinhoca pos (minhocasEstado e))

-- FUNÇÕES AUXILIARES E DE ESTADO

isMorta :: Minhoca -> Bool; isMorta = (== Morta) . vidaMinhoca
data MinhocaStatus = NoAr | NaAgua | NoChao deriving (Eq, Show)

getMinhocaStatus :: Posicao -> Estado -> MinhocaStatus
getMinhocaStatus pos@(x,y) e
    | not (posicaoValida (mapaEstado e) pos) = NoChao
    | otherwise =
        let terr = terrenoNaPosicao (mapaEstado e) pos; posAbaixo = (x+1, y)
            isLivreAbaixo = posicaoValida (mapaEstado e) posAbaixo &&
                            terrenoNaPosicao (mapaEstado e) posAbaixo == Ar &&
                            not (posicaoOcupadaPorBarril posAbaixo (objetosEstado e)) &&
                            not (posicaoOcupadaPorQualquerMinhoca posAbaixo (minhocasEstado e))
        in case terr of Agua -> NaAgua; Ar | isLivreAbaixo -> NoAr; _ -> NoChao

criaObjeto :: Objeto -> Estado -> Estado
criaObjeto obj e
    | not (posicaoValida (mapaEstado e) (getObjetoPos obj)) = e
    | otherwise = e { objetosEstado = ordenaObjetos (obj : objetosEstado e) }
  where getObjetoPos (Barril p _) = p; getObjetoPos (Disparo p _ _ _ _) = p

ordenaObjetos :: [Objeto] -> [Objeto]
ordenaObjetos = sortBy (\o1 o2 -> compare (chave o1) (chave o2))
  where chave (Barril p _) = (p, True); chave (Disparo p _ _ _ _) = (p, False)

mataMinhoca :: NumMinhoca -> Maybe Posicao -> VidaMinhoca -> Estado -> Estado
mataMinhoca idx novaPos novaVida = alteraMinhoca idx (\m -> m { posicaoMinhoca = novaPos, vidaMinhoca = novaVida })

atualizaPosicaoMinhoca :: NumMinhoca -> Posicao -> Estado -> Estado
atualizaPosicaoMinhoca idx novaPos = alteraMinhoca idx (\m -> m { posicaoMinhoca = Just novaPos })

gastaMunicao :: NumMinhoca -> TipoArma -> Estado -> Estado
gastaMunicao idx t = alteraMinhoca idx (gasta t) where
    gasta Jetpack    m = m { jetpackMinhoca = jetpackMinhoca m - 1 }
    gasta Escavadora m = m { escavadoraMinhoca = escavadoraMinhoca m - 1 }
    gasta Bazuca     m = m { bazucaMinhoca = bazucaMinhoca m - 1 }
    gasta Mina       m = m { minaMinhoca = minaMinhoca m - 1 }
    gasta Dinamite   m = m { dinamiteMinhoca = dinamiteMinhoca m - 1 }

alteraMinhoca :: NumMinhoca -> (Minhoca -> Minhoca) -> Estado -> Estado
alteraMinhoca idx f e = case splitAt idx (minhocasEstado e) of
    (antes, m:depois) -> e { minhocasEstado = antes ++ [f m] ++ depois }
    _ -> e

alteraTerreno :: Posicao -> Terreno -> Estado -> Estado
alteraTerreno (x,y) terrNovo e =
  let mapa = mapaEstado e
      (mapaAntes, mapaDepois) = splitAt x mapa
  in case mapaDepois of
       (linha:mapaResto) ->
         let (linhaAntes, linhaDepois) = splitAt y linha
         in case linhaDepois of
              (_:linhaResto) ->
                let linhaNova = linhaAntes ++ [terrNovo] ++ linhaResto
                in e { mapaEstado = mapaAntes ++ [linhaNova] ++ mapaResto }
              _ -> e
       _ -> e

temMunicao :: Minhoca -> TipoArma -> Bool
temMunicao m Jetpack = jetpackMinhoca m > 0; temMunicao m Escavadora = escavadoraMinhoca m > 0
temMunicao m Bazuca = bazucaMinhoca m > 0; temMunicao m Mina = minaMinhoca m > 0; temMunicao m Dinamite = dinamiteMinhoca m > 0

jaTemDisparoAtivo :: NumMinhoca -> TipoArma -> [Objeto] -> Bool
jaTemDisparoAtivo dono tipo = any (\o -> case o of Disparo _ _ t _ d -> d == dono && t == tipo; _ -> False)

posicaoValida :: Mapa -> Posicao -> Bool
posicaoValida m (x,y) = not (null m) && x >= 0 && x < length m && y >= 0 && y < length (head m)

terrenoOpaco :: Mapa -> Posicao -> Bool
terrenoOpaco m p = case terrenoNaPosicao m p of Pedra -> True; Terra -> True; _ -> False

terrenoNaPosicao :: Mapa -> Posicao -> Terreno
terrenoNaPosicao m (x,y) | posicaoValida m (x,y) = (m !! x) !! y | otherwise = Pedra

somaPosicao :: Posicao -> Direcao -> Posicao
somaPosicao (x,y) Norte = (x-1, y);
somaPosicao (x,y) Sul = (x+1, y);
somaPosicao (x,y) Este = (x, y+1);
somaPosicao (x,y) Oeste = (x, y-1);
somaPosicao (x,y) Nordeste = (x-1, y+1);
somaPosicao (x,y) Noroeste = (x-1, y-1);
somaPosicao (x,y) Sudeste = (x+1, y+1);
somaPosicao (x,y) Sudoeste = (x+1, y-1);

posicaoOcupadaPorBarril :: Posicao -> [Objeto] -> Bool
posicaoOcupadaPorBarril pos = any (\o -> case o of Barril p _ -> p == pos; _ -> False)

posicaoOcupadaPorOutraMinhoca :: Posicao -> NumMinhoca -> [Minhoca] -> Bool
posicaoOcupadaPorOutraMinhoca pos myIdx = any (\(idx, m) -> idx /= myIdx && posicaoMinhoca m == Just pos) . zip [0..]

posicaoOcupadaPorQualquerMinhoca :: Posicao -> [Minhoca] -> Bool
posicaoOcupadaPorQualquerMinhoca pos = any (\m -> posicaoMinhoca m == Just pos)