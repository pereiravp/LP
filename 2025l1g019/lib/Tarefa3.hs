{-|
Module      : Tarefa3
Description : Avançar tempo do jogo.
Módulo para a realização da Tarefa 3 de LI1/LP1 em 2025/26.
-}
module Tarefa3(avancaEstado, aplicaDanos, avancaMinhoca, avancaObjeto, calculaExplosao) where
import Data.Either
import Labs2025
import Tarefa0_2025

type Dano = Int
type Danos = [(Posicao,Dano)]

-- Função principal
avancaEstado :: Estado -> Estado
avancaEstado e@(Estado mapa objetos minhocas) = foldr aplicaDanos e' danoss
    where
    minhocas' = map (uncurry $ avancaMinhoca e) (zip [0 :: Int ..] minhocas)
    (objetos',danoss) = partitionEithers $ map (uncurry $ avancaObjeto $ e { minhocasEstado = minhocas' }) (zip [0 :: Int ..] objetos)
    e' = Estado mapa objetos' minhocas'

-- Para um dado estado, dado o índice de uma minhoca na lista de minhocas e o estado dessa minhoca, retorna o novo estado da minhoca no próximo tick.
avancaMinhoca :: Estado -> NumMinhoca -> Minhoca -> Minhoca
avancaMinhoca e _ m =
    case posicaoMinhoca m of
        Nothing -> m
        Just pos ->
            if estaNoArOuAgua e pos
            then cairMinhoca e m pos
            else m

-- Verifica se uma posição está no ar ou na água 
estaNoArOuAgua :: Estado -> Posicao -> Bool
estaNoArOuAgua e@(Estado mapa _ _) (l,c) =
    let posInferior = (l+1, c)
    in (not (posDentroMapa posInferior mapa) || ((obterTerreno posInferior mapa ==Ar || obterTerreno posInferior mapa ==Agua) && ePosicaoEstadoLivre posInferior e))


-- Faz a minhoca cair uma posição
cairMinhoca :: Estado -> Minhoca -> Posicao -> Minhoca
cairMinhoca (Estado mapa _ _) m (l,c) =
    let novaPosicao = (l+1, c)
    in if not (posDentroMapa novaPosicao mapa)
       then m { posicaoMinhoca = Nothing, vidaMinhoca = Morta }
       else if obterTerreno novaPosicao mapa == Agua
            then m { posicaoMinhoca = Just novaPosicao, vidaMinhoca = Morta }
            else m { posicaoMinhoca = Just novaPosicao }

-- Para um dado estado, dado o índice de um objeto na lista de objetos e o estado desse objeto
avancaObjeto :: Estado -> NumObjeto -> Objeto -> Either Objeto Danos
avancaObjeto e _ obj = case obj of
    Barril pos explode -> avancaBarril e pos explode
    Disparo pos dir tipo tempo dono -> avancaDisparo e pos dir tipo tempo dono

-- Avança o estado de um barril
avancaBarril :: Estado -> Posicao -> Bool -> Either Objeto Danos
avancaBarril e@(Estado mapa _ _) pos explode
    | explode              =Right (calculaExplosao mapa pos 5)  
    | estaNoArOuAgua e pos =Left ( Barril pos True)  
    | otherwise            = Left (Barril pos False)  

-- Avança o estado de um disparo
avancaDisparo :: Estado -> Posicao -> Direcao -> TipoArma -> Maybe Int -> NumMinhoca -> Either Objeto Danos
avancaDisparo e@(Estado _ _ _) pos dir tipo tempo dono = case tipo of
    Bazuca -> avancaBazuca e pos dir dono
    Mina -> avancaMina e pos dir tempo dono
    Dinamite -> avancaDinamite e pos dir tempo dono
    _ -> Left (Disparo pos dir tipo tempo dono)

-- Avança um disparo de bazuca
avancaBazuca :: Estado -> Posicao -> Direcao -> NumMinhoca -> Either Objeto Danos
avancaBazuca (Estado mapa _ _) pos dir dono =
    let novaPos = moverPosicao pos dir
    in if not (posDentroMapa novaPos mapa)
       then Right (calculaExplosao mapa pos 5)
       else let terrenoNovo =obterTerreno novaPos mapa
       in if terrenoNovo==Terra ||terrenoNovo==Pedra
        then Right (calculaExplosao mapa pos 5)
       else Left (Disparo novaPos dir Bazuca Nothing dono)

-- Avança um disparo de mina
avancaMina :: Estado -> Posicao -> Direcao -> Maybe Int -> NumMinhoca -> Either Objeto Danos
avancaMina e@(Estado mapa _ minhocas) pos dir tempo dono = case tempo of
    Nothing ->
        -- Verifica se deve ativar (minhoca inimiga próxima)
        if deveAtivarMina pos dono minhocas
        then Left (Disparo pos Norte Mina (Just 2) dono)
        else avancaMinaFisica e pos dir dono
    Just 0 -> Right (calculaExplosao mapa pos 3)
    Just t ->
        if estaNoArOuAgua e pos
        then avancaMinaCai pos dono (Just (t-1))
        else Left (Disparo pos Norte Mina (Just (t-1)) dono)

-- Verifica se uma mina deve ser ativada
deveAtivarMina :: Posicao -> NumMinhoca -> [Minhoca] -> Bool
deveAtivarMina posMina dono minhocas = any (estaPertoInimiga posMina) (zip [0..] minhocas)
    where
      estaPertoInimiga :: Posicao -> (NumMinhoca, Minhoca) -> Bool
      estaPertoInimiga pos (i, m) = i /= dono && estaPerto pos m

estaPerto::Posicao->Minhoca->Bool
estaPerto posDaMina m =
    case posicaoMinhoca m of
    Just posMinhoca -> distanciaMax posDaMina posMinhoca<=1
    Nothing-> False

-- Distância máxima entre duas posições
distanciaMax :: Posicao -> Posicao -> Int
distanciaMax (l1,c1) (l2,c2) = max (abs (l1-l2)) (abs (c1-c2))

-- Aplica física da gravidade à mina 
avancaMinaFisica :: Estado -> Posicao -> Direcao -> NumMinhoca -> Either Objeto Danos
avancaMinaFisica e@(Estado _ _ _) pos _ dono
    | estaNoArOuAgua e pos = avancaMinaCai pos dono Nothing
    | otherwise = Left (Disparo pos Norte Mina Nothing dono)

-- Mina cai
avancaMinaCai :: Posicao -> NumMinhoca -> Maybe Int -> Either Objeto Danos
avancaMinaCai (l,c) dono tempo = Left (Disparo (l+1, c) Norte Mina tempo dono)

-- Avança um disparo de dinamite
avancaDinamite :: Estado -> Posicao -> Direcao -> Maybe Int -> NumMinhoca -> Either Objeto Danos
avancaDinamite e@(Estado mapa _ _) pos dir tempo dono = case tempo of
    Nothing -> Left (Disparo pos dir Dinamite (Just 4) dono)
    Just 0 -> Right (calculaExplosao mapa pos 7)
    Just t ->
        if estaNoArOuAgua e pos
        then avancaDinamiteParabola e pos dir (t-1) dono
        else Left ( Disparo pos Norte Dinamite (Just (t-1)) dono)

-- Dinamite em parábola (move + roda 45º)
avancaDinamiteParabola :: Estado -> Posicao -> Direcao -> Int -> NumMinhoca -> Either Objeto Danos
avancaDinamiteParabola (Estado mapa _ _) pos dir t dono =
    let novaDir =if dir==Norte ||dir ==Sul
                then Norte
                else rodarDirecao45 dir
        novaPos = if dir ==Norte||dir==Sul
                 then moverPosicao pos Sul
                 else moverPosicao pos novaDir
    in if not (posDentroMapa novaPos mapa)
       then Right (calculaExplosao mapa pos 7)
       else Left ( Disparo novaPos novaDir Dinamite (Just t) dono)

-- Roda direção 45º no sentido horário (para parábola)
rodarDirecao45 :: Direcao -> Direcao
rodarDirecao45 dir = case dir of
    Norte -> Nordeste; Nordeste -> Este; Este -> Sudeste; Sudeste -> Sul
    Sul -> Sudoeste; Sudoeste -> Oeste; Oeste -> Noroeste; Noroeste -> Norte

-- Move uma posição numa direção
moverPosicao :: Posicao -> Direcao -> Posicao
moverPosicao (l,c) dir = case dir of
    Norte -> (l-1, c); Sul -> (l+1, c); Este -> (l, c+1); Oeste -> (l, c-1)
    Nordeste -> (l-1, c+1); Noroeste -> (l-1, c-1)
    Sudeste -> (l+1, c+1); Sudoeste -> (l+1, c-1)

-- Calcula os danos de uma explosão em círculo
calculaExplosao :: Mapa -> Posicao -> Int -> Danos
calculaExplosao mapa (l,c) diametro =
    [(pos, dano) |
    dl<-[-raio..raio],
    dc<-[-raio..raio],
     let pos = (l+dl,c+dc),
     posDentroMapa pos mapa,
     let dano = calcularDano diametro dl dc ,
     dano >0 ]
  where
    raio = diametro `div` 2

-- Calcula dano baseado no diâmetro e distância ao centro
calcularDano :: Int ->Int-> Int -> Int
calcularDano diametro dl dc=
    let dano
          | dl==0 && dc==0 = diametro*10
          | dl==0 ||dc==0 = let dist=abs dl+ abs dc
                       in (diametro-2*dist)*10
          | otherwise = let dist=max (abs dl) (abs dc)
                   in (diametro-3*dist)*10
    in max 0 dano

-- Para uma lista de posições afetadas por uma explosão, recebe um estado e calcula o novo estado em que esses danos são aplicados
aplicaDanos :: Danos -> Estado -> Estado
aplicaDanos danos (Estado mapa objetos minhocas) =
    let mapa' = aplicaDanosMapa danos mapa
        minhocas' = aplicaDanosMinhocas danos minhocas
    in Estado mapa' objetos minhocas'

-- Aplica danos ao mapa (destrói Terra → Ar)
aplicaDanosMapa :: Danos -> Mapa -> Mapa
aplicaDanosMapa danos mapa =
    foldr (\(pos, _) m -> destroiPosicao pos m) mapa danos

-- Aplica danos às minhocas
aplicaDanosMinhocas :: Danos -> [Minhoca] -> [Minhoca]
aplicaDanosMinhocas danos = map (aplicaDanoMinhoca danos)

-- Aplica dano total a uma minhoca
aplicaDanoMinhoca :: Danos -> Minhoca -> Minhoca
aplicaDanoMinhoca danos m = case posicaoMinhoca m of
    Nothing -> m
    Just pos ->
        let danoTotal = sum [d | (p, d) <- danos, p == pos]
        in if danoTotal > 0
           then reduzirVida m danoTotal
           else m

-- Reduz a vida de uma minhoca
reduzirVida :: Minhoca -> Int -> Minhoca
reduzirVida m dano = case vidaMinhoca m of
    Morta -> m
    Viva vida ->
        let novaVida = vida - dano
        in if novaVida <= 0
           then m { vidaMinhoca = Morta }
           else m { vidaMinhoca = Viva novaVida }