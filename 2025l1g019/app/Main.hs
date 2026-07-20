module Main where

import Graphics.Gloss
import Worms
import Desenhar (desenha)
import Eventos (reageEventos)
import Tempo (reageTempo)

fundo :: Color
fundo = makeColorI 50 50 100 255 

window :: Display
window = FullScreen 

fps :: Int
fps = 60

main :: IO ()
main = do
    imagens <- carregaImagens
    -- Começa com o Mapa 1 por defeito
    let estadoIni = estadoInicial imagens 1 
    play window fundo fps estadoIni desenha reageEventos reageTempo

carregaImagens :: IO Imagens
carregaImagens = do
    worm      <- loadBMP "assets/Worm.bmp"
    grassL    <- loadBMP "assets/grassHalfLeft.bmp"
    grassM    <- loadBMP "assets/grassHalfMid.bmp"
    grassR    <- loadBMP "assets/grassHalfRight.bmp"
    slice01   <- loadBMP "assets/slice01.bmp"
    slice02   <- loadBMP "assets/slice02.bmp"
    slice03   <- loadBMP "assets/slice03.bmp"
    slice04   <- loadBMP "assets/slice04.bmp"
    slice05   <- loadBMP "assets/slice05.bmp"
    slice06   <- loadBMP "assets/slice06.bmp"
    slice07   <- loadBMP "assets/slice07.bmp" 
    bazuca    <- loadBMP "assets/Bazuca.bmp"
    missil    <- loadBMP "assets/Missil.bmp"
    helmet    <- loadBMP "assets/Helmet.bmp"
    fundoImg  <- loadBMP "assets/Back.bmp" 
    logo      <- loadBMP "assets/Logo.bmp"
    escavadora <- loadBMP "assets/Escavadora.bmp"
    dinamite   <- loadBMP "assets/Dinamite.bmp"
    barril     <- loadBMP "assets/Barril.bmp"
    mina       <- loadBMP "assets/Mina.bmp"
    jetpackImg <- loadBMP "assets/Jetpack.bmp"
    winImg     <- loadBMP "assets/YouWin.bmp"
    cruzImg    <- loadBMP "assets/Cruz.bmp" 
    
    -- CARREGA OS PREVIEWS DOS MAPAS
    mapa1Img   <- loadBMP "assets/mapa1.bmp"
    mapa2Img   <- loadBMP "assets/mapa2.bmp"

    return $ Imagens
        { imgWorm = worm
        , imgGrassLeft = grassL
        , imgGrassMid = grassM
        , imgGrassRight = grassR
        , imgSlice01 = slice01
        , imgSlice02 = slice02
        , imgSlice03 = slice03
        , imgSlice04 = slice04
        , imgSlice05 = slice05
        , imgSlice06 = slice06
        , imgSlice07 = slice07
        , imgBazuca = bazuca
        , imgMissil = missil
        , imgHelmet = helmet
        , imgFundo = fundoImg
        , imgLogo = logo
        , imgEscavadora = escavadora
        , imgDinamite = dinamite
        , imgBarril = barril
        , imgMina = mina
        , imgJetpack = jetpackImg
        , imgWin = winImg
        , imgCruz = cruzImg
        , imgMapa1 = mapa1Img
        , imgMapa2 = mapa2Img
        }