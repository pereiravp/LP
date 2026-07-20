{-# OPTIONS_GHC -w #-}

-- | Este ficheiro contém *magia* para correr o feedback. Nâo tem nada de útil para o projeto. Ignorem o mais que puderem!
module Magic where

import Control.Monad
import Data.String
import Data.Word
import Data.Int
import qualified Web.Browser as Browser
import qualified Network.URI.Encode as URI
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Bits.Coded
import Data.Bits.Coding
import Data.Bytes.Put (runPutS,MonadPut)
import Data.Bytes.Get (runGetS,MonadGet)
import GHC.Generics
import Control.DeepSeq
import Control.Exception

import Labs2025

-- * Feedback

type Error a = Either String a

deriving instance Generic Terreno
instance NFData Terreno

deriving instance Generic Objeto
instance NFData Objeto

deriving instance Generic VidaMinhoca
instance NFData VidaMinhoca

deriving instance Generic Minhoca
instance NFData Minhoca

deriving instance Generic TipoArma
instance NFData TipoArma

deriving instance Generic Direcao
instance NFData Direcao

deriving instance Generic Jogada
instance NFData Jogada

deriving instance Generic Estado
instance NFData Estado

data TaskData
    = T1 { t1_inputs :: [Estado], t1_outputs :: [Error Bool] }
    | T2 { t2_inputs :: [(NumMinhoca,Jogada,Estado)], t2_outputs :: [Error Estado] }
    | T3 { t3_inputs :: [Estado], t3_outputs :: [Error Estado] }
    | T4 { t4_inputs :: [Estado], t4_outputs :: [Error [(NumMinhoca,Jogada)]] }
    deriving (Eq,Ord,Show,Read,Generic)

instance NFData TaskData

taskNumber :: TaskData -> Int
taskNumber (T1 {}) = 1
taskNumber (T2 {}) = 2
taskNumber (T3 {}) = 3
taskNumber (T4 {}) = 4

runTest :: NFData a => a -> IO (Error a)
runTest a = catch (evaluate $! force $! Right a) onError
    where
    onError :: SomeException -> IO (Error a)
    onError e = return $! Left $! show e

runFeedback :: TaskData -> IO ()
runFeedback dta = do
    let feedbackpage = "https://haslab.github.io/Teaching/LI1/2526/feedback.html"
    let feedbackfile = "t" ++ show (taskNumber dta) ++ ".feedback"
    let bytes = encToBytes dta
    let base64 = encToBase64 dta
    writeFile feedbackfile base64
    let uri = feedbackpage ++ "?flags=" ++ URI.encode base64
    _ <- Browser.openBrowser uri
    putStrLn $ "\x1b[33m*\x1b[0m Página de feedback aberta no browser."
    putStrLn $ "\x1b[33m*\x1b[0m Se não conseguir visualizar faça upload do ficheiro " ++ show feedbackfile ++ " em " ++ "\x1b[36m"  ++ feedbackpage ++ "\x1b[0m"

-- * bit stuff

type BitEncoder a = forall m. MonadPut m => a -> Coding m ()
type BitDecoder a = forall m. MonadGet m => Coding m a
type Bytes = BS.ByteString

encToBytes :: Coded a => a -> Bytes
encToBytes a = runPutS $ runEncode $ encode a >> putAligned (return ())

decFromBytes :: Coded a => Bytes -> Maybe a
decFromBytes bytes = case runGetS (runDecode decode) bytes of
    Left err -> Nothing
    Right a -> Just a

instance Monad m => Semigroup (Coding m ()) where
    x <> y = x >> y

instance Monad m => Monoid (Coding m ()) where
    mempty = return ()
    mappend x y = x >> y

instance Coded Bool where
    encode b = putBit b
    decode = getBit

instance Coded Int where
    encode = encodeAsInt32
    decode = decodeAsInt32

instance Coded Word8 where
    encode = encodeAsWord8
    decode = decodeAsWord8

instance Coded BS.ByteString where
    encode = encode . BS.unpack
    decode = liftM BS.pack decode

instance {-# OVERLAPS #-} Coded String where
    encode = encode . BS8.pack
    decode = liftM BS8.unpack decode

instance (Coded a,Coded b) => Coded (a,b) where
    encode (a,b) = encode a <> encode b
    decode = decode >>= \a -> decode >>= \b -> return (a,b)

instance (Coded a,Coded b,Coded c) => Coded (a,b,c) where
    encode (a,b,c) = encode a <> encode b <> encode c
    decode = decode >>= \a -> decode >>= \b -> decode >>= \c -> return (a,b,c)

instance (Coded a,Coded b) => Coded (Either a b) where
    encode (Left  a) = encode False >> encode a
    encode (Right b) = encode True >> encode b
    decode = decode >>= \b -> case b of
        False -> liftM Left decode 
        True -> liftM Right decode

instance Coded a => Coded (Maybe a) where
    encode Nothing = encode False
    encode (Just a) = encode True <> encode a
    decode = decode >>= \b -> case b of
        False -> return Nothing
        True -> decode >>= \a -> return (Just a)

instance {-# OVERLAPPABLE #-} Coded a => Coded [a] where
    encode xs = encodeAsWord32 (length xs) <> mconcat (map encode xs)
    decode = decodeAsWord32 >>= \sz -> replicateM sz decode

instance Coded Estado where
    encode e = encode (mapaEstado e) <> encode (objetosEstado e) <> encode (minhocasEstado e)
    decode = decode >>= \mapa -> decode >>= \objetos -> decode >>= \minhocas -> return (Estado mapa objetos minhocas)

instance Coded Jogada where
    encode (Dispara arma dir) = encode False <> encode arma <> encode dir
    encode (Move dir) = encode True <> encode dir
    decode = decode >>= \b -> case b of
        False -> decode >>= \arma -> decode >>= \dir -> return (Dispara arma dir)
        True -> decode >>= \dir -> return (Move dir)

instance Coded Objeto where
    encode (Barril posicao explode) = encode False <> encode posicao <> encode explode
    encode (Disparo posicao dir tipo tempo dono) = encode True <> encode posicao <> encode dir <> encode tipo <> encode tempo <> encode dono
    decode = decode >>= \b -> case b of
        False -> decode >>= \posicao -> decode >>= \explode -> return (Barril posicao explode)
        True -> decode >>= \posicao -> decode >>= \dir -> decode >>= \tipo -> decode >>= \tempo -> decode >>= \dono -> return (Disparo posicao dir tipo tempo dono)
    
instance Coded Minhoca where
    encode (Minhoca pos vida jet esc baz min din) = encode pos <> encode vida <> encode jet <> encode esc <> encode baz <> encode min <> encode din
    decode = decode >>= \pos -> decode >>= \vida -> decode >>= \jet -> decode >>= \esc -> decode >>= \baz -> decode >>= \min -> decode >>= \din -> return (Minhoca pos vida jet esc baz min din)

instance Coded VidaMinhoca where
    encode Morta = encode False
    encode (Viva i) = encode True <> encode i
    decode = decode >>= \b -> case b of
        False -> return Morta
        True -> decode >>= \i -> return (Viva i)
    
instance Coded Terreno where
    encode t = encodeAsWord2 (fromEnum t)
    decode = liftM toEnum decodeAsWord2

instance Coded Direcao where
    encode t = encodeAsWord3 (fromEnum t)
    decode = liftM toEnum decodeAsWord3

instance Coded TipoArma where
    encode t = encodeAsWord3 (fromEnum t)
    decode = liftM toEnum decodeAsWord3

instance Coded TaskData where
    encode (T1 i o) = encodeAsWord2 (0::Int) <> encode i <> encode o
    encode (T2 i o) = encodeAsWord2 (1::Int) <> encode i <> encode o
    encode (T3 i o) = encodeAsWord2 (2::Int) <> encode i <> encode o
    encode (T4 i o) = encodeAsWord2 (3::Int) <> encode i <> encode o
    decode = decodeAsWord2 >>= \(w::Int) -> case w of
        0 -> decode >>= \i -> decode >>= \o -> return (T1 i o)
        1 -> decode >>= \i -> decode >>= \o -> return (T2 i o)
        2 -> decode >>= \i -> decode >>= \o -> return (T3 i o)
        3 -> decode >>= \i -> decode >>= \o -> return (T4 i o)
        _ -> fail "unknown TaskData"

encodeAsWord2 :: Integral n => BitEncoder n
encodeAsWord2 = mconcat . map encode . numberToBits 2
    
decodeAsWord2 :: Integral n => BitDecoder n
decodeAsWord2 = liftM numberFromBits $ replicateM 2 decode

encodeAsWord3 :: Integral n => BitEncoder n
encodeAsWord3 = mconcat . map encode . numberToBits 3
    
decodeAsWord3 :: Integral n => BitDecoder n
decodeAsWord3 = liftM numberFromBits $ replicateM 3 decode

encodeAsWord8 :: Integral n => BitEncoder n
encodeAsWord8 = mconcat . map encode . numberToBits 8
    
decodeAsWord8 :: Integral n => BitDecoder n
decodeAsWord8 = liftM numberFromBits $ replicateM 8 decode

encodeAsWord32 :: Integral n => BitEncoder n
encodeAsWord32 = mconcat . map encode . numberToBits 32

decodeAsWord32 :: Integral n => BitDecoder n
decodeAsWord32 = liftM numberFromBits $ replicateM 32 decode

int32ToWord32 :: Int32 -> Word32
int32ToWord32 = fromIntegral

word32ToInt32 :: Word32 -> Int32
word32ToInt32 w
    | w <= fromIntegral (maxBound :: Int32) = fromIntegral w
    | otherwise = fromIntegral (w - 2^(32::Int32))

encodeAsInt32 :: Integral n => BitEncoder n
encodeAsInt32 = encodeAsWord32 . int32ToWord32 . fromIntegral

decodeAsInt32 :: Integral n => BitDecoder n
decodeAsInt32 = liftM (fromIntegral . word32ToInt32) decodeAsWord32

numberToBits :: Integral n => Int -> n -> [Bool]
numberToBits sz = reverse . pad . conv
  where
    conv :: Integral n => n -> [Bool]
    conv 0 = []
    conv x = (x `mod` 2 == 1) : conv (x `div` 2)
    pad :: [Bool] -> [Bool]
    pad bits = take sz (bits ++ repeat False)

numberFromBits :: Integral n => [Bool] -> n
numberFromBits = foldr (\bit acc -> (if bit then 1 else 0) + 2 * acc) 0 . reverse

-- * base64 stuff

encToBase64 :: Coded a => a -> String
encToBase64 = BS8.unpack . Base64.encode . encToBytes

decFromBase64 :: Coded a => String -> Maybe a
decFromBase64 = decFromBytes . Base64.decodeLenient . fromString
