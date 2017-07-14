import Data.Array.IArray

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BSC
import Data.Char (ord)
import qualified Data.Map as M
import Data.Word
import System.Posix.Env
import Test.QuickCheck

-- Convert a number to a list of digits in a given base
toNary :: Integer -> Integer -> [Integer]
toNary b 0 = [0]
toNary b x = loop ms x
  where ms            = reverse $ takeWhile (<= x) (map (b ^) [0..])
        loop [] _     = []
        loop (m:ms) x = x `div` m : loop ms (x `rem` m)

-- Convert a list of digits to a number in a given base
fromNary :: Integer -> [Integer] -> Integer
fromNary b xs = sum $ zipWith (*) (reverse xs) (iterate (*b) 1)

encode :: (Integral a, Num b) => Integer -> Integer -> [a] -> [b]
encode m n = map fromIntegral . toNary m . fromNary n . map fromIntegral

asciiEncode :: [Word8] -> String
asciiEncode [] = ""
asciiEncode xs = map (chars !!) (encode (fromIntegral $ length chars) 256 xs)

wordEncode :: Array Int BS.ByteString -> [Word8] -> [BS.ByteString]
wordEncode _ []     = []
wordEncode words xs =
  map (words !) (encode (fromIntegral . rangeSize . bounds $ words) 256 xs)

byteEncode :: String -> Maybe [Word8]
byteEncode "" = Just []
byteEncode xs = f <$> mapM charMap xs
  where f = encode 256 (fromIntegral $ length chars)

chars :: [Char]
chars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['!','?']

charMap :: Char -> Maybe Int
charMap = flip M.lookup (M.fromList (zip chars [0..]))

pad :: Num a => Int -> [a] -> [a]
pad n xs = reverse (take n (reverse xs ++ repeat 0))

test :: [Word8] -> Bool
test xs =
  fmap (pad 3) (byteEncode (asciiEncode (take 3 xs))) == Just (pad 3 (take 3 xs))

onParts :: Int -> ([a] -> [b]) -> [a] -> [b]
onParts _ _ [] = []
onParts n f xs =
  let (ys,zs) = splitAt n xs in
  f ys ++ onParts n f zs

armor :: BS.ByteString -> BS.ByteString
armor = BS.pack . fmap (fromIntegral . ord) . onParts 3 asciiEncode . BS.unpack

wordArmor :: Array Int BS.ByteString -> BS.ByteString -> BS.ByteString
wordArmor dict = BSC.unlines . onParts 9 (wordEncode dict) . BS.unpack

dictFile :: String -> String
dictFile homeDir = homeDir ++ "/.armor/18dict"

main :: IO ()
main = do
  Just home <- getEnv "HOME"
  dict <- listArray (0,262143) . fmap BSC.pack . lines <$> readFile (dictFile home)
  BS.interact (wordArmor dict)
