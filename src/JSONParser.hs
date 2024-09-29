{-# HLINT ignore "Use lambda-case" #-}

module JSONParser where
import Numeric (showHex)
import Data.Char (ord, isDigit, digitToInt, chr, isHexDigit, isSpace)
import Data.List (intercalate)
import Test.QuickCheck
import Control.Applicative (Alternative(..), optional)
import Control.Monad (replicateM)
import Data.Bits (shiftL)


data JValue = JNull
            | JBool Bool
            | JString String
            | JNumber { int :: Integer, frac :: [Int], exponent :: Integer }
            | JArray [JValue]
            | JObject [(String, JValue)]
            deriving (Eq)

instance Show JValue where
  show JNull = "null"
  show (JBool True) = "true"
  show (JBool False) = "false"
  show (JString s) = showJSONString s
  show (JNumber n [] 0) = show n
  show (JNumber n f 0) = show n ++ "." ++ concatMap show f
  show (JNumber n [] e) = show n ++ "e" ++ show e
  show (JNumber n f e) = show n ++ "." ++ concatMap show f ++ "e" ++ show e
  show (JArray a) = "[" ++ intercalate ", " (map show a) ++ "]"
  show (JObject a) = "{" ++ intercalate ", " (map showKeyValue a) ++ "}"
    where showKeyValue (k, v) = showJSONString k ++ ": " ++ show v



showJSONString :: String -> String
showJSONString s = "\"" ++ concatMap showJSONChar s ++ "\""

showJSONChar :: Char -> String
showJSONChar '\'' = "\'"
showJSONChar '\"' = "\\\""
showJSONChar '\\' = "\\\\"
showJSONChar '/' = "\\/"
showJSONChar '\b' = "\\b"
showJSONChar '\f' = "\\f"
showJSONChar '\n' = "\\n"
showJSONChar '\r' = "\\r"
showJSONChar '\t' = "\\t"
showJSONChar c | isControl c = "\\u" ++ showJSONNonASCIIChar c
               | otherwise = [c]
               where
                 showJSONNonASCIIChar c =
                   let a = "0000" ++ showHex (ord c) "" in drop (length a - 4) a

isControl :: Char -> Bool
isControl c = c `elem` ['\0' .. '\31']



jNullGen :: Gen JValue
jNullGen = pure JNull

jBoolGen :: Gen JValue
jBoolGen = JBool <$> arbitrary

jNumberGen :: Gen JValue
jNumberGen = JNumber <$> arbitrary <*> listOf (choose (0, 9)) <*> arbitrary

jsonStringGen :: Gen String
jsonStringGen = concat <$> listOf (oneof [ vectorOf 1 arbitraryUnicodeChar
                                         , escapedUnicodeChar])
  where
    escapedUnicodeChar = ("\\u" ++) <$> vectorOf 4 (elements hexDigitalLetters)
    hexDigitalLetters = ['0'..'9'] ++ ['a'..'f'] ++ ['A'..'F']

jStringGen :: Gen JValue
jStringGen = JString <$> jsonStringGen

jArrayGen :: Int -> Gen JValue
jArrayGen = fmap JArray . scale (`div` 2) . listOf . jValueGen . (`div` 2)

jObjectGen :: Int -> Gen JValue
jObjectGen = fmap JObject . scale (`div` 2) . listOf . objKV . (`div` 2)
  where objKV n = (,) <$> jsonStringGen <*> jValueGen n

jValueGen :: Int -> Gen JValue
jValueGen n = if n < 5
              then frequency [(4, oneof scalarGens), (1, oneof (compositeGens n))]
              else frequency [(1, oneof scalarGens), (4, oneof (compositeGens n))]
  where
    scalarGens = [jNullGen, jBoolGen, jNumberGen, jStringGen]
    compositeGens n = [jArrayGen n, jObjectGen n]

instance Arbitrary JValue where
  arbitrary = sized jValueGen

-- TODO: Try to understand
jsonWhitespaceGen :: Gen String
jsonWhitespaceGen =
  scale (round . sqrt . fromIntegral)
  . listOf
  . elements
  $ [' ' , '\n' , '\r' , '\t']

stringify :: JValue -> Gen String
stringify = pad . go
  where
    surround l r j = l ++ j ++ r
    pad gen = surround <$> jsonWhitespaceGen <*> jsonWhitespaceGen <*> gen
    commaSeparated = pad . pure . intercalate ","

    go value = case value of
      JArray elements ->
        mapM (pad . stringify) elements
          >>= fmap (surround "[" "]") . commaSeparated
      JObject kvs ->
        mapM stringifyKV kvs >>= fmap (surround "{" "}") . commaSeparated
      _allOthers           -> return $ show value

    stringifyKV (k, v) =
      surround <$> pad (pure $ showJSONString k) <*> stringify v <*> pure ":"

-- STOP TODO



newtype Parser i o = Parser { runParser :: i -> Maybe (i, o)}

instance Functor (Parser i) where
  -- fmap f parser :: (a -> b) -> Parser a -> Parser b
  fmap f parser = Parser $ \i -> case runParser parser i of
                          Nothing -> Nothing
                          Just (i', o) -> Just (i', f o)
  -- The following uses that Maybe is already a functor
  -- fmap (fmap f) applies fmap f on the inner type of the Maybe
  -- fmap f parser = Parser $ \i -> fmap (fmap f) (runParser parser i)

instance Applicative (Parser i) where
  -- pure :: o -> Parser i o
  pure x = Parser $ \i -> pure (i, x)
  -- (<*>) :: Parser i (a -> b) -> Parser i a -> Parser i b
  pf <*> po = Parser $ \i -> case runParser pf i of
                               Nothing -> Nothing
                               --Just (rest, f) -> fmap (fmap f) (runParser po rest)
                               Just (rest, f) -> fmap f <$> runParser po rest

instance Alternative (Parser i) where
  empty = Parser $ const empty
  p1 <|> p2 = Parser $ \i -> runParser p1 i <|> runParser p2 i

instance Monad (Parser i) where
  -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= f = Parser $ \i -> case runParser p i of
                             Nothing -> Nothing
                             Just (rest, o) -> runParser (f o) rest


satisfy :: (a -> Bool) -> Parser [a] a
satisfy predicate = Parser $ \s -> case s of
                                     (x:xs) | predicate x -> Just (xs, x)
                                     _ -> Nothing

char :: Char -> Parser String Char
char c = satisfy (== c)

digit1 :: Parser String Int
digit1 = Parser $ \i -> case runParser (satisfy isDigit) i of
                          Nothing -> Nothing
                          Just (i', o) -> Just (i', digitToInt o)
digit :: Parser String Int
digit = digitToInt <$> satisfy isDigit
--digit = fmap digitToInt (satisfy isDigit)

string1 :: String -> Parser String String
string1 "" = Parser $ \i -> Just (i, "")
string1 (c:cs) = Parser $ \i -> case runParser (char c) i of
                                  Nothing -> Nothing
                                  Just (rest, _) -> case runParser (string1 cs) rest of
                                    Nothing -> Nothing
                                    Just (rest', _) -> Just (rest', c:cs)

string2 :: String -> Parser String String
string2 "" = Parser $ \i -> pure (i, "")
string2 (c:cs) = Parser $ \i -> case runParser (char c) i of
                                  Nothing -> Nothing
                                  -- Outer fmap (fmap (c:)): apply the inner value of Maybe on to (fmap (c:))
                                  --Just (rest, _) -> fmap (fmap (c:)) (runParser (string2 cs) rest)
                                  Just (rest, _) -> fmap (c:) <$> runParser (string2 cs) rest

string :: String -> Parser String String
string "" = pure ""
string (c:cs) = (:) <$> char c <*> string cs



jNull :: Parser String JValue
jNull = JNull <$ string "null" 
-- (<$) cames from Functor, returns the first argument wrapped into the functor if second succeeds
-- (<$) :: a -> f b -> f a

jBool :: Parser String JValue
jBool = JBool True <$ string "true" <|> JBool False <$ string "false"


jsonChar :: Parser String Char
jsonChar =     '"'  <$ string "\\\""
           <|> '\\' <$ string "\\\\"
           <|> '/'  <$ string "\\/"
           <|> '\b' <$ string "\\b"
           <|> '\f' <$ string "\\f"
           <|> '\n' <$ string "\\n"
           <|> '\r' <$ string "\\r"
           <|> '\t' <$ string "\\t"
           <|> unicodeChar
           <|> satisfy (\c -> not (c == '\"' || c == '\\' || isControl c))
  where
    unicodeChar = 
      chr . fromIntegral . digitsToNumber 16 0
        <$> (string "\\u" *> replicateM 4 hexDigit)

    hexDigit = digitToInt <$> satisfy isHexDigit


digitsToNumber :: Int -> Integer -> [Int] -> Integer
digitsToNumber base = foldl (\num d -> num * fromIntegral base + fromIntegral d)


jString :: Parser String JValue
jString = JString <$> (char '"' *> jString')
  where
    jString' = do
      optFirst <- optional jsonChar
      case optFirst of
        Nothing -> "" <$ char '"'
        Just first | not (isSurrogate first) ->
                     (first:) <$> jString'
        Just first -> do
          second <- jsonChar
          if isHighSurrogate first && isLowSurrogate second
            then (combineSurrogates first second :) <$> jString'
            else empty

highSurrogateLowerBound, highSurrogateUpperBound :: Int
highSurrogateLowerBound = 0xD800
highSurrogateUpperBound = 0xDBFF

lowSurrogateLowerBound, lowSurrogateUpperBound :: Int
lowSurrogateLowerBound  = 0xDC00
lowSurrogateUpperBound  = 0xDFFF

isHighSurrogate, isLowSurrogate, isSurrogate :: Char -> Bool
isHighSurrogate a =
  ord a >= highSurrogateLowerBound && ord a <= highSurrogateUpperBound
isLowSurrogate a  =
  ord a >= lowSurrogateLowerBound && ord a <= lowSurrogateUpperBound
isSurrogate a     = isHighSurrogate a || isLowSurrogate a

combineSurrogates :: Char -> Char -> Char
combineSurrogates a b = chr $
  ((ord a - highSurrogateLowerBound) `shiftL` 10)
  + (ord b - lowSurrogateLowerBound) + 0x10000


prop_genParseJString :: Property
prop_genParseJString =
  forAllShrink jStringGen shrink $ \js ->
    case runParser jString (show js) of
      Nothing     -> False
      Just (_, o) -> o == js


jUInt :: Parser String Integer
jUInt =     (\d ds -> digitsToNumber 10 0 (d:ds)) <$> digit19 <*> digits
        <|> fromIntegral <$> digit

digit19 :: Parser String Int
digit19 = digitToInt <$> satisfy (\x -> isDigit x && x /= '0')

digits :: Parser String [Int]
digits = some digit

jInt' :: Parser String Integer
jInt' = signInt <$> optional (char '-') <*> jUInt

signInt :: Maybe Char -> Integer -> Integer
signInt (Just '-') i = negate i
signInt _          i = i

jFrac :: Parser String [Int]
jFrac = char '.' *> digits

jExp :: Parser String Integer
jExp = (char 'e' <|> char 'E')
  *> (signInt <$> optional (char '+' <|> char '-') <*> jUInt)

jInt :: Parser String JValue
jInt = JNumber <$> jInt' <*> pure [] <*> pure 0

jIntExp :: Parser String JValue
jIntExp = JNumber <$> jInt' <*> pure [] <*> jExp

jIntFrac :: Parser String JValue
jIntFrac = (\i f -> JNumber i f 0) <$> jInt' <*> jFrac

jIntFracExp :: Parser String JValue
jIntFracExp = (\ (JNumber i f _) e -> JNumber i f e) <$> jIntFrac <*> jExp
-- ~(Jumber i f _), the ~ makes it a lazy pattern matching

jNumber :: Parser String JValue
jNumber = jIntFracExp <|> jIntExp <|> jIntFrac <|> jInt

prop_genParseJNumber :: Property
prop_genParseJNumber =
  forAllShrink jNumberGen shrink $ \jn ->
    case runParser jNumber (show jn) of
      Nothing -> False
      Just (_, o) -> o == jn

sourroundedBy :: Parser String a -> Parser String b -> Parser String a
sourroundedBy p1 p2 = p2 *> p1 <* p2

seperatedBy :: Parser i v -> Parser i s -> Parser i [v]
seperatedBy v s =     (:) <$> v <*> many (s *> v)
                  <|> pure []

spaces :: Parser String String
spaces = many (char ' ' <|> char '\n' <|> char '\r' <|> char '\t')

jArray :: Parser String JValue
jArray = JArray <$>
  (char '['
   *> (jValue `seperatedBy` char ',' `sourroundedBy` spaces)
   <* char ']')

prop_genParseJArray :: Property
prop_genParseJArray =
  forAllShrink (sized jArrayGen) shrink $ \ja -> do
    jas <- dropWhile isSpace <$> stringify ja
    return . counterexample (show jas) $ case runParser jArray jas of
      Nothing     -> False
      Just (_, o) -> o == ja


jObject :: Parser String JValue
jObject = JObject <$>
  (char '{' *> pair `seperatedBy` char ',' `sourroundedBy` spaces <* char '}')
  where
    --pair :: JValue -> JValue -> (JValue, JValue)
    pair = (\ (JString s) j -> (s, j))
      <$> (jString `sourroundedBy` spaces)
      <* char ':'
      <*> jValue

prop_genParseJObject :: Property
prop_genParseJObject =
  forAllShrink (sized jObjectGen) shrink $ \jo -> do
    jos <- dropWhile isSpace <$> stringify jo
    return . counterexample (show jos) $ case runParser jObject jos of
      Nothing     -> False
      Just (_, o) -> o == jo


jValue :: Parser String JValue
jValue = jValue' `sourroundedBy` spaces
  where
    jValue' =     jNull
              <|> jBool
              <|> jString
              <|> jNumber
              <|> jArray
              <|> jObject

parseJSON :: String -> Maybe JValue
parseJSON s = case runParser jValue s of
                Just ("", j) -> Just j
                _allOthers -> Nothing

prop_genParseJSON :: Property
prop_genParseJSON = forAllShrink (sized jValueGen) shrink $ \value -> do
  json <- stringify value
  return . counterexample (show json) . (== Just value) . parseJSON $ json



runTests :: IO ()
runTests = do
  putStrLn "== prop_genParseJString =="
  quickCheck prop_genParseJString

  putStrLn "== prop_genParseJNumber =="
  quickCheck prop_genParseJNumber

  putStrLn "== prop_genParseJArray =="
  quickCheck prop_genParseJArray

  putStrLn "== prop_genParseJObject =="
  quickCheck prop_genParseJObject

  putStrLn "== prop_genParseJSON =="
  quickCheck prop_genParseJSON
