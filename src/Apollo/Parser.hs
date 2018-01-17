module Apollo.Parser
( Parser
, parse
, satisfy
, string
, string_
, eof
, digit
, int
) where

import Control.Applicative

newtype Parser a
  = Parser
    { unParser :: String -> Maybe (String, a) }
  deriving Functor

parse :: Parser a -> String -> Maybe (String, a)
parse = unParser

instance Applicative Parser where
  pure x = Parser $ \s -> Just (s, x)
  Parser f <*> Parser x = Parser $ \s ->
    case f s of
      Just (s', f') -> fmap f' <$> x s'
      Nothing -> Nothing

instance Alternative Parser where
  empty = Parser (const Nothing)
  Parser p1 <|> Parser p2 = Parser $ \s -> maybe (p2 s) pure (p1 s)

instance Monad Parser where
  return = pure
  Parser x >>= k = Parser $ \s -> case x s of
    Just (s', x') -> unParser (k x') s'
    Nothing -> Nothing

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser $ \s -> case s of
  [] -> Nothing
  (c:cs) -> if p c then Just (cs, c) else Nothing

string :: String -> Parser String
string [] = pure []
string (c:cs) = (:) <$> satisfy (c ==) <*> string cs

string_ :: String -> Parser ()
string_ [] = pure ()
string_ (c:cs) = satisfy (c ==) >> string_ cs

digit :: Parser Int
digit = read . pure <$> satisfy (`elem` s) where
  s :: String
  s = "0123456789"

-- | A positive integer.
int :: Parser Int
int = sum . zipWith (*) p10s . reverse <$> some digit where
  p10s :: [Int]
  p10s = (ten ^) <$> [z ..]
  ten = 10 :: Int
  z = 0 :: Int

eof :: Parser ()
eof = Parser $ \s -> case s of
  [] -> Just ([], ())
  _ -> Nothing
