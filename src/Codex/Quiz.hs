{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

--
-- Multiple choice quizzes
--
module Codex.Quiz
  ( Quiz(..)
  , Question(..)
  , Choices(..)
  , Answers(..)
  , Key
  , shuffleQuiz
  , lookupAnswer
  , listLabels
  ) where

import           Codex.Types
import           Codex.Page
import           Codex.Random (Rand)
import qualified Codex.Random as Rand

import qualified Text.Pandoc.Definition as P
import qualified Text.Pandoc.Walk as P

import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Char
import           Data.Monoid
import           Data.Maybe (fromMaybe)
import           Data.Hashable

import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.Aeson (ToJSON, FromJSON)
import           Data.List (groupBy)

-- | a quiz has a preamble and a list of questions
data Quiz
  = Quiz { preamble :: [P.Block]
         , questions :: [Question]
         }

data Question
  = Question { identifier :: String
             , description :: [P.Block]
             , choices :: Choices
             }

data Choices
  = FillIn Text (Text -> Text)
  -- bundle the answer key with a normalization function
  -- for now, this always removes spaces
  | Alternatives Bool P.ListAttributes Alts
  -- list of multiple choices 

type Alts =  [(Key, Bool, [P.Block])]

type Key = String

-- | answers to a quiz 
-- mapping from question identifier to (possibly many) keys
newtype Answers = Answers (HashMap String [Key])
  deriving (Show, Semigroup, Monoid, ToJSON, FromJSON)
  

-- | lookup selections for a specific question
lookupAnswer :: Question -> Answers -> [Key]
lookupAnswer Question{..} (Answers hm) 
  = fromMaybe [] $ HashMap.lookup identifier hm


-- | deterministic shuffle questions & answers in a quiz
shuffleQuiz :: UserLogin -> Page -> Quiz
shuffleQuiz uid page
  = Rand.run (makeQuiz page >>= shuffle1 >>= shuffle2) seed
  where
    meta = pageMeta page
    seed = fromMaybe (hash uid) (lookupFromMeta "random-seed" meta)
    opt1 = fromMaybe False $ lookupFromMeta "shuffle-questions" meta
    opt2 = fromMaybe False $ lookupFromMeta "shuffle-answers" meta
    shuffle1 = if opt1 then shuffleQuestions else return
    shuffle2 = if opt2 then shuffleChoices else return 


-- | split up a list of blocks into questions
makeQuiz :: Page -> Rand Quiz
makeQuiz (P.Pandoc _ blocks)
  = Quiz blocks' <$> sequence (map Rand.choose groups)
  where
    blocks' = takeWhile (not.isQuestion) blocks
    blocks''= dropWhile (not.isQuestion) blocks
    groups  = groupQuestions (makeQuestions blocks'')

-- | group together questions with the same group attribute
groupQuestions :: [Question] -> [[Question]]
groupQuestions
  = groupBy (\q q' -> group q `eq` group q')
  where
    group = lookup "group" . headerAttrs . questionHeader 
    eq (Just g) (Just g') = g == g'
    eq _        _         = False

-- | split a list of blocks into questions
makeQuestions :: [P.Block] -> [Question]
makeQuestions [] = []
makeQuestions (block : blocks)
  | isQuestion block =
    makeQuestion block blocks' : makeQuestions blocks''
  | otherwise =
    makeQuestions blocks''
  where
    blocks'  = takeWhile (not . isQuestion) blocks
    blocks'' = dropWhile (not . isQuestion) blocks



-- | get the header block from a question
questionHeader :: Question -> P.Block
questionHeader Question{..} = head description


-- | check if a block is a question header
isQuestion :: P.Block -> Bool
isQuestion b = "question" `elem` headerClasses b

-- | get class atributes for a header block
headerClasses :: P.Block -> [String]
headerClasses (P.Header _ (_, classes, _) _) = classes
headerClasses _                            = []

headerAttrs :: P.Block -> [(String,String)]
headerAttrs (P.Header _ (_, _, kvs) _) = kvs
headerAttrs _                        = []

headerIdent :: P.Block -> Maybe String
headerIdent (P.Header _ (id, _,_) _) = Just id
headerIdent _                        = Nothing

removeKey :: String -> P.Block -> P.Block
removeKey k (P.Header n (id, classes, kvs) inlines)
  = P.Header n (id, classes, filter (\(k',_) ->  k' /= k) kvs) inlines
removeKey _ b = b


makeQuestion :: P.Block -> [P.Block] -> Question
makeQuestion header rest
  = Question { identifier = fromMaybe "" (headerIdent header)
             , description = (header':prefix) ++ posfix
             , choices =
                 if  "fillin" `elem` headerClasses header
                 then FillIn (T.concat $ map T.pack answers) normalize
                 else Alternatives multiples attrs alts
             }
  where
    header' = removeKey "answer" header
    answers = [a | ("answer",a) <- headerAttrs header]
    normalize = T.filter (not.isSpace) 
    prefix = takeWhile (not . isList) rest
    posfix = drop 1 $ dropWhile (not . isList) rest
    (attrs,items) = fromMaybe (emptyAttrs,[]) $
                    getFirst $ P.query getList rest
    alts = [ (label, truth, item)
           | (label, item) <- zip (listLabels attrs) items
           , let truth = label `elem` answers
           ]
    multiples = "multiple" `elem` headerClasses header
    emptyAttrs = (1, P.DefaultStyle, P.DefaultDelim)
    isList (P.OrderedList _ _) = True
    isList _                   = False
    getList (P.OrderedList attrs items)  = First (Just (attrs, items))
    getList _                            = First Nothing
    


-- | shuffle questions and answers
shuffleQuestions :: Quiz -> Rand Quiz
shuffleQuestions (Quiz preamble questions)
  = Quiz preamble <$> Rand.shuffle questions

-- | shuffle answers alternatives in multiple choice questions
shuffleChoices :: Quiz -> Rand Quiz
shuffleChoices (Quiz preamble questions)
  = Quiz preamble <$> mapM shuffle questions
  where
    shuffle q@Question{..} = case choices of
      FillIn{} ->
        return q   -- no shuffling required
      Alternatives multiples attrs alts -> do
        alts' <- Rand.shuffle alts -- shuffle alternatives
        return q { choices = Alternatives multiples attrs alts' }

-----------------------------------------------------------
-- Pandoc stuff
-----------------------------------------------------------
-- | enumerate Pandoc list labels
listLabels :: P.ListAttributes -> [String]
listLabels (start, style, _) = drop (start-1) $ listNumbers style

listNumbers :: P.ListNumberStyle -> [String]
listNumbers P.LowerAlpha = map (:"") ['a'..'z']
listNumbers P.UpperAlpha = map (:"") ['A'..'Z']
listNumbers P.LowerRoman = lowerRomans
listNumbers P.UpperRoman = upperRomans
listNumbers _          = map show [1::Int .. 100]


lowerRomans, upperRomans :: [String]
lowerRomans = map makeRoman [1..100]
upperRomans = map (map toUpper) lowerRomans


makeRoman :: Int -> String
makeRoman 0 = ""
makeRoman n
  | n>0 = symbols ++ makeRoman (n - value)
  | otherwise = error "lowerRoman: negative argument"
  where (value, symbols) = head $ dropWhile ((>n).fst) numerals

numerals :: [(Int, String)]
numerals =
  [ (1000, "m"), (900, "cm"), (500, "d"), (400, "cd"),
    (100, "c"), (90, "xc"), (50, "l"), (40, "xl"),
    (10, "x"),  (9, "ix"), (5, "v"), (4, "iv"), (1, "i") ]

