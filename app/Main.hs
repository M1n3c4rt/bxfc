{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

module Main where
import System.Environment (getArgs)
import qualified Data.Set as S
import Data.Char (chr, ord)
import qualified Data.ByteString as BS
import Text.Read (readMaybe)
import qualified Data.HashMap.Strict as HM
import Data.Bifunctor (second)
import Data.Maybe (fromJust)
import Debug.Trace (traceShowId, traceShow)
import Data.List (elemIndex)

main :: IO ()
main = do
    args <- getArgs
    case length args of
        0 -> putStr helpMessage
        4 -> processArgs args
        x -> errorWithoutStackTrace $ "Expected 4 arguments (filename, input type, output type, input) but got " ++ show x

helpMessage :: String
helpMessage = unlines
    [
        "bxfc - An interpreter written in Haskell for the esoteric programming language Boxfuscate",
        "",
        "Usage: bxfc [filename] [input type] [output type] [input]",
        "(or run without arguments to show this message)",
        "The filename points to a .bxfc file in which the Boxfuscate code is written.",
        "",
        "If the input type is  -s, read the input as a string of ASCII characters.",
        "                      -n,                     non-negative integer.",
        "                      -f,                     binary file.",
        "",
        "If the output type is -s, display output as a string of ASCII characters.",
        "                      -n,                     non-negative integer.",
        "                      -b,                     string of bits.",
        "",
        "Note: numbers are read and displayed with the least significant bit at position 0."
    ]

data InputType = IString | INumber | IFile deriving (Show, Eq)
data OutputType = OString | ONumber | OBits deriving (Show, Eq)

readInputType :: String -> Maybe InputType
readInputType x = case x of
    "-s" -> Just IString
    "-n" -> Just INumber
    "-f" -> Just IFile
    _ -> Nothing

readOutputType :: String -> Maybe OutputType
readOutputType x = case x of
    "-s" -> Just OString
    "-n" -> Just ONumber
    "-b" -> Just OBits
    _ -> Nothing

readInput :: InputType -> String -> IO (S.Set Integer)
readInput it s = case it of
    IString ->
        if not (all ((<256) . ord) s) then errorWithoutStackTrace "characters in input out of range 0-255"
        else return $ toSet $ concatMap toByte s
    INumber -> case readMaybe s of
        Just n -> if n < 0 then errorWithoutStackTrace "input number cannot be negative" else return $ toSet $ toBin n
        Nothing -> errorWithoutStackTrace "input given with type -n but not an integer"
    IFile -> do
        contents <- BS.readFile s
        readInput IString $ map (chr . fromIntegral) $ BS.unpack contents
    where
        toByte n = reverse $ take 8 $ (++repeat False) $ toBin $ fromIntegral $ ord n

        toBin :: Integer -> [Bool]
        toBin 0 = []
        toBin n = odd n : toBin (n `div` 2)

        toSet = S.fromList . map fst . filter snd . zip [0::Integer ..]

showOutput :: OutputType -> S.Set Integer -> IO ()
showOutput ot ps = case ot of
    OString -> putStrLn $ map (chr . fromIntegral . toInt . reverse) $ take8s bits
    ONumber -> print $ toInt bits
    OBits -> putStrLn $ unwords $ map (map (\b -> if b then '1' else '0')) $ take8s bits
    where
        culled = S.filter (>=0) ps
        m = maximum culled
        bits = if null culled then [] else map (`S.member` culled) [0..m]

        toInt :: [Bool] -> Integer
        toInt (x:xs) = (if x then 1 else 0) + 2 * toInt xs
        toInt [] = 0

        take8s xs = if length xs <= 8 then [take 8 (xs ++ repeat False)] else take 8 xs : take8s (drop 8 xs)

processArgs :: [String] -> IO ()
processArgs [filename,it,ot,input] = do
    contents <- readFile filename

    let inputType =
            case readInputType it of
                Nothing -> errorWithoutStackTrace "input type must be one of -s, -n or -f"
                Just t -> t

    let outputType =
            case readOutputType ot of
                Nothing -> errorWithoutStackTrace "output type must be one of -s, -n or -b"
                Just t -> t

    bits <- readInput inputType input
    showOutput outputType $ run (parseContents contents) bits
processArgs _ = undefined

data LineType =
    Empty
    | Thin
    | Thick
    | Double
    | ThinDotted2
    | ThickDotted2
    | ThinDotted3
    | ThickDotted3
    | ThinDotted4
    | ThickDotted4
    | Curved deriving (Eq, Show)
type Cell = [((Integer,Integer),LineType)]
type Grid = HM.HashMap (Integer,Integer) Cell

north :: (Integer,Integer)
north = (0,-1)
east :: (Integer,Integer)
east = (1,0)
south :: (Integer,Integer)
south = (0,1)
west :: (Integer,Integer)
west = (-1,0)

cellMap :: HM.HashMap Char Cell
cellMap = HM.fromList $ map (second (zip [north, east, south, west]))
    [
        ('─',[Empty,Thin ,Empty,Thin ]),
        ('━',[Empty,Thick,Empty,Thick]),
        ('│',[Thin ,Empty,Thin ,Empty]),
        ('┃',[Thick,Empty,Thick,Empty]),
        ('┄',[Empty      ,ThinDotted3 ,Empty      ,ThinDotted3 ]),
        ('┅',[Empty      ,ThickDotted3,Empty      ,ThickDotted3]),
        ('┆',[ThinDotted3 ,Empty      ,ThinDotted3 ,Empty      ]),
        ('┇',[ThickDotted3,Empty      ,ThickDotted3,Empty      ]),
        ('┈',[Empty      ,ThinDotted4 ,Empty      ,ThinDotted4 ]),
        ('┉',[Empty      ,ThickDotted4,Empty      ,ThickDotted4]),
        ('┊',[ThinDotted4 ,Empty      ,ThinDotted4 ,Empty      ]),
        ('┋',[ThickDotted4,Empty      ,ThickDotted4,Empty      ]),
        ('┌',[Empty,Thin ,Thin ,Empty]),
        ('┍',[Empty,Thick,Thin ,Empty]),
        ('┎',[Empty,Thin ,Thick,Empty]),
        ('┏',[Empty,Thick,Thick,Empty]),
        ('┐',[Empty,Empty,Thin ,Thin ]),
        ('┑',[Empty,Empty,Thin ,Thick]),
        ('┒',[Empty,Empty,Thick,Thin ]),
        ('┓',[Empty,Empty,Thick,Thick]),
        ('└',[Thin ,Thin ,Empty,Empty]),
        ('┕',[Thin ,Thick,Empty,Empty]),
        ('┖',[Thick,Thin ,Empty,Empty]),
        ('┗',[Thick,Thick,Empty,Empty]),
        ('┘',[Thin ,Empty,Empty,Thin ]),
        ('┙',[Thin ,Empty,Empty,Thick]),
        ('┚',[Thick,Empty,Empty,Thin ]),
        ('┛',[Thick,Empty,Empty,Thick]),
        ('├',[Thin ,Thin ,Thin ,Empty]),
        ('┝',[Thin ,Thick,Thin ,Empty]),
        ('┞',[Thick,Thin ,Thin ,Empty]),
        ('┟',[Thin ,Thin ,Thick,Empty]),
        ('┠',[Thick,Thin ,Thick,Empty]),
        ('┡',[Thick,Thick,Thin ,Empty]),
        ('┢',[Thin ,Thick,Thick,Empty]),
        ('┣',[Thick,Thick,Thick,Empty]),
        ('┤',[Thin ,Empty,Thin ,Thin ]),
        ('┥',[Thin ,Empty,Thin ,Thick]),
        ('┦',[Thick,Empty,Thin ,Thin ]),
        ('┧',[Thin ,Empty,Thick,Thin ]),
        ('┨',[Thick,Empty,Thick,Thin ]),
        ('┩',[Thick,Empty,Thin ,Thick]),
        ('┪',[Thin ,Empty,Thick,Thick]),
        ('┫',[Thick,Empty,Thick,Thick]),
        ('┬',[Empty,Thin ,Thin ,Thin ]),
        ('┭',[Empty,Thin ,Thin ,Thick]),
        ('┮',[Empty,Thick,Thin ,Thin ]),
        ('┯',[Empty,Thick,Thin ,Thick]),
        ('┰',[Empty,Thin ,Thick,Thin ]),
        ('┱',[Empty,Thin ,Thick,Thick]),
        ('┲',[Empty,Thick,Thick,Thin ]),
        ('┳',[Empty,Thick,Thick,Thick]),
        ('┴',[Thin ,Thin ,Empty,Thin ]),
        ('┵',[Thin ,Thin ,Empty,Thick]),
        ('┶',[Thin ,Thick,Empty,Thin ]),
        ('┷',[Thin ,Thick,Empty,Thick]),
        ('┸',[Thick,Thin ,Empty,Thin ]),
        ('┹',[Thick,Thin ,Empty,Thick]),
        ('┺',[Thick,Thick,Empty,Thin ]),
        ('┻',[Thick,Thick,Empty,Thick]),
        ('┼',[Thin ,Thin ,Thin ,Thin ]),
        ('┽',[Thin ,Thin ,Thin ,Thick]),
        ('┾',[Thin ,Thick,Thin ,Thin ]),
        ('┿',[Thin ,Thick,Thin ,Thick]),
        ('╀',[Thick,Thin ,Thin ,Thin ]),
        ('╁',[Thin ,Thin ,Thick,Thin ]),
        ('╂',[Thick,Thin ,Thick,Thin ]),
        ('╃',[Thick,Thin ,Thin ,Thick]),
        ('╄',[Thick,Thick,Thin ,Thin ]),
        ('╅',[Thin ,Thin ,Thick,Thick]),
        ('╆',[Thin ,Thick,Thick,Thin ]),
        ('╇',[Thick,Thick,Thin ,Thick]),
        ('╈',[Thin ,Thick,Thick,Thick]),
        ('╉',[Thick,Thin ,Thick,Thick]),
        ('╊',[Thick,Thick,Thick,Thin ]),
        ('╋',[Thick,Thick,Thick,Thick]),
        ('╌',[Empty       ,ThinDotted2 ,Empty       ,ThinDotted2 ]),
        ('╍',[Empty       ,ThickDotted2,Empty       ,ThickDotted2]),
        ('╎',[ThinDotted2 ,Empty       ,ThinDotted2 ,Empty       ]),
        ('╏',[ThickDotted2,Empty       ,ThickDotted2,Empty       ]),
        ('═',[Empty ,Double,Empty ,Double]),
        ('║',[Double,Empty ,Double,Empty ]),
        ('╒',[Empty ,Double,Thin  ,Empty ]),
        ('╓',[Empty ,Thin  ,Double,Empty ]),
        ('╔',[Empty ,Double,Double,Empty ]),
        ('╕',[Empty ,Empty ,Thin  ,Double]),
        ('╖',[Empty ,Empty ,Double,Thin  ]),
        ('╗',[Empty ,Empty ,Double,Double]),
        ('╘',[Thin  ,Double,Empty ,Empty ]),
        ('╙',[Double,Thin  ,Empty ,Empty ]),
        ('╚',[Double,Double,Empty ,Empty ]),
        ('╛',[Thin  ,Empty ,Empty ,Double]),
        ('╜',[Double,Empty ,Empty ,Thin  ]),
        ('╝',[Double,Empty ,Empty ,Double]),
        ('╞',[Thin  ,Double,Thin  ,Empty ]),
        ('╟',[Double,Thin  ,Double,Empty ]),
        ('╠',[Double,Double,Double,Empty ]),
        ('╡',[Thin  ,Empty ,Thin  ,Double]),
        ('╢',[Double,Empty ,Double,Thin  ]),
        ('╣',[Double,Empty ,Double,Double]),
        ('╤',[Empty ,Double,Thin  ,Double]),
        ('╥',[Empty ,Thin  ,Double,Thin  ]),
        ('╦',[Empty ,Double,Double,Double]),
        ('╧',[Thin  ,Double,Empty ,Double]),
        ('╨',[Double,Thin  ,Empty ,Thin  ]),
        ('╩',[Double,Double,Empty ,Double]),
        ('╪',[Thin  ,Double,Thin  ,Double]),
        ('╫',[Double,Thin  ,Double,Thin  ]),
        ('╬',[Double,Double,Double,Double]),
        ('╭',[Empty ,Curved,Curved,Empty ]),
        ('╮',[Empty ,Empty ,Curved,Curved]),
        ('╯',[Curved,Empty ,Empty ,Curved]),
        ('╰',[Curved,Curved,Empty ,Empty ]),
        ('╱',[Empty,Empty,Empty,Empty]),
        ('╲',[Empty,Empty,Empty,Empty]),
        ('╳',[Empty,Empty,Empty,Empty]),
        (' ',[Empty,Empty,Empty,Empty]),
        ('╴',[Empty,Empty,Empty,Thin ]),
        ('╵',[Thin ,Empty,Empty,Empty]),
        ('╶',[Empty,Thin ,Empty,Empty]),
        ('╷',[Empty,Empty,Thin ,Empty]),
        ('╸',[Empty,Empty,Empty,Thick]),
        ('╹',[Thick,Empty,Empty,Empty]),
        ('╺',[Empty,Thick,Empty,Empty]),
        ('╻',[Empty,Empty,Thick,Empty]),
        ('╼',[Empty,Thick,Empty,Thin ]),
        ('╽',[Thin ,Empty,Thick,Empty]),
        ('╾',[Empty,Thin ,Empty,Thick]),
        ('╿',[Thick,Empty,Thin ,Empty])
    ]

parseContents :: String -> Grid
parseContents cs = HM.fromList final
    where
        ls = lines cs
        lsx = map (zip [0 :: Integer ..]) ls
        lsy = concat $ zipWith (\c xs -> map (c,) xs) [0..] lsx
        final = map (\(y,(x,c)) -> ((x,y),HM.lookupDefault e c cellMap)) lsy
        e = errorWithoutStackTrace "non box-drawing characters in program"

isNeighbour :: LineType -> LineType -> Bool
isNeighbour a b = b `elem` case a of
    Empty -> [Empty,ThinDotted2,ThickDotted2,ThinDotted3,ThickDotted3,ThinDotted4,ThickDotted4]
    Thin -> [Thin,Curved,ThinDotted2,ThinDotted3,ThinDotted4]
    Thick -> [Thick,ThickDotted2,ThickDotted3,ThickDotted4]
    Double -> [Double]
    ThinDotted2 -> [Empty,Curved,Thin,ThinDotted2,ThinDotted3,ThinDotted4]
    ThinDotted3 -> [Empty,Curved,Thin,ThinDotted2,ThinDotted3,ThinDotted4]
    ThinDotted4 -> [Empty,Curved,Thin,ThinDotted2,ThinDotted3,ThinDotted4]
    ThickDotted2 -> [Empty,Thick,ThickDotted2,ThickDotted3,ThickDotted4]
    ThickDotted3 -> [Empty,Thick,ThickDotted2,ThickDotted3,ThickDotted4]
    ThickDotted4 -> [Empty,Thick,ThickDotted2,ThickDotted3,ThickDotted4]
    Curved -> [Curved,Thin,ThinDotted2,ThinDotted3,ThinDotted4]

check :: Grid -> Grid
check grid = HM.filterWithKey helper grid
    where helper c = not . all (\(d,a) -> isNeighbour a $ fromJust $ lookup (-d) $ HM.lookupDefault (map (,Empty) [north,east,south,west]) (c+d) grid)

findStart :: Grid -> ((Integer,Integer),(Integer,Integer))
findStart grid = case filter helper $ HM.toList grid of
    [] -> errorWithoutStackTrace "no starting point"
    [(p,s)] -> (p,fst $ head $ filter ((/=Empty) . snd) s)
    _ -> errorWithoutStackTrace "ambiguous starting point"
    where
        helper (_,xs) = (==1) $ length $ filter (/=Empty) $ map snd xs

run :: Grid -> S.Set Integer -> S.Set Integer
run a b = case HM.toList $ check a of
    [] -> let (start,dir) = findStart a in step 0 start dir a b
    xs ->  errorWithoutStackTrace $ "found neighbouring characters with mismatched line types at positions: " ++ show (map fst xs)

step :: Integer -> (Integer, Integer) -> (Integer, Integer) -> Grid -> S.Set Integer -> S.Set Integer
step pointer pos dir grid memory = case nextAction of
    (_,Empty) -> errorWithoutStackTrace "you should not be seeing this error; something has gone horribly wrong"
    (d,Thin) -> step (pointer-1) (pos+d) d grid memory
    (d,Thick) -> step (pointer+1) (pos+d) d grid memory
    (d,Double) -> step pointer (pos+d) d grid (flipBit pointer)
    (d,ThinDotted2) -> step pointer (jump ThinDotted2 pos d) d grid memory
    (d,ThickDotted2) -> step pointer (jump ThickDotted2 pos d) d grid memory
    (d,Curved) -> step pointer (pos+d) d grid memory
    (d,ThinDotted3) -> if all (<=pointer) memory then memory else step pointer (jump ThinDotted3 pos d) d grid memory
    (d,ThickDotted3) -> if all (<=pointer) memory then memory else step pointer (jump ThickDotted3 pos d) d grid memory
    (_,ThinDotted4) -> memory
    (_,ThickDotted4) -> memory
    where
        currentCell = HM.lookupDefault (map (,Empty) [north,east,south,west]) pos grid
        nextActions = filter ((/=Empty) . snd) $ filter ((/= -dir) . fst) currentCell
        nextAction = case nextActions of
            [] -> errorWithoutStackTrace "you shoudld not be seeing this error; something has gone horribly wrong"
            [x] -> x
            [x,y] -> if pointer `S.member` memory then
                    if elemIndex (fst x) [east,north,west,south] < elemIndex (fst y) [east,north,west,south] then x else y
                else
                    if elemIndex (fst x) [east,north,west,south] > elemIndex (fst y) [east,north,west,south] then x else y
            xs -> head $ filter ((==dir) . fst) xs

        flipBit x = if x `S.member` memory then S.delete x memory else S.insert x memory
        jump t p d = let next = HM.lookupDefault (map (,Empty) [north,east,south,west]) (p+d) grid in
            if (/=Empty) $ fromJust $ lookup d next then p+d+d else jump t (p+d) d


instance Num (Integer,Integer) where
    (a,b) + (c,d) = (a+c,b+d)
    negate (a,b) = (-a,-b)