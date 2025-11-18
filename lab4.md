% Лабораторная работа № 4. Интерпретатор алгоритмического языка программирования
% 6 ноября 2025 г.
% Андрей Кабанов, ИУ9-11М

# Цель работы
Получение опыта применения функционального программирования для реализации 
интерпретатора алгоримического языка программирования.

# Реализация и тестирование

```haskell
module Main where

import Control.Monad (when)
import Control.Monad.Except
  ( ExceptT,
    MonadError (throwError),
    runExceptT,
  )
import Control.Monad.State
  ( MonadIO (liftIO),
    StateT (runStateT),
    gets,
    modify,
  )
import Data.Char (isDigit)
import Data.List (words)
import Data.Map qualified as Map
import Distribution.Compat.Prelude (unless)
import System.Environment (getArgs)
import System.Exit (exitSuccess)
import System.IO (hFlush, stdout)

type Stack = [Int]

type Dictionary = Map.Map String Int

type Variables = Map.Map String Int

data Token
  = Number Int
  | Word String
  | StringLiteral String
  | JMPFALSE Int
  | JMPFALSEST Int
  | JMP Int
  | RETURN
  deriving (Show, Eq)

data ForthState = ForthState
  { program :: [Token],
    wordCounter :: Int,
    stack :: Stack,
    returnStack :: [Int],
    compileStack :: [Int],
    dictionary :: Dictionary,
    memory :: Map.Map Int Int,
    nextVarAddr :: Int,
    compiling :: Bool,
    currentWord :: String,
    wordBody :: [Token]
  }
  deriving (Show)

type ForthM = ExceptT String (StateT ForthState IO)

initialState :: ForthState
initialState =
  ForthState
    { program = [],
      wordCounter = 0,
      stack = [],
      returnStack = [],
      compileStack = [],
      dictionary = builtins,
      memory = Map.empty,
      nextVarAddr = 0,
      compiling = False,
      currentWord = "",
      wordBody = []
    }

builtins :: Dictionary
builtins =
  Map.fromList
    [ ("+", 1),
      ("-", 2),
      ("*", 3),
      ("/", 4),
      ("mod", 5),
      ("dup", 6),
      ("drop", 7),
      ("swap", 8),
      ("over", 9),
      ("negate", 10),
      (".", 11),
      (".s", 12),
      ("emit", 13),
      ("cr", 14),
      ("=", 15),
      ("<", 16),
      (">", 17),
      ("and", 18),
      ("or", 19),
      ("not", 20),
      ("key", 21),
      ("bye", 22),
      ("rot", 23),
      ("@", 24),
      ("!", 25),
      ("variable", 26)
    ]

interpretProgram :: String -> ForthM ()
interpretProgram input = do
  let tokens = parseTokens input
  let programWithHalt = tokens ++ [Word "bye"]
  modify $ \s -> s {program = programWithHalt, wordCounter = 0}
  interpret

interpret :: ForthM ()
interpret = do
  state <- gets id
  let tokens = program state
  let counter = wordCounter state

  when (counter < length tokens) $ do
    let token = tokens !! counter
    compilingMode <- gets compiling

    if compilingMode
      then handleCompilation token
      else handleExecution token
  where
    handleExecution :: Token -> ForthM ()
    handleExecution token = do
      case token of
        Word ":" -> do
          startWordDefinition
          modify $ \s -> s {wordCounter = wordCounter s + 1}
          interpret
        Number n -> do
          modify $ \s -> s {stack = n : stack s, wordCounter = wordCounter s + 1}
          interpret
        Word w -> do
          dict <- gets dictionary
          case Map.lookup w dict of
            Just idx | idx >= 100 && idx < 1000 -> do
              modify $ \s -> s {returnStack = (wordCounter s + 1) : returnStack s}
              modify $ \s -> s {wordCounter = idx - 100}
              p <- gets program
              interpret
            Just idx | idx >= 1000 -> do
              let addr = idx - 1000
              modify $ \s -> s {stack = addr : stack s, wordCounter = wordCounter s + 1}
              interpret
            Just idx -> do
              executeBuiltin idx
              newState <- gets id
              modify $ \s -> s {wordCounter = wordCounter newState + 1}
              interpret
            Nothing -> throwError $ "Неизвестное слово: " ++ w
        StringLiteral str -> do
          modify $ \s ->
            s
              { stack = map fromEnum str ++ stack s,
                wordCounter = wordCounter s + 1
              }
          interpret
        JMPFALSE addr -> do
          stk <- gets stack
          case stk of
            cond : rest -> do
              modify $ \s -> s {stack = rest}
              if cond == 0
                then modify $ \s -> s {wordCounter = wordCounter s + addr}
                else modify $ \s -> s {wordCounter = wordCounter s + 1}
              interpret
            [] -> throwError "Стек пуст для JMPFALSE"
        JMPFALSEST addr -> do
          stk <- gets stack
          case stk of
            cond : rest -> do
              modify $ \s -> s {stack = rest}
              if cond == 0
                then modify $ \s -> s {wordCounter = wordCounter s + addr}
                else modify $ \s -> s {wordCounter = wordCounter s + 1}
              interpret
            [] -> throwError "Стек пуст для JMPFALSE"
        JMP addr -> do
          modify $ \s -> s {wordCounter = wordCounter s + addr}
          interpret
        RETURN -> do
          returnFromWord

returnFromWord :: ForthM ()
returnFromWord = do
  state <- gets id
  case returnStack state of
    returnAddr : rest -> do
      modify $ \s -> s {wordCounter = returnAddr, returnStack = rest}
      interpret
    [] -> do
      modify $ \s -> s {wordCounter = length (program s)}

parseTokens :: String -> [Token]
parseTokens input =
  let cleanedInput = removeComments input
      wordsList = splitWords cleanedInput
   in map parseToken wordsList
  where
    splitWords :: String -> [String]
    splitWords = words

    removeComments :: String -> String
    removeComments [] = []
    removeComments ('(' : xs) =
      let afterComment = skipComment 1 xs
       in removeComments afterComment
    removeComments (x : xs) = x : removeComments xs

    skipComment :: Int -> String -> String
    skipComment 0 rest = rest
    skipComment _ [] = []
    skipComment n ('(' : xs) = skipComment (n + 1) xs
    skipComment n (')' : xs) = skipComment (n - 1) xs
    skipComment n (_ : xs) = skipComment n xs

    parseToken :: String -> Token
    parseToken s
      | all isDigit s = Number (read s)
      | head s == '-' && not (null (tail s)) && all isDigit (tail s) =
          Number (read s)
      | otherwise = Word s

executeBuiltin :: Int -> ForthM ()
executeBuiltin idx = case idx of
  1 -> binaryOp (+) -- +
  2 -> binaryOp (-) -- -
  3 -> binaryOp (*)
  4 -> binaryOp div -- /
  5 -> binaryOp mod -- mod
  6 -> do
    -- dup
    stk <- gets stack
    case stk of
      x : _ -> modify $ \s -> s {stack = x : stk}
      [] -> throwError "Стек пуст для dup"
  7 -> do
    -- drop
    stk <- gets stack
    case stk of
      _ : xs -> modify $ \s -> s {stack = xs}
      [] -> throwError "Стек пуст для drop"
  8 -> do
    -- swap
    stk <- gets stack
    case stk of
      x : y : xs -> modify $ \s -> s {stack = y : x : xs}
      _ -> throwError "Недостаточно элементов для swap"
  9 -> do
    -- over
    stk <- gets stack
    case stk of
      x : y : xs -> modify $ \s -> s {stack = y : x : y : xs}
      _ -> throwError "Недостаточно элементов для over"
  10 -> do
    -- negate
    stk <- gets stack
    case stk of
      x : xs -> modify $ \s -> s {stack = (-x) : xs}
      [] -> throwError "Стек пуст для negate"
  11 -> do
    -- .
    stk <- gets stack
    case stk of
      x : xs -> do
        liftIO $ putStr (show x ++ " ")
        liftIO $ hFlush stdout
        modify $ \s -> s {stack = xs}
      [] -> throwError "Стек пуст для ."
  12 -> do
    -- .s
    stk <- gets stack
    liftIO $ putStrLn $ "<" ++ show (length stk) ++ "> " ++ unwords (map show stk)
    liftIO $ hFlush stdout
  13 -> do
    -- emit
    stk <- gets stack
    case stk of
      x : xs -> do
        liftIO $ putChar (toEnum x)
        liftIO $ putChar (toEnum 32)
        liftIO $ hFlush stdout
        modify $ \s -> s {stack = xs}
      [] -> throwError "Стек пуст для emit"
  14 -> do
    liftIO $ putStrLn ""
    liftIO $ hFlush stdout
  15 -> binaryOp (\a b -> if a == b then 1 else 0) -- =
  16 -> binaryOp (\a b -> if a < b then 1 else 0) -- <
  17 -> binaryOp (\a b -> if a > b then 1 else 0) -- >
  18 -> binaryOp (\a b -> if a /= 0 && b /= 0 then 1 else 0) -- and
  19 -> binaryOp (\a b -> if a /= 0 || b /= 0 then 1 else 0) -- or
  20 -> unaryOp (\a -> if a == 0 then 1 else 0) -- not
  21 -> do
    -- key
    char <- liftIO getChar
    modify $ \s -> s {stack = fromEnum char : stack s}
  22 -> do
    -- bye
    modify $ \s -> s {wordCounter = length (program s)}
  23 -> do
    -- rot
    stk <- gets stack
    case stk of
      x : y : z : xs -> modify $ \s -> s {stack = y : z : x : xs}
      _ -> throwError "Недостаточно элементов для rot"
  24 -> do
    -- @
    stk <- gets stack
    case stk of
      addr : rest -> do
        mem <- gets memory
        case Map.lookup addr mem of
          Just value -> modify $ \s -> s {stack = value : rest}
          Nothing -> throwError $ "Неизвестный адрес: " ++ show addr
      _ -> throwError "Ожидался адрес для @"
  25 -> do
    -- !
    stk <- gets stack
    case stk of
      addr : value : rest -> do
        mem <- gets memory
        modify $ \s -> s {memory = Map.insert addr value mem, stack = rest}
      _ -> throwError "Ожидался адрес и значение для !"
  26 -> do
    -- variable
    state <- gets id
    let tokens = program state
    let counter = wordCounter state + 1
    when (counter < length tokens) $ do
      case tokens !! counter of
        Word varName -> do
          dict <- gets dictionary
          mem <- gets memory
          nextAddr <- gets nextVarAddr
          when (Map.member varName dict) $
            throwError $
              "Слово " ++ varName ++ " уже существует"
          modify $ \s ->
            s
              { dictionary = Map.insert varName (1000 + nextAddr) dict,
                memory = Map.insert nextAddr 0 mem,
                nextVarAddr = nextAddr + 1,
                wordCounter = counter
              }
        _ -> throwError "Ожидалось имя переменной после variable"
  _ -> throwError "Неизвестный код встроенного слова"
  where
    binaryOp :: (Int -> Int -> Int) -> ForthM ()
    binaryOp op = do
      stk <- gets stack
      case stk of
        x : y : xs -> modify $ \s -> s {stack = op y x : xs}
        _ -> throwError "Недостаточно элементов для бинарной операции"

    unaryOp :: (Int -> Int) -> ForthM ()
    unaryOp op = do
      stk <- gets stack
      case stk of
        x : xs -> modify $ \s -> s {stack = op x : xs}
        _ -> throwError "Недостаточно элементов для унарной операции"

startWordDefinition :: ForthM ()
startWordDefinition = do
  state <- gets id
  let tokens = program state
  let counter = wordCounter state + 1
  when (counter < length tokens) $ do
    case tokens !! counter of
      Word name -> do
        dict <- gets dictionary
        when (Map.member name dict) $
          throwError $
            "Слово " ++ name ++ " уже определено"

        modify $ \s ->
          s
            { compiling = True,
              currentWord = name,
              wordBody = [],
              compileStack = [],
              wordCounter = counter
            }
      _ -> throwError "Ожидалось имя слова после ':'"

endWordDefinition :: ForthM ()
endWordDefinition = do
  state <- gets id
  when (compiling state) $ do
    let bodyWithReturn = reverse (RETURN : wordBody state)

    let articleStart = length (program state)
    modify $ \s ->
      s
        { program = program s ++ bodyWithReturn,
          dictionary = Map.insert (currentWord s) (articleStart + 100) (dictionary s),
          compiling = False,
          currentWord = "",
          wordBody = [],
          compileStack = []
        }

addToWordBody :: Token -> ForthM ()
addToWordBody token = do
  modify $ \s -> s {wordBody = token : wordBody s}

handleCompilation :: Token -> ForthM ()
handleCompilation token = do
  case token of
    Word ";" -> do
      endWordDefinition
      modify $ \s -> s {wordCounter = wordCounter s + 1}
      interpret
    Word "if" -> do
      state <- gets id

      let jmpFalsePos = length (wordBody state)
      addToWordBody (JMPFALSE 0)
      modify $ \s -> s {compileStack = jmpFalsePos : compileStack s}
      modify $ \s -> s {wordCounter = wordCounter s + 1}
      interpret
    Word "then" -> do
      state <- gets id
      case compileStack state of
        jmpPos : rest -> do
          let currentPos = length (wordBody state)
          let updatedBody = patchJump (wordBody state) jmpPos (currentPos - jmpPos)
          modify $ \s -> s {wordBody = updatedBody, compileStack = rest}
          modify $ \s -> s {wordCounter = wordCounter s + 1}
          interpret
        [] -> throwError "Нет соответствующего if для then"
    Word "break" -> do
      state <- gets id
      let tokens = program state
      let counter = wordCounter state
      let breakPos = findLoopEnd tokens (counter) 0
      when (breakPos == -1) $
        throwError "break вне цикла"
      addToWordBody (JMP (breakPos - counter))
      modify $ \s -> s {wordCounter = wordCounter s + 1}
      interpret
    Word "begin" -> do
      state <- gets id
      let loopStart = length (wordBody state)
      modify $ \s -> s {compileStack = loopStart : compileStack s}
      modify $ \s -> s {wordCounter = wordCounter s + 1}
      interpret
    Word "until" -> do
      state <- gets id
      case compileStack state of
        beginPos : rest -> do
          let currentPos = length (wordBody state)
          addToWordBody (JMPFALSEST (beginPos - currentPos))
          modify $ \s -> s {compileStack = rest, wordCounter = wordCounter s + 1}
          interpret
        [] -> throwError "Нет соответствующего begin для until"
    Word "exit" -> do
      addToWordBody RETURN
      modify $ \s -> s {wordCounter = wordCounter s + 1}
      interpret
    Word w -> do
      addToWordBody (Word w)
      modify $ \s -> s {wordCounter = wordCounter s + 1}
      interpret
    Number n -> do
      addToWordBody (Number n)
      modify $ \s -> s {wordCounter = wordCounter s + 1}
      interpret
    StringLiteral str -> do
      addToWordBody (StringLiteral str)
      modify $ \s -> s {wordCounter = wordCounter s + 1}
      interpret
    _ -> do
      addToWordBody token
      modify $ \s -> s {wordCounter = wordCounter s + 1}
      interpret

patchJump :: [Token] -> Int -> Int -> [Token]
patchJump body jumpPos newAddr =
  let (before, jumpInst : after) = splitAt jumpPos (reverse body)
   in case jumpInst of
        JMPFALSE _ -> reverse (before ++ [JMPFALSE newAddr] ++ after)
        _ -> body

findLoopEnd :: [Token] -> Int -> Int -> Int
findLoopEnd tokens pos depth
  | pos >= length tokens = -1
  | otherwise = case tokens !! pos of
      Word "begin" -> findLoopEnd tokens pos (depth + 1)
      Word "until" ->
        if depth == 0
          then pos + 1
          else
            findLoopEnd tokens (pos + 1) (depth - 1)
      _ -> findLoopEnd tokens (pos + 1) depth

testForth :: IO ()
testForth = do
  let testCases =
        [ "1 2 + .", -- 3
          "5 3 - .", -- 2
          "2 3 * .", -- 6
          "10 2 / .", -- 5
          "7 3 mod .", -- 1
          "5 dup . .", -- 5 5
          "1 2 swap . .", -- 1 2
          "1 2 over . . .", -- 1 2 1
          "3 2 1 .s rot .s", -- 3 2 1
          "3 2 1  .s swap .s rot .s",
          "5 5 = .", -- 1 (true)
          "5 3 = .", -- 0 (false)
          "3 5 < .", -- 1 (true)
          "5 3 < .", -- 0 (false)
          "5 3 > .", -- 1 (true)
          "3 5 > .", -- 0 (false)
          ": a 1 ; : b 2 ; : c a b swap . . ; c",
          ": abs dup 0 < if negate then ; 5 .", -- 5
          ": abs dup 0 < if negate then ; -5 abs .", -- 5
          ": loop 0 begin dup . 1 + dup 5 = until ; loop",
          ": loop 0 begin dup . . break 1 + dup 5 = until ; loop",
          ": factorial .s  1 .s  begin over .s  * .s  swap 1 - .s dup .s swap rot .s 1\
          \ > not until swap drop ; 5 factorial .", -- 120
          ": factorial dup 2 < if drop 1 exit then .s  1 .s  begin over .s  * .s\
          \  swap 1 - .s dup .s swap rot .s 1 > not until swap drop ; 0 factorial .",
          ": loop 0 begin dup . dup 2 > if drop break then 1 + dup 5 = until ; loop",
          ": factorial dup 1 > if dup 1 - factorial * exit then drop 1\
          \ ; 5 factorial .",
          ": av variable x 10 x ! x @ . ;",
          ": av variable x 10 x ! x @ . ; av",
          ": factorial variable c c ! variable p 1 p ! c @ 2 < if 1 exit then begin\
          \ c @ p @ * p ! c @ 1 - c ! c @ 1 > not until p @ ; 5 factorial .",
          "variable x 10 x ! x @ ." -- 10
        ]

  putStrLn "Запуск тестов:"
  mapM_ runTest testCases
  where
    runTest code = do
      putStrLn $ "Тест: " ++ code
      result <- runStateT (runExceptT (interpretProgram code)) initialState
      case result of
        (Left err, _) -> putStrLn $ "  Ошибка: " ++ err
        (Right _, state) -> putStrLn $ "  Стек: " ++ show (stack state)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      putStrLn "Выберите режим:"
      putStrLn "1 - Запуск тестов"
      putStrLn "2 - Выход"
      choice <- getLine
      case choice of
        "1" -> testForth
        "2" -> return ()
        _ -> putStrLn "Неверный выбор" >> main
    [filename] -> do
      program <- readFile filename
      putStrLn $ "Выполнение программы из файла: " ++ filename
      result <- runStateT (runExceptT (interpretProgram program)) initialState
      case result of
        (Left err, _) -> putStrLn $ "Ошибка: " ++ err
        (Right _, state) -> do
          putStrLn "Программа завершена."
          unless (null (stack state)) $
            putStrLn $
              "Стек: " ++ show (stack state)
    _ -> putStrLn "Использование: forth [файл.fth]"
```

# Вывод
В ходе работы был успешно применен функциональный подход для реализации 
интерпретатора языка FORTH, что позволило на практике освоить принципы 
построения языковых процессоров в парадигме функционального программирования. 