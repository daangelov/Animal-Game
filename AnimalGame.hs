module AnimalGame where

import System.Directory (doesFileExist)
import Text.Read        (readMaybe)

import Data.Char        (isAlphaNum, isSpace)

data AnimalTree = Animal String
                | Question String AnimalTree AnimalTree
    deriving (Show, Read)

-- Return database name
db :: String
db = "animals.bg.db"

-- Takes AnimalTree and inserts it in the database
insertInDb :: AnimalTree -> IO ()
insertInDb = writeFile db . show

-- Inserts a new Question in an AnimalTree by given list of answers
insertInTree :: AnimalTree -> [String] -> AnimalTree -> AnimalTree
insertInTree (Animal _)       _              nq = nq
insertInTree (Question q y n) ("Да":others) nq = Question q (insertInTree y others nq) n
insertInTree (Question q y n) ("Не":others)  nq = Question q y (insertInTree n others nq)
insertInTree (Question _ _ _) _              _ = error "Невалиден списък от отговори!"

-- Prompts user to answer questions and saves answers in a new Question
createNewQuestion :: String -> IO AnimalTree
createNewQuestion failedAnimal = do
    putStrLn "Кое животно си беше намислил?"
    newAnimal <- getLineAlphaNum
    putStrLn "Въведи въпрос, който:"
    putStrLn $ " - отговаря с \"Да\" за " ++ newAnimal
    putStrLn $ " - отговаря с \"Не\" за " ++ failedAnimal
    newQuestion <- getLineAlphaNum
    let nq = Question newQuestion (Animal newAnimal) (Animal failedAnimal)
    return nq

-- Takes a tree of animals and starts the questioning
play :: AnimalTree -> IO ()
play animalTree = play' animalTree []
    where play' :: AnimalTree -> [String] -> IO ()
          play' (Animal a) answers = do
              ask $ a ++ " ли е твоето животно?"
              ans <- answer
              if (ans == "Да")
                 then putStrLn "Печеля!" -- >> play animalTree -- Uncomment for automatic restart
                 else do
                    putStrLn "Предавам се!"
                    putStrLn "Моля, помогни ми да се подобря!"

                    nq <- createNewQuestion a
                    let newTree = insertInTree animalTree answers nq
                    insertInDb newTree
                    putStrLn "Запазих, благродая!"
                    -- play newTree -- Uncomment for automatic restart

          play' (Question q y n) answers = do
              ask q
              ans <- answer
              if (ans == "Да")
                 then play' y $ answers ++ [ans]
                 else play' n $ answers ++ [ans]

-- Prints question and returns answer
ask :: String -> IO ()
ask s = putStrLn $ s ++ " (Да/Не)"

-- Validate answer and return it
answer :: IO String
answer = do ans <- getLineAlphaNum
            case filter isAlphaNum ans of
                 "Да" -> return "Да"
                 "Не"  -> return "Не"
                 _     -> putStrLn "Невалиден отговор. Моля отговорете с Да или Не" >> answer

getLineAlphaNum :: IO String
getLineAlphaNum = do
    str <- getLine
    return $ filter (\c -> isAlphaNum c || isSpace c) str

loadDb :: IO (Maybe AnimalTree)
loadDb = do
    fileExist <- doesFileExist db
    when' (not fileExist) (writeFile db "")
    fmap readMaybe $ readFile db

-- This function start the game
main :: IO ()
main = do
    animalTree <- loadDb
    putStrLn "Базата от данни е заредена!"
    case animalTree of
         Nothing -> play (Animal "Куче") -- default animal
         Just t  -> putStrLn "Намисли си животно, аз ще се опитам да го отгатна!" >>
                    play t

when' :: Bool -> IO () -> IO ()
when' b m = if b
            then m
            else pure ()
