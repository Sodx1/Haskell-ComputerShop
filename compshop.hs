{-# LANGUAGE OverloadedStrings #-}

import System.IO
import Text.ParserCombinators.Parsec
  ( anyChar,
    char,
    digit,
    string,
    manyTill,
    optional,
    many,
    parse,
    try,
    ParseError,
    GenParser
  )
import Control.Applicative ((<$>), (<*))
import Data.List (nub)
import Text.Read (readMaybe)

-- Структура компонента
data Component = Component
  { componentName :: String,
    componentPrice :: Int,
    componentType :: String,
    componentSpec :: String,
    componentDescription :: String
  }
  deriving (Show, Read)

-- Парсер для компонентов
componentParser :: GenParser Char () Component
componentParser = do
  string "Component{componentName = \""
  name <- manyTill anyChar (try $ string "\", ")
  string "componentPrice="
  price <- read <$> manyTill digit (try $ string ", ")
  string "componentType=\""
  componentType <- manyTill anyChar (try $ string "\", ")
  string "componentSpec=\""
  spec <- manyTill anyChar (try $ string "\", ")
  string "componentDescription=\""
  description <- manyTill anyChar (try $ string "\"")
  char '}'
  return (Component name price componentType spec description)

-- Парсер для всего файла
fileParser :: GenParser Char () [Component]
fileParser = many (componentParser <* optional (char '\n'))

-- Чтение файла и парсинг данных
parseFile :: FilePath -> IO (Either ParseError [Component])
parseFile filename = do
  contents <- readFile filename
  return (parse fileParser filename contents)

-- Вывод доступных компонентов
printAvailableComponents :: [Component] -> IO ()
printAvailableComponents components = do
  putStrLn "Available components:"
  mapM_ (\comp -> putStrLn $ componentName comp ++ " (" ++ componentType comp ++ ")") components

-- Вывод информации о выбранном компоненте
printComponentInfo :: Component -> IO ()
printComponentInfo comp = do
  putStrLn ("\nComponent: " ++ componentName comp)
  putStrLn ("Price: " ++ show (componentPrice comp) ++ "$")
  putStrLn ("Type: " ++ componentType comp)
  putStrLn ("Specs: " ++ componentSpec comp)
  putStrLn ("Description: \n" ++ componentDescription comp)

-- Получение всех компонентов определенного типа
getComponentsByType :: [Component] -> String -> [Component]
getComponentsByType components compType =
  filter (\comp -> componentType comp == compType) components

-- Загрузка спецификаций из компонентов
loadSpecificationsFromComponents :: FilePath -> IO [String]
loadSpecificationsFromComponents filename = do
  result <- parseFile filename
  case result of
    Left err -> do
      putStrLn ("Parsing error: " ++ show err)
      return []
    Right components -> do
      let specs = nub $ map componentSpec components
      return specs

-- Выбор компонентов пользователем
chooseComponentSpec :: [String] -> IO String
chooseComponentSpec specs = do
  putStrLn "Choose the component specification:"
  printNumberedOptions specs
  choice <- getLine
  case readMaybe choice of
    Just index ->
      if index >= 1 && index <= length specs
        then return (specs !! (index - 1))
        else retry
    _ -> retry
  where
    retry = do
      putStrLn "Invalid choice. Try again."
      chooseComponentSpec specs

-- Функция для печати пронумерованных опций
printNumberedOptions :: [String] -> IO ()
printNumberedOptions options = do
  mapM_ (\(index, option) -> putStrLn (show index ++ ". " ++ option)) (zip [1 ..] options)

-- Основная функция сборки компьютера
assembleComputer :: Component -> IO ()
assembleComputer comp = do
  putStrLn "Assembling the computer..."
  putStrLn "Selected component:"
  printComponentInfo comp
  putStrLn $ "Total cost: " ++ show (componentPrice comp) ++ "$"
  putStrLn "Thank you for your purchase!"

-- Опция сборки компьютера
assembleComputerOption :: IO ()
assembleComputerOption = do
  specs <- loadSpecificationsFromComponents "components.txt"
  result <- parseFile "components.txt"
  case result of
    Left err -> putStrLn ("Parsing error: " ++ show err)
    Right components -> do
      componentType <- chooseComponentType
      let filteredComponents = getComponentsByType components componentType
      if null filteredComponents
        then putStrLn "No components found for the specified type"
        else do
          componentSpecChoice <- chooseComponentSpec (nub $ map componentSpec filteredComponents)
          let selectedComponents = filter (\comp -> componentSpecChoice == componentSpec comp) filteredComponents
          if null selectedComponents
            then putStrLn "No components found for the specified specification"
            else do
              putStrLn "Available components for the chosen specification:"
              mapM_ printComponentInfo selectedComponents
              putStrLn "Do you want to buy a component? (y/n)"
              choice <- getLine
              case choice of
                "y" -> do
                  putStrLn "Enter the number of the component you want to buy:"
                  buyChoice <- getLine
                  case readMaybe buyChoice of
                    Just index ->
                      if index >= 1 && index <= length selectedComponents
                        then assembleComputer (selectedComponents !! (index - 1))
                        else putStrLn "Invalid choice. Purchase canceled."
                    _ -> putStrLn "Invalid choice. Purchase canceled."
                _ -> putStrLn "Purchase canceled."
      main



-- Выбор типа компонента
chooseComponentType :: IO String
chooseComponentType = do
  putStrLn "Enter the component type you want to add to your computer (Processor, Graphics Card, Motherboard, RAM, Hard Drive, Power Supply):"
  getLine

-- Основная функция программы
main :: IO ()
main = do
  putStrLn "Welcome To Computer Shop!"
  putStrLn "What do you want to do?"
  putStrLn "1. View the list of components"
  putStrLn "2. Assemble the computer"
  putStrLn "3. Exit"

  choice <- getLine
  case choice of
    "1" -> do
      result <- parseFile "components.txt"
      case result of
        Left err -> putStrLn ("Parsing error: " ++ show err)
        Right components -> do
          putStrLn "Enter the component type you want to view (Processor, Graphics Card, Motherboard, RAM, Hard Drive, Power Supply):"
          componentType <- getLine
          let filteredComponents = getComponentsByType components componentType
          if null filteredComponents
            then putStrLn "No components found for the specified type"
            else mapM_ printComponentInfo filteredComponents
          main

    "2" -> assembleComputerOption

    "3" -> putStrLn "Goodbye, dude!"

    _ -> do
      putStrLn "Wrong choice. Try again."
      main
