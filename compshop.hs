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

-- Структура заказа
data Order = Order
  { orderComponents :: [Component],
    orderTotalCost :: Int
  }
  deriving (Show)

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

-- Опция сборки компьютера
assembleComputerOption :: Order -> Int -> IO Order
assembleComputerOption order budget
  | budget <= 0 = do
    putStrLn "Invalid budget. Exiting. (-1 or 0)"
    return order
  | otherwise = do
    specs <- loadSpecificationsFromComponents "components.txt"
    result <- parseFile "components.txt"
    case result of
      Left err -> do
        putStrLn ("Parsing error: " ++ show err)
        return order
      Right components -> do
        componentType <- chooseComponentType
        if componentType == "0"
          then do
            putStrLn "Exiting assemble."
            return order
          else do
            let filteredComponents = getComponentsByType components componentType
            if null filteredComponents
              then do
                putStrLn "No components found for the specified type"
                return order
              else do
                componentSpecChoice <- chooseComponentSpec (nub $ map componentSpec filteredComponents)
                let selectedComponents = filter (\comp -> componentSpecChoice == componentSpec comp) filteredComponents
                if null selectedComponents
                  then do
                    putStrLn "No components found for the specified specification"
                    return order
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
                              then do
                                let selectedComponent = selectedComponents !! (index - 1)
                                if orderTotalCost order + componentPrice selectedComponent <= budget
                                  then do
                                    putStrLn $ "You add " ++ componentName selectedComponent ++ " for " ++ show (componentPrice selectedComponent) ++ "$"
                                    let newOrder = Order {orderComponents = selectedComponent : orderComponents order, orderTotalCost = orderTotalCost order + componentPrice selectedComponent}
                                    putStrLn $ "Total cost so far: " ++ show (orderTotalCost newOrder) ++ "$"
                                    assembleComputerOption newOrder budget
                                  else do
                                    putStrLn "Purchase canceled. Negative budget."
                                    return order
                              else do
                                putStrLn "Invalid choice. Purchase canceled."
                                return order
                          _ -> do
                            putStrLn "Invalid choice. Purchase canceled."
                            return order
                      _ -> do
                        putStrLn "Purchase canceled."
                        return order

-- Выбор типа компонента
chooseComponentType :: IO String
chooseComponentType = do
  putStrLn "Enter the component type you want to add to your computer (Processor, Graphics Card, Motherboard, RAM, Hard Drive, Power Supply) or press 0 to exit assemble:"
  getLine

-- Просмотр всего заказа
viewOrder :: Order -> IO ()
viewOrder order = do
  putStrLn "Your order:"
  mapM_ printComponentInfo (orderComponents order)

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

    "2" -> do
      putStrLn "Enter your budget:"
      budget <- read <$> getLine :: IO Int
      let initialOrder = Order {orderComponents = [], orderTotalCost = 0}
      finalOrder <- assembleComputerOption initialOrder budget
      viewOrder finalOrder
      putStrLn $ "Total cost of your order: " ++ show (orderTotalCost finalOrder) ++ "$"
      main

    "3" -> putStrLn "Goodbye, dude!"

    _ -> do
      putStrLn "Wrong choice. Try again."
      main
