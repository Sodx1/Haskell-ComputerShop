import System.IO
import Text.ParserCombinators.Parsec
import Control.Applicative ((<$>), (<*))
{-# LANGUAGE OverloadedStrings #-}

-- Структура компонента
data Component = Component
  { componentName :: String
  , componentPrice :: Int
  , componentType :: String
  , componentSpec :: String
  , componentDescription :: String
  } deriving (Show, Read)

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



---1 кнопка----------------------------------

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

--------------------------------------------------------------

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
                  printAvailableComponents components
                  putStrLn "Enter the component type you want to view:"
                  componentType <- getLine
                  let filteredComponents = getComponentsByType components componentType
                  if null filteredComponents
                    then putStrLn "No components found for the specified type"
                    else mapM_ printComponentInfo filteredComponents
                  main
                  
        "2" -> do
            putStrLn "Sorry, the function is not implemented\n"
            main
        "3" -> putStrLn "Goodbye, dude!"
        _ -> do
            putStrLn "Wrong choice. Try again."
            main