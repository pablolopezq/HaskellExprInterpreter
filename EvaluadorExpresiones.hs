import Data.Stack
import Data.Bool
import Data.List
import Data.Char

caracteres = ['+', '-', '*', '/', '(', ')', '%', '^']
operadores = ['+', '-', '*', '/', '%', '^']
priority = [('+', 6),('-', 6),('*', 7),('/', 7),('%', 7),('^', 8), ('(', 9)]

isValidChar :: Char -> Bool
isValidChar c = isOperator c || isNum c || c == ' ' || c `elem` caracteres

isOperator :: Char -> Bool
isOperator c = c `elem` operadores

isChar c = c `elem` operadores

isNum :: Char -> Bool
isNum c = c `elem` ['0' .. '9']

isNumberP :: String -> Bool
isNumberP ""  = False
isNumberP "." = False
isNumberP xs  =
  case dropWhile isDigit xs of
    ""       -> True
    ('.':ys) -> all isDigit ys
    _        -> False

checkInvalidCharacters :: String -> Bool
checkInvalidCharacters s = do
    let elementos_lista = words s
    let cantidad_elementos = length elementos_lista - 1
    let expresion = reverse s
    let recorre p = (p < 0) || (isValidChar (expresion !! p) && recorre (p - 1))
    let salida = recorre cantidad_elementos
    salida

checkStartEnd :: String -> Bool
checkStartEnd s = do
    let n = length s
    not (isChar (head s) || isChar (last s))

checkValidExpression :: String -> Bool
checkValidExpression s = checkInvalidCharacters s && checkStartEnd s

checkContinuousOperators s = do
    let elementos_lista = words s
    let cantidad_elementos = length elementos_lista - 2
    let expresion = reverse s
    let recorre p = (p < 0) || (isOperator (expresion !! p) || isOperator (expresion !! (p + 1)) && recorre (p - 1))
    let salida = recorre cantidad_elementos
    salida

getPrecedence c
    | c == "+" = 6
    | c == "-" = 6
    | c == "*" = 7
    | c == "/" = 7
    | c == "%" = 7
    | c == "^" = 8
    | c == "(" = 9

checkPrecedenceAux (firstP, secondP)
    | firstP == secondP = "="
    | firstP < secondP = "<"
    | firstP > secondP = ">"

checkPrecedence (first, second) =   do
                                    if first == ")" then do
                                        first
                                    else do
                                        let firstP = getPrecedence first
                                        let secondP = getPrecedence second
                                        checkPrecedenceAux (firstP, secondP)

addLessPrecedence (c, stack, output) = do
    if (not (null stack)) then do
        let prec = checkPrecedence (c, head stack)
        if(prec == "<") then do
            let first = head stack
            let newOutput = first : output
            let newStack = tail stack
            addLessPrecedence (c, newStack, newOutput)
        else do
            let newStack = c : stack
            (newStack, output)
    else do
        let newStack = c : stack
        (newStack, output)

addRightParen (stack, output) = do
    if(not (null stack)) then do
        if(head stack == "(") then do
            (tail stack, output)
        else do
            let t = head stack
            let newOutput = t : output
            let newStack = tail stack
            addRightParen (newStack, newOutput)
    else (stack, output)

addOperator (c, stack, output, precedence)
    | precedence == "=" = do
                let stackHead = head stack
                let stackTail = tail stack
                let newStack = c : stackTail
                let newOutput = stackHead : output
                (newStack, newOutput)
    | precedence == ">" = do
                let newStack = c : stack
                (newStack, output)
    | precedence == "<" = addLessPrecedence (c, stack, output)
    | precedence == ")" = addRightParen (stack, output)


--addOperatorToStack :: String -> [String] -> [String] -> ([String], [String])
addOperatorToStack (c, stack, output) = do
                                            if (not (null stack)) then do
                                                let pop = head stack
                                                if(pop == "(") then
                                                    addOperator (c, stack, output, ">")
                                                else do
                                                let precedence = checkPrecedence (c, pop)
                                                addOperator (c, stack, output, precedence)
                                            else
                                                addOperator (c, stack, output, ">")

--addToken :: String -> [String] -> [String] -> ([String], [String])
addToken (c, stack, output) =   if isNumberP c then do
                                    let out = c : output
                                    (stack, out)
                                else do
                                    let stk = fst (addOperatorToStack (c, stack, output))
                                    let out1 = snd (addOperatorToStack (c, stack, output))
                                    (stk, out1)

--toPostFix :: String -> String
toPostFix s = do
    let elementos_lista = words s
    let cantidad_elementos = length elementos_lista - 1
    let expresion = reverse elementos_lista
    let stack = []
    let output = []
    let recorre (p, stack, output) =    if p >= 0 then do 
                                            let temp = addToken (expresion !! p, stack, output)
                                            --putStrLn (expresion !! p)
                                            recorre (p - 1, fst temp, snd temp) 
                                        else reverse output ++ stack
    let salida = recorre (cantidad_elementos, stack, output)
    salida

evalAux (op, n1, n2)
    | op == "+" = n1 + n2
    | op == "-" = n1 - n2
    | op == "*" = n1 * n2
    | op == "/" = n2 `div` n1
    | op == "%" = n2 `mod` n1
    | op == "^" = n2 ^ n1

eval :: (String, String, String) -> String
eval (op, first, second) = do
    let n1 = read first :: Int
    let n2 = read second :: Int
    show (evalAux(op, n1, n2))

evaluate exp = do
    let stack = []
    evaluate2 (exp, stack)

evaluate2 (exp, stack) = do
    if(not (null exp)) then do
        let x = head exp
        if(isNumberP (x)) then do
            let newStack = x : stack
            let newExp = tail exp
            evaluate2 (newExp, newStack)
        else do
            let n1 = stack !! 0
            let n2 = stack !! 1
            let res = eval (x, n1, n2)
            let tempStack = drop 2 stack
            let newStack = res : tempStack
            let newExp = tail exp
            evaluate2 (newExp, newStack)
    else do
        head stack

evaluateExpression expresion = do
    let post = toPostFix expresion
    let res = evaluate post
    putStrLn res

main = do
    putStrLn "Ingrese la expresion"
    expresion <- getLine
    if (checkValidExpression expresion) then
        evaluateExpression expresion
    else
        putStrLn "Invalid Expresion"