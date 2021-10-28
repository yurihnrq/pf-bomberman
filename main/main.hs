-- Construção do Jogador
type Mochila = ((Item, Int), (Item, Int), (Item, Int))
data Direcao = N | S | L | O deriving (Show, Eq, Enum, Ord, Read)
type Coordenada = (Int, Int)
type ID = String
type Jogador = (ID,Coordenada,Direcao,Mochila)

-- Funções relativas ao Jogador:
-- Retorna ID do Jogador
getID :: Jogador -> ID
getID (id,_,_,_) = id

-- Retorna Coordenada do Jogador
getCoord :: Jogador -> Coordenada
getCoord (_,c,_,_) = c

-- Define um novo valor de Coordenada para o Jogador.
setCoord :: Jogador -> Coordenada -> Jogador
setCoord (id,_,d,m) c = (id,c,d,m)

-- Retorna Direcao do Jogador
getDir :: Jogador -> Direcao
getDir (_,_,d,_) = d

-- Define novo valor de Direcao para o Jogador
setDir :: Jogador -> Direcao -> Jogador
setDir (id,c,_,m) dir = (id,c,dir,m)

-- Retorna Mochila do Jogador
getBag :: Jogador -> Mochila
getBag (_,_,_,b) = b

-- Define novo valor de Mochila para o Jogador
setBag :: Jogador -> Mochila -> Jogador
setBag (id,c,d,_) m = (id,c,d,m)

-- Incrementa a quantidade de um Item na Mochila
incItem :: Jogador -> Item -> Jogador
incItem j i
    | i == Patins = setBag j ((i1,q1+1),(i2,q2),(i3,q3))
    | i == Arremesso = setBag j ((i1,q1),(i2,q2+1),(i3,q3))
    | otherwise = error "Valor inválido"
    where ((i1,q1),(i2,q2),(i3,q3)) = getBag j

-- Jogadores iniciais.
j1 :: Jogador
j1 = ("Jogador 1", (1,1), S, ((Patins, 0),(Arremesso, 0),(Bomba, 2)))

j2 :: Jogador
j2 = ("Jogador 2", (1,8), S, ((Patins, 0),(Arremesso, 0),(Bomba, 2)))

j3 :: Jogador
j3 = ("Jogador 3", (8,1), S, ((Patins, 0),(Arremesso, 0),(Bomba, 2)))

j4 :: Jogador
j4 = ("Jogador 4", (8,8), S, ((Patins, 0),(Arremesso, 0),(Bomba, 2)))

-- Construção do Tabuleiro
data Item = Grama | Patins | Arremesso | Bomba | Jogador Jogador deriving (Show, Eq, Ord)
type Celula = [Item]
type Linha = (Celula, Celula, Celula, Celula, Celula, Celula, Celula, Celula)
type Tabuleiro = (Linha, Linha, Linha, Linha, Linha, Linha, Linha, Linha)

-- Tabuleiro Inicial
table1 :: Tabuleiro
table1 = (([Grama, Jogador j1],[Grama],[Grama],[Grama],[],[Grama],[Grama],[Grama,Jogador j2]),
         ([Grama, Arremesso],[Grama],[Grama],[Grama],[],[Grama],[Grama],[Grama]),
         ([Grama],[Grama],[Grama],[Grama,Patins],[],[],[],[]),
         ([Grama],[Grama],[Grama],[Grama],[],[],[Grama],[Grama]),
         ([],[Grama,Arremesso],[Grama],[Grama],[Grama],[],[Grama],[Grama]),
         ([],[],[Grama],[Grama],[Grama],[Grama],[],[]),
         ([],[],[Grama,Arremesso],[Grama],[Grama],[Grama],[],[]),
         ([Grama,Jogador j3],[Grama],[Grama],[Grama],[Grama],[Grama],[Grama,Patins],[Grama,Jogador j4]))

-- Funções relativas ao tabuleiro:
-- Obter valor de uma Linha.
getLinha :: Tabuleiro -> Int -> Linha
getLinha (l,_,_,_,_,_,_,_) 1 = l
getLinha (_,l,_,_,_,_,_,_) 2 = l
getLinha (_,_,l,_,_,_,_,_) 3 = l
getLinha (_,_,_,l,_,_,_,_) 4 = l
getLinha (_,_,_,_,l,_,_,_) 5 = l
getLinha (_,_,_,_,_,l,_,_) 6 = l
getLinha (_,_,_,_,_,_,l,_) 7 = l
getLinha (_,_,_,_,_,_,_,l) 8 = l
getLinha _ _ = error "Parâmetros inválidos"

-- Obtem valor de uma Celula.
getCelula :: Linha -> Int -> Celula
getCelula (c,_,_,_,_,_,_,_) 1 = c
getCelula (_,c,_,_,_,_,_,_) 2 = c
getCelula (_,_,c,_,_,_,_,_) 3 = c
getCelula (_,_,_,c,_,_,_,_) 4 = c
getCelula (_,_,_,_,c,_,_,_) 5 = c
getCelula (_,_,_,_,_,c,_,_) 6 = c
getCelula (_,_,_,_,_,_,c,_) 7 = c
getCelula (_,_,_,_,_,_,_,c) 8 = c
getCelula _ _ = error "Parâmetros inválidos"

-- Define um novo valor para uma Linha do Tabuleiro.
setLinha :: Tabuleiro -> Int -> Linha -> Tabuleiro
setLinha (_,l2,l3,l4,l5,l6,l7,l8) 1 l = (l,l2,l3,l4,l5,l6,l7,l8)
setLinha (l1,_,l3,l4,l5,l6,l7,l8) 2 l = (l1,l,l3,l4,l5,l6,l7,l8)
setLinha (l1,l2,_,l4,l5,l6,l7,l8) 3 l = (l1,l2,l,l4,l5,l6,l7,l8)
setLinha (l1,l2,l3,_,l5,l6,l7,l8) 4 l = (l1,l2,l3,l,l5,l6,l7,l8)
setLinha (l1,l2,l3,l4,_,l6,l7,l8) 5 l = (l1,l2,l3,l4,l,l6,l7,l8)
setLinha (l1,l2,l3,l4,l5,_,l7,l8) 6 l = (l1,l2,l3,l4,l5,l,l7,l8)
setLinha (l1,l2,l3,l4,l5,l6,_,l8) 7 l = (l1,l2,l3,l4,l5,l6,l,l8)
setLinha (l1,l2,l3,l4,l5,l6,l7,_) 8 l = (l1,l2,l3,l4,l5,l6,l7,l)
setLinha _ _ _ = error "Parâmetros inválidos"

-- Define um novo valor para uma Celula de uma Linha.
setCelula :: Linha -> Int -> Celula -> Linha
setCelula (_,c2,c3,c4,c5,c6,c7,c8) 1 c = (c,c2,c3,c4,c5,c6,c7,c8)
setCelula (c1,_,c3,c4,c5,c6,c7,c8) 2 c = (c1,c,c3,c4,c5,c6,c7,c8)
setCelula (c1,c2,_,c4,c5,c6,c7,c8) 3 c = (c1,c2,c,c4,c5,c6,c7,c8)
setCelula (c1,c2,c3,_,c5,c6,c7,c8) 4 c = (c1,c2,c3,c,c5,c6,c7,c8)
setCelula (c1,c2,c3,c4,_,c6,c7,c8) 5 c = (c1,c2,c3,c4,c,c6,c7,c8)
setCelula (c1,c2,c3,c4,c5,_,c7,c8) 6 c = (c1,c2,c3,c4,c5,c,c7,c8)
setCelula (c1,c2,c3,c4,c5,c6,_,c8) 7 c = (c1,c2,c3,c4,c5,c6,c,c8)
setCelula (c1,c2,c3,c4,c5,c6,c7,_) 8 c = (c1,c2,c3,c4,c5,c6,c7,c)
setCelula _ _ _ = error "Parâmetros inválidos"

-- Retorna o valor armazenado em uma Coordenada.
getPos :: Tabuleiro -> Coordenada -> Celula
getPos t (x,y) = getCelula (getLinha t x) y

-- Define um novo valor para para uma Coordenada do Tabuleiro.
setPos :: Tabuleiro -> Celula -> Coordenada -> Tabuleiro
setPos t c (x,y) = setLinha t x (setCelula (getLinha t x) y c)

-- Obtem dados de um Item contruído com o construtor Jogador.
itemJogador :: Item -> Jogador
itemJogador (Jogador x) = x
itemJogador _ = error "Não é jogador"

-- Verificar se um Item é do tipo Jogador.
checkJogador :: Item -> Bool
checkJogador (Jogador _) = True
checkJogador _ = False

-- Percorre uma Celula ([Item]) em busca de um Jogador.
getJogador :: Celula -> Jogador
getJogador [] = error "Não há jogador nesta celula"
getJogador (h:t)
    | checkJogador h = itemJogador h
    | otherwise = getJogador t

-- Verifica se uma Celula é válida para receber um Jogador.
celulaValida :: Celula -> Bool
celulaValida [] = True
celulaValida (h:t)
    | h > Arremesso = False
    | otherwise = celulaValida t

-- Remove um Jogador de uma Celula.
removeJogador :: Celula -> Celula
removeJogador [] = []
removeJogador (h:t)
    | checkJogador h = t
    | otherwise = h:removeJogador t

-- Insere um Jogador em uma Celula.
-- Se a Celula possui um Item, ele é armazenado na Mochila do Jogador.
insertJogador :: Celula -> Jogador -> Celula
insertJogador [] _ = []
insertJogador l j
    | hr == Arremesso = reverse (Jogador (incItem j Arremesso):tr)
    | hr == Patins = reverse (Jogador (incItem j Patins):tr)
    | hr == Grama = hr:[Jogador j]
    | otherwise = error "Celula não é válida"
    where rl = reverse l
          hr = head rl
          tr = tail rl

-- Move um Jogador em Coordenada coordenada do Tabuleiro na Direcao informada.
-- Aqui x representa Linhas e y colunas.
-- Quanto menor o valor de x, mais ao Norte.
-- Quanto menor o valor de y, mais ao Oeste.
moveJogador :: Tabuleiro -> Coordenada -> Direcao -> Tabuleiro
moveJogador t c@(x,y) d
    | d == N && celulaValida (getPos t (x-1,y)) = setPos table' celDNorte (x-1,y)
    | d == S && celulaValida (getPos t (x+1,y)) = setPos table' celDSul (x+1,y)
    | d == L && celulaValida (getPos t (x,y+1)) = setPos table' celDLeste (x,y+1)
    | d == O && celulaValida (getPos t (x,y-1)) = setPos table' celDOeste (x,y-1)
    | otherwise = error "Parâmetros inválidos"
    where
        jog  = setDir (getJogador (getPos t c)) d -- Modifica a Direcao para qual o Jogador olha para a Direcao do movimento.
        celO = removeJogador (getPos t c) -- Obtém uma versão da Celula de origem sem o Jogador.
        celDSul   = insertJogador (getPos t (x+1,y)) (setCoord jog (x+1,y)) -- Celula de destino com o Jogador que andou na Direcao S (Sul).
        celDNorte = insertJogador (getPos t (x-1,y)) (setCoord jog (x-1,y)) -- Celula de destino com o Jogador que andou na Direcao N (Norte).
        celDLeste = insertJogador (getPos t (x,y+1)) (setCoord jog (x,y+1)) -- Celula de destino com o Jogador que andou na Direcao L (Leste).
        celDOeste = insertJogador (getPos t (x,y-1)) (setCoord jog (x,y-1)) -- Celula de destino com o Jogador que andou na Direcao O (Oeste).
        table' = setPos t celO c -- Atualiza a Celula de origem no Tabuleiro.

-- Coloca uma Bomba na frente do Jogador que está na Coordenada informada.
setBomba :: Tabuleiro -> Coordenada -> Tabuleiro
setBomba t c@(x,y)
    | last celD < Patins = setPos t celR (coordDir c dirJ)
    | otherwise = error "Não é possível colocar bomba nesse local"
    where
        dirJ = getDir (getJogador (getPos t c)) -- Obtem a Direcao do Jogador na Coordenada informada.
        celD = getPos t (coordDir c dirJ) -- Obtem a Celula onde a Bomba deve ser colocada.
        celR = reverse (Bomba:celD) -- Coloca a Bomba na Celula de destino.
        coordDir (x,y) d -- Função para obter a Coordenada em que a Bomba deve ser colocada baseada na Direcao para qual o Jogador está olhando.
            | d == N = (x-1,y)
            | d == S = (x+1,y)
            | d == L = (x,y+1)
            | d == O = (x,y-1)
            | otherwise = error "Direção inválida"

-- Explode a Bomba que se encontra na Coordenada informada.
-- Foi considerado um raio de explosão de 2 casas. Essa é a capacidade da Bomba na Mochila dos Jogadores. 
-- Não há item que incremente o raio de explosão da Bomba.
-- Para a implementação de um Item que incremente o raio de explosão, seria necessário criar um construtor Bomba Int em data Item.
explodeBomba :: Tabuleiro -> Coordenada -> Tabuleiro
explodeBomba t c
    | last cel == Bomba = explode t c 0
    | otherwise = error "Não há bomba nessa Coordenada"
    where
        cel = getPos t c -- Obtem a Celula que contém a Bomba.
        coordValida (x',y') -- Função que verifica se uma Coordenada (x,y) pertence ao Tabuleiro de tamanho 8x8.
            | x' > 0 && x' < 9 && y' > 0 && y' < 9 = True
            |otherwise = False
        explode table c'@(x,y) i -- Função que altera o Tabuleiro conforme necessário para realizar o "estrago" da explosão.
            | i > 9 = table -- Condição para encerrar a função. Os 9 passos necessários já foram realizados.
            | coordValida (x+1,y) && not (null (getPos table (x+1,y))) && i == 0 = explode (setPos table [Grama] (x+1,y)) c' (i+1)
            | coordValida (x+2,y) && not (null (getPos table (x+2,y))) && i == 1 = explode (setPos table [Grama] (x+2,y)) c' (i+1)
            | coordValida (x,y+1) && not (null (getPos table (x,y+1))) && i == 2 = explode (setPos table [Grama] (x,y+1)) c' (i+1)
            | coordValida (x,y+2) && not (null (getPos table (x,y+2))) && i == 3 = explode (setPos table [Grama] (x,y+2)) c' (i+1)
            | coordValida (x-1,y) && not (null (getPos table (x-1,y))) && i == 4 = explode (setPos table [Grama] (x-1,y)) c' (i+1)
            | coordValida (x-2,y) && not (null (getPos table (x-2,y))) && i == 5 = explode (setPos table [Grama] (x-2,y)) c' (i+1)
            | coordValida (x,y-1) && not (null (getPos table (x,y-1))) && i == 6 = explode (setPos table [Grama] (x,y-1)) c' (i+1)
            | coordValida (x,y-2) && not (null (getPos table (x,y-2))) && i == 7 = explode (setPos table [Grama] (x,y-2)) c' (i+1)
            | coordValida c' && not (null (getPos table c')) && i == 8 = explode (setPos table [Grama] c') c' (i+1)
            | otherwise = explode table c' (i+1) -- Caso uma condição acima não seja satisfeita, ainda é necessário percorrer as Coordenadas restantes, por isso a função não encerra aqui.

main :: IO ()
main = do
    putStr "\n"
    printTabuleiro table1 1
    loop table1

loop :: Tabuleiro -> IO ()
loop table = do
    putStr "-- Bomberman --\n"
    putStr "M - Mover jogador\n"
    putStr "C - Colocar bomba\n"
    putStr "E - Expodir bomba\n"
    putStr "!sair - Sair do jogo\n"
    putStr ">> "
    op <- getLine
    if op == "!sair"
        then putStr "Você saiu do jogo.\n"
    else if op == "M"
        then moveJogadorIO table
    else if op == "C"
        then setBombaIO table
    else if op == "E"
        then explodeBombaIO table
    else do
        putStr "Opção inválida.\n"
        loop table

moveJogadorIO :: Tabuleiro -> IO ()
moveJogadorIO table = do
    putStr "Insira a coordenada do jogador\n"
    putStr "x: "
    x <- getLine 
    putStr "y: "
    y <- getLine
    putStr "Insira a direção para qual ele deve andar (N,S,L,O): "
    dir <- getLine
    putStr "\n"
    printTabuleiro (moveJogador table (read x::Int, read y::Int) (read dir::Direcao)) 1
    loop $ moveJogador table (read x::Int, read y::Int) (read dir::Direcao)

setBombaIO :: Tabuleiro -> IO ()
setBombaIO table = do
    putStr "Insira a coordenada do jogador\n"
    putStr "x: "
    x <- getLine 
    putStr "y: "
    y <- getLine
    putStr "\n"
    printTabuleiro (setBomba table (read x::Int, read y::Int)) 1
    loop $ setBomba table (read x::Int, read y::Int)

explodeBombaIO :: Tabuleiro -> IO ()
explodeBombaIO table = do
    putStr "Insira a coordenada da bomba\n"
    putStr "x: "
    x <- getLine 
    putStr "y: "
    y <- getLine
    putStr "\n"
    printTabuleiro (explodeBomba table (read x::Int, read y::Int)) 1
    loop $ explodeBomba table (read x::Int, read y::Int)

printCelula :: Celula -> Int -> IO ()
printCelula cel 8
    | null cel = putStr "( )\n"
    | checkJogador $ last cel = putStr $ getID (getJogador cel) ++ "\n"
    | otherwise = putStr $ show (last cel) ++ "\n"
printCelula cel _ 
    | null cel = putStr "( ) | "
    | checkJogador $ last cel = putStr $ getID (getJogador cel) ++ " | "
    | otherwise = putStr $ show (last cel) ++ " | "

printLinha' :: Linha -> Int -> IO ()
printLinha' (c,_,_,_,_,_,_,_) 1 = printCelula c 1
printLinha' (_,c,_,_,_,_,_,_) 2 = printCelula c 2
printLinha' (_,_,c,_,_,_,_,_) 3 = printCelula c 3
printLinha' (_,_,_,c,_,_,_,_) 4 = printCelula c 4
printLinha' (_,_,_,_,c,_,_,_) 5 = printCelula c 5
printLinha' (_,_,_,_,_,c,_,_) 6 = printCelula c 6
printLinha' (_,_,_,_,_,_,c,_) 7 = printCelula c 7
printLinha' (_,_,_,_,_,_,_,c) 8 = printCelula c 8
printLinha' _ _ = error "Parâmetros inválidos"

printLinha :: Linha -> Int -> IO ()
printLinha l i = do
    if i >= 1 && i <= 8
        then do
            printLinha' l i
            printLinha l (i+1)
    else
        putStr ""

printTabuleiro' :: Tabuleiro -> Int -> IO ()
printTabuleiro' (l,_,_,_,_,_,_,_) 1 = printLinha l 1
printTabuleiro' (_,l,_,_,_,_,_,_) 2 = printLinha l 1
printTabuleiro' (_,_,l,_,_,_,_,_) 3 = printLinha l 1
printTabuleiro' (_,_,_,l,_,_,_,_) 4 = printLinha l 1
printTabuleiro' (_,_,_,_,l,_,_,_) 5 = printLinha l 1
printTabuleiro' (_,_,_,_,_,l,_,_) 6 = printLinha l 1
printTabuleiro' (_,_,_,_,_,_,l,_) 7 = printLinha l 1
printTabuleiro' (_,_,_,_,_,_,_,l) 8 = printLinha l 1
printTabuleiro' _ _ = error "Parâmetros inválidos"

printTabuleiro :: Tabuleiro -> Int -> IO ()
printTabuleiro t i = do
    if i >= 1 && i <= 8
        then do
            printTabuleiro' t i
            printTabuleiro t (i+1)
    else
        putStr "\n"
