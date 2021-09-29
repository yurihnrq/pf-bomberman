type Mochila = ((Item, Int), (Item, Int), (Item, Int))
data Direcao = N | S | L | O deriving (Show, Eq, Enum, Ord)
type Coordenada = (Int, Int)

type Jogador = (String,Coordenada,Direcao,Mochila)

-- Funções relativas ao Jogador:
-- Retorna ID do Jogador
getID :: Jogador -> String
getID (id,_,_,_) = id

-- Retorna Coordenada do Jogador
getCoord :: Jogador -> Coordenada
getCoord (_,c,_,_) = c

-- Retorna Direcao do Jogador
getDir :: Jogador -> Direcao
getDir (_,_,d,_) = d

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

-- Define novo valor de Direcao para o Jogador
setDir :: Jogador -> Direcao -> Jogador
setDir (id,c,_,m) dir = (id,c,dir,m)

-- Jogadores iniciais.
j1 :: Jogador
j1 = ("Jogador_1", (1,1), S, ((Patins, 0),(Arremesso, 0),(Bomba, 2)))

j2 :: Jogador
j2 = ("Jogador_2", (6,5), S, ((Patins, 0),(Arremesso, 0),(Bomba, 2)))

j3 :: Jogador
j3 = ("Jogador_3", (8,3), S, ((Patins, 0),(Arremesso, 0),(Bomba, 2)))

data Item = Grama | Patins | Arremesso | Bomba | Jogador Jogador deriving (Show, Eq, Ord)
type Celula = [Item] 
type Linha = (Celula, Celula, Celula, Celula, Celula, Celula, Celula, Celula)
type Tabuleiro = (Linha, Linha, Linha, Linha, Linha, Linha, Linha, Linha)

-- Funções relativas ao tabuleiro:
table :: Tabuleiro
table = (([Grama, Jogador j1],[Grama],[Grama],[Grama],[],[Grama],[Grama],[Grama]),
         ([Grama],[Grama],[Grama],[Grama],[],[Grama],[Grama],[Grama]),
         ([Grama],[Grama],[Grama],[Grama,Patins],[],[],[],[]),
         ([Grama],[Grama],[Grama],[Grama],[],[],[Grama],[Grama]),
         ([],[Grama,Arremesso],[Grama],[Grama],[Grama],[],[Grama],[Grama]),
         ([],[],[Grama],[Grama],[Grama,Jogador j2],[Grama],[],[]),
         ([],[],[Grama,Arremesso],[Grama],[Grama],[Grama],[],[]),
         ([Grama],[Grama],[Grama,Jogador j3],[Grama],[Grama],[Grama],[Grama,Patins],[Grama]))

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
setPos t c (x,y) = setLinha table x (setCelula (getLinha t x) y c)

-- Obtem dados de um Item contruído com o construtor Jogador.
itemJogador :: Item -> Jogador
itemJogador (Jogador x) = x

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
    | otherwise = h:(removeJogador t)

-- Insere um Jogador em uma Celula
insertJogador :: Celula -> Jogador -> Celula
insertJogador [] _ = []
insertJogador l j
    | hr == Arremesso = reverse ((Jogador (incItem j Arremesso)):tr)
    | hr == Patins = reverse (Jogador (incItem j Patins):tr)
    | hr == Grama = hr:([Jogador j])
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
    | d == N && celulaValida (getPos t (x-1,y)) = setPos table' celDNorte ((x-1),y)
    | d == S && celulaValida (getPos t (x+1,y)) = setPos table' celDSul ((x+1),y)
    | d == L && celulaValida (getPos t (x,y+1)) = setPos table' celDLeste (x,(y+1))
    | d == O && celulaValida (getPos t (x,y-1)) = setPos table' celDOeste (x,(y-1))
    | otherwise = error "Parâmetros inválidos"
    where 
        jog  = getJogador (getPos t c)
        celO = removeJogador (getPos t c)
        celDNorte = insertJogador (getPos t (x-1,y)) jog
        celDSul   = insertJogador (getPos t (x+1,y)) jog
        celDLeste = insertJogador (getPos t (x,y+1)) jog
        celDOeste = insertJogador (getPos t (x,y-1)) jog
        table' = setPos t celO c

