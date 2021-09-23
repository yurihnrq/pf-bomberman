type Mochila = [(Item, Int)]
data Direcao = N | S | L | O deriving (Show, Eq, Enum, Ord)
type Coordenada = (Int, Int)

type Jogador = (String,Coordenada,Direcao,Mochila)

data Item = Grama | Patins | Arremesso | Bomba | Jogador Jogador deriving (Show, Eq, Ord)
type Celula = [Item]
type Linha = (Celula, Celula, Celula, Celula, Celula, Celula, Celula, Celula)
type Tabuleiro = (Linha, Linha, Linha, Linha, Linha, Linha, Linha, Linha)


getID :: Jogador -> String
getID (id,_,_,_) = id

getCoord :: Jogador -> Coordenada
getCoord (_,c,_,_) = c

getDir :: Jogador -> Direcao
getDir (_,_,d,_) = d

getBag :: Jogador -> Mochila
getBag (_,_,_,b) = b


j1 :: Jogador
j1 = ("Jogador_1", (6,7), N, [(Patins, 0),(Arremesso, 0),(Bomba, 2)])

l :: [Item]
l = [Patins, Grama, Jogador j1]

j2 :: Jogador
j2 = ("Jogador_2", (2,3), N, [(Patins, 0),(Arremesso, 0),(Bomba, 2)])

j3 :: Jogador
j3 = ("Jogador_3", (5,2), N, [(Patins, 0),(Arremesso, 0),(Bomba, 2)])

setDir :: Jogador -> Direcao -> Jogador
setDir (id,c,_,m) dir = (id,c,dir,m)

-- useBomb
-- setTable
-- 
data Fuzzy = Falso | Pertinencia Double | Verdadeiro deriving (Show)

