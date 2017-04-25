--Punto 1

{-Tomamos la decision de modelar al tipo de cliente como Data, debido a que esto nos permite visualizar facilmente (expresividad) la informacion con la que cuenta y sus diferentes tipos de dato definidos por constructores.
Una tupla no hubiese sido la mejor decision, debido a que, si bien hubiese servido para modelar Clientes, Personas, Alumnos, etc. en cuanto a nuestro modelado simplemente nos limitamos a contar con Clientes y de esta manera acotar el manejo de tipos de dato. Además, una tupla no es intuitiva y para utilizarla su estructura debe ser conocida por el usuario, por lo que sería difícil su compresión.
Al contar con los Constructores anteriormente mencionados, pudimos lograr legibilidad en el código y evitar confusiones respecto a los tipos de dato. Sin olvidar que estos pueden ser utilizados como funciones.-}

data Cliente = Cliente {
nombre         :: Nombre,
resistencia :: Resistencia,
amigos         :: Amigos,
bebidas :: Bebidas
} deriving Show

data Bebida = GrogXD | LaJarraLoca | Klusener String | Tintico | Soda Int deriving Show

type Nombre = String
type Resistencia = Int
type Amigos = [Cliente]
type Bebidas = [Bebida]

tomarTragos cliente [] = cliente
tomarTragos cliente (cab:cola) = tomarTragos (cab cliente) cola

--Punto 2
rodri = Cliente "Rodri" 55 [] [Tintico]
marcos = Cliente "Marcos" 40 [rodri] [Klusener "Guinda"]
cristian = Cliente "Cristian" 2 [] [GrogXD, LaJarraLoca]
ana = Cliente "Ana" 120 [marcos, rodri] []


--Punto 3
comoEsta :: Cliente -> [Char]

comoEsta (Cliente _ resistencia amigos bebidas) | resistencia >50 = "fresco"
                                        | ((>1).(length))amigos = "piola"
                                        | otherwise = "duro"
                                                        

--Punto 4
hacerseAmigo :: Cliente -> Cliente -> Cliente
listaNombres :: [Cliente] -> [Nombre]
poseeEseAmigo :: Cliente -> Cliente -> Bool
tienenElMismoNombre :: Cliente -> Cliente -> Bool
agregarAmigo :: Cliente -> Cliente -> Cliente

listaNombres amigos = map nombre amigos
poseeEseAmigo (Cliente _ _ amigos _) (Cliente nombre1 _ _ _) = elem nombre1 (listaNombres amigos)
tienenElMismoNombre (Cliente nombre _ _ _) (Cliente nombre1 _ _ _) = nombre == nombre1
agregarAmigo (Cliente nombre resistencia amigos bebidas) cliente2 = (Cliente nombre resistencia (cliente2: amigos) bebidas)

hacerseAmigo cliente1 cliente2 | poseeEseAmigo cliente1 cliente2 = error "No puede ser agregado nuevamente porque ya son amigos"
                               | tienenElMismoNombre cliente1 cliente2 = error "No pueden ser amigos porque tienen el mismo nombre"
                               | otherwise = agregarAmigo cliente1 cliente2


--Punto 5)
bajarResistenciaAAmigos :: [Cliente] -> [Cliente]
descontar10 :: Cliente -> Cliente
nombreModificado :: [Char] -> Int -> [Char]
erresSegunFuerza :: Int -> [Char]
grogXD :: Cliente -> Cliente
laJarraLoca :: Cliente -> Cliente
klusener :: Foldable t => t a -> Cliente -> Cliente
tintico :: Cliente -> Cliente
soda :: Int -> Cliente -> Cliente

bajarResistenciaAAmigos amigos = map descontar10 amigos
descontar10 (Cliente nombre resistencia amigos bebidas) = (Cliente nombre (resistencia-10) amigos bebidas)
nombreModificado nombre fuerza =  (("e" ++).((erresSegunFuerza fuerza) ++). ("p" ++)) nombre
erresSegunFuerza fuerza = replicate fuerza 'r'

grogXD (Cliente nombre resistencia amigos bebidas) = Cliente nombre 0 amigos (GrogXD : bebidas)
laJarraLoca (Cliente nombre resistencia amigos bebidas) = descontar10 (Cliente nombre resistencia (bajarResistenciaAAmigos amigos) (LaJarraLoca : bebidas))
--TODO: agregar la bebida a la lista de bebidas en klusener
klusener sabor (Cliente nombre resistencia amigos bebidas) = Cliente nombre (resistencia - length(sabor)) amigos (bebidas)
tintico (Cliente nombre resistencia amigos bebidas) = Cliente nombre (resistencia + 5* (length amigos)) amigos (Tintico : bebidas)
soda fuerza (Cliente nombre resistencia amigos bebidas) = Cliente (nombreModificado nombre fuerza) resistencia amigos ((Soda fuerza):bebidas)


--Punto 6
rescatarse :: (Num a, Ord a) => Cliente -> a -> Cliente

rescatarse (Cliente nombre resistencia amigos bebidas) horas | horas <=3 = (Cliente nombre (resistencia+100) amigos bebidas)
                                                     | otherwise = (Cliente nombre (resistencia+200) amigos bebidas)
                                                                    
--Punto 7: Ver en Pruebas.txt, se implementa directamente en workspace.