--Punto 1

{-Tomamos la decision de modelar al tipo de cliente como Data, debido a que esto nos permite visualizar facilmente (expresividad) la informacion con la que cuenta y sus diferentes tipos de dato definidos por constructores.
Una tupla no hubiese sido la mejor decision, debido a que, si bien hubiese servido para modelar Clientes, Personas, Alumnos, etc. en cuanto a nuestro modelado simplemente nos limitamos a contar con Clientes y de esta manera acotar el manejo de tipos de dato. Además, una tupla no es intuitiva y para utilizarla su estructura debe ser conocida por el usuario, por lo que sería difícil su compresión.
Al contar con los Constructores anteriormente mencionados, pudimos lograr legibilidad en el código y evitar confusiones respecto a los tipos de dato. Sin olvidar que estos pueden ser utilizados como funciones.-}

data Cliente = Cliente {
nombre         :: Nombre,
resistencia :: Resistencia,
amigos         :: Amigos 
} deriving Show


type Nombre = String
type Resistencia = Int
type Amigos = [Cliente]

--Punto 2
rodri = Cliente "Rodri" 55 []
marcos = Cliente "Marcos" 40 [rodri]
cristian = Cliente "Cristian" 2 []
ana = Cliente "Ana" 120 [marcos, rodri]


--Punto 3
comoEsta :: Cliente -> [Char]

comoEsta (Cliente _ resistencia amigos) | resistencia >50 = "fresco"
                                        | ((>1).(length))amigos = "piola"
                                        | otherwise = "duro"
                                                        

--Punto 4
hacerseAmigo :: Cliente -> Cliente -> Cliente
listaNombres :: [Cliente] -> [Nombre]
poseeEseAmigo :: Cliente -> Cliente -> Bool
tienenElMismoNombre :: Cliente -> Cliente -> Bool
agregarAmigo :: Cliente -> Cliente -> Cliente

listaNombres amigos = map nombre amigos
poseeEseAmigo (Cliente _ _ amigos) (Cliente nombre1 _ _) = elem nombre1 (listaNombres amigos)
tienenElMismoNombre (Cliente nombre _ _) (Cliente nombre1 _ _) = nombre == nombre1
agregarAmigo (Cliente nombre resistencia amigos) cliente2 = (Cliente nombre resistencia (cliente2: amigos))

hacerseAmigo cliente1 cliente2 | poseeEseAmigo cliente1 cliente2 = cliente1--"Ya son amigos, no hago nada"
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
descontar10 (Cliente nombre resistencia amigos) = (Cliente nombre (resistencia-10) amigos)
nombreModificado nombre fuerza =  (("e" ++).((erresSegunFuerza fuerza) ++).("p" ++)) nombre
erresSegunFuerza fuerza = replicate fuerza 'r'

grogXD (Cliente nombre resistencia amigos) = Cliente nombre 0 amigos
laJarraLoca (Cliente nombre resistencia amigos) = descontar10 (Cliente nombre resistencia (bajarResistenciaAAmigos amigos))
klusener sabor (Cliente nombre resistencia amigos) = Cliente nombre (resistencia - length(sabor)) amigos
tintico (Cliente nombre resistencia amigos) = Cliente nombre (resistencia + 5* (length amigos)) amigos
soda fuerza (Cliente nombre resistencia amigos) = Cliente (nombreModificado nombre fuerza) resistencia amigos


--Punto 6
rescatarse :: (Num a, Ord a) => Cliente -> a -> Cliente

rescatarse (Cliente nombre resistencia amigos) horas | horas <=3 = (Cliente nombre (resistencia+100) amigos)
                                                     | otherwise = (Cliente nombre (resistencia+200) amigos)
                                                                    
--Punto 7: Ver en Pruebas.txt, se implementa directamente en workspace.