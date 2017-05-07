import Data.List
import Text.Show.Functions
--Punto 1

{-Tomamos la decision de modelar al tipo de cliente como Data, debido a que esto nos permite visualizar facilmente (expresividad) la informacion con la que cuenta y sus diferentes tipos de dato definidos por constructores.
Una tupla no hubiese sido la mejor decision, debido a que, si bien hubiese servido para modelar Clientes, Personas, Alumnos, etc. en cuanto a nuestro modelado simplemente nos limitamos a contar con Clientes y de esta manera acotar el manejo de tipos de dato. Además, una tupla no es intuitiva y para utilizarla su estructura debe ser conocida por el usuario, por lo que sería difícil su compresión.
Al contar con los Constructores anteriormente mencionados, pudimos lograr legibilidad en el código y evitar confusiones respecto a los tipos de dato. Sin olvidar que estos pueden ser utilizados como funciones.-}

data Cliente = Cliente {
nombre      :: Nombre,
resistencia :: Resistencia,
amigos      :: Amigos,
bebidas     :: Bebidas
} deriving Show

type Nombre = String
type Resistencia = Int
type Amigos = [Cliente]
type Bebidas = [Cliente->Cliente]

--Punto 2
rodri = Cliente "Rodri" 55 [] [tintico]
marcos = Cliente "Marcos" 40 [rodri] [klusener "Guinda"]
cristian = Cliente "Cristian" 2 [] [grogXD, laJarraLoca]
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
poseeEseAmigo (Cliente nombre1 _ _ _) (Cliente _ _ amigos _) = elem nombre1 (listaNombres amigos)
tienenElMismoNombre (Cliente nombre1 _ _ _) (Cliente nombre _ _ _) = nombre == nombre1
agregarAmigo cliente1 (Cliente nombre resistencia amigos bebidas) = (Cliente nombre resistencia (cliente1: amigos) bebidas)

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
klusener :: String -> Cliente -> Cliente
tintico :: Cliente -> Cliente
soda :: Int -> Cliente -> Cliente

bajarResistenciaAAmigos amigos = map descontar10 amigos
descontar10 (Cliente nombre resistencia amigos bebidas) = (Cliente nombre (resistencia-10) amigos bebidas)
nombreModificado nombre fuerza =  (("e" ++).((erresSegunFuerza fuerza) ++). ("p" ++)) nombre
erresSegunFuerza fuerza = replicate fuerza 'r'

grogXD (Cliente nombre resistencia amigos bebidas) = Cliente nombre 0 amigos bebidas
laJarraLoca (Cliente nombre resistencia amigos bebidas) = descontar10 (Cliente nombre resistencia (bajarResistenciaAAmigos amigos) bebidas)
klusener sabor (Cliente nombre resistencia amigos bebidas) = Cliente nombre (resistencia - length(sabor)) amigos bebidas
tintico (Cliente nombre resistencia amigos bebidas) = Cliente nombre (resistencia + 5* (length amigos)) amigos bebidas
soda fuerza (Cliente nombre resistencia amigos bebidas) = Cliente (nombreModificado nombre fuerza) resistencia amigos bebidas


--Punto 6
rescatarse :: (Num a, Ord a) => a -> Cliente -> Cliente

rescatarse horas (Cliente nombre resistencia amigos bebidas) | horas <=3 = (Cliente nombre (resistencia+100) amigos bebidas)
                                                     | otherwise = (Cliente nombre (resistencia+200) amigos bebidas)
                                                                    
--Punto 7: Ver en Pruebas.txt, se implementa directamente en workspace.

------------------------------------------------------------TP 2---------------------------------------------------------------
 --Punto 1.b)
tomarUnaBebida bebida (Cliente nombre resistencia amigos bebidas) = bebida (Cliente nombre resistencia amigos (bebidas++[bebida]))


--Punto  1.c)
tomarTragos cliente [] = cliente
tomarTragos cliente listaTragos =  tomarTragos (tomarUnaBebida (head listaTragos) cliente) (tail listaTragos)

--Punto 1.d)
dameOtro cliente = tomarUnaBebida (last (bebidas cliente)) cliente

--Punto 2.a)
nuevaResistencia cliente trago = resistencia(tomarUnaBebida trago cliente)

cualesPuedeTomar cliente tragos = filter ((>0).nuevaResistencia cliente) tragos

--Punto 2.b)
cuantasPuedeTomar cliente tragos = length (cualesPuedeTomar cliente tragos)

--Punto 3.a)
robertoCarlos = Cliente "Roberto Carlos" 165 [] []

data Itinerario = Itinerario {
nombreItinerario :: String,
duracionEstimada :: Float,
detalle :: Detalles
} deriving Show

type Detalles = [Cliente -> Cliente]

mezclaExplosiva = Itinerario "Mezcla Explosiva" 2.5 [tomarUnaBebida grogXD, tomarUnaBebida grogXD, tomarUnaBebida (klusener "Huevo"), tomarUnaBebida (klusener "Frutilla")]
itinerarioBasico = Itinerario "Itinerario Basico" 5.0 [tomarUnaBebida laJarraLoca, tomarUnaBebida (klusener "chocolate"), rescatarse 2, tomarUnaBebida (klusener "huevo")]
salidaDeAmigos = Itinerario "Salida De Amigos" 1.0 [tomarUnaBebida (soda 1), tomarUnaBebida tintico, (hacerseAmigo robertoCarlos), tomarUnaBebida laJarraLoca]


--Punto 3.b)
ejecutarItinerario itinerario cliente = ejecutar (detalle itinerario) cliente

ejecutar [] cliente = cliente
ejecutar (cabItinerario:colaItinerario) cliente = ejecutar colaItinerario (cabItinerario cliente)

--Punto 4.a)
intensidad itinerario = genericLength(detalle itinerario)/(duracionEstimada itinerario)

--Punto 4.b)

intinerarioMasIntenso itinerarios = foldl (mayorSegun intensidad) (head itinerarios) (tail itinerarios)

mayorSegun intensidad unItinerario otroItinerario | intensidad unItinerario > intensidad otroItinerario = unItinerario
                                                  | otherwise = otroItinerario

ejecutarMasIntenso cliente itinerarios = ejecutarItinerario (intinerarioMasIntenso itinerarios) cliente


--Punto 5.a)

tragosChuck i  = (soda i):(tragosChuck (i+1))

chuckNorris = Cliente "Chuck" 1000 [ana] (tragosChuck 1)

--Punto 5.b)
--No, chuckNorris no puede pedir otro trago con la función dameOtro debido a que ésta toma el último trago de la lista de 
--bebidas del cliente, y en el caso de chuckNorris, su lista de bebidas es infinita. Debido al concepto de evalución diferida
--Haskell se queda intentando reducir cuál es la última bebida.

--Main> dameOtro chuckNorris
--(se queda reduciendo)

--Punto 5.d)
--Si se puede porque solo evalua la resistencia de ambos, no llega a evaluar la lista infinita que tiene "Chuck" por lazy evaluation. Es decir, 
--si no se utiliza una abstracción Haskell no la evalua.


--Punto 6)
-- laJarraPopular 0 cliente = cliente
-- laJarraPopular espirituosidad (Cliente nombre resistencia (amigosCab:amigosCola) bebidas) = laJarraPopular (espirituosidad-1) (hacerGrupoAmigos (Cliente nombre resistencia (amigosCab:amigosCola) bebidas) amigosCab)

-- hacerGrupoAmigos cliente (Cliente nombre resistencia amigos bebidas) = hacerMuchosamigos cliente amigos

-- hacerMuchosamigos cliente [] = cliente
-- hacerMuchosamigos cliente (cab:cola) = hacerMuchosamigos (hacerseAmigo cab cliente) cola -}
