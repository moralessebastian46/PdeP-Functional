--Punto 1
--Modelar el tipo de dato cliente. Justificar el criterio utilizado.

{-Se tomo la decision de definir al tipo de cliente como Data, debido a que esto nos permite contar con una mayor expresividad de la informacion con la que cuenta y sus diferentes tipos de dato.
Una lista no hubiese sido una buena decision, debido a que no podriamos insertar diferentes tipos de dato como se requiere.-}
data Cliente = Cliente Nombre Resistencia Amigos deriving Show
type Nombre = String
type Resistencia = Int
--type Amigos = [String]
type Amigos = [Cliente]

nombre :: Cliente -> Nombre
resistencia :: Cliente -> Resistencia
amigos :: Cliente -> Amigos

nombre (Cliente nombre _ _) = nombre
resistencia (Cliente _ resistencia _) = resistencia
amigos (Cliente _ _ amigos) = amigos

--Punto 2
{- Modelar a Rodri, que tiene 55 de resistencia y no considera a nadie como amigo. 
También modelar a Marcos, un cliente que tiene resistencia 40 y considera a Rodri como su único amigo.
Tenemos a Cristian, un cliente cuya resistencia es 2, y no considera a nadie como amigo.
Por último está Ana, una cliente que tiene 120 de resistencia y considera a Marcos y a Rodri como amigos.-}
rodri = Cliente "Rodri" 55 []
marcos = Cliente "Marcos" 40 [rodri]
cristian = Cliente "Cristian" 2 []
ana = Cliente "Ana" 120 [marcos, rodri]

--Punto 3
{-Desarrollar la función comoEsta que dice cómo está un cliente
Si su resistencia es mayor a 50, está “fresco”
Si no está “fresco”, pero tiene más de un amigo, está “piola”
En caso contrario, está “duro”-}

comoEsta :: Cliente -> [Char]

comoEsta (Cliente _ resistencia amigos) | resistencia >50 = "fresco"
                                             | resistencia <=50 && length(amigos)>1 = "piola"
                                             | otherwise = "duro"
                                                        
--Punto 4
{-Hacer que un cliente reconozca como amigo a otro cliente, respetando las restricciones definidas por el negocio 
(no agregar más de una vez al mismo amigo ni agregarse a sí mismo como amigo, basándose en que dos clientes 
son iguales si tienen el mismo nombre).-}
listaNombres amigos = map nombre amigos

noPoseeEseAmigo (Cliente _ _ amigos) (Cliente nombre1 _ _) = (elem nombre1 (listaNombres amigos) == False)
noTienenElMismoNombre (Cliente nombre _ _) (Cliente nombre1 _ _) = nombre /= nombre1
agregarAmigo (Cliente nombre resistencia amigos) cliente2 = (Cliente nombre resistencia (cliente2: amigos))

--TODO: Definir si hace falta tener la funcion agregarAmigo. Segun la decision que tomemos revisar las pruebas de este punto.

hacerseAmigo cliente1 cliente2 | noPoseeEseAmigo cliente1 cliente2 == False = cliente1--"Ya son amigos"
                               | noTienenElMismoNombre cliente1 cliente2 == False = cliente1--"No pueden ser amigos porque tienen el mismo nombre"
                             | otherwise = agregarAmigo cliente1 cliente2

--Punto 5)
{-Representar con la abstracción que crea conveniente a cada una de las bebidas mencionadas 
y cómo queda un cliente luego de tomar cualquiera de las bebidas mencionadas.-}
data Bebidas = GrogXD | LaJarraLoca | Klusener Sabor | Tintico | Soda Fuerza deriving (Show)
type Sabor = String
type Fuerza = Int

cantidadLetrasGusto (Klusener sabor) = length sabor
cantAmigosQueTiene amigos = length amigos
bajarResistenciaAAmigos amigos = map descontar10 amigos
descontar10 (Cliente nombre resistencia amigos) = (Cliente nombre (resistencia-10) amigos)
nombreModificado nombre fuerza =  (++) "e" ((++) (erresSegunFuerza fuerza) (((++) "p") nombre))
--TODO

{-Genera una lista que repite una cierta cantidad de veces al elemento dado
replicate :: Int -> a -> [a]	

la Soda, que dependiendo de la fuerza agrega “erp” al inicio del nombre del cliente, con tantas r como indique la fuerza. Por ejemplo, si Rodri se toma una soda con fuerza 2 y luego una soda con fuerza 5, su nombre queda “errrrrperrpRodri” (y queda con la misma resistencia y amigos).-}
erresSegunFuerza :: Int -> [Char]

erresSegunFuerza fuerza = replicate fuerza 'r'

tomarBebida tipoBebida cliente = trago tipoBebida cliente

trago GrogXD (Cliente nombre resistencia amigos) = Cliente nombre (resistencia - resistencia) amigos
trago (Klusener sabor) (Cliente nombre resistencia amigos) = Cliente nombre (resistencia - cantidadLetrasGusto(Klusener sabor)) amigos
trago Tintico (Cliente nombre resistencia amigos) = Cliente nombre (resistencia + 5* (cantAmigosQueTiene amigos)) amigos 
trago LaJarraLoca (Cliente nombre resistencia amigos) = descontar10 (Cliente nombre resistencia (bajarResistenciaAAmigos amigos))
trago (Soda fuerza) (Cliente nombre resistencia amigos) = Cliente (nombreModificado nombre fuerza) resistencia amigos

--Punto 6
{-Hacer que un cliente pueda rescatarse-}
--revisar...
rescatarse (Cliente nombre resistencia amigos) 0 = (Cliente nombre resistencia amigos) 
rescatarse (Cliente nombre resistencia amigos) horas | horas >0 && horas <=3 = (Cliente nombre (resistencia+100) amigos)
                                                     | otherwise = (Cliente nombre (resistencia+200) amigos)