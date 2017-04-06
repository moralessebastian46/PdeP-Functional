{-NOTA:
Saqué todas las consigas porque quedaba un poco largo el tp, agregué el punto 7 (por las dudas) y además 
agregué los tipos de TODAS las funciones utilizadas, si les parece demasiado quiten las que les parezcan que sobran.
Pensé en hacer un archivo aparte con los tipos para que quede un poco más prolijo y legible el tp, pero no nos pidieron eso.
Ahora en el punto 5 las bebidas son funciones, lo probé y compiló. Les dejo aca abajo las pruebas nuevas-}


{-PRUEBAS DEL PUNTO 5
*Main> beberGrogXD ana
Cliente "Ana" 0 [Cliente "Marcos" 40 [Cliente "Rodri" 55 []],Cliente "Rodri" 55 []]
*Main> beberLaJarraLoca ana
Cliente "Ana" 110 [Cliente "Marcos" 30 [Cliente "Rodri" 55 []],Cliente "Rodri" 45 []]
*Main> beberKlusener "huevo" ana
Cliente "Ana" 115 [Cliente "Marcos" 40 [Cliente "Rodri" 55 []],Cliente "Rodri" 55 []]
*Main> beberKlusener "chocolate" ana
Cliente "Ana" 111 [Cliente "Marcos" 40 [Cliente "Rodri" 55 []],Cliente "Rodri" 55 []]
*Main> beberTintico cristian
Cliente "Cristian" 2 []
*Main> beberTintico ana
Cliente "Ana" 130 [Cliente "Marcos" 40 [Cliente "Rodri" 55 []],Cliente "Rodri" 55 []]
*Main> beberSoda 2 rodri
Cliente "errpRodri" 55 []
*Main> beberSoda 10 ana
Cliente "errrrrrrrrrpAna" 120 [Cliente "Marcos" 40 [Cliente "Rodri" 55 []],Cliente "Rodri" 55 []]
*Main> beberSoda 0 ana
Cliente "epAna" 120 [Cliente "Marcos" 40 [Cliente "Rodri" 55 []],Cliente "Rodri" 55 []]
-}




--Punto 1
{-Se tomo la decision de definir al tipo de cliente como Data, debido a que esto nos permite contar con una mayor expresividad de la informacion con la que cuenta y sus diferentes tipos de dato.
Una lista no hubiese sido una buena decision, debido a que no podriamos insertar diferentes tipos de dato como se requiere.-}

data Cliente = Cliente Nombre Resistencia Amigos deriving Show
type Nombre = String
type Resistencia = Int
type Amigos = [Cliente]

nombre :: Cliente -> Nombre
resistencia :: Cliente -> Resistencia
amigos :: Cliente -> Amigos

nombre (Cliente nombre _ _) = nombre
resistencia (Cliente _ resistencia _) = resistencia
amigos (Cliente _ _ amigos) = amigos


--Punto 2
rodri = Cliente "Rodri" 55 []
marcos = Cliente "Marcos" 40 [rodri]
cristian = Cliente "Cristian" 2 []
ana = Cliente "Ana" 120 [marcos, rodri]


--Punto 3
comoEsta :: Cliente -> [Char]

comoEsta (Cliente _ resistencia amigos) | resistencia >50 = "fresco"
                                        | resistencia <=50 && length(amigos)>1 = "piola"
                                        | otherwise = "duro"
                                                        

--Punto 4
hacerseAmigo :: Cliente -> Cliente -> Cliente
listaNombres :: [Cliente] -> [Nombre]
noPoseeEseAmigo :: Cliente -> Cliente -> Bool
noTienenElMismoNombre :: Cliente -> Cliente -> Bool
agregarAmigo :: Cliente -> Cliente -> Cliente

listaNombres amigos = map nombre amigos
noPoseeEseAmigo (Cliente _ _ amigos) (Cliente nombre1 _ _) = (elem nombre1 (listaNombres amigos) == False)
noTienenElMismoNombre (Cliente nombre _ _) (Cliente nombre1 _ _) = nombre /= nombre1
agregarAmigo (Cliente nombre resistencia amigos) cliente2 = (Cliente nombre resistencia (cliente2: amigos))

hacerseAmigo cliente1 cliente2 | noPoseeEseAmigo cliente1 cliente2 == False = cliente1--"Ya son amigos"
                               | noTienenElMismoNombre cliente1 cliente2 == False = cliente1--"No pueden ser amigos porque tienen el mismo nombre"
                               | otherwise = agregarAmigo cliente1 cliente2


--Punto 5)
cantidadLetrasGusto :: Foldable t => t a -> Int
cantAmigosQueTiene :: Foldable t => t a -> Int
bajarResistenciaAAmigos :: [Cliente] -> [Cliente]
descontar10 :: Cliente -> Cliente
nombreModificado :: [Char] -> Int -> [Char]
erresSegunFuerza :: Int -> [Char]
beberGrogXD :: Cliente -> Cliente
beberLaJarraLoca :: Cliente -> Cliente
beberKlusener :: Foldable t => t a -> Cliente -> Cliente
beberTintico :: Cliente -> Cliente
beberSoda :: Int -> Cliente -> Cliente

cantidadLetrasGusto sabor = length sabor
cantAmigosQueTiene amigos = length amigos
bajarResistenciaAAmigos amigos = map descontar10 amigos
descontar10 (Cliente nombre resistencia amigos) = (Cliente nombre (resistencia-10) amigos)
nombreModificado nombre fuerza =  (++) "e" ((++) (erresSegunFuerza fuerza) (((++) "p") nombre))
erresSegunFuerza fuerza = replicate fuerza 'r'

beberGrogXD (Cliente nombre resistencia amigos) = Cliente nombre (resistencia - resistencia) amigos
beberLaJarraLoca (Cliente nombre resistencia amigos) = descontar10 (Cliente nombre resistencia (bajarResistenciaAAmigos amigos))
beberKlusener sabor (Cliente nombre resistencia amigos) = Cliente nombre (resistencia - cantidadLetrasGusto(sabor)) amigos
beberTintico (Cliente nombre resistencia amigos) = Cliente nombre (resistencia + 5* (cantAmigosQueTiene amigos)) amigos
beberSoda fuerza (Cliente nombre resistencia amigos) = Cliente (nombreModificado nombre fuerza) resistencia amigos


--Punto 6
rescatarse :: (Num a, Ord a) => Cliente -> a -> Cliente

rescatarse (Cliente nombre resistencia amigos) 0 = (Cliente nombre resistencia amigos) 
rescatarse (Cliente nombre resistencia amigos) horas | horas >0 && horas <=3 = (Cliente nombre (resistencia+100) amigos)
                                                     | otherwise = (Cliente nombre (resistencia+200) amigos)
													 
													 
--Punto 7
{-
*Main> ((tomarBebida (Klusener "huevo")).(flip rescatarse 2).(tomarBebida (Klusener "chocolate")).(tomarBebida LaJarraLoca)) ana
Cliente "Ana" 196 [Cliente "Marcos" 30 [Cliente "Rodri" 55 []],Cliente "Rodri" 45 []]
-}