--Punto 1
{-Se tomo la decision de definir al tipo de cliente como Data, debido a que esto nos permite contar con una mayor expresividad de la informacion con la que cuenta y sus diferentes tipos de dato.
Una lista no hubiese sido una buena decision, debido a que no podriamos insertar diferentes tipos de dato como se requiere.-}

type Nombre = String
type Resistencia = Int
type Amigos = [Cliente]
data Cliente = Cliente Nombre Resistencia Amigos deriving Show

getNombre :: Cliente -> Nombre
getNombre (Cliente nombre _ _) = nombre

getResistencia :: Cliente -> Resistencia
getResistencia (Cliente _ resistencia _) = resistencia

getAmigos :: Cliente -> Amigos
getAmigos (Cliente _ _ amigos) = amigos


--Punto 2
rodri = Cliente "Rodri" 55 []
marcos = Cliente "Marcos" 40 [rodri]
cristian = Cliente "Cristian" 2 []
ana = Cliente "Ana" 120 [marcos, rodri]


--Punto 3
comoEsta :: Cliente -> [Char]
comoEsta (Cliente _ resistencia amigos) | resistencia > 50 = "fresco"
                                        | resistencia <=50 && ((>1).(length))amigos = "piola"
                                        | otherwise = "duro"

--Punto 4
mismoNombre :: String -> Cliente -> Bool
mismoNombre n1 (Cliente n2 _ _) = n1 == n2

reconocerAmigo :: Cliente -> Cliente -> Bool
reconocerAmigo (Cliente _ _ amigos) (Cliente nomAmigo _ _) = any (mismoNombre nomAmigo) amigos


agregarAmigo :: Cliente -> Cliente -> Cliente
agregarAmigo cliente amigo | (getNombre cliente) == (getNombre amigo) = cliente
                           | (reconocerAmigo cliente amigo) = cliente
                           | otherwise = Cliente (getNombre cliente) (getResistencia cliente) (amigo: (getAmigos cliente))


--Punto 5
type Sabor = String
type Fuerza = Int
type AfectaAmigos = Bool

data Bebida = BebidaDestroyer Nombre AfectaAmigos |
              BebidaNormal Nombre Resistencia AfectaAmigos |
              BebidaSabor Nombre Sabor AfectaAmigos |
              BebidaTintico Nombre Resistencia |
              BebidaGasificada Nombre Fuerza deriving Show

grogXD = BebidaDestroyer "Grog XD" False
jarraLoca = BebidaNormal "Jarra Loca" (-10) True
klusenerHuevo = BebidaSabor "Klusener" "Huevo" False 
klusenerChocolate = BebidaSabor "Klusener" "Chocolate" False
klusenerFrutilla = BebidaSabor "Klusener" "Frutilla" False
tintico = BebidaTintico "Tintico" 5
soda10 = BebidaGasificada "Soda" 10
soda5 = BebidaGasificada "Soda" 5

getResBebida :: Bebida -> Resistencia
getResBebida (BebidaNormal _ resistencia _ ) = resistencia
getResBebida (BebidaSabor _ sabor _ ) = -length(sabor)
getResBebida otra = 0

getAfectaAmigos :: Bebida -> AfectaAmigos
getAfectaAmigos (BebidaNormal _ _ afecta ) = afecta
getAfectaAmigos (BebidaSabor _ _ afecta ) = afecta
getAfectaAmigos otra = False

getFuerza :: Bebida -> Int
getFuerza (BebidaGasificada _ fuerza ) = fuerza
getFuerza otra = 0

cambiarResistencia :: Bebida -> Resistencia -> Int -> Resistencia
cambiarResistencia (BebidaDestroyer _ _) _ _ = 0
cambiarResistencia (BebidaTintico _ resBebida) resistencia cantAmigos = resistencia + (resBebida * cantAmigos)
cambiarResistencia bebida resistencia _ = resistencia + (getResBebida bebida)

cambiarResistenciaCliente :: Bebida -> Cliente -> Cliente
cambiarResistenciaCliente bebida (Cliente nombre resistencia amigos) = Cliente nombre (cambiarResistencia bebida resistencia (length amigos)) amigos

modificarResistenciaAmigos :: Amigos -> Bebida -> Amigos
modificarResistenciaAmigos amigos bebida | (getAfectaAmigos bebida) && ((getResBebida bebida) /= 0) = map (cambiarResistenciaCliente bebida) amigos
                                         | otherwise = amigos

modificarNombre :: Nombre -> Bebida -> Nombre
modificarNombre nombre (BebidaGasificada _ fuerza) = "e" ++ (replicate fuerza 'r') ++ "p" ++ nombre
modificarNombre nombre _ = nombre

beber :: Bebida -> Cliente -> Cliente
beber bebida (Cliente nombre resistencia amigos) = Cliente (modificarNombre nombre bebida) (cambiarResistencia bebida resistencia (length amigos)) (modificarResistenciaAmigos amigos bebida)


--Punto 6
type Horas = Integer

rescatarse :: Horas -> Cliente -> Cliente
rescatarse horas (Cliente nombre resistencia amigos) | horas <= 3 = Cliente nombre (resistencia + 100) amigos
                                                     | otherwise = Cliente nombre (resistencia + 200) amigos

--Punto 7
--((beber klusenerHuevo).(rescatarse 2).(beber klusenerChocolate).(beber jarraLoca)) ana
