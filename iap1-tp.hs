-- Completar con los datos del grupo
--
-- Nombre de Grupo: xx
-- Integrante 1: Nombre Apellido, email, LU
-- Integrante 2: Nombre Apellido, email, LU
-- Integrante 3: Nombre Apellido, email, LU
-- Integrante 4: Nombre Apellido, email, LU

type Usuario = (Integer, String) -- (id, nombre)
type Relacion = (Usuario, Usuario) -- usuarios que se relacionan
type Publicacion = (Usuario, String, [Usuario]) -- (usuario que publica, texto publicacion, likes)
type RedSocial = ([Usuario], [Relacion], [Publicacion])

-- Funciones basicas

usuarios :: RedSocial -> [Usuario]
usuarios (us, _, _) = us

relaciones :: RedSocial -> [Relacion]
relaciones (_, rs, _) = rs

publicaciones :: RedSocial -> [Publicacion]
publicaciones (_, _, ps) = ps

idDeUsuario :: Usuario -> Integer
idDeUsuario (id, _) = id 

nombreDeUsuario :: Usuario -> String
nombreDeUsuario (_, nombre) = nombre 

usuarioDePublicacion :: Publicacion -> Usuario
usuarioDePublicacion (u, _, _) = u

likesDePublicacion :: Publicacion -> [Usuario]
likesDePublicacion (_, _, us) = us

-- Ejercicios

{-
describir qué hace la funcióm: Recibe una RedSocial, de la cual se posiciona en el primer Usuario de la
lista de Usuarios. Luego, toma el nombre de este primer Usuario y lo agrega a una lista. A continuación,
continúa construyendo la lista de manera recursiva, utilizando la misma función 'nombresDeUsuarios', pero
esta vez recibiendo como parámetro la misma RedSocial sin el primer Usuario cuyo nombre ya fue agregado a
la lista, repitiendo así el proceso hasta recibir a la RedSocial con la lista de Usuarios vacía, lo que
dará fin a la recursión y retornará la lista con todos los nombres de Usuarios que se fueron tomando.
-}
nombresDeUsuarios :: RedSocial -> [String]
nombresDeUsuarios (us, rs, ps) | us == [] = []
                                 | otherwise = nombreDeUsuario (head us) : nombresDeUsuarios (tail us, rs, ps)

{-
describir qué hace la función: Recibe una RedSocial y un Usuario (u), se posiciona en la primera Relacion
de la RedSocial y evalúa si u es uno de los dos Usuarios de dicha Relacion (u1 y u2), dando lugar a dos 
posibilidades:
        1- si esta condición se cumple, toma al otro Usuario de la Relacion (por ejemplo, si u es igual a
u1, la función toma a u2) y lo agrega a la lista.
        2- si esta condición no se cumple, entonces u no pertenece a la relación (u no es u1, ni tampoco es
        u2), por lo que no se agregará nada a la lista.
Luego de realizar esta evaluación, se continúa la construcción de la lista, repitiendo el mismo proceso a
través de recursión con cada relación perteneciente a la RedSocial, evaluando amigosDe con el mismo usuario u,
pero eliminando de la RedSocial a la Relacion ya evaluada en cada iteración hasta recibir a la RedSocial con
la lista de Relaciones vacía, con la que finaliza y se retorna la lista con los ususarios que se fueron tomando.
-}
amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe (_, [], _) _ = []
amigosDe (us, ((u1,u2):rs), ps) u | u == u1 = u2 : amigosDe (us, rs, ps) u
                                  | u == u2 = u1 : amigosDe (us, rs, ps) u
                                  | otherwise = amigosDe (us, rs, ps) u

-- describir qué hace la función: .....
cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos = undefined

-- describir qué hace la función: .....
usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos = undefined

-- describir qué hace la función: .....
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos = undefined

-- describir qué hace la función: .....
publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe = undefined

-- describir qué hace la función: .....
publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA = undefined

-- describir qué hace la función: .....
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones = undefined

-- describir qué hace la función: .....
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel = undefined

-- describir qué hace la función: .....
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos = undefined


-- Funciones auxiliares:

-- describir qué hace la función: Dada una red social válida, devuelve el dato del primer usuario dentro
-- de la lista de usuarios.
primerUsuario :: RedSocial -> Usuario
primerUsuario red = head (usuarios red)

-- describir qué hace la función: Dada una red social válida, devuelve la misma red social, pero
-- eliminando el primer usuario de la lista de usuarios.
quitarPrimerUsuario :: RedSocial -> RedSocial
quitarPrimerUsuario (us, rs, ps) = (tail us, rs, ps)