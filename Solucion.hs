-- Completar con los datos del grupo
--
-- Nombre de Grupo: xx
-- Integrante 1: Nombre Apellido, email, LU
-- Integrante 2: Nombre Apellido, email, LU
-- Integrante 3: Nombre Apellido, email, LU
-- Integrante 4: Nombre Apellido, email, LU

module Solucion where

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

-- describir qué hace la función: Recorre la lista de Usuarios, tomando el nombre de cada uno y añadiéndolo a una lista.
-- Finaliza cuando solo quede un elemento en la lista de Usuarios y devuelve la lista de nombres.
nombresDeUsuarios :: RedSocial -> [String]
nombresDeUsuarios ([], _, _) = [] -- Si no hay usuarios, devuelve una lista vacía.
nombresDeUsuarios ([u], _, _) = [nombreDeUsuario u]
nombresDeUsuarios (u:us, rs, ps) | pertenece (nombreDeUsuario u) (nombresDeUsuarios (us, rs, ps)) = nombresDeUsuarios (us, rs, ps)
                                 | otherwise = nombreDeUsuario u : nombresDeUsuarios (us, rs, ps)



-- describir qué hace la función: Recorre la lista de Relaciones, evaluando en cada una si el usuario 'u' pertenece a la relación
-- y agregando a la lista de amigos a los usuarios relacionados. Finaliza cuando se vacía la lista de Usuarios y devuelve la lista de amigos de u.
amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe (_, [], _) _ = [] -- Si no hay relaciones, devuelve una lista vacía.
amigosDe (us, (u1,u2):rs, ps) u | u == u1 = u2 : amigosDe (us, rs, ps) u
                                | u == u2 = u1 : amigosDe (us, rs, ps) u
                                | otherwise = amigosDe (us, rs, ps) u



-- describir qué hace la función: Utiliza la función auxiliar longitud para contar la cantidad de elementos de
-- amigosDe el Usuario 'u' en la RedSocial 'red' pasados como parámetro.
cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos red u = longitud (amigosDe red u)



-- describir qué hace la función: Recorre la lista de Usuarios, comparando si el usuario evaluado tiene una cantidadDeAmigos
-- mayor al usuarioConMasAmigos del resto de la lista, finalizando cuando encuentre al usuario con más amigos y retornándolo.
usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos ([u], _, _) = u
usuarioConMasAmigos (u:us, rs, ps) | cantidadDeAmigos (u:us, rs, ps) u >= cantidadDeAmigos (us, rs, ps) (usuarioConMasAmigos (us, rs, ps)) = u
                                   | otherwise = usuarioConMasAmigos (us, rs, ps)



-- describir qué hace la función: Recorre la lista de Usuarios, evaluando en cada uno si su cantidad de amigos es mayor a 1000000.
-- Finaliza cuando encuentra un Usuario que cumpla la condición, o cuando se vacía la lista de Usuarios.
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos ([u], rs, ps) = cantidadDeAmigos ([u], rs, ps) u > 10
estaRobertoCarlos (u:us, rs, ps) | cantidadDeAmigos (u:us, rs, ps) u > 10 = True
                                 | otherwise = estaRobertoCarlos (us, rs, ps)



-- describir qué hace la función: Recorre la lista de Publicaciones, evaluando en cada una si el autor de la publicación coincide con el usuario 'u'.
-- Crea una lista con las Publicaciones que cumplan con la condición y la retorna cuando se vacía la lista de Publicaciones.
publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe (_, _, []) _ = []
publicacionesDe (us, rs, p:ps) u | usuarioDePublicacion p == u = p : publicacionesDe (us, rs, ps) u
                                 | otherwise = publicacionesDe (us, rs, ps) u



-- describir qué hace la función: Recorre la lista de Publicaciones, evaluando en cada una si el usuario 'u' pertenece a los likes de cada una. Crea una lista
-- con las publicaciones que cumplan con la condición y la retorna cuando se vacía la lista de Publicaciones.
publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA (_, _, []) _ = []
publicacionesQueLeGustanA (us, rs, p:ps) u | pertenece u (likesDePublicacion p) = p : publicacionesQueLeGustanA (us, rs, ps) u
                                           | otherwise = publicacionesQueLeGustanA (us, rs, ps) u



-- describir qué hace la función: Utiliza una función auxiliar mismosElementos para comprobar que las publicaciones que le gustan a ambos usuarios
-- sean las mismas, sin importar el órden en que vengan dadas ambas listas.
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones red u1 u2 = mismosElementos (publicacionesQueLeGustanA red u1) (publicacionesQueLeGustanA red u2)



-- describir qué hace la función: Recorre la lista de Usuarios, omitiendo al propio 'u1' pasado como parámetro. Por cada usuario 'u2', evalúa si la lista
-- de publicaciones que le gustan a 'u2' coincide con la lista de publicaciones de 'u1', retornando True en caso de encontrar un usuario que cumpla la condición,
-- o False en caso de vaciar la lista de Usuarios.
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel ([], _, _) _ = False
tieneUnSeguidorFiel (u2:us, rs, ps) u1 | publicacionesDe (u2:us, rs, ps) u1 == [] = False --Esta línea verifica que el Usuario parámetro tenga al menos una publicación.
                                       | u1 /= u2 && perteneceLista (publicacionesDe (u2:us, rs, ps) u1) (publicacionesQueLeGustanA (u2:us, rs, ps) u2) = True
                                       | otherwise = tieneUnSeguidorFiel (us, rs, ps) u1



-- describir qué hace la función: Utiliza la función auxiliar cadenaDeAmigos para obtener la secuencia completa de amigos de 'u1', y verifica si 'u2' pertenece a dicha cadena.
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos red u1 u2 = pertenece u2 (cadenaDeAmigos red [u1] [])



-- Funciones auxiliares:

longitud :: [t] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

pertenece :: (Eq t) => t -> [t] -> Bool
pertenece _ [] = False
pertenece e (x:xs) | e == x = True
                   | otherwise = pertenece e xs

perteneceLista :: (Eq t) => [t] -> [t] -> Bool
perteneceLista [] _ = True
perteneceLista (x:xs) l2 | pertenece x l2 = perteneceLista xs l2
                         | otherwise = False

mismosElementos :: (Eq t) => [t] -> [t] -> Bool
mismosElementos l1 l2 = longitud l1 == longitud l2 && perteneceLista l1 l2 && perteneceLista l2 l1

sinRepetidos :: (Eq t) => [t] -> [t]
sinRepetidos [] = []
sinRepetidos (x:xs) | pertenece x xs = sinRepetidos xs
                    | otherwise = x : sinRepetidos xs

cadenaDeAmigos :: RedSocial -> [Usuario] -> [Usuario] -> [Usuario]
cadenaDeAmigos _ [] cadena = cadena
cadenaDeAmigos red (u:us) cadena | pertenece u cadena = cadenaDeAmigos red us cadena
                                 | otherwise = cadenaDeAmigos red (us ++ (amigosDe red u)) (u:cadena)