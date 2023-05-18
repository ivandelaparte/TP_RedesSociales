-- Nombre de Grupo: MIAT
-- Integrante 1: Aaron Agustin Cuellar, aaroncuellar2003@gmail.com, 810/23
-- Integrante 2: Tobias Ezequiel Seirgalea, tobyseirgalea@gmail.com, 78/23
-- Integrante 3: Marcos Elian Wendy, wendymarcos2@gmail.com, 344/22
-- Integrante 4: Ivan Luciano de la Parte Aguirre, ivandelaparte@gmail.com, 184/22

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


-- Recibe una Red Social y almacena en una lista los nombres de usuarios sin repetir
-- Devuelve la lista de Nombres
nombresDeUsuarios :: RedSocial -> [String]
nombresDeUsuarios ([], _, _) = [] -- Si no hay usuarios, devuelve una lista vacía.
nombresDeUsuarios ([u], _, _) = [nombreDeUsuario u]
nombresDeUsuarios (u:us, rs, ps) | pertenece (nombreDeUsuario u) (nombresDeUsuarios (us, rs, ps)) = nombresDeUsuarios (us, rs, ps)
                                 | otherwise = nombreDeUsuario u : nombresDeUsuarios (us, rs, ps)



-- Recibe una Red Social y un Usuario. comprueba si el usuario pertenece a alguna relacion en las relaciones de la red social. y concatena al amigo.
-- Devuelve una lista de Usuarios que son amigos del Usuario pasado por parametro.
amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe (_, [], _) _ = [] -- Si no hay relaciones, devuelve una lista vacía.
amigosDe (us, (u1,u2):rs, ps) u | u == u1 = u2 : amigosDe (us, rs, ps) u
                                | u == u2 = u1 : amigosDe (us, rs, ps) u
                                | otherwise = amigosDe (us, rs, ps) u



-- Recibe una Red Social y un Usuario.
-- Devuelve el cardinal de amigos del Usuario pasado por parametro. 
cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos red u = longitud (amigosDe red u)



-- Recibe una Red Social
-- Devuelve el primer Usuario con mas amigos de la Red Social.
usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos ([u], _, _) = u
usuarioConMasAmigos (u:us, rs, ps) | cantidadDeAmigos (u:us, rs, ps) u >= cantidadDeAmigos (us, rs, ps) (usuarioConMasAmigos (us, rs, ps)) = u
                                   | otherwise = usuarioConMasAmigos (us, rs, ps)



-- Recibe una red Social y busca algún usuario con más de 10 amigos
-- Devuelve True si se encontró. False si no.
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos ([], rs, ps) = False
estaRobertoCarlos (u:us, rs, ps) = cantidadDeAmigos (u:us, rs, ps) u > 10 || estaRobertoCarlos (us, rs, ps)



-- Recibe una Red Social y un Usuario
-- Devuelve una lista de Publicaciones que fueron hechas por el Usuario pasado por parametro.
publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe (_, _, []) _ = []
publicacionesDe (us, rs, p:ps) u | usuarioDePublicacion p == u = p : publicacionesDe (us, rs, ps) u
                                 | otherwise = publicacionesDe (us, rs, ps) u



-- Recibe una Red Social y un Usuario
-- Devuelve una lista de Publicaciones que le gustan al Usuario pasado por parametro.
publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA (_, _, []) _ = []
publicacionesQueLeGustanA (us, rs, p:ps) u | pertenece u (likesDePublicacion p) = p : publicacionesQueLeGustanA (us, rs, ps) u
                                           | otherwise = publicacionesQueLeGustanA (us, rs, ps) u



-- Recibe una Red Social y dos Usuarios distintos 
-- Devuelve True si ambos usuarios tienen las mismas publicaciones que le gustan, False si no.
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones red u1 u2 = mismosElementos (publicacionesQueLeGustanA red u1) (publicacionesQueLeGustanA red u2)



-- Recibe una Red Social y un Usuario
-- Devuelve True si el Usuario pasado por parametro tiene un seguidor fiel, False si no.
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel ([], _, _) _ = False
tieneUnSeguidorFiel (u2:us, rs, ps) u1 | publicacionesDe (u2:us, rs, ps) u1 == [] = False --Esta línea verifica que el Usuario parámetro tenga al menos una publicación.
                                       | idDeUsuario u1 /= idDeUsuario u2 && perteneceLista (publicacionesDe (u2:us, rs, ps) u1) (publicacionesQueLeGustanA (u2:us, rs, ps) u2) = True
                                       | otherwise = tieneUnSeguidorFiel (us, rs, ps) u1



-- Recibe una Red Social y dos Usuarios distintos
-- Devuelve True si existe una secuencia de amigos que conecte a ambos Usuarios, False si no.
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos red u1 u2 = pertenece u2 (secuenciaDeAmigos red [u1] [])



-- Funciones auxiliares:

-- Recibe una lista de elementos y Devuelve su longitud
longitud :: [t] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

-- Recibe un elemento y una lista de elementos.
-- Devuelve True si el elemento pertenece a la lista, False si no.
pertenece :: (Eq t) => t -> [t] -> Bool
pertenece _ [] = False
pertenece e (x:xs) | e == x = True
                   | otherwise = pertenece e xs

-- Recibe dos listas de elementos.
-- Devuelve True si todos los elementos de la primera lista pertenecen a la segunda lista, False si no.
perteneceLista :: (Eq t) => [t] -> [t] -> Bool
perteneceLista [] _ = True
perteneceLista (x:xs) l2 | pertenece x l2 = perteneceLista xs l2
                         | otherwise = False

-- Recibe dos listas de elementos y Devuelve True si ambas listas tienen los mismos elementos, False si no.
mismosElementos :: (Eq t) => [t] -> [t] -> Bool
mismosElementos l1 l2 = longitud l1 == longitud l2 && perteneceLista l1 l2 && perteneceLista l2 l1

-- Recibe una lista de elementos y Devuelve una lista sin elementos repetidos.
sinRepetidos :: (Eq t) => [t] -> [t]
sinRepetidos [] = []
sinRepetidos (x:xs) | pertenece x xs = sinRepetidos xs
                    | otherwise = x : sinRepetidos xs

-- Recibe una Red Social y dos usuarios distintos.
-- Devuelve una lista de Usuarios
secuenciaDeAmigos :: RedSocial -> [Usuario] -> [Usuario] -> [Usuario]
secuenciaDeAmigos _ [] cadena = cadena
secuenciaDeAmigos red (u:us) cadena | pertenece u cadena = secuenciaDeAmigos red us cadena
                                    | otherwise = secuenciaDeAmigos red (us ++ (amigosDe red u)) (u:cadena)