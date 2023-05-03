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
amigosDe (us, (u1,u2):rs, ps) u | u == u1 = u2 : amigosDe (us, rs, ps) u
                                | u == u2 = u1 : amigosDe (us, rs, ps) u
                                | otherwise = amigosDe (us, rs, ps) u

{-
describir qué hace la función: Utiliza la función auxiliar longitud para contar la cantidad de elementos de
amigosDe el Usuario 'u' en la RedSocial 'red' pasados como parámetro.
-}
cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos red u = longitud (amigosDe red u)

{-
describir qué hace la función: Cuando se le pasa una RedSocial cuya lista de Usuarios sólo tiene un Usuario,
devuelve este mismo Usuario. Cuando la lista de Usuarios tiene más de un Usuario, entonces se posiciona en el
primero de la lista ('u') y utiliza la funcion cantidadDeAmigos para comparar la cantidad de amigos de 'u' con
la cantidad de amigos del usuarioConMasAmigos de la misma RedSocial, pero sin el primer usuario 'u', dando lugar
a dos posibilidades:
        1- Si 'u' tiene una cantidadDeAmigos mayor o igual a la del usuarioConMasAmigos de la cola, entonces
        la función devuelve a 'u'.
        2- Si existe un Usuario con más amigos que 'u', entonces se devuelve la función, esta vez sin 'u',
        repitiendo el proceso hasta encontrar el Usuario cuya cantidadDeAmigos sea la más grande.
-}
usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos ([u], _, _) = u
usuarioConMasAmigos (u:us, rs, ps) | cantidadDeAmigos (u:us, rs, ps) u >= cantidadDeAmigos (us, rs, ps) (usuarioConMasAmigos (us, rs, ps)) = u
                                   | otherwise = usuarioConMasAmigos (us, rs, ps)

{-
describir qué hace la función: De manera recursiva, recorre la lista de Usuarios de la RedSocial pasada como
parámetro. Si la cantidadDeAmigos del primer Usuario 'u' de la lista es mayor a 1000000, la función finaliza y devuelve
True; de lo contrario, se aplica la misma función estaRobertoCarlos a la RedSocial, esta vez sin el primer Usuario 'u'.
Si no encuentra un caso True, la función recorre la lista de Usuarios hasta que sólo tenga un elemento, entonces
evaluará la condición una última vez sobre este elemento, y en caso de no cumplirse, finalmente devolverá False.
-}
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos ([u], rs, ps) = cantidadDeAmigos ([u], rs, ps) u > 1000000
estaRobertoCarlos (u:us, rs, ps) | cantidadDeAmigos (u:us, rs, ps) u > 1000000 = True
                                 | otherwise = estaRobertoCarlos (us, rs, ps)

{-
describir qué hace la función: Esta función recorre de manera recursiva la lista de Publicaciones de la RedSocial
pasada como parámetro para crear una lista. Cuando se posiciona en la primera Publicación de la lista, evalúa usando
una función auxiliar si el usuarioDePublicacion coincide con el Usuario pasado como parámetro, al cual agregará a la
lista en caso de cumplirse la condición y continuará con la recursión en el resto de Publicaciones. En caso de no
coincidir el Usuario con el autor de la Publicación, entonces continuará la recursión sin agregar nada a la lista.
La función finaliza cuando recibe a la RedSocial con la lista de Publicaciones vacía, lo que devuelve simplemente
una lista vacía.
-}
publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe (_, _, []) _ = []
publicacionesDe (us, rs, p:ps) u | usuarioDePublicacion p == u = p : publicacionesDe (us, rs, ps) u
                                 | otherwise = publicacionesDe (us, rs, ps) u

{-
describir qué hace la función: Funciona de manera similar al ejercicio anterior. Recorre la lista de publicaciones,
utilizando las funciones auxiliares pertenece y likesDePublicacion para comprobar si el usuario 'u' está en la lista
de Usuarios a los que les gustó la primera Publicación de la lista de Publicaciones, dando lugar a dos posiblidades:
        1- Si esta primera Publicación 'p' cumple la condición, entonces la agrega a una lista que contendrá al resto
        de Publicaciones que cumplan la condición, aplicando la misma función a la RedSocial, pero esta vez con la tail
        de la lista de Publicaciones.
        2- Si la 'p' evaluada no cumple la condición, entonces la ignora y devuelve el resultado de aplicar la misma función
        sobre la RedSocial con la tail de la lista de Publicaciones.
La recursión finaliza cuando se recibe a la RedSocial con la lista de Publicaciones vacía, lo que sin importar el 'u'
devolverá una lista vacía.
-}
publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA (_, _, []) _ = []
publicacionesQueLeGustanA (us, rs, p:ps) u | pertenece u (likesDePublicacion p) = p : publicacionesQueLeGustanA (us, rs, ps) u
                                           | otherwise = publicacionesQueLeGustanA (us, rs, ps) u

{-
describir qué hace la función: Utiliza la función publicacionesQueLeGustanA para obtener las listas de Publicaciones que le
gustan a los usuarios 'u1' y 'u2' pasados como parámetro, y utiliza la función auxiliar mismosElementos para comprobar que
estas dos listas obtenidas contengan las mismas Publicaciones, sin importar el órden en el que estén dadas ambas listas.
-}
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones red u1 u2 = mismosElementos (publicacionesQueLeGustanA red u1) (publicacionesQueLeGustanA red u2)

{-
describir qué hace la función: En primer lugar, verifica que el Usuario 'u1' pasado como parámetro tenga al menos una publicación,
ya que de lo contrario no será un candidato válido para tener un seguidor fiel, por lo que devolverá False.
Luego, la función utilizará la función auxiliar perteneceLista para verificar que la lista de Publicaciones de 'u1' (publicacionesDe)
esté contenida en la lista de Publicaciones que le gustan (publicacionesQueLeGustanA) a 'u2', que será el primer elemento de la
lista de Usuarios de la RedSocial pasada como parámetro. Si 'u1' es distinto de 'u2' y se cumple la condición antes mencionada, entonces
'u2' califica como seguidor fiel de 'u1', y la función devuelve True; si una de las dos condiciones no se cumple, entonces se aplica
de manera recursiva la misma función (tieneUnSeguidorFiel), con el mismo Usuario 'u1' y la misma RedSocial, sólo que esta vez omitiendo
el primer Usuario 'u2' de la lista de Usuarios (se aplica tail en la lista de Usuarios).
Si la función no encuentra un candidato que cumpla las condiciones para ser un seguidor fiel, recorrerá la lista de Usuarios hasta
obtener a la lista vacía, con lo que devolverá False ya que se habrán acabado los candidatos.
-}
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel ([], _, _) _ = False
tieneUnSeguidorFiel (u2:us, rs, ps) u1 | publicacionesDe (u2:us, rs, ps) u1 == [] = False --Esta línea verifica que el Usuario parámetro tenga al menos una publicación.
                                       | u1 /= u2 && perteneceLista (publicacionesDe (u2:us, rs, ps) u1) (publicacionesQueLeGustanA (u2:us, rs, ps) u2) = True
                                       | otherwise = tieneUnSeguidorFiel (us, rs, ps) u1

-- describir qué hace la función: .....
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos = undefined


-- Funciones auxiliares:

{-
describir qué hace la función: Utiliza recursión para contar la cantidad de elementos de una lista, por cada
elemento x, suma 1 a la cantidad de elementos de esa misma lista pero sin el x, hasta llegar a una lista vacía
con la que finaliza la recursión. 
-}
longitud :: [t] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

{-
describir qué hace la función: Utiliza recursión para recorrer la Lista pasada como parámetro, en cada iteración
evalúa si el primer elemento de la Lista ('x') coincide con el elemento pasado como parámetro ('e'). Si la condición
se cumple para alguno de los elementos de la Lista, devuelve True; si no se cumple, recorrerá la Lista hasta que se
encuentre vacía, con lo que devolverá False.
-}
pertenece :: (Eq t) => t -> [t] -> Bool
pertenece _ [] = False
pertenece e (x:xs) | e == x = True
                   | otherwise = pertenece e xs

{-
describir qué hace la función: Dadas dos listas 'l1' y 'l2', recorre 'l1' evaluando si cada elemento pertenece
a 'l2': Si un elemento 'x' de 'l1' no pertenece a 'l2', se acaba la función y devuelve False; si no encuentra
un elemento que no pertenezca a 'l2', continúa la recursión hasta que la lista 'l1' se encuentre vacía, con lo
que devolverá True sin importar el contenido de 'l2'.
-}
perteneceLista :: (Eq t) => [t] -> [t] -> Bool
perteneceLista [] _ = True
perteneceLista (x:xs) l2 | pertenece x l2 = perteneceLista xs l2
                         | otherwise = False

{-
describir qué hace la función: Dadas dos listas 'l1' y 'l2', utiliza la función perteneceLista para comprobar si se
cumplen al mismo tiempo que 'l1' pertenece a 'l2' y vicecersa.
-}
mismosElementos :: (Eq t) => [t] -> [t] -> Bool
mismosElementos l1 l2 = perteneceLista l1 l2 && perteneceLista l2 l1