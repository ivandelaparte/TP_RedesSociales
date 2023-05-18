import Test.HUnit
import Solucion

main = runTestTT todosLosTest
run1 = runTestTT ejercicio1
run2 = runTestTT ejercicio2
run3 = runTestTT ejercicio3
run4 = runTestTT ejercicio4
run5 = runTestTT ejercicio5
run6 = runTestTT ejercicio6
run7 = runTestTT ejercicio7
run8 = runTestTT ejercicio8
run9 = runTestTT ejercicio9
run10 = runTestTT ejercicio10

todosLosTest = test [
    ejercicio1,
    ejercicio2,
    ejercicio3,
    ejercicio4,
    ejercicio5,
    ejercicio6,
    ejercicio7,
    ejercicio8,
    ejercicio9,
    ejercicio10
    ]

-- ACLARACIÓN: En algunos de los ejercicios, la especificación admite más de una respuesta válida (Por ejemplo, en las funciones que devuelven una lista, no importa
-- el órden de los elementos dentro de la misma, dando origen a un conjunto de respuestas donde se respetan los elementos pero varía el órden de los mismos). Teniendo
-- en cuenta que realizamos Test de Caja Negra, utilizaremos las funciones 'pertenece' y 'mismosElementos' para testear abarcando todo el abanico de respuestas posibles.

-- ejercicio1 toma como parámetro la lista de Usuarios (us), con las siguientes categorías:
--      us:
--          us tiene elementos?
--          us tiene elementos repetidos?
ejercicio1 = test [
    "Caso 1: lista de Usuarios vacía" ~: (nombresDeUsuarios redSinUsuarios) ~?= [],
    "Caso 2: lista de Usuarios sin nombres repetidos" ~: mismosElementos (nombresDeUsuarios redA) ["Iron Man", "Captain America", "Hulk"] ~?= True,
    "Caso 3: lista de Usuarios con al menos un nombre repetido" ~: mismosElementos (nombresDeUsuarios redB) ["Hulk", "Iron Man", "Captain America"] ~?= True --Nótese que devuelve la lista sin nombres repetidos.
    ]

-- ejercicio2 toma como parámetro la lista de Relaciones (rs) y el Usuario seleccionado (u), con las siguientes categorías:
--      rs:
--          rs tiene elementos?
--      u:
--          u tiene amigos?
ejercicio2 = test [
    "Caso 1: lista de Relaciones vacía" ~: (amigosDe redSinRelacionesNiPublicaciones usuario1) ~?= [],
    "Caso 2: Usuario sin amigos" ~: (amigosDe redA usuario1) ~?= [],
    "Caso 3: Usuario con al menos un amigo" ~: mismosElementos (amigosDe redB usuario1) [usuario2, usuario3] ~?= True
    ]

-- ejercicio3 toma como parámetro la lista de Relaciones (rs) y el Usuario seleccionado (u), con las siguientes categorías:
--      rs:
--          rs tiene elementos?
--      u:
--          u tiene amigos?
ejercicio3 = test [
    "Caso 1: lista de Relaciones vacía" ~: (cantidadDeAmigos redSinRelacionesNiPublicaciones usuario1) ~?= 0,
    "Caso 2: Usuario sin amigos" ~: (cantidadDeAmigos redA usuario1) ~?= 0,
    "Caso 3: Usuario con al menos un amigo" ~: (cantidadDeAmigos redB usuario1) ~?= 2
    ]

-- ejercicio4 toma como parámetro la lista de Relaciones (rs) y la cantidad de Usuarios con más amigos (ucma), con las siguientes categorías:
--      rs:
--          rs tiene elementos?
--      ucma:
--          ucma > 1?
ejercicio4 = test [
    "Caso 1: lista de Relaciones vacía" ~: pertenece (usuarioConMasAmigos redSinRelacionesNiPublicaciones) (usuarios redSinRelacionesNiPublicaciones) ~?= True, --Puede devolver cualquier Usuario, porque todos tienen 0 amigos.
    "Caso 2: un Usuario tiene la mayor cantidad de amigos" ~: (usuarioConMasAmigos redB) ~?= usuario3, --Devuelve el único Usuario que tiene más amigos.
    "Caso 3: varios Usuarios tienen la mayor cantidad de amigos" ~: pertenece (usuarioConMasAmigos redA) [usuario2, usuario3] ~?= True --Devuelve alguno de los Usuarios que tienen más amigos.
    ]

-- ejercicio5 toma como parámetro la lista de Usuarios (us), la lista de Relaciones (rs) y la cantidad de Usuarios Roberto Carlos (urc), con las siguientes categorías:
--      us:
--          us tiene elementos?
--      rs:
--          rs tiene elementos?
--      urc:
--          urc > 0?
ejercicio5 = test [
    "Caso 1: lista de Usuarios vacía" ~: (estaRobertoCarlos redSinUsuarios) ~?= False,
    "Caso 2: lista de Relaciones vacía" ~: (estaRobertoCarlos redSinRelacionesNiPublicaciones) ~?= False,
    "Caso 3: Ningún Usuario tiene más de 10 amigos" ~: (estaRobertoCarlos redA) ~?= False,
    "Caso 4: Al menos un Usuario tiene más de 10 amigos" ~: (estaRobertoCarlos redC) ~?= True
    ]

-- ejercicio6 toma como parámetro la lista de Publicaciones (ps) y el Usuario seleccionado (u), con las siguientes categorías:
--      ps:
--          ps tiene elementos?
--      u:
--          u tiene publicaciones?
ejercicio6 = test [
    "Caso 1: lista de Publicaciones vacía" ~: (publicacionesDe redSinRelacionesNiPublicaciones usuario1) ~?= [],
    "Caso 2: Usuario sin publicaciones" ~: (publicacionesDe redA usuario3) ~?= [],
    "Caso 3: Usuario con al menos una publicación" ~: mismosElementos (publicacionesDe redA usuario1) [publicacion1_1, publicacion1_2] ~?= True
    ]

-- ejercicio7 toma como parámetro la lista de Publicaciones (ps) y el Usuario seleccionado (u), con las siguientes categorías:
--      ps:
--          ps tiene elementos?
--      u:
--          u tiene likes?
ejercicio7 = test [
    "Caso 1: lista de Publicaciones vacía" ~: (publicacionesQueLeGustanA redSinRelacionesNiPublicaciones usuario1) ~?= [],
    "Caso 2: Usuario sin publicaciones likeadas" ~: (publicacionesQueLeGustanA redA usuario1) ~?= [],
    "Caso 3: Usuario con al menos una publicación likeada" ~: mismosElementos (publicacionesQueLeGustanA redA usuario3) [publicacion1_1, publicacion1_2, publicacion2_1] ~?= True
    ]

-- ejercicio8 toma como parámetro la lista de Publicaciones (ps) y los Usuarios seleccionados (u1 y u2), con las siguientes categorías:
--      ps:
--          ps tiene elementos?
--      relaciones entre u1 y u2:
--          misma cantidad de likes?
--          mismas publicaciones likeadas?
ejercicio8 = test [
    "Caso 1: lista de Publicaciones vacía" ~: (lesGustanLasMismasPublicaciones redSinRelacionesNiPublicaciones usuario1 usuario2) ~?= True, --Da verdadero ya que ambas listas de publicaciones son vacías.
    "Caso 2: u1 y u2 tienen distintas cantidades de publicaciones likeadas" ~: (lesGustanLasMismasPublicaciones redA usuario1 usuario3) ~?= False,
    "Caso 3: misma cantidad de likes, pero en distintas publicaciones" ~: (lesGustanLasMismasPublicaciones redC usuario2 usuario5) ~?= False,
    "Caso 4: misma cantidad de likes, en las mismas publicaciones" ~: (lesGustanLasMismasPublicaciones redC usuario5 usuario9) ~?= True
    ]

-- ejercicio9 toma como parámetro la lista de Usuarios (us) y el Usuario seleccionado (u), con las siguientes categorías:
--      us:
--          cantidad de elementos de us > 1?
--      u:
--          u tiene publicaciones?
--          cuántos seguidores fieles?
--          u seguidor fiel de sí mismo?
ejercicio9 = test [
    "Caso 1: u es el único Usuario" ~: (tieneUnSeguidorFiel redD usuario1) ~?= False,
    "Caso 2: u no tiene Publicaciones" ~: (tieneUnSeguidorFiel redC usuario6) ~?= False,
    "Caso 3: nadie likeó todas las publicaciones de u" ~: (tieneUnSeguidorFiel redC usuario2) ~?= False,
    "Caso 4: u likeó sus propias publicaciones, pero nadie más lo hizo." ~: (tieneUnSeguidorFiel redC usuario11) ~?= False,
    "Caso 5: Al menos un Usuario distinto de u likeó todas sus publicaciones" ~: (tieneUnSeguidorFiel redC usuario1) ~?= True
    ]

-- ejercicio10 toma como parámetro la lista de Relaciones (rs) y los Usuarios seleccionados (u1 y u2), con las siguientes categorías:
--      rs:
--          rs tiene elementos?
--      relaciones entre u1 y u2:
--          u1 y u2 son amigos?
--          existe cadena de amigos entre u1 y u2?
ejercicio10 = test [
    "Caso 1: lista de Relaciones vacía" ~: (existeSecuenciaDeAmigos redSinRelacionesNiPublicaciones usuario1 usuario2) ~?= False,
    "Caso 2: u1 y u2 son amigos" ~: (existeSecuenciaDeAmigos redA usuario2 usuario3) ~?= True,
    "Caso 3: u1 y u2 no son amigos, pero existe cadena entre ellos" ~: (existeSecuenciaDeAmigos redB usuario1 usuario20) ~?= True,
    "Caso 4: u1 y u2 no son amigos y no existe cadena entre ellos" ~: (existeSecuenciaDeAmigos redC usuario1 usuario13) ~?= False
    ]

-- Lista de Usuarios "normales":
usuario1 = (1, "Iron Man")
usuario2 = (2, "Captain America")
usuario3 = (3, "Hulk")
usuario4 = (4, "Thor")
usuario5 = (5, "Black Widow")
usuario6 = (6, "Hawkeye")
usuario7 = (7, "Scarlet Witch")
usuario8 = (8, "Vision")
usuario9 = (9, "Falcon")
usuario10 = (10, "War Machine")
usuario11 = (11, "Spiderman")
usuario12 = (12, "Captain Marvel")
usuario13 = (13, "Thanos")
usuario14 = (14, "Ebony Maw")

-- Lista de Usuarios que repiten nombre:
usuario20 = (20, "Iron Man")
usuario21 = (21, "Captain America")

-- Lista de relaciones:
relacion1_2 = (usuario1, usuario2)
relacion1_3 = (usuario3, usuario1)
relacion1_4 = (usuario1, usuario4)
relacion1_5 = (usuario1, usuario5)
relacion1_6 = (usuario1, usuario6)
relacion1_7 = (usuario1, usuario7)
relacion1_8 = (usuario1, usuario8)
relacion1_9 = (usuario1, usuario9)
relacion1_10 = (usuario1, usuario10)
relacion1_11 = (usuario1, usuario11)
relacion1_12 = (usuario1, usuario12)
relacion2_3 = (usuario2, usuario3)
relacion3_20 = (usuario3, usuario20)
relacion3_21 = (usuario21, usuario3)
relacion13_14 = (usuario13, usuario14)

-- Lista de publicaciones:
-- Publicaciones de usuario1:
publicacion1_1 = (usuario1, "Yo soy Iron Man.", [usuario2, usuario3])
publicacion1_2 = (usuario1, "Un genio, millonario, Playboy, filántropo.", [usuario3])
publicacion1_3 = (usuario1, "J.A.R.V.I.S, a veces tiene que correr antes de caminar.", [usuario1])
-- Publicaciones de usuario2:
publicacion2_1 = (usuario2, "Vengadores, ¡Unidos!", [usuario3])
publicacion2_2 = (usuario2, "Si me necesitas, allí estaré.", [])
-- Publicaciones de usuario3:
publicacion3_1 = (usuario3, "¡Hulk aplasta!", [usuario1, usuario2, usuario3, usuario5, usuario9])
publicacion3_2 = (usuario3, "Hmm... Debilucho.", [usuario3, usuario5, usuario9])
-- Publicaciones de usuario11:
publicacion11_1 = (usuario11, "Un gran poder conlleva una gran responsabilidad.", [usuario11, usuario1, usuario3])
publicacion11_2 = (usuario11, "Creo que merezco un lugar en los Vengadores.", [usuario11])

usuariosA = [usuario1, usuario2, usuario3]
relacionesA = [relacion2_3]
publicacionesA = [publicacion1_1, publicacion1_2, publicacion2_1]
redA = (usuariosA,relacionesA,publicacionesA)

usuariosB = [usuario1, usuario2, usuario3, usuario20, usuario21]
relacionesB = [relacion1_2, relacion1_3, relacion3_20, relacion3_21]
redB = (usuariosB, relacionesB, [])

usuariosC = [usuario1, usuario2, usuario3, usuario4, usuario5, usuario6, usuario7, usuario8, usuario9, usuario10, usuario11, usuario12, usuario13, usuario14]
relacionesC = [relacion1_2, relacion1_3, relacion1_4, relacion1_5, relacion1_6, relacion1_7, relacion1_8, relacion1_9, relacion1_10, relacion1_11, relacion1_12, relacion13_14]
publicacionesC = [publicacion1_1, publicacion1_2, publicacion2_1, publicacion2_2,publicacion3_1, publicacion3_2, publicacion11_1, publicacion11_2]
redC = (usuariosC, relacionesC, publicacionesC)

usuariosD = [usuario1]
relacionesD = []
publicacionesD = [publicacion1_3]
redD = (usuariosD, relacionesD, publicacionesD)

redSinUsuarios = ([], [], [])
redSinRelacionesNiPublicaciones = (usuariosA, [], [])