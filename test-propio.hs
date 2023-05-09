import Test.HUnit
import Solucion

main = runTestTT todosLosTest
run1 = runTestTT ejercicio1
run2 = runTestTT ejercicio2
run3 = runTestTT ejercicio3
run4 = runTestTT ejercicio4
run5 = runTestTT ejercicio5

todosLosTest = test [
    ejercicio1,
    ejercicio2,
    ejercicio3,
    ejercicio4,
    ejercicio5
    ]

ejercicio1 = test [
    "Caso 1: lista de Usuarios vacía" ~: (nombresDeUsuarios redSinUsuarios) ~?= [],
    "Caso 2: lista de Usuarios sin nombres repetidos" ~: (nombresDeUsuarios redA) ~?= ["Iron Man", "Captain America", "Hulk"],
    "Caso 3: lista de Usuarios con nombres repetidos" ~: (nombresDeUsuarios redB) ~?= ["Hulk", "Iron Man", "Captain America"] --Nótese que devuelve la lista sin nombres repetidos.
    ]

ejercicio2 = test [
    "Caso 1: lista de Relaciones vacía" ~: (amigosDe redSinRelaciones usuario1) ~?= [],
    "Caso 2: Usuario sin amigos" ~: (amigosDe redA usuario1) ~?= [],
    "Caso 3: Usuario con amigos" ~: (amigosDe redB usuario1) ~?= [(2,"Captain America"), (3,"Hulk")]
    ]

ejercicio3 = test [
    "Caso 1: lista de Relaciones vacía" ~: (cantidadDeAmigos redSinRelaciones usuario1) ~?= 0,
    "Caso 2: Usuario sin amigos" ~: (cantidadDeAmigos redA usuario1) ~?= 0,
    "Caso 3: Usuario con amigos" ~: (cantidadDeAmigos redB usuario1) ~?= 2
    ]

ejercicio4 = test [
    "Caso 1: lista de Relaciones vacía" ~: (usuarioConMasAmigos redSinRelaciones) ~?= usuario1, --Puede devolver cualquier Usuario, porque todos tienen 0 amigos.
    "Caso 2: un Usuario tiene la mayor cantidad de amigos" ~: (usuarioConMasAmigos redB) ~?= usuario3, --Devuelve el único Usuario que tiene más amigos.
    "Caso 3: varios Usuarios tienen la mayor cantidad de amigos" ~: (usuarioConMasAmigos redA) ~?= usuario2 --Devuelve alguno de los Usuarios que tienen más amigos.
    ]

ejercicio5 = test [
    "Caso 1: lista de Usuarios vacía" ~: (estaRobertoCarlos redSinUsuarios) ~?= False,
    "Caso 2: lista de Relaciones vacía" ~: (estaRobertoCarlos redSinRelaciones) ~?= False,
    "Caso 3: Ningún Usuario tiene más de 10 amigos" ~: (estaRobertoCarlos redA) ~?= False,
    "Caso 4: Al menos un Usuario tiene más de 10 amigos" ~: (estaRobertoCarlos redC) ~?= True
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

-- Lista de Usuarios que repiten nombre:
usuario20 = (20, "Iron Man")
usuario21 = (21, "Captain America")

-- Lista de relaciones:
relacion1 = (usuario2, usuario3)
relacion2 = (usuario1, usuario2)
relacion3 = (usuario3, usuario1)
relacion4 = (usuario3, usuario20)
relacion5 = (usuario21, usuario3)
relacion6 = (usuario1, usuario4)
relacion7 = (usuario1, usuario5)
relacion8 = (usuario1, usuario6)
relacion9 = (usuario1, usuario7)
relacion10 = (usuario1, usuario8)
relacion11 = (usuario1, usuario9)
relacion12 = (usuario1, usuario10)
relacion13 = (usuario1, usuario11)
relacion14 = (usuario1, usuario12)

usuariosA = [usuario1, usuario2, usuario3]
relacionesA = [relacion1]
redA = (usuariosA,relacionesA,[])

usuariosB = [usuario1, usuario2, usuario3, usuario20, usuario21]
relacionesB = [relacion2, relacion3, relacion4, relacion5]
redB = (usuariosB, relacionesB, [])

usuariosC = [usuario1, usuario2, usuario3, usuario4, usuario5, usuario6, usuario7, usuario8, usuario9, usuario10, usuario11, usuario12]
relacionesC = [relacion2, relacion3, relacion6, relacion7, relacion8, relacion9, relacion10, relacion11, relacion12, relacion13, relacion14]
redC = (usuariosC, relacionesC, [])

redSinUsuarios = ([], [], [])
redSinRelaciones = (usuariosA, [], [])