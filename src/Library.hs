module Library where
import PdePreludat

-- Dado en el enunciado
cambiarElemento :: Number -> a -> [a] -> [a]
cambiarElemento posicion elemento lista = 
    take (posicion - 1) lista ++ [ elemento ] ++ drop posicion lista

-- 1
data Edificio = Edificio {
    pisos :: [Piso],
    valorSuperficie :: Number,
    coeficienteRobustez :: Number
}

data Piso = Piso {
    departamentos :: [Departamento]
    --numero :: Number
}

data Departamento = Departamento {
    superficie :: Number,
    habitabilidad :: Number
}

-- 2
determinarEdificio :: (Number -> Bool) -> Edificio -> Bool
determinarEdificio condicion = 
    all (condicion.cantidadDepartamentos) . pisos

cantidadDepartamentos = length . departamentos

-- 2.a
cheto :: Edificio -> Bool
cheto = determinarEdificio (==1)

-- 2.b
pajarera :: Edificio -> Bool
pajarera = determinarEdificio (>=6)

-- 2.c
piramide :: Edificio -> Bool
piramide edificio = 
    all (\(a, b) -> cantidadDepartamentos a > cantidadDepartamentos b ) paresDePisos
    where paresDePisos = zip (pisos edificio) (tail $ pisos edificio)

-- 3
departamentoMasCaro :: Edificio -> Number
departamentoMasCaro edificio =
    precio edificio . foldl1 (mayorPrecio edificio) . concatMap departamentos . pisos $ edificio

mayorPrecio edificio departamentoA departamentoB 
    | precio edificio departamentoA > precio edificio departamentoB = departamentoA
    | otherwise = departamentoB

precio edificio = 
    (coeficienteRobustez edificio *) . (valorSuperficie edificio *) . superficie 

-- 4.a
merge :: [Departamento] -> Departamento
merge departamentos = Departamento {
    superficie = sum $ map superficie departamentos,
    habitabilidad = 
        sum (map habitabilidad departamentos) / length departamentos
}

-- 4.b
split :: Number -> Departamento -> [Departamento]
split nro departamento = 
    replicate nro $ departamento {superficie  = superficie departamento / nro}

-- 5
type Catastrofe = Edificio -> Edificio
-- 5.a
incendio :: Number -> Edificio -> Edificio
incendio nroPiso edificio = 
    edificio {
        pisos = pisosNoAfectados ++ pisosAfectados,
        coeficienteRobustez = coeficienteRobustez edificio / 2  
    }
    where
        pisosNoAfectados = take (nroPiso - 1) (pisos edificio)
        pisosAfectados = map incendiarPiso (drop (nroPiso - 1) (pisos edificio))

incendiarPiso :: Piso -> Piso
incendiarPiso piso = piso {
    departamentos = map incendiarDepartamento $ departamentos piso
}

reducirHabitabilidad :: Number -> Departamento -> Departamento
reducirHabitabilidad cantidad departamento = 
    departamento {
        habitabilidad = max 0 $ habitabilidad departamento - cantidad
    }

incendiarDepartamento :: Departamento -> Departamento
incendiarDepartamento = reducirHabitabilidad 30

-- 5.b
-- Versión hecha en clase
plaga :: Number -> Number -> Edificio -> Edificio
plaga nroPiso cantidad edificio =
    edificio {
        pisos = cambiarElemento nroPiso pisoNuevo $ pisos edificio
    }
    where 
        pisoNuevo = Piso { 
            departamentos = map (reducirHabitabilidad cantidad) $ departamentos pisoViejo
        }
        pisoViejo = pisos edificio !! (nroPiso - 1)

-- Versión alternativa usando cambiarPiso
plaga' :: Number -> Number -> Edificio -> Edificio
plaga' nroPiso cantidad edificio =
    cambiarPiso nroPiso nuevosDepartamentos edificio
        where 
            nuevosDepartamentos = map (reducirHabitabilidad cantidad) $ departamentos pisoViejo
            pisoViejo = pisos edificio !! (nroPiso - 1)

cambiarPiso :: Number -> [Departamento] -> Edificio -> Edificio
cambiarPiso nroPiso nuevosDepartamentos edificio = edificio { pisos = cambiarElemento nroPiso pisoNuevo $ pisos edificio }
    where pisoNuevo = Piso { departamentos = nuevosDepartamentos }

-- 5.c
terremoto :: Number -> Edificio -> Edificio
terremoto = reducirRobustez

reducirRobustez :: Number -> Edificio -> Edificio
reducirRobustez cantidad edificio = edificio {
    coeficienteRobustez = max 0 $ coeficienteRobustez edificio - cantidad
}


-- El punto 6 lo hicimos verbal por falta de tiempo, y trato de poner lo que hablamos.
-- Tarea para el hogar: 6.c y 6.d son casi iguales a la nueva versión de plaga'... pensar qué podemos cambiar para no repetir (casi) todo
-- 6.a
ampliacion :: Number -> Number -> Edificio -> Edificio
ampliacion cantidad superficieTotal edificio = edificio { pisos = pisos edificio ++ [nuevoPiso] }
    where nuevoPiso = Piso { departamentos = split cantidad Departamento { superficie = superficieTotal, habitabilidad = 100 } }

-- 6.b
fumigacion :: Edificio -> Edificio
fumigacion edificio = edificio { pisos = map fumigarPiso $ pisos edificio }

fumigarPiso :: Piso -> Piso
fumigarPiso piso = piso { departamentos = map fumigarDepartamento $ departamentos piso }

fumigarDepartamento :: Departamento -> Departamento
fumigarDepartamento departamento
    | habitabilidad departamento < 60 = modificarHabitabilidad 20 departamento
    | otherwise = departamento

modificarHabitabilidad :: Number -> Departamento -> Departamento
modificarHabitabilidad variacion departamento = 
    departamento {
        habitabilidad = max 0 $ habitabilidad departamento + variacion
    }
-- Y ahora podríamos reemplazar la versión anterior de reducirHabitabilidad...
reducirHabitabilidad' :: Number -> Departamento -> Departamento
reducirHabitabilidad' cantidad = modificarHabitabilidad (negate cantidad)

-- 6.c
mergeEdificio :: Number -> Edificio -> Edificio
mergeEdificio nroPiso edificio = cambiarPiso nroPiso departamentosNuevos edificio
    where
        departamentosNuevos = [merge $ departamentos pisoViejo]
        pisoViejo = pisos edificio !! (nroPiso - 1)

-- 6.d
splitEdificio :: Number -> Number -> Edificio -> Edificio
splitEdificio cantidad nroPiso edificio = cambiarPiso nroPiso departamentosNuevos edificio
    where
        departamentosNuevos = init (departamentos pisoViejo) ++ split cantidad (last departamentosViejos)
        departamentosViejos = departamentos pisoViejo
        pisoViejo = pisos edificio !! (nroPiso - 1)