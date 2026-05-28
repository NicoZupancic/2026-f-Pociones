module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero
---------------------------POSTRES---------------------------
data Postre = UnPostre {
    sabores :: [String],
    peso :: Number,
    temperatura :: Number
} deriving (Show, Eq)

unBizcochoBorracho :: Postre 
unBizcochoBorracho = UnPostre {
    sabores = ["fruta", "crema"],
    peso = 100,
    temperatura = 25
}

type Hechizo = Postre -> Postre

incendio :: Hechizo 
incendio postre = postre {
    temperatura = temperatura postre + 1,
    peso = peso postre * 0.95
}

immobulus :: Hechizo
immobulus postre =  postre { temperatura = 0 }

wingardiumLeviosa :: Hechizo 
wingardiumLeviosa postre = postre {
    sabores = "concentrado" : sabores postre,
    peso = peso postre * 0.9
}

diffindo :: Number -> Hechizo
diffindo porcentaje postre = postre {
    peso = peso postre * (1 - porcentaje / 100)
}

riddikulus :: String -> Hechizo
riddikulus sabor postre = postre {
    sabores = reverse sabor : sabores postre
}

avadaKedavra :: Hechizo 
avadaKedavra postre = (immobulus postre) { sabores = []}

postresListos :: Postre -> Bool
postresListos postre = peso postre > 0 && (not . null . sabores) postre 
                       && temperatura postre > 0

mesaLista :: Hechizo -> [Postre] -> Bool
mesaLista hechizo = all postresListos . map hechizo

promedio :: [Number] -> Number
promedio [] = 0
promedio lista = sum lista / length lista

promedioPostres :: [Postre] -> Number
promedioPostres  = promedio . map peso . filter postresListos 

---------------------------MAGOS---------------------------
data Mago = UnMago {
    hechizosAprendidos :: [Hechizo],
    horrocruxes :: Number
}

aprenderHechizo :: Hechizo -> Mago -> Mago
aprenderHechizo hechizo mago = mago {
    hechizosAprendidos = hechizo : hechizosAprendidos mago
}

ganarHorrocrux :: Mago -> Mago
ganarHorrocrux mago = mago { horrocruxes = horrocruxes mago + 1 }

practicarUnHechizo :: Hechizo -> Postre -> Mago -> Mago
practicarUnHechizo hechizo postre mago | hechizo postre == avadaKedavra postre = (ganarHorrocrux . aprenderHechizo hechizo) mago
                                       | otherwise                             = aprenderHechizo hechizo mago

cantidadDeSabores :: Postre -> Hechizo -> Number
cantidadDeSabores postre hechizo = length (sabores (hechizo postre))

hechizoGanador :: Postre -> Hechizo -> Hechizo -> Hechizo
hechizoGanador postre h1 h2 
    | cantidadDeSabores postre h1 >= cantidadDeSabores postre h2 = h1
    | otherwise                                                  = h2

mejorHechizo :: Postre -> Mago -> Hechizo
mejorHechizo postre mago = foldl1 (hechizoGanador postre) (hechizosAprendidos mago)

---------------------------INFINITA MAGIA---------------------------
postreInfinita :: [Postre]
postreInfinita = repeat unBizcochoBorracho

magoInfinito :: Mago
magoInfinito = UnMago {
    hechizosAprendidos = repeat incendio,
    horrocruxes = 1
}


 