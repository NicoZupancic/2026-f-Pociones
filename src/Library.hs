module Library where
import PdePreludat
import GHC.Num (Num, subtract)

doble :: Number -> Number
doble numero = numero + numero

data Heroe = UnHeroe {
    vida :: Number,
    defensa :: Number,
    ataque :: Number
} deriving Show

caballero :: Heroe
caballero = UnHeroe {
    vida = 1000,
    defensa = 20,
    ataque = 50
}

pepita :: Heroe 
pepita = UnHeroe {
    vida = 1001,
    defensa = 21,
    ataque = 51
}

type Modificacion = Number -> Number

poder :: Heroe -> Number
poder heroe = 3 * defensa heroe + ataque heroe + vida heroe /2

cambiarVida :: Modificacion -> Heroe -> Heroe
cambiarVida modificacion heroe = heroe {vida = modificacion (vida heroe)} 

cambiarDefensa :: Modificacion -> Heroe -> Heroe
cambiarDefensa modificacion heroe = heroe {defensa = modificacion (defensa heroe)}

cambiarAtaque :: Modificacion -> Heroe -> Heroe
cambiarAtaque modificacion heroe = heroe {ataque = modificacion (ataque heroe)}

sumarVida :: Number -> Heroe -> Heroe
sumarVida numero  = cambiarVida (+ numero)

type Pocion = Heroe -> Heroe

pocionBase :: Pocion
pocionBase = sumarVida 10

pocionPremium :: Pocion
pocionPremium = pocionBase . pocionBase

crazyPotion :: Pocion
crazyPotion = cambiarVida (*1.33) . pocionPremium . cambiarAtaque (*2)

agua :: Pocion
agua heroe = heroe

pocionElite :: Pocion
pocionElite heroe | esPoderoso heroe = cambiarDefensa (*10) heroe
                  | otherwise        = agua heroe

esPoderoso :: Heroe -> Bool
esPoderoso heroe = poder heroe > 100

pocionArtesanal :: Number -> Pocion
pocionArtesanal cantidad = pocionBase . cambiarAtaque (/cantidad) . pocionBase

pocionArriesgada :: Pocion
pocionArriesgada = pocionBase . cambiarDefensa (\x -> 3) . crazyPotion

licuadoDePociones :: Pocion -> Pocion
licuadoDePociones pocion = pocionArtesanal 10 . pocion . pocionBase

aplicarDefensaArtesanal :: Number -> Heroe -> Heroe
aplicarDefensaArtesanal defensa = pocionArtesanal 5 . cambiarDefensa (+ defensa)

defensaSegun :: Heroe -> Number
defensaSegun heroe | ataque heroe > 100 = 50
                   | ataque heroe >= 50 && ataque heroe < 100 = 30
                   | otherwise          = 10

pocionGradual :: Pocion
pocionGradual heroe = aplicarDefensaArtesanal (defensaSegun heroe) heroe

pocionVampirica :: Pocion
pocionVampirica heroe | defensa heroe > vida heroe = (sacarDefensa . aumentarVida) heroe
                      | otherwise                  = (sacarVida . aumentarAtaque) heroe
sacarDefensa :: Pocion
sacarDefensa = cambiarDefensa (\x -> x - 20)

aumentarVida :: Pocion
aumentarVida = cambiarVida (+40)

sacarVida :: Pocion
sacarVida = cambiarVida (\x -> x - 10)

aumentarAtaque :: Pocion
aumentarAtaque = cambiarAtaque (+30)

leSirve :: Pocion -> Heroe -> Bool
leSirve pocion heroe = not (esPoderoso heroe) && esPoderoso (pocion heroe)

aQuienesLeSirve :: Pocion -> [Heroe] -> [Heroe]
aQuienesLeSirve pocion = filter (leSirve pocion)

sujetoDePrueba :: Heroe
sujetoDePrueba = UnHeroe {vida = 100, defensa = 30, ataque = 25}

potencial :: Pocion -> Number
potencial pocion = poder (pocion sujetoDePrueba) 

pocionesMasFuertes :: Number -> [Pocion] -> [Pocion]
pocionesMasFuertes valor = filter ((> valor) . potencial)

unHeroeEsFuerte :: Pocion -> Heroe -> Bool
unHeroeEsFuerte pocion heroe = poder heroe > potencial pocion

algunoEsSuficientementeFuerte :: Pocion -> [Heroe] -> Bool
algunoEsSuficientementeFuerte pocion = any (unHeroeEsFuerte pocion)
