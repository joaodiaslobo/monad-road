{- |
Module      : Tarefa1_2022li1g002
Description : Validação de um mapa
Copyright   : João d'Araújo Dias Lobo <a104356@alunos.uminho.pt>
              Rita da Cunha Camacho <a104439@alunos.uminho.pt>
 
Módulo para a realização da Tarefa 1 do projeto de LI1 em 2022/23.
-}
module Tarefa1_2022li1g002 where
 
import LI12223

{- | A função 'mapaValido' através das funções auxiliares 'mapaValidoAnaliseLinhas' e 'mapaValidoAnaliseGeral' verifica se um dado mapa não viola nenhuma das seguintes restrições:

*  Não existem obstáculos em terrenos impróprios, e.g. troncos em estradas ou relvas, árvores em rios ou estradas, etc.
*  Rios contíguos têm direcções opostas.
*  Troncos têm, no máximo, 5 unidades de comprimento.
*  Carros têm, no máximo, 3 unidades de comprimento.
*  Em qualquer linha existe, no mínimo, um “obstáculo” Nenhum. Ou seja, uma linha não pode ser composta exclusivamente por obstáculos, precisando de haver pelo menos um espaço livre.
*  O comprimento da lista de obstáculos de cada linha corresponde exactamente à largura do mapa.
*  Contiguamente, não devem existir mais do que 4 rios, nem 5 estradas ou relvas.

=== Exemplos de utilização:

Mapa exemplo representado graficamente na imagem seguinte:

![Representação do mapa](https://i.imgur.com/lZ6it9f.png)

>>> mapaValido (Mapa 5 [(Relva, [Arvore, Nenhum, Arvore, Nenhum, Arvore]),(Estrada (-1), [Nenhum, Nenhum, Nenhum, Carro, Carro]),(Relva, [Arvore, Nenhum, Nenhum, Arvore, Arvore])])
True
-}
mapaValido :: Mapa -- ^O mapa a ser analisado.
    -> Bool -- ^Resultado da função do tipo 'Bool', se 'True' o mapa é válido, se 'False' o mapa não respeita uma ou mais das regras mencionadas abaixo. 
mapaValido (Mapa _ []) = False
mapaValido (Mapa l t) = mapaValidoAnaliseLinhas (Mapa l t) && mapaValidoAnaliseGeral (Mapa l t)

{- | A função __recursiva__ 'mapaValidoAnaliseLinhas' através das funções auxiliares 'terrObsValidos', 'quantObsValida', 'existeNenhum' e 'dimensObsValida' analisa linha a linha se as regras estipuladas para um mapa ser válido se confirmam.-}
mapaValidoAnaliseLinhas :: Mapa -- ^O mapa a ser analisado.
    -> Bool -- ^Resultado da função do tipo 'Bool', se 'True' o mapa é válido, se 'False' o mapa não respeita uma ou mais das regras estipuladas.
mapaValidoAnaliseLinhas (Mapa _ []) = True
mapaValidoAnaliseLinhas (Mapa l (h:t)) = 
    let linha = snd h in 
        (terrObsValidos h && quantObsValida l linha && existeNenhum linha && dimensObsValida linha) && mapaValidoAnaliseLinhas (Mapa l t)
 
{- | A função __não recursiva__ 'mapaValidoAnaliseGeral' através das funções auxiliares 'riosContiguosVelocidades', 'contaRios', 'contaEstradas' e 'contaRelvas' verifica se as regras de repetição de terrenos são respeitadas e se os rios contíguos têm sentido inverso.-}
mapaValidoAnaliseGeral :: Mapa -- ^O mapa a ser analisado.
    -> Bool -- ^Resultado da função do tipo 'Bool', se 'True' o mapa é válido, se 'False' o mapa não respeita uma ou mais das regras estipuladas.
mapaValidoAnaliseGeral (Mapa l li) = 
    let terrenos = unzipperTerreno li in 
        riosContiguosVelocidades terrenos && contaRios terrenos 0 && contaEstradas terrenos 0 && contaRelvas terrenos 0
 
{- | A função __recursiva__ 'terrObsValidos' através da função auxiliar 'obstaculoEmTerrenoValido' verifica para uma linha se os obstáculos pertencem ao terreno da mesma.

=== Exemplos de utilização:

>>> terrObsValidos (Relva, [Arvore, Nenhum, Arvore, Arvore, Nenhum])
True

>>> terrObsValidos (Rio 2, [Tronco, Nenhum, Nenhum, Carro, Tronco])
False
-}
terrObsValidos :: (Terreno, [Obstaculo]) -- ^Tuplo que representa uma linha, composto por 'Terreno' e uma lista de 'Obstaculo'
    -> Bool -- ^Resultado da função do tipo 'Bool', se 'True' os obstáculos correspondem ao terreno indicado, se 'False' pelo menos um obstáculo não pertence ao terreno indicado.
terrObsValidos (_, []) = True
terrObsValidos (Rio v, h:t) = obstaculoEmTerrenoValido (Rio v) h && terrObsValidos (Rio v, t)
terrObsValidos (Estrada v, h:t) = obstaculoEmTerrenoValido (Estrada v) h && terrObsValidos (Estrada v, t)
terrObsValidos (Relva, h:t) = obstaculoEmTerrenoValido Relva h && terrObsValidos (Relva, t)
 
{- | A função __não recursiva__ 'obstaculoEmTerrenoValido' verifica um obstáculo está no terreno certo.-}
obstaculoEmTerrenoValido :: Terreno -- ^Terreno onde o obstáculo se encontra 
    -> Obstaculo -- ^Obstáculo a ser analisado
    -> Bool -- ^Resultado da função do tipo 'Bool', se 'True' o obstáculo está no terreno certo, se 'False' o obstáculo não está num terreno válido.
obstaculoEmTerrenoValido (Rio _) Tronco = True
obstaculoEmTerrenoValido (Rio _) Nenhum = True
obstaculoEmTerrenoValido (Estrada _) Carro = True
obstaculoEmTerrenoValido (Estrada _) Nenhum = True
obstaculoEmTerrenoValido Relva Arvore = True
obstaculoEmTerrenoValido Relva Nenhum = True
obstaculoEmTerrenoValido _ _ = False

{- | A função __recursiva__ 'riosContiguosVelocidades' através da função auxiliar 'velocidadesInversas' verifica se __todos__ os rios contíguos têm a velocidade com sentido inverso.

=== Exemplos de utilização:
>>> riosContiguosVelocidades [Rio (-2), Rio 3, Rio (-1), Rio 5]
True

>>> riosContiguosVelocidades [Rio (1), Rio 3, Rio (-1)]
False
-}
riosContiguosVelocidades :: [Terreno] -- ^Lista de terrenos, na função é filtrada para "estudar" apenas rios. 
    -> Bool -- ^Resultado da função do tipo 'Bool', se 'True' rios contíguos têm velocidades inversas, se 'False' não.
riosContiguosVelocidades [] = True
riosContiguosVelocidades ((Rio v1):(Rio v2):t) = velocidadesInversas (v1,v2) && riosContiguosVelocidades (Rio v2:t)
riosContiguosVelocidades (h:t) = riosContiguosVelocidades t

{- | A função auxiliar __não recursiva__ 'velocidadesInversas' verifica se duas velocidades têm sentidos contrários.

A função retorna o valor lógico da condição abaixo (sendo v1 e v2 os valores da velocidade de dois rios):

\[
v1 \cdot v2  < 0
\]
-}
velocidadesInversas :: (Velocidade,Velocidade) -- ^Par de velocidades.
    -> Bool -- ^Retorna 'True' se as velocidades tiverem sentido inverso.
velocidadesInversas (v1,v2) 
    | v1 * v2 < 0 = True
    | otherwise = False

{- | A função __não recursiva__ 'dimensObsValida' através das funções auxiliares 'contaTroncos' e 'contaCarros' verifica se carros e troncos têm dimensões válidas. 

=== Exemplos de utilização:
>>> dimensObsValida [Nenhum,Carro,Carro,Carro,Nenhum,Carro]
True

>>> dimensObsValida [Carro,Carro,Carro,Carro,Nenhum]
False
-}
dimensObsValida :: [Obstaculo] -- ^Lista de obstáculos.
    -> Bool -- ^Retorna 'True' se os obstáculos tiverem as dimensões certas.
dimensObsValida l = contaTroncos l 0 && contaCarros l 0

{- | A função __recursiva__ 'contaTroncos' verifica que todos os troncos presentes numa lista de obstáculos têm a dimensão válida. -}
contaTroncos :: [Obstaculo] -- ^Lista de obstáculos.
    -> Int -- ^Contador de troncos.
    -> Bool -- ^Retorna 'True' se todos os troncos tiverem dimensões válidas.
contaTroncos [] n = True
contaTroncos (Tronco:t) n
    | n + 1 > 5 = False
    | otherwise = contaTroncos t (n+1)
contaTroncos (_:t) n = contaTroncos t 0
 
{- | A função __recursiva__ 'contaCarros' verifica que todos os carros presentes numa lista de obstáculos têm a dimensão válida. -}
contaCarros :: [Obstaculo] -- ^Lista de obstáculos.
    -> Int -- ^Contador de troncos.
    -> Bool -- ^Retorna 'True' se todos os troncos tiverem dimensões válidas.
contaCarros [] n = True
contaCarros (Carro:t) n
    | n + 1 > 3 = False
    | otherwise = contaCarros t (n+1)
contaCarros (_:t) n = contaCarros t 0
 
{- | A função __recursiva__ 'existeNenhum' verifica que há pelo menos um obstáculo do tipo 'Nenhum' presente numa lista de obstáculos. -}
existeNenhum :: [Obstaculo] -- ^Lista de obstáculos
    -> Bool -- ^Retorna 'True' se existe pelo menos um elemento 'Nenhum' na lista de obstáculos.
existeNenhum l = elem Nenhum l
 
{- | A função __não recursiva__ 'quantObsValida' verifica que o número de obstáculos corresponde à largura do mapa. -}
quantObsValida :: Largura  -- ^Largura do mapa.
    -> [Obstaculo] -- ^Lista de obstáculos
    -> Bool -- ^Retorna 'True' se o número de obstáculos corresponder à largura do mapa.
quantObsValida l o = length o == l

{- | A função __recursiva__ 'contaRios' verifica que o número de rios seguidos de uma lista de terrenos não passa o limite estipulado. 
A quantidade máxima de rios contíguos é definida 'maxRios'.
-}
contaRios :: [Terreno] -- ^Lista de terrenos
    -> Int -- ^Contador de rios.
    -> Bool -- ^Retorna 'True' se a regra for respeitada.
contaRios [] n = True
contaRios ((Rio _):t) n
    | n + 1 > maxRios = False
    | otherwise = contaRios t (n+1)
contaRios (_:t) n = contaRios t 0
 
{- | A função __recursiva__ 'contaRelvas' verifica que o número de relvas seguidas de uma lista de terrenos não passa o limite estipulado. 
A quantidade máxima de relvas contíguos é definida 'maxRelvas'.
-}
contaRelvas :: [Terreno] -- ^Lista de terrenos
    -> Int -- ^Contador de relvas.
    -> Bool -- ^Retorna 'True' se a regra for respeitada.
contaRelvas [] n = True
contaRelvas (Relva:t) n
    | n + 1 > maxRelvas = False
    | otherwise = contaRelvas t (n+1)
contaRelvas (_:t) n = contaRelvas t 0
 
{- | A função __recursiva__ 'contaEstradas' verifica que o número de estradas seguidas de uma lista de terrenos não passa o limite estipulado. 
A quantidade máxima de estradas contíguas é definida 'maxEstradas'.
-}
contaEstradas :: [Terreno] -- ^Lista de terrenos
    -> Int -- ^Contador de estradas.
    -> Bool -- ^Retorna 'True' se a regra for respeitada.
contaEstradas [] n = True
contaEstradas ((Estrada _):t) n
    | n + 1 > maxEstradas = False
    | otherwise = contaEstradas t (n+1)
contaEstradas (_:t) n = contaEstradas t 0
 
{- | A função auxiliar __recursiva__ 'unzipperTerreno' retorna uma lista de terrenos retirados de uma lista de tuplos compostos por terreno e uma lista de obstáculos. 

=== Exemplos de utilização:

>>>unzipperTerreno [(Relva, [Arvore,Nenhum,Nenhum]),(Rio 2, [Tronco,Tronco,Tronco]),(Estrada 1, [Carro, Nenhum, Carro])]
[Relva,Rio 2,Estrada 1]
-}
unzipperTerreno :: [(Terreno, [Obstaculo])] -- ^Par de terreno e lista de obstáculos
    -> [Terreno] -- ^Retorna lista de terrenos
unzipperTerreno [] = []
unzipperTerreno ((ts,_):t) = ts:unzipperTerreno t

{- | Quantidade máxima de rios contíguos.-}
maxRios = 4
{- | Quantidade máxima de relvas contíguas.-}
maxRelvas = 5
{- | Quantidade máxima de estradas contíguas.-}
maxEstradas = 5