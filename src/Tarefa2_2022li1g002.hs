{- |
Module      : Tarefa2_2022li1g002
Description : Geração contínua de um mapa
Copyright   : João d'Araújo Dias Lobo <a104356@alunos.uminho.pt>
              Rita da Cunha Camacho <a104439@alunos.uminho.pt>

Módulo para a realização da Tarefa 2 do projeto de LI1 em 2022/23.
-}
module Tarefa2_2022li1g002 where

import LI12223
import System.Random ( mkStdGen, Random(randoms) )

{- | A função __não recursiva__ 'estendeMapa' recebe um mapa e uma seed /(um número de 1 a 100 usado para gerar os números aleatórios)/.

=== Funcionamento da aleatoriedade:
Através da seed recebida como argumento, a função 'obterRandoms' gera uma lista com números de 19 dígitos.
O tamanho desta lista corresponde à largura do mapa mais uma unidade. A 'head' da lista é usada para gerar o terreno, já os restantes elementos são utilizados para gerar os obstáculos.

=== Exemplo de utilização:

>>>estendeMapa (Mapa 5 [(Relva, [Arvore, Nenhum, Arvore, Nenhum, Arvore]),(Estrada (-1), [Nenhum, Nenhum, Nenhum, Carro, Carro]),(Relva, [Arvore, Nenhum, Nenhum, Arvore, Arvore])])
Mapa 5 [(Relva, [Arvore, Nenhum, Arvore, Nenhum, Arvore]),(Estrada (-1), [Nenhum, Nenhum, Nenhum, Carro, Carro]),(Relva, [Arvore, Nenhum, Nenhum, Arvore, Arvore]),(Rio, [Nenhum, Tronco, Tronco, Nenhum, Tronco])]
-}
estendeMapa :: Mapa -- ^Mapa antecedente da nova linha.
    -> Int -- ^Seed.
    -> Mapa -- ^Mapa com a nova linha.
estendeMapa (Mapa l to) seed =
    let randoms = obterRandoms seed (1+l)
        terreno = gerarTerreno (head randoms) (proximosTerrenosValidos (Mapa l (reverse to)) (head randoms))
        obstaculos = gerarObstaculos (tail randoms) l terreno [] in
            Mapa l ((terreno, obstaculos):to)

{- | A função __recursiva__ 'gerarObstaculos' recebe a lista de inteiros criada pela função auxiliar 'obterRandoms', a largura do mapa, o terreno da nova linha e os possíveis obstáculos para o mesmo. A função irá devolver uma lista com todos os obstáculos a introduzir nessa nova linha, de um certo tipo de terreno.-}
gerarObstaculos :: [Int] -- ^Lista de números gerados aleatoriamente. 
    -> Int -- ^Largura do mapa que irá ser prolongado. 
    -> Terreno -- ^Tipo de terreno para o qual se gerará obstáculos.
    -> [Obstaculo] -- ^Lista com os tipos de obstáculos possíveis para um certo tipo de terreno.
    -> [Obstaculo] -- ^Obstáculos gerados específicos para a nova linha.
gerarObstaculos [] _ _ obs = obs 
gerarObstaculos (h:ss) l t o = let obsValidos = (proximosObstaculosValidos l (t, o)); obs = obsValidos !! (mod h (length obsValidos)) in
        gerarObstaculos ss l t (o++[obs])

{- | A função __não recursiva__ 'gerarTerreno' recebe um inteiro, a cabeça da lista de inteiros gerados aleatoriamente pela função auxiliar 'obterRandoms', e ainda uma lista com todos os tipos de terreno possíveis. Assim, a função irá devolver um tipo de terreno, o qual será aplicado na nova linha a adicionar ao mapa inicial.-}
gerarTerreno :: Int -- ^Primeiro número gerado que irá ser aplicado para descobrir qual o tipo de terreno escolhido aleatoriamente.
    -> [Terreno] -- ^Lista com os tipos de terreno possíveis da qual se escolherá um aleatoriamente.
    -> Terreno -- ^Terreno escolhido aleatoriamente.
gerarTerreno s l = (l !! (mod s (length l)))

{- | A função __não recursiva__ 'obterRandoms' recebe um inteiro, a Seed, e outro inteiro, a soma de 1 unidade ao valor da largura do mapa a prolongar. Com estes valores, a função ao ser executada irá devolver uma lista de inteiros gerados aleatoriamente, que serão utilizados nas funções 'gerarTerreno' e 'gerarObstaculos'.-}
obterRandoms :: Int -- ^Seed.
    -> Int -- ^Número inteiro obtido através da soma de 1 unidade à largura do mapa a prolongar.
    -> [Int] -- ^Lista de números gerados aleatoriamente.
obterRandoms seed n = take n $ randoms (mkStdGen seed)

{- | A função __não recursiva__ 'proximosTerrenosValidos' recebe o mapa a prolongar e irá devolver uma lista de terrenos válidos para a nova linha, tendo já sido previamente aplicadas as condições necessárias do jogo, analisadas pelas funções auxiliares 'rioAux', 'estradaAux' e 'relvaAux'.

=== Exemplo de utilização:
>>>proximosTerrenosValidos (Mapa 5 [(Relva, [Arvore, Nenhum, Arvore, Nenhum, Arvore]),(Estrada (-1), [Nenhum, Nenhum, Nenhum, Carro, Carro]),(Relva, [Arvore, Nenhum, Nenhum, Arvore, Arvore])])
[Rio 0, Estrada 0, Relva]
-}
proximosTerrenosValidos :: Mapa -- ^Mapa antecedente à nova linha, ou seja, a prolongar.
    -> Int -- ^Número pseudo aleatório que vai ser usado para gerar uma velocidade (para terrenos rio e estrada).
    -> [Terreno] -- ^Lista de terrenos válidos que podem pertencer à nova linha a adicionar.
proximosTerrenosValidos (Mapa _ l) r = 
    let ultTerreno = fst (last l)
        velocidade = geraVelocidade r in
        case ultTerreno of
            Rio x ->  if x < 0 then rioAux (Mapa 0 l) 0 (abs velocidade) else rioAux (Mapa 0 l) 0 (-(abs velocidade))             
            Estrada _ -> estradaAux (Mapa 0 l) 0 velocidade
            Relva -> relvaAux (Mapa 0 l) 0 velocidade

{- | A função __não recursiva__ 'geraVelocidade' é um algoritmo pseudo-aleatório para gerar a velocidade de um rio ou estrada.-}
geraVelocidade :: Int -> Velocidade
geraVelocidade r = let vel = (mod (floor (sqrt (abs (fromIntegral r)))) (c-f+1) + f) in 
    if even r then -vel else vel
    where (f,c) = velocidadeMaxMin

{- | A função __recursiva__ 'rioAux' recebe o mapa a prolongar e um inteiro, que irá funcionar como contador, devolvendo a lista de terrenos válidos para aplicar na nova linha, tendo em conta que a linha anterior é um rio e os tipos de terreno das linhas que precedem esta.-}
rioAux :: Mapa -- ^Mapa antecedente à nova linha, ou seja, a prolongar.
    -> Int -- ^Contador.
    -> Velocidade -- ^Velocidade caso seja necessária.
    -> [Terreno] -- ^Lista de terrenos válidos que podem pertencer à nova linha a adicionar quando a última linha tem como terreno um rio.
rioAux (Mapa _ []) n v = [Rio v, Estrada v, Relva]
rioAux (Mapa _ l) n v =
    let ultTerreno = fst (last l) in
        case ultTerreno of
            Rio _ -> 
                if n < 3 then rioAux (Mapa 0 (init l)) (n+1) v
                else [Estrada v, Relva]
            _ -> [Rio v, Estrada v, Relva]

{- | A função __recursiva__ 'estradaAux' recebe o mapa a prolongar e um inteiro, que irá funcionar como contador, devolvendo a lista de terrenos válidos para aplicar na nova linha, tendo em conta que a linha anterior é uma estrada e os tipos de terreno das linhas que precedem esta.-}
estradaAux :: Mapa -- ^Mapa antecedente à nova linha, ou seja, a prolongar.
    -> Int -- ^Contador.
    -> Velocidade -- ^Velocidade caso seja necessária.
    -> [Terreno] -- ^Lista de terrenos válidos que podem pertencer à nova linha a adicionar quando a última linha tem como terreno uma estrada.
estradaAux (Mapa _ []) n v = [Rio v, Estrada v, Relva]
estradaAux (Mapa _ l) n  v =
    let ultTerreno = fst (last l) in
        case ultTerreno of
            Estrada _ -> 
                if n < 4 then estradaAux (Mapa 0 (init l)) (n+1) v
                else [Rio v, Relva]
            _ -> [Rio v, Estrada v, Relva]

{- | A função __recursiva__ 'relvaAux' recebe o mapa a prolongar e um inteiro, que irá funcionar como contador, devolvendo a lista de terrenos válidos para aplicar na nova linha, tendo em conta que a linha anterior é relva e os tipos de terreno das linhas que precedem esta.-}
relvaAux :: Mapa -- ^Mapa antecedente à nova linha, ou seja, a prolongar.
    -> Int -- ^Contador.
    -> Velocidade -- ^Velocidade caso seja necessária.
    -> [Terreno] -- ^Lista de terrenos válidos que podem pertencer à nova linha a adicionar quando a última linha tem como terreno relva.
relvaAux (Mapa _ []) n v = [Rio v, Estrada v, Relva]
relvaAux (Mapa _ l) n v =
    let ultTerreno = fst (last l) in
        case ultTerreno of
            Relva ->
                if n < 4 then relvaAux (Mapa 10 (init l)) (n+1) v
                else [Rio v, Estrada v]
            _ -> [Rio v, Estrada v, Relva]

{- | A função __não recursiva__ 'proximosObstaculosValidos' recebe a largura do mapa a prolongar e uma linha do mapa, devolvendo uma lista de obstáculos possíveis para preencher a lista dos mesmos da nova linha tendo em conta o tipo de terreno da mesma, utilizando as funções auxiliares 'proximosObstaculosEstradaAux' e 'proximosObstaculosRioAux'.

=== Exemplo de utilização:
>>>proximosObstaculosValidos 5 (Relva, [Arvore, Nenhum, Arvore, Nenhum])
[Nenhum, Arvore]
-}
proximosObstaculosValidos :: Int -- ^Largura do mapa a prolongar.
    -> (Terreno, [Obstaculo]) -- ^Par constituído pelo tipo de terreno e por lista de obstáculos que descreve uma linha do mapa.
    -> [Obstaculo] -- ^Lista de obstáculos válidos para a nova linha a ser adicionada.
proximosObstaculosValidos l (terr, obs) =
    let cheio = length obs == l
        ultNenhum = length obs + 1 == l && notElem Nenhum obs in
        case terr of
            Relva -> if cheio then [] else 
                if ultNenhum then [Nenhum] else
                    [Nenhum, Arvore]
            Estrada _ -> if cheio then [] else
                if ultNenhum then [Nenhum] else if length obs + 1 == l then ultimoObstaculoCaso (Estrada 0) obs else
                    proximosObstaculosEstradaAux obs 0
            Rio _ -> if cheio then [] else 
                if ultNenhum then [Nenhum] else if length obs + 1 == l then ultimoObstaculoCaso (Rio 0) obs else
                    proximosObstaculosRioAux obs 0

{- | A função __não recursiva__ 'ultimoObstaculoCaso' foi implementada para evitar um caso de repetição de obstáculos. Quando estão a ser escolhidos os próximos obstáculos válidos para uma linha, é preciso ter em conta os obstáculos no início da mesma, pois sem esta função, como a linha se move colocando o último elemento no início da lista (velocidade positiva) ou o primeiro elemento no final da lista (velocidade negativa), em alguns casos as dimensões dos obstáculos podem ultrapassar os limites definidos sabendo que podem ser constituídos por elementos no início e no fim da lista, estando no processo de entrada na outra ponta da lista. -}
ultimoObstaculoCaso :: Terreno -- ^Terreno onde o obstáculo está a ser gerado.
    -> [Obstaculo] -- ^Estado da linha antes de adicionar o obstáculo.
    -> [Obstaculo] -- ^Lista de obstáculos válidos.
ultimoObstaculoCaso terr obs =
    case terr of
        Estrada _ -> if contaPrimeirasUltimasOcorrencias obs Carro True + contaPrimeirasUltimasOcorrencias obs Carro False >= 3 then [Nenhum] else [Nenhum,Carro] 
        _ -> if contaPrimeirasUltimasOcorrencias obs Tronco True + contaPrimeirasUltimasOcorrencias obs Tronco False >= 5 then [Nenhum] else [Nenhum,Tronco] 

{- | A função 'contaPrimeirasUltimasOcorrencias', dependendo do argumento 'Bool', calcula o número de ocorrências iniciais ou finais de um elemento numa lista. -}
contaPrimeirasUltimasOcorrencias :: Eq a => [a] -- ^Lista de um elemento genérico. 
    -> a -- ^Elemento genérico a ser contado.
    -> Bool -- ^Argumento booleano que define se se deve estudar os elementos finais ou iniciais.
    -> Int -- ^Número de ocorrências iniciais ou finais de um dado elementi 
contaPrimeirasUltimasOcorrencias [] _ _ = 0
contaPrimeirasUltimasOcorrencias (h:t) e True = if h == e then 1 + contaPrimeirasUltimasOcorrencias t e True else 0 
contaPrimeirasUltimasOcorrencias l e False = if last l == e then 1 + contaPrimeirasUltimasOcorrencias (init l) e False else 0 

{- | A função __recursiva__ 'proximosObstaculosEstradaAux' recebe uma lista de obstáculos e um inteiro, que irá funcionar como contador, devolvendo uma lista de obstáculos possíveis de acrescentar tendo em conta os obstáculos anteriores e que o terreno é uma estrada.-}
proximosObstaculosEstradaAux :: [Obstaculo] -- ^Lista de obstáculos de uma linha do mapa a prolongar se o terreno for estrada.
    -> Int -- ^Contador.
    -> [Obstaculo] -- ^Lista de obstáculos possíveis para uma estrada verificando as condições necessárias exigidas.
proximosObstaculosEstradaAux _ 3 = [Nenhum]
proximosObstaculosEstradaAux [] _ = [Nenhum, Carro]
proximosObstaculosEstradaAux l n = if last l == Carro then proximosObstaculosEstradaAux (init l) (n+1) else [Nenhum, Carro]

{- | A função __recursiva__ 'proximosObstaculosEstradaAux' recebe uma lista de obstáculos e um inteiro, que irá funcionar como contador, devolvendo uma lista de obstáculos possíveis de acrescentar tendo em conta os obstáculos anteriores e que o seu terreno será um rio.-}
proximosObstaculosRioAux :: [Obstaculo] -- ^Lista de obstáculos de uma linha do mapa a prolongar se o terreno for rio.
    -> Int -- ^Contador.
    -> [Obstaculo] -- ^Lista de obstáculos possíveis para um rio verificando as condições necessárias exigidas.
proximosObstaculosRioAux _ 5 = [Nenhum]
proximosObstaculosRioAux [] _ = [Nenhum, Tronco]
proximosObstaculosRioAux l n = if last l == Tronco then proximosObstaculosRioAux (init l) (n+1) else [Nenhum, Tronco]

velocidadeMaxMin = (1, 3)