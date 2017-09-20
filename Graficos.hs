module Graficos where

import Control.Lens
import Graphics.Gloss

import Basico

-- Traça os pontos que limitam o jogo
limites :: Int -> Int -> [Ponto]
limites l a = let xm = l `div` 2
                  ym = a `div` 2
              in [ (-xm,y) | y <- [(-ym)..(ym)] ] ++
                 [ (x,ym)  | x <- [(-xm)..(xm)] ] ++
                 [ (xm,y)  | y <- [(-ym)..(ym)] ] ++
                 [ (x,-ym) | x <- [(-xm)..(xm)] ]

-- Unidade básica do jogo
bloco :: Ponto -> Picture
bloco (x,y) = translate (fromIntegral x * fst tamBloco)
                        (fromIntegral y * snd tamBloco) $
                        rectangleSolid (fst tamBloco) (snd tamBloco)

-- Desenha conjunto de blocos que compoem a cobra
desenharCobra :: Estado -> [Picture]
desenharCobra s = map (Color green) $ map bloco $ view jogador s

-- Desenha conjunto de blocos que definem o espaço do jogo
desenharQuadro :: Estado -> [Picture]
desenharQuadro s = map bloco $ view quadro s

-- Desenha um único bloco que representa a fruta
desenharFruta :: Estado -> [Picture]
desenharFruta s = [Color red $ bloco $ view alvo s]

-- Renderiza todo o plano do jogo
renderizar estado = pictures $ concat $ map ($estado) [ desenharCobra
                                                      , desenharQuadro
                                                      , desenharFruta ]