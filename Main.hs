module Main where

-- Motor principal do jogo

import Data.Maybe
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

import Basico
import Dinamica
import Graficos

-- Definindo o estado inicial do jogo
estadoInicial :: Estado
estadoInicial = Estado
                { _quadro       = limites largura altura
                , _jogador      = [ (-(largura `div` 4)+1, 0)
                                  , (-(largura `div` 4)  , 0)
                                  , (-(largura `div` 4)-1, 0)
                                  ]
                , _pontuacao    = 3
                , _alvo         = (largura `div` 4, 0)
                , _direcAtual   = [Dir,Dir]
                , _semente      = 0
                } where largura = (fst tamJanela `div` round (fst tamBloco))
                        altura  = (snd tamJanela `div` round (snd tamBloco))

-- Usando Gloss.play como motor
glossPlay = play
{-janela-}  (InWindow  "Snake" tamJanela (20, 20))
{-cor-}     white
{-fps-}     pont2vel
{-mundo-}   estadoInicial
{-render-}  renderizar
{-evento-}  atualizaDir
{-passo-}   atualizaJogo
  where pont2vel = round . (*20) . (/log 10) . log . fromIntegral 
                 $ _pontuacao estadoInicial

-- Atualizar estado do jogo
atualizaJogo :: Float -> Estado -> Estado
atualizaJogo dt s = (movimentoAuto dt) . (detectParede estadoInicial)
                  . (detectCauda estadoInicial) . detectPonto $ s


-- Funcao para atualizar direcao segundo entrada do jogador
atualizaDir :: Event -> Estado -> Estado
atualizaDir acao mundo = maybe mundo atualiza (lerUsuario acao)
  where atualiza d = if (validarDir atual d == True) && (validarDir ultim d == True)
                       then mundo {_direcAtual = [d, atual]}
                       else mundo
        atual      = head $        _direcAtual mundo
        ultim      = head $ tail $ _direcAtual mundo

main :: IO ()
main = glossPlay

