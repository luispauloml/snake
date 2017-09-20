module Main where

import Control.Lens
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
                , _direcAtual   = Dir
                , _ultimoMove   = Dir
                , _semente      = 0
                } where largura = (fst tamJanela `div` round (fst tamBloco))
                        altura  = (snd tamJanela `div` round (snd tamBloco))

-- Motor completo do jogo
glossPlay = play
{-janela-}  (InWindow  "Snake" tamJanela (20, 20))
{-cor-}     white
{-fps-}     pont2vel
{-mundo-}   estadoInicial
{-render-}  renderizar
{-evento-}  movimentoJogador
{-passo-}   atualizaJogo
  where pont2vel = round . (*20) . (/log 10) . log . fromIntegral $ view pontuacao estadoInicial

-- Atualizar estado do jogo
atualizaJogo :: Float -> Estado -> Estado
atualizaJogo dt s = (movimentoAuto dt) . detectParede 
                   . detectCauda . detectPonto $ s


-- Funcao para mover segundo entrada do jogador
movimentoJogador :: Event -> Estado -> Estado
movimentoJogador acao mundo = maybe mundo atualiza (lerUsuario acao)
  where atualiza d = if (validarDir atual d == True) && (validarDir ultim d == True)
                       then mundo {_direcAtual = d}
                       else mundo
        atual      = view direcAtual mundo
        ultim      = view ultimoMove mundo

main :: IO ()
main = glossPlay


