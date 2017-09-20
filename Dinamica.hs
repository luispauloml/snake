module Dinamica where

import Control.Lens
import Data.List (elem)
import Data.Maybe
import Graphics.Gloss.Interface.Pure.Game

import Basico

-- Movimenta a cobra segundo uma direção                 
moverSnake :: Direc -> Snake -> Snake
moverSnake dir snake = passo dir (head snake) : (init snake)

-- Valida direção segundo regras do movimento da cobra (d: atual / n: nova dir)
validarDir :: Direc -> Direc -> Bool
validarDir d n
    | (d == Dir  || d == Esq)   && (n == Dir  || n == Esq)   = False
    | (d == Cima || d == Baixo) && (n == Cima || n == Baixo) = False
    | otherwise = True
    
-- Associar tecla a uma direcao
lerUsuario :: Event -> Maybe Direc
lerUsuario (EventKey k s m p)
    | k == (SpecialKey KeyUp)    && s == Down = Just Cima
    | k == (SpecialKey KeyDown)  && s == Down = Just Baixo
    | k == (SpecialKey KeyLeft)  && s == Down = Just Esq
    | k == (SpecialKey KeyRight) && s == Down = Just Dir
lerUsuario evento = Nothing
    
-- Atualizar a cobrinha usnado o tempo para atualizar seed da maçã
movimentoAuto :: Float -> Estado -> Estado
movimentoAuto dt s = s { _ultimoMove = dir
                       , _jogador   = moverSnake dir snake
                       , _semente  = seed + round (1000 * dt) }
  where dir   = view direcAtual s
        snake = view jogador    s
        seed  = view semente    s
  
-- A cobra come a fruta
detectPonto :: Estado -> Estado
detectPonto s
    | (head snake) == (posFruta) = s'
    | otherwise                  = s
      where snake    = view jogador   s
            posFruta = view alvo      s
            pontos   = view pontuacao s
            tempo    = view semente   s
            fruta'   = novoAlvo s
            s'       = s { _jogador   = snake ++ [last snake]
                         , _alvo      = (fst fruta')
                         , _semente   = (snd fruta')
                         , _pontuacao = pontos + 1  }

-- Calcula nova posição da maçã
novoAlvo :: Estado -> (Fruta, Int)
novoAlvo estado = worker (fruta', seed') estado
  where worker (f, s) e
            | elem f (view jogador e) == True = novoAlvo $ e { _semente = s }
            | otherwise                       = (f, s)
        seed'    = msr_rng $ view semente estado
        fruta'   = ( mod seed' largura - (largura `div` 2)
                   , mod seed' altura  - (altura  `div` 2) )
        largura  = (fst tamJanela `div` round (fst tamBloco)) - 2
        altura   = (snd tamJanela `div` round (snd tamBloco)) - 2

-- Toca a parede
detectParede :: Estado -> Estado
detectParede s
    | (abs . fst . head $ snake) == (abs . fst . head $ area) ||
      (abs . snd . head $ snake) == (abs . snd . head $ area)  = s'
    | otherwise = s
      where snake = view jogador    s
            area  = view quadro     s
            dir   = view direcAtual s
            s'    = gameOver s 

-- Toca a pórpria cauda
detectCauda :: Estado -> Estado
detectCauda s
    | elem cabeca (tail snake) == True = s'
    | otherwise                        = s
  where snake  = view jogador s
        cabeca = head snake
        s'     = gameOver s


-- Ação quando gameover
gameOver :: Estado -> Estado
gameOver s = s { _jogador = reverse snake , _direcAtual = invDir dir }
  where snake = view jogador    s
        dir   = view direcAtual s