{-# LANGUAGE TemplateHaskell #-}

module Basico where

import Control.Lens

type Ponto = (Int, Int) -- unidade basica do jogo
type Snake = [Ponto]    -- para armazenar os pontos da cobra
type Fruta = Ponto      -- dado da posicao do alvo (a fruta, ou maca)

--  Para facilatar lidar com direcoes de movimento
data Direc = Cima | Baixo | Esq | Dir deriving (Show, Eq, Enum)

-- Função básica para inverter alguma direção
invDir :: Direc -> Direc
invDir Cima  = Baixo
invDir Baixo = Cima
invDir Esq   = Dir
invDir Dir   = Esq

-- Incrementa as coordenadas de um ponto segundo uma direção
passo :: Direc -> Ponto -> Ponto
passo Cima  (x,y) = (x,y+1)
passo Baixo (x,y) = (x,y-1)
passo Esq   (x,y) = (x-1,y)
passo Dir   (x,y) = (x+1,y)

-- Estado completo do jogo
data Estado = Estado
            { _quadro       :: [Ponto]  -- limites do tabuleiro
            , _jogador      :: Snake    -- posicao das partes da cobra
            , _pontuacao    :: Int      -- comprimento da cobra
            , _alvo         :: Fruta    -- posicao da fruta
            , _direcAtual   :: [Direc]    -- direcao atual do movimento
--            , _ultimoMove   :: Direc    -- direcao do ultimo movimento
            , _semente      :: Int      -- seed para gerar nova posicao da fruta
            } deriving (Eq)
            
makeLenses ''Estado
            
instance Show Estado where
    show e = "{quadro = [" ++ show (length (view quadro e)) ++ "] ; "
           ++ "jogador = [" ++ show (length (view jogador e)) ++ "] ; "
           ++ "pontuacao = " ++ show (view pontuacao e) ++ " ; "
           ++ "alvo = " ++ show (view alvo e) ++ " ; "
           ++ "dirs = " ++ show (view direcAtual e) ++ " ; " 
           ++ "t = " ++ show (view semente e) ++ "}"
             
-- RNG <https://rosettacode.org/wiki/Linear_congruential_generator#Haskell>
msr_rng :: Int -> Int
msr_rng = (`div` 2^16) . (\n -> (214013 * n + 2531011) `mod` 2^31)
           
-- Tamanho da janela e dos blocos
tamJanela = (300, 200) :: (Int, Int)
tamBloco = (10,10) :: (Float, Float)
