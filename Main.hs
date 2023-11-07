module Main where

import Ficheiro
import Menu
import Fila_Espera
import Funcoes_Aux

main :: IO ()
main = do
          carregar_senhas_;
          carregar_levantamentos_;
          carregar_depositos_;
          carregar_utentes_;
          carregar_transferencias_;
          menu_principal;

fila :: IO ()
fila = do
          carregar_senhas_;
          carregar_senhas_v;
          senha <-ler_senhas_;
          menu_fila_espera (senha);