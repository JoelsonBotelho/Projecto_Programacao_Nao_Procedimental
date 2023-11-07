module Fila_Espera where

import Tipo
import Funcoes_Aux
import Ficheiro

menu_fila_espera :: Senhas -> IO ()
menu_fila_espera senha = do 
  clear; putStr "############### MENU FILA DE ESPERA ##################\n\n"
  putStrLn "\tA - Criação de conta\n\tB - Depósito\n\tC - Levantamento\n\tD - Transferência\n\tE - Extracto\n\tF - Consulta\n\tG - Sair"
  putStr "\n####################################################\n\n\tPor favor, escolha uma opção\n\t»» "
  letra <- getLine; codigo <- gerar_senha letra 1
  let vet= inserir_senha senha letra codigo
  clear;
  if(letra >= "A" && letra <= "G") then do
      putStrLn ("\n\t\tSenha : "++letra++"-"++(show codigo)++"\n\t\tAguarde a chamada\n");
      escrever_no_arquivo ("senhas.txt") (remove_chars_2 (listar_senhas vet));
      escrever_no_arquivo ("senhas_valida.txt") (remove_chars_2 (listar_senhas vet));
      putStrLn "\tDigite Enter Para Continuar";
      getChar;
      menu_fila_espera (vet)
    else sair;

gerar_senha:: String -> Int -> IO Int
gerar_senha letra codigo= do
  senha <-ler_senha_valida; listaSenh<- ler_senhas_;
  if ((existe_senha listaSenh letra codigo) || (existe_senha senha letra codigo)) then do gerar_senha (letra) (codigo +1) else do return codigo