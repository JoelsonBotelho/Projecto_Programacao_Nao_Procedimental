module Levantar where

import Data.Time
import Funcoes_Aux
import Ficheiro
import Tipo

levantamento :: IO ()
levantamento = do 
  list<- ler_utentes_; getChar; clear;
  putStr "############################## LEVANTAMENTO ##############################\n\n\tInforme o seu número de conta\n\t»» "
  conta <- getLine
  valor <- valor_levantar
  horas_sistema <- getZonedTime --PEGAR A HORA DO SISTEMA 
  if(read valor >0) 
    then do
      if(existe_numero_conta list (read conta)) 
        then do
          if((saldo_conta (list) (read conta))>=(read valor))
            then do
              escrever_no_arquivo "utentes.txt" (remove_chars_2 (listar_utentes (depositar (list) (read conta) (-(read valor)))));
              escrever_arquivo "levantar.txt" (conta++","++valor++","++(take 10 (show horas_sistema))++","++(take 8( drop 11(show horas_sistema)))++"|") "";
              espera "\n\t\tLevantando .";
              putStrLn ("\n\t\tlevantamento feito com sucesso!\n\n"++ listar_levantamento (list) (read valor) (read conta));
          else do
            putStr ("\n\tImposivel tirar essa quantia porque o teu saldo atual é de: "++(show (saldo_conta (list) (read conta)))++"\nDESESJA LEVANTAR NOVAMENTE?\n\t1 - SIM\t2 - NÃO\n\t»»")
            opcao <- getLine
            if(opcao=="1") then levantamento else sair
        else do
          putStr ("\t\tConta não existe!!\n\tDESESJA LEVANTAR NOVAMENTE?\n\t1 - SIM\t2 - NÃO\n\t»» ")
          opcao <- getLine
          if(opcao=="1") then levantamento else sair
      else sair
  putStr "\n\tDigite ENTER para continuar\n\t "; getChar; putStrLn ""  

valor_levantar:: IO String
valor_levantar=do 
  putStr "\tInforme o valor que deseja Levantar\n\t»» "
  valor<- getLine
  if((length valor)>=4 && (eh_numero valor) && (read valor)>=1000) then return valor
    else do
      putStr "\t\tVALOR  INVALIDO!\n\t\tSó é possivel LEVANTAR  de 1000kz pra cima\n\t\tDESEJA LER NOVAMENTE?\n\t1 - SIM\t\t2 - NÃO\n\t\t»» "
      opcao<-getLine
      if(opcao=="1") then valor_levantar else return ("0")
      