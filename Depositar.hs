module Depositar where

import Data.Time
import Funcoes_Aux
import Tipo
import Ficheiro

deposito :: IO ()
deposito = do
  getChar; clear; cli<- ler_utentes_;
  putStr "\n########################### DEPOSÍTO #############################\n\n\tInforme o número da conta para qual deseja depositar\n\t»» "
  conta <- getLine; valor <- valor_deposito; horas_sistema <- getZonedTime 
  if((read (valor))>0) then do
    if(not(existe_numero_conta cli (read conta))) then do
      putStr "\n\tConta não existe!!\n\tDESEJA DEPOSITAR NOVAMENTE?\n\t1 - SIM\t2 - NÃO\n\t»» "
      opcao <- getLine
      if(opcao=="1") then deposito else sair
    else do
      escrever_no_arquivo "utentes.txt" (remove_chars_2 (listar_utentes (depositar (cli) (read conta) (read valor))));
      escrever_arquivo "deposito.txt" (conta++","++valor++","++(take 10 (show horas_sistema))++","++(take 8( drop 11(show horas_sistema)))++"|") "";
      espera ("\tDepositando ."); cli2<-ler_utentes_;
      putStrLn ("\n"++(listar_utente cli2 (read conta))++"\n\tDeposito feito com sucesso");
  else do
    putStr "\n\tDeposito não efeituado\n\tNão é permitido o deposito de valores a baixa de 100kz\n\tDESEJA DEPOSITAR NOVAMENTE?\t1 - SIM\t2 - NÃO\n\t»» "
    opcao <- getLine
    if (opcao=="1") then deposito else sair;
      putStr "\n\tDigite ENTER para continuar\n\t"; getChar; putStrLn ""      

valor_deposito::IO String
valor_deposito = do 
  putStr "\tInforme o valor que deseja depositar\n\t»» "
  valor<- getLine
  if((length valor)>=3 && (eh_numero valor) && (read valor)>=100) then return valor
  else do
    putStr "\tVALOR  INVALIDO!\n\tSó é possivel DEPOSITAR de 100kz pra cima\n\tDESEJA LER NOVAMENTE?\n\t1 - SIM\t\t2 - NÃO\n\t»» "
    opcao<-getLine
    if(opcao=="1") then valor_deposito else return ("0")                    
