module Transferencia where

import Funcoes_Aux
import Ficheiro
import Tipo
import Data.Time

transferencia :: IO ()
transferencia = do 
  getChar; clear; listCli<- ler_utentes_;
  putStr "\n############################## TRANSFERÊNCIA ##############################\n\n\tInforme o seu número de conta\n\t»» "
  conta <- getLine
  putStr "\tInforme o número de conta que deseja transferir\n\t»» "
  iban <- getLine
  valor <- valor_transferir
  horas_sistema <- getZonedTime
  if(read valor>0) then transferir listCli (read conta) (read iban) (read valor) (valor++","++(take 10 (show horas_sistema))++","++(take 8( drop 11(show horas_sistema)))++"|") else putStrLn ""
  
valor_transferir:: IO String
valor_transferir=do 
  putStr "\tInforme o valor que deseja transferir\n\t»» "
  valor<- getLine
  if((length valor)>=3 && (eh_numero valor) && (read valor)>=100) then return valor
  else do
    putStr "\t\tVALOR  INVALIDO!\n\tSó é possivel TRANSFERIR de 100kz pra cima\n\t\tDESEJA LER NOVAMENTE?\n\t\t1 - SIM\t\t2 - NÃO\n\t\t»» "
    opcao<-getLine
    if(opcao=="1") then do valor_transferir else return "0"
  
depositar_transferencia :: Utentes-> Int -> Int -> Float -> String ->IO ()
depositar_transferencia listaArq conta iban valor dados = do 
  if(existe_numero_conta listaArq conta) then do
    escrever_no_arquivo "utentes.txt" (remove_chars_2 (listar_utentes (transferencia_ (listaArq) (conta) (iban) (valor))))
    escrever_arquivo "deposito.txt" ( show (iban)++","++dados) "";
    escrever_arquivo "levantar.txt" ( show (conta)++","++dados) "";
    escrever_arquivo "transferencia.txt" (show conta++","++(show iban)++","++dados) ""; 
    espera "\n\t\tTransferindo.";
    putStrLn "\n\tTranferência feita com sucesso";
  else putStr "\t\tConta não existe!!"
      
transferir :: Utentes->Int->Int->Float->String->IO ()
transferir listaArq conta iban valor dados = do 
  if(((existe_numero_conta listaArq conta) && (existe_numero_conta (listaArq) (iban))) && (conta/=iban)) then do
    if((saldo_conta (listaArq) (conta))>=valor) then do
      espera "\n\t\tVerificando O IBAN .";
      putStr (("\n\tNome: " ++(remove_chars_2 (conta_nome (listaArq) (iban))))++("    Conta: "++ show (numero_conta (listaArq) (iban)))++ ("    Valor a transferir: "++(show valor))++"\n\n\t\tConfirmar a Transferencia?\n\t\t\t1 -SIM\n\t\tQualquer tecla para cancelar\n\t»» ");
      opcao <- getLine;
      if(opcao=="1") then do depositar_transferencia listaArq (conta) (iban) (valor) dados
      else putStrLn "\t\tTranferência Cancelada"
    else do putStrLn ("\tImposivel tirar essa quantia porque o teu saldo atual é de: "++show(saldo_conta (listaArq) (conta)))
  else if(not(existe_numero_conta (listaArq) conta))  then do putStrLn "\t\tConta não existe!" else putStrLn "\t\tIBAN Invalido!!" 
  putStr "\n\tDigite ENTER para continuar\n\t"; getChar; putStrLn ""                 
