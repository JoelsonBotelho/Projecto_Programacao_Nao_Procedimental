module Leitura where

import Tipo
import Control.Concurrent
import System.Process
import Funcoes_Aux

ler_dia :: IO String
ler_dia = do 
  putStr "\tInsira o Dia\n\t»» "
  dia <- getLine
  if(validar_data dia 1) 
    then do
      return (dia)
    else do
      putStr " \tDIA INVALIDO!\n\tDESEJA LER NOVAMENTE?\n\t1 - SIM\t2 - NÃO\n\t»» "
      opcao<-getLine
      if(opcao=="1") then do  ler_dia else return "0"

ler_mes :: IO String
ler_mes = do 
  putStr "\tInsira o mes\n\t»» "
  mes<- getLine
  if validar_data mes 2
     then do
        return mes 
     else do
        putStr " \tMÊS INVALIDO!\n\tDESEJA LER NOVAMENTE?\n\t1 - SIM\t2 - NÃO\n\t»» "
        opcao<-getLine
        if(opcao=="1") then do  ler_mes else return "0"                    

ler_bi :: Utentes -> IO String
ler_bi lista = do 
  putStr "\tInsira o número do Bilhete de Identidade\n\t»» "
  bi<- getLine
  if(bi_valido bi) && (not(existe_bi lista bi)) then return bi
    else do
      putStr "\tNÚMERO DE BILHETE INVALIDO OU JÁ EXISTE!\n\tDESEJA LER NOVAMENTE?\n\t1 - SIM\t2 - NÃO\n\t»» "
      opcao<-getLine
      if(opcao=="1") then do ler_bi lista else return "0"  

ler_telefone :: Utentes->IO String
ler_telefone lista = do 
  putStr "\tInsira o número de telefone\n\t»» "
  telefone<- getLine
  if((length telefone)==9 && (eh_numero telefone) && not(existe_telefone lista (read telefone)) ) then return telefone
    else do
      putStr "\tNÚMERO DE TELEFONE INVALIDO Ou JÁ EXISTENTE!\n\tDESEJA LER NOVAMENTE?\n\t1 - SIM\t2 - NÃO\n\t»» "
      opcao<-getLine
      if(opcao=="1") then ler_telefone lista else return "0"         

ler_ano :: IO String
ler_ano = do 
  putStr "\tInsira o ano\n\t»» "
  ano<- getLine
  if(validar_data ano 3) 
     then return ano
     else do
       putStr " \tANO INVALIDO!\n\tDESEJA LER NOVAMENTE?\n\t1 - SIM\t2 - NÃO\n\t»» "
       opcao<-getLine
       if(opcao=="1") then do  ler_ano else return "0"      

ler_sobrenome :: IO String
ler_sobrenome = do
  putStr "\tInsira o Sobrenome\n\t»» "
  sobrenome <- getLine
  if((length sobrenome)>1 && (eh_palavra sobrenome)) then return sobrenome
    else do
       putStr "\tSOBRENOME INVALIDO!\n\tDESEJA LER NOVAMENTE?\n\t1 - SIM\t2 - NÃO\n\t»» "
       opcao<-getLine
       if(opcao=="1") then ler_sobrenome else return "0"
  
ler_nome :: IO String
ler_nome = do 
  putStr "\tInsira o nome\n\t»» "
  nome<- getLine
  if((length nome) > 1 && (eh_palavra nome)) then return nome
     else do
       putStr "\tNOME INVALIDO!\n\tDESEJA LER NOVAMENTE?\n\t1 - SIM\t2 - NÃO\n\t»» "
       opcao<-getLine
       if(opcao=="1") then ler_nome else return "0"                           
