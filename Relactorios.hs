module Relactorios where

import Data.Time
import Ficheiro
import Tipo
import Funcoes_Aux
import Leitura

relactorios_cadastro::Int->IO ()
relactorios_cadastro opcao = do 
  clear; lista <- ler_utentes_;
  if(lista/=[]) then do
    if(opcao ==1) then do
      putStrLn ("######################## LISTAR TODOS CADASTROS #######################\n\n"++(todos_utentes lista)++"\n");
    else do
      putStr ("######################## LISTAR TODOS CADASTROS #######################\n\n\t1 - Hoje\t2 - Qualquer Dia\n\t»» ")
      opc <- getLine
      case opc of 
        "1" -> do
          putStr ("######################## LISTAR TODOS CADASTROS DE HOJE #######################\n\n")
          horas_sistema <- getZonedTime;
          if((n_cadastro_dia lista (take 10 (show horas_sistema)))>0) then putStrLn (utentes_dia lista (take 10 (show horas_sistema))) else putStrLn "\tLista Vazia"
        "2" -> do
          putStr ("######################## LISTAR TODOS CADASTROS #######################\n\n\t")
          dia <- ler_dia
          mes <- ler_mes
          if((n_cadastro_dia lista ("2021-"++(menor_10 mes)++"-"++(menor_10 dia)))>0) then putStrLn (utentes_dia lista ("2021-"++(menor_10 mes)++"-"++(menor_10 dia))) else putStrLn "\t\tLista Vazia"
        otherwise -> relactorios_cadastro opcao
  else putStrLn "######################## LISTAR TODOS CADASTROS #######################\n\t\tLista Vazia\n"

relactorio_deposito::Int->IO ()
relactorio_deposito opcao = do 
  clear; lista <- ler_deposito_;
  if(lista/=[]) then do
    if(opcao ==1) then do
      putStrLn ("######################## LISTAR TODOS DEPOSITOS #######################\n\n"++(listar_todos_depositos lista)++"\n");
    else do
      putStr ("######################## LISTAR TODOS DEPOSITOS #######################\n\n\t1 - Hoje\t2 - Qualquer Dia\n\t»» ")
      opc <- getLine
      case opc of 
        "1" -> do
          putStr ("######################## LISTAR TODOS DEPOSITOS DE HOJE #######################\n\n\t")
          horas_sistema <- getZonedTime;
          if((n_depositos_dia lista (take 10 (show horas_sistema)))>0) then putStrLn (listar_depositos_dia lista (take 10 (show horas_sistema))) else putStrLn "\t\tLista Vazia"
        "2" -> do
          putStr ("######################## LISTAR TODOS DEPOSITOS #######################\n\n\t")
          dia <- ler_dia
          mes <- ler_mes
          if((n_depositos_dia lista ("2021-"++ (menor_10 mes) ++"-"++ (menor_10 dia)))>0) then putStrLn (listar_depositos_dia lista ("2021-"++ (menor_10 mes)  ++"-"++ (menor_10 dia))) else putStrLn "\t\tLista Vazia"
        otherwise -> relactorio_deposito opcao
  else putStrLn "\n\tLista Vazia\n"

relactorios_levantamento::Int->IO ()
relactorios_levantamento opcao = do 
  clear
  lista <- ler_levantamentos_;
  if(lista/=[]) 
    then do
      if(opcao ==1) 
        then do
          putStr ("######################## LISTAR TODOS LEVANTAMENTOS #######################\n\n"++(listar_todos_levantamentos lista)++"\n");
        else do
          putStr ("######################## LISTAR TODOS LEVANTAMENTOS #######################\n\n\t1 - Hoje\t2 - Qualquer Dia\n\t»» ")
          opc <- getLine
          case opc of 
              "1" -> do
                putStr ("######################## LISTAR TODOS LEVANTAMENTOS DE HOJE #######################\n\n")
                horas_sistema <- getZonedTime;
                if(n_levantamentos_dia lista (take 10 (show horas_sistema)))>0 then putStrLn (listar_levantamentos_dia lista (take 10 (show horas_sistema))) else putStrLn "\n\t\tLista Vazia"
              "2" -> do
                putStr ("######################## LISTAR TODOS LEVANTAMENTOS #######################\n\n")
                dia <- ler_dia
                mes <- ler_mes
                if((n_levantamentos_dia lista ("2021-"++(menor_10 mes)++"-"++(menor_10 dia)))>0) then putStrLn (listar_levantamentos_dia lista ("2021-"++(menor_10 mes)++"-"++(menor_10 dia))) else putStrLn "\n\t\tLista Vazia"
              otherwise -> relactorios_levantamento 2
    else putStrLn "Lista Vazia"

relactorios_transferencia::Int->IO ()
relactorios_transferencia opcao = do
  clear;lista <- ler_transferencias_;
  if(lista/=[]) then do
    if(opcao ==1) then putStr ("######################## LISTAR TODOS TRANSFERÊNCIAS #######################\n\n"++ (listar_todas_transferencia lista));
      else do
        putStr ("######################## LISTAR TODOS TRANSFERÊNCIAS #######################\n\n\t1 - Hoje\t2 - Qualquer Dia\n\t»»")
        opc <- getLine
        case opc of 
          "1" -> do
            putStr ("######################## LISTAR TODOS TRANSFERÊNCIAS DE HOJE #######################\n\n")
            horas_sistema <- getZonedTime;
            if((n_transferencias_dia lista (take 10 (show horas_sistema)))>0) then putStrLn (listar_transferencia_dia lista (take 10 (show horas_sistema))) else putStrLn "\t\tLista Vazia"
          "2" -> do
            putStrLn ("######################## LISTAR TODOS TRANSFERÊNCIAS #######################\n")
            dia <- ler_dia
            mes <- ler_mes
            if((n_transferencias_dia lista ("2021-"++(menor_10 mes)++"-"++(menor_10 dia)))>0) then putStrLn (listar_transferencia_dia lista ("2021-"++(menor_10 mes)++"-"++(menor_10 dia))) else putStrLn "\n\t\tLista Vazia"
          otherwise -> relactorios_transferencia 2
  else putStrLn "Lista Vazia"