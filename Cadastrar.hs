module Cadastrar where

import Data.Time
import Ficheiro
import Tipo
import Funcoes_Aux
import Leitura

--Cadastrar Utente
cadastro :: IO ()
cadastro = do 
  getChar ; clear; lista <- ler_utentes_
  putStrLn "################### CADASTRAMENTO ##################\n"
  nome <- ler_nome; sobrenome <- ler_sobrenome; telefone <- ler_telefone lista; dia <-ler_dia; mes<- ler_mes; ano<- ler_ano; bi <- ler_bi lista
  let codigo = (gerar_codigo lista 1)
  horas_sistema <- getZonedTime
  if(dia =="0" || mes == "0" || nome == "0" || sobrenome == "0" || telefone =="0" || ano =="0" || bi=="0") then do
    putStr "\n\tDados Invalidos\n\tDeseja Preencher Novamente?\n\t1 - SIM\t2 - NÃO\n\t»»"
    opcao <- getLine
    if(opcao=="1") then cadastro else sair
  else do 
    let dados=(cadastrar_utente lista (codigo) nome sobrenome dia mes ano bi (read telefone) 0.0 (take 10 (show horas_sistema)))
    putStr ("\n\tNOME: "++nome++"    SOBRENOME: "++sobrenome++"    CODIGO: "++(show codigo)++"    DATA DE NASCIMENTO: "++dia++"/"++mes++"/"++ano++"    BI: "++bi++"    TELEFONE: "++telefone++" \n\tDeseja Salvar?\n\n\t1 - SIM\t\t2 - ALTERAR\n\t\tOutra tecla para terminar...\n\t»» ")
    opc <- getLine
    case opc of
      "1" -> do
        espera ("\n\t\tCadastrando .");
        escrever_no_arquivo "utentes.txt" (listar_utentes (dados)) 
        putStr "\tConta criada com sucesso\n"
      "2" -> cadastro
      _ -> sair
  putStr "\n\tDigite ENTER para continuar\n\t»» "; getChar; putStrLn ""      

gerar_codigo :: Utentes -> Int -> Int
gerar_codigo listCli codigo= if (existe_numero_conta (listCli) (codigo)) then gerar_codigo (listCli) (codigo +1) else codigo
