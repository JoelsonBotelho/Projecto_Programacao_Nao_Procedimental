module Menu where

import System.Exit
import Data.Time
import Funcoes_Aux
import Ficheiro
import Leitura
import Tipo
import Levantar
import Cadastrar
import Depositar
import Transferencia
import Relactorios

menu_principal::IO ()
menu_principal = do 
  clear
  putStr ("####################### LOGIN ####################\n\n\tInforme o Nome do Utilizador\n\t»» "); nome <- getLine
  putStr("\tInforme a Senha do Utilizador\n\t»» "); senha <- getLine; putStrLn "####################################################\n"
  if(eh_usuario nome senha) then menu_atendimento
  else do 
    putStr "Acesso Negado\n\n\tPressione ENTER para Continuar!...\n\tT - Para Terminar O Programa...\n\n"; opc <- getChar
    if(opc=='t' || opc=='T') then exitSuccess else do menu_principal

menu_atendimento::IO ()
menu_atendimento = do 
  clear
  putStr "############### MENU ATENDIMENTO ##################\n\n\t1 - Atendimento Geral\n\t2 - Relatório Diário\n\t3 - Relatório geral\n\t4 - Sair\n\n########################################################\n\n\tPor favor, escolha uma opção\n\t»» "
  opcao <- getLine
  case opcao of
    "1" -> do 
      chamando;
      menu_atendimento;
    "2" -> do 
      menu_relatorio_diario; 
      menu_atendimento; 
    "3" -> do 
      menu_relatorio_geral; 
      menu_atendimento; 
    "4" -> sair;
    _  -> menu_atendimento; 
  
atendemento_senha :: IO ()
atendemento_senha = do 
  clear;cli <- ler_utentes_; putStr "############### ATENDIMENTO RÁPIDO ##################\n\n"
  putStrLn "\t1 - Criação de conta\n\t2 - Depósito\n\t3 - Levantamento\n\t4 - Transferência\n\t5 - Extracto\n\t6 - Consulta\n\t7 - Sair"
  putStr "\n####################################################\n\n\tPor favor, escolha uma opção\n\t»» "
  opcao <- getLine;
  case opcao of 
    "1" -> do
      putStrLn ("\n\tDigite ENTER para continuar");
      cadastro
      chamando
    "2" -> do{
      if(cli/=[]) then do{
        putStrLn ("\n\tDigite ENTER para continuar");
        deposito;
        };else putStrLn "\n\tNão Existe Nenhuma Conta";
      chamando;};
    "3" ->do{
      if(cli/=[]) then do{
        putStrLn ("\n\tDigite ENTER para continuar");
        levantamento;
        };else putStrLn "\n\tNão Existe Nenhuma Conta";
      chamando;};
    "4" -> do{
      if(length cli>=2) then do{
        putStrLn ("\n\tDigite ENTER para continuar");
        transferencia;
        };else putStrLn "\tConta Que Pretende Transferir ou receber não existe"; 
      chamando};
    "5" -> do
      putStrLn ("\n\tDigite ENTER para continuar");
      estrato;chamando;
    "6"-> do 
      putStrLn ("\n\tDigite ENTER para continuar");
      chamar_consultar; chamando;
    otherwise -> chamando

chamando::IO ()
chamando = do 
  clear
  putStr "############### MENU ATENDIMENTO GERAL ##################\n\n\t1 - Chamar\n\t2 - Atender Pessoas Com Prioridade\n\t3 - Sair\n\n####################################################\n\n\tPor favor, escolha uma Serviço\n\t»» "
  opcao <- getLine; senha <-ler_senhas_; cli <- ler_utentes_
  menu_atendimento_geral cli senha opcao
  
menu_atendimento_geral :: Utentes -> Senhas -> String -> IO ()
menu_atendimento_geral cli senha_list a = do 
  case a of
    "1" -> do{
      if(senha_list/=[]) then do
        espera ("\tChamando .");putStrLn ("\n\n\t\tSenha: "++(mostrar_chamada senha_list)++"\n\tDigite ENTER para continuar");
        let vet= (tail senha_list);
        case fst( head senha_list) of {
          "A" -> do{
            cadastro;
            escrever_no_arquivo ("senhas.txt") (remove_chars_2 (listar_senhas vet));
            chamando;};
          "B" -> do{
            if(cli/=[]) then do{
              deposito;
              escrever_no_arquivo ("senhas.txt") (remove_chars_2 (listar_senhas vet));
            };else putStrLn "\n\tNão Existe Nenhuma Conta";
            chamando;};
          "C" -> do{
            if(cli/=[]) then do{
              levantamento;
              escrever_no_arquivo ("senhas.txt") (remove_chars_2 (listar_senhas vet));
            };else putStrLn "\n\tNão Existe Nenhuma Conta";
            chamando};
          "D" -> do{
            if(length cli>=2) then do{
                transferencia;
                escrever_no_arquivo ("senhas.txt") (remove_chars_2 (listar_senhas vet));
            };else putStrLn "\tConta Que Pretende Transferir ou receber não existe"; 
            chamando};
          "E" -> do{
            estrato;
            escrever_no_arquivo ("senhas.txt") (remove_chars_2 (listar_senhas vet));
            chamando;};
          "F" -> do{
            chamar_consultar;
            escrever_no_arquivo ("senhas.txt") (remove_chars_2 (listar_senhas vet));
            chamando;};
          otherwise -> chamando;};
      else do
        putStrLn "\tNão Existe Senha Para O Serviço Solicitado\n\tDirige A Fila De Espera Por Favor!\n\n\tDigite Qualquer TECLA pra Continuar";
        getChar;
        chamando;};
    "2" -> atendemento_senha
    "3" ->sair
    _ -> chamando

mostrar_chamada :: Senhas -> String
mostrar_chamada ((letra,cod):xs) = letra++""++(show cod)

chamar_consultar :: IO ()
chamar_consultar = do 
  clear
  putStr "\n############################################################################\n\n\tInforme O Número da conta que deseja consultar\n\t»» "
  conta <- getLine;lista <-ler_utentes_;
  if(existe_numero_conta lista (read conta)) then do
    putStrLn ("############################# CONSULTAR ##################################\n\n\t"++(listar_utente (lista) (read conta))++"\n")
  else do
    putStr ("\tA conta: "++(conta)++" não existe!\n\tDeseja consultar Novamente?\n\t1 - SIM\t2 - NÃO\n\t»» ")
    opcao <- getLine
    case opcao of 
      "1" -> chamar_consultar
      otherwise -> sair
  putStr "\n\tDigite ENTER para continuar\n\t"; getChar; putStrLn ""       
                                         
estatistica :: Int->IO ()
estatistica opcao = do 
  clear
  trans <- ler_transferencias_; depos <- ler_deposito_;leva <- ler_levantamentos_;lista <- ler_utentes_;
  if(opcao ==1) then do
    putStr ("######################## LISTAR TODOS ESTATÍSTICAS #######################\n\n\t"++(show (length lista)++" Utentes Cadastrados\n\t")++(show (length depos)++" Depositos Feitos\n\t")++(show (length leva)++" Levantamos Feitos\n\t")++(show (length trans)++" Transferências Feitas\n\t"));
  else do
    putStr ("######################## ESTATÍSTICAS #######################\n\n\t1 - Hoje\t2 - Qualquer Dia\n\tEscolher a opção\n\t»» ")
    opc <- getLine
    case opc of 
      "1" -> do
        putStr ("######################## LISTAR TODOS ESTATÍSTICAS DE HOJE #######################\n\n");
        horas_sistema <- getZonedTime;
        putStr ("\t"++(show (n_cadastro_dia lista (take 10 (show horas_sistema)))++" Utentes Cadastrados\n\t")++(show (n_transferencias_dia trans (take 10 (show horas_sistema)))++" Transferências Feitas\n\t")++(show (n_depositos_dia depos (take 10 (show horas_sistema)))++" Depositos Feitos\n\t")++(show (n_levantamentos_dia leva (take 10 (show horas_sistema)))++" Levantamos Feitos\n\n"));
      "2" -> do
        putStr ("######################## LISTAR TODOS ESTATÍSTICAS #######################\n\n\t")
        dia <- ler_dia
        mes <- ler_mes
        if((dia/="0") && (mes/="0")) then do 
          putStr ("######################## LISTAR TODOS ESTATÍSTICAS #######################\n\n\t")
          putStr (show (n_cadastro_dia lista ("2021-"++(menor_10 mes)++"-"++(menor_10 dia)))++" Utentes Cadastrados\n\t");
          putStr (show (n_transferencias_dia trans ("2021-"++(menor_10 mes)++"-"++(menor_10 dia)))++" Transferências Feitas\n\t");
          putStr ( show(n_depositos_dia depos ("2021-"++(menor_10 mes)++"-"++(menor_10 dia)))++" Depositos Feitos\n\t");
          putStrLn ((show (n_levantamentos_dia leva ("2021-"++(menor_10 mes)++"-"++(menor_10 dia))))++" Levantamos Feitos\n");
        else estatistica 2
      otherwise -> estatistica 2

estrato::IO ()
estrato  = do 
  getChar; 
  clear
  putStr "\n######################## LISTAR TODOS ESTATÍSTICAS #######################\n\n\t1 - Mensal\t2 - Todo Estrato\n\t»» "
  opcao <- getLine; transf <- ler_transferencias_;deposi <- ler_deposito_;levanta <- ler_levantamentos_;cli <- ler_utentes_;
  if(opcao=="1" || opcao=="2") then do
    putStr "\n\tInforma O Número da Conta\n\t»» "
    conta <-getLine
    if((existe_numero_conta cli (read conta))) then do
      if(opcao == "1") then do
        horas_sistema <- getZonedTime;
        putStr ("\n"++ (n_transferencias_mes transf (read conta) (take 10 (show horas_sistema))));
        putStr ("\n"++ (n_depositos_mes deposi (read conta) (take 10 (show horas_sistema))));
        putStr ("\n"++ (n_levantamentos_mes levanta (read conta) (take 10 (show horas_sistema))));
        putStrLn ("\n\t\t\t\tSaldo Disponível: "++show (saldo_conta cli (read conta))++"\n");
      else do
        putStrLn (todas_transferencias transf (read conta));
        putStrLn (todas_depositos deposi (read conta));
        putStrLn (todos_levantamentos levanta (read conta));
        putStrLn ("\n\t\t\t\tSaldo Disponível: "++show (saldo_conta cli (read conta))++"\n");
    else do
      putStr "\n\tConta Não Existe\n\tDeseja tirar ESTRATO Novamente?\n\t1 - SIM\t2 - NÃO\n\t»» "
      opcao <- getLine
      case opcao of
        "1" -> estrato;
        "2" -> sair;
        _ -> estrato;
  else estrato;
  putStr "\tDigite ENTER para continuar"; getChar; putStrLn ""
 
menu_relatorio_diario :: IO ()
menu_relatorio_diario = do
  clear
  putStr "############### MENU RELATÓRIO DIÁRIO ##################\n\n\t1 - Criação de conta\n\t2 - Depósito\n\t3 - Levantamento\n\t4 - Transferência\n\t5 - Estatística\n\t6 - Voltar\n\n###############################################################\n\n"
  putStr "\tPor favor, escolha uma opção\n\t»» "
  opcao <- getLine
  case (opcao) of
    "1" ->do
      putStrLn "\n\tDigite ENTER Para Continuar"
      getChar
      relactorios_cadastro 2
      putStrLn "\n\tDigite ENTER Para Continuar"
      getChar
      menu_relatorio_diario
    "2" ->do
      putStrLn "\n\tDigite ENTER Para Continuar"
      getChar
      relactorio_deposito 2
      putStrLn "\n\tDigite ENTER Para Continuar"
      getChar
      menu_relatorio_diario
    "3" ->do
      putStrLn "\n\tDigite ENTER Para Continuar"
      getChar
      relactorios_levantamento 2
      putStrLn "\n\tDigite ENTER Para Continuar"
      getChar
      menu_relatorio_diario
    "4" ->do
      putStrLn "\n\tDigite ENTER Para Continuar"
      getChar
      relactorios_transferencia 2
      putStrLn "\n\tDigite ENTER Para Continuar"
      getChar
      menu_relatorio_diario
    "5" -> do
      putStrLn "\n\tDigite ENTER Para Continuar"
      getChar;estatistica 2;putStrLn "\n\tDigite ENTER Para Continuar";getChar;
      menu_relatorio_diario
    "6" -> sair
    _  -> menu_relatorio_diario
 
menu_relatorio_geral :: IO ()
menu_relatorio_geral = do
  clear
  putStr "############################## MENU RELATÓRIO GERAL #################################\n\n"
  putStrLn "\t1 - Criação de conta\n\t2 - Depósito\n\t3 - Levantamento\n\t4 - Transferência\n\t5 - Estatística\n\t6 - Voltar\n"
  putStr "####################################################\n\n"
  putStr "\tPor favor, escolha uma opção\n\t»» "
  opcao <- getLine
  case (opcao) of
    "1" ->do
      putStrLn "\n\tDigite ENTER Para Continuar"
      getChar
      relactorios_cadastro 1
      putStrLn "\n\tDigite ENTER Para Continuar"
      getChar
      menu_relatorio_geral
    "2" ->do
      putStrLn "\n\tDigite ENTER Para Continuar"
      getChar
      relactorio_deposito 1
      putStrLn "\n\tDigite ENTER Para Continuar"
      getChar
      menu_relatorio_geral
    "3" ->do
      putStrLn "\n\tDigite ENTER Para Continuar"
      getChar
      relactorios_levantamento 1
      putStrLn "\n\tDigite ENTER Para Continuar"
      getChar
      menu_relatorio_geral
    "4" ->do
      putStrLn "\n\tDigite ENTER Para Continuar"
      getChar
      relactorios_transferencia 1
      putStrLn "\n\tDigite ENTER Para Continuar"
      getChar
      menu_relatorio_geral
    "5" -> do
      putStrLn "\n\tDigite ENTER Para Continuar"
      getChar
      estatistica 1
      putStrLn "\n\tDigite ENTER Para Continuar"
      getChar
      menu_relatorio_geral
    "6" -> sair
    _ -> menu_relatorio_geral