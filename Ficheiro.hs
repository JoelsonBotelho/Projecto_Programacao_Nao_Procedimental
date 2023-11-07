module Ficheiro where

import System.IO
import System.IO.Error 
import Control.Exception 
import Funcoes_Aux
import Tipo

escrever_no_arquivo::String->String->IO ()
escrever_no_arquivo caminho conteudo = do
  arquivo <- openFile caminho WriteMode;
  hPutStrLn arquivo conteudo;
  hFlush arquivo;
  hClose arquivo;
escrever_arquivo::String->String->String->IO ()
escrever_arquivo caminho valor mensagem = do
  arq <- openFile caminho AppendMode
  hPutStr arq valor
  putStr mensagem
  hFlush arq
  hClose arq
ler_utentes_ :: IO Utentes
ler_utentes_ = do
  arquivo <- openFile "utentes.txt" ReadMode;
  t<- hIsEOF arquivo;
  if(t) then do
    hClose arquivo;
    return [];
  else do
    dados <- hGetLine arquivo;
    hClose arquivo;
    return (utentes (map words (lines (remove_chars (dados)))));
  
ler_transferencias_ :: IO Transferencias
ler_transferencias_ = do
  arquivo <- openFile "transferencia.txt" ReadMode;
  t<- hIsEOF arquivo;
  if(t) then do
    hClose arquivo;
    return [];
  else do
    dados <- hGetLine arquivo;
    hClose arquivo;
    return (tranferencias (map words (lines (remove_chars (dados)))));
  
ler_deposito_ :: IO Depositos
ler_deposito_ = do
  arquivo <- openFile "deposito.txt" ReadMode;
  t<- hIsEOF arquivo;
    if(t) then do
      hClose arquivo;
      return [];
  else do
    dados <- hGetLine arquivo;
    hClose arquivo;
    return (depositos (map words (lines (remove_chars (dados)))));

ler_senhas_ :: IO Senhas
ler_senhas_ = do
  arquivo <- openFile "senhas.txt" ReadMode;
  t<- hIsEOF arquivo;
  if(t) then do
    hClose arquivo;
    return [];
  else do
    dados <- hGetLine arquivo;
    hClose arquivo;
    return (senhas (map words (lines (remove_chars (dados)))));

ler_senha_valida :: IO Senhas
ler_senha_valida = do
  arquivo <- openFile "senhas_valida.txt" ReadMode;
  t<- hIsEOF arquivo;
  if(t) then do
    hClose arquivo;
    return [];
  else do
    dados <- hGetLine arquivo;
    hClose arquivo;
    return (senhas (map words (lines (remove_chars (dados)))));

ler_levantamentos_ :: IO Levantamentos
ler_levantamentos_ = do
  arquivo <- openFile "levantamento.txt" ReadMode;
  t<- hIsEOF arquivo;
  if(t) then do
    hClose arquivo;
    return [];
  else do
    dados <- hGetLine arquivo;
    hClose arquivo;
    return (levantamentos (map words (lines (remove_chars (dados))))); 

carregar_senhas_ :: IO ()
carregar_senhas_ = do
  {catch(ler_arq) tratar_erro;}
  where
    ler_arq = do arquivo<-openFile "senhas.txt" ReadMode; hClose arquivo;  return ();
    tratar_erro erro = if(isDoesNotExistError erro) 
    then do
    {
      arq <- openFile "senhas.txt" WriteMode;
      hPutStrLn arq "";
      hClose arq;
      return ();}
    else ioError erro;
  
carregar_utentes_ :: IO ()
carregar_utentes_ = do
  {catch(ler_arq) tratar_erro;}
  where
    ler_arq = do arquivo<-openFile "utentes.txt" ReadMode; hClose arquivo; return ();
    tratar_erro erro = if(isDoesNotExistError erro) 
    then do
    {
      arq <- openFile "utentes.txt" WriteMode;
      hPutStrLn arq "";
      hClose arq;
      return ();}
    else ioError erro;
  
carregar_transferencias_ :: IO ()
carregar_transferencias_ = do
  {catch(ler_arq) tratar_erro;}
  where
    ler_arq = do arquivo<-openFile "transferencia.txt" ReadMode; hClose arquivo; return ();
    tratar_erro erro = if(isDoesNotExistError erro) 
    then do
    {
      arq <- openFile "transferencia.txt" WriteMode;
      hPutStrLn arq "";
      hClose arq;
      return ();}
    else ioError erro;
  
carregar_depositos_ ::IO ()
carregar_depositos_ = do
  {catch(ler_arq) tratar_erro;}
  where
    ler_arq = do arquivo<-openFile "deposito.txt" ReadMode; hClose arquivo; return ();
    tratar_erro erro = if(isDoesNotExistError erro) 
    then do
    {
      arq <- openFile "deposito.txt" WriteMode;
      hPutStrLn arq "";
      hClose arq;
      return ();}
    else ioError erro;
  
carregar_levantamentos_::IO ()
carregar_levantamentos_ = do
  {catch(ler_arq) tratar_erro;}
  where
    ler_arq = do arquivo<-openFile "levantar.txt" ReadMode; hClose arquivo; return ();
    tratar_erro erro = if(isDoesNotExistError erro) 
    then do
    {
      arq <- openFile "levantar.txt" WriteMode;
      hPutStrLn arq "";
      hClose arq;
      return ();}
    else ioError erro;
  
carregar_senhas_v :: IO ()
carregar_senhas_v = do
  {catch(ler_arq) tratar_erro;}
  where
    ler_arq = do arquivo<-openFile "senhas_valida.txt" ReadMode; hClose arquivo;  return ();
    tratar_erro erro = if(isDoesNotExistError erro) 
    then do
    {
      arq <- openFile "senhas_valida.txt" WriteMode;
      hPutStrLn arq "Q,1|";
      hClose arq;
      return ();}
    else ioError erro;