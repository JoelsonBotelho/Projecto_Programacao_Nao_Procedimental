module Funcoes_Aux where

import Tipo
import Control.Concurrent
import System.Process

clear = system "cls"

--Verificações
eh_numero:: String -> Bool
eh_numero [] = False
eh_numero [x] = x `elem` ['0'..'9']
eh_numero (x:xs) = if x `elem` ['0'..'9'] then eh_numero xs else False

eh_palavra :: String -> Bool
eh_palavra [] = False
eh_palavra [x] = x `elem` ['a'..'z'] || x `elem` ['A'..'Z']
eh_palavra (x:xs) = x `elem` ['a'..'z'] || x `elem` ['A'..'Z'] && eh_palavra xs

-- Validação do BI
bi_valido :: String -> Bool
bi_valido bi = eh_valido
  where
    bi_tamanho = (length bi) == 14
    first_numeros = eh_numero(take 9 bi)
    letras = eh_palavra(take 2 (drop 9 bi))
    last_numeros = eh_numero(drop 11 bi)
    eh_valido = bi_tamanho && first_numeros && letras && last_numeros

menor_10::String->String
menor_10 numero = if((read numero<10) && (length numero)==1) then "0"++numero else numero

--Remove alguns caracteres
remove_chars :: String->String
remove_chars [] = ""
remove_chars (x:xs) |(x==',')=['\t']++ remove_chars xs
                    |(x=='|')=['\n']++ remove_chars xs
                    |(x==' ')=['_']++ remove_chars xs
                    |(x=='\n')= remove_chars xs
                    |otherwise =[x]++ remove_chars xs
--Remove alguns caracteres
remove_chars_2 :: String->String
remove_chars_2 [] = ""
remove_chars_2 (x:xs) |(x=='[' || x==']' || x=='(' || x==')' )= remove_chars_2 xs
                   | (x=='_') = [' ']++ remove_chars_2 xs
                   |otherwise =[x]++ remove_chars_2 xs

--Sair
sair :: IO()
sair = do putStr("\tA Sair."); threadDelay 50000; putStr("."); threadDelay 50000; putStrLn(".")
espera::String->IO ()
espera text = do putStr (text); threadDelay 50000; putStr("."); threadDelay 50000; putStrLn(".")

--Validação do Usuário
eh_usuario :: String -> String -> Bool
eh_usuario nome senha = eh_valido
  where
    edvaldo = (nome == "vitena" && senha == "2022")
    joelson = (nome == "botelho" && senha == "2023")
    moises = (nome == "jorge" && senha == "2021")
    eh_valido = edvaldo || joelson || moises

--Cadastrar Utente
cadastrar_utente :: Utentes -> Int -> String -> String -> String -> String -> String -> String -> Int -> Float -> String -> Utentes
cadastrar_utente [] numero nome sobrenome dia mes ano bi telefone saldo data_cadastro = 
  [(numero, nome ++" "++ sobrenome, dia ++"/"++ mes ++"/"++ ano, bi, telefone, saldo, data_cadastro)]
cadastrar_utente lista numero nome sobrenome dia mes ano bi telefone saldo data_cadastro =
  lista ++ [(numero, nome ++" "++ sobrenome, dia ++"/"++ mes ++"/"++ ano, bi, telefone, saldo, data_cadastro)]

--Depositar
depositar :: Utentes -> Int -> Float -> Utentes
depositar ((numero, nome, data_, bi, telefone, saldo, data_cadastro):xs) numero2 valor | (numero == numero2) = (numero, nome, data_, bi, telefone, (saldo + (valor)), data_cadastro):(depositar xs numero2 valor)
                                                                                       | (numero /= numero2) = (numero, nome, data_, bi, telefone, saldo, data_cadastro):(depositar xs numero2 valor)
                                                                                       | otherwise = depositar xs numero2 valor
depositar [] _ _ = []

-- Transferência
transferencia_ :: Utentes -> Int -> Int -> Float -> Utentes
transferencia_ ((numero, nome, data_, bi, telefone, saldo, data_cadastro):xs) numero2 iban valor | (numero == numero2) = (numero, nome, data_, bi, telefone, (saldo - valor),data_cadastro):(transferencia_ xs numero2 iban valor)
                                                                                                 | (iban == numero) = (numero, nome, data_, bi, telefone, (saldo + valor),data_cadastro):(transferencia_ xs numero2 iban valor)
                                                                                                 | (numero /= numero2) = (numero, nome, data_, bi, telefone, saldo, data_cadastro):(transferencia_ xs numero2 iban valor)
                                                                                                 | otherwise = transferencia_ xs numero2 iban valor
transferencia_ [] _ _ _ = []

--Inserir 
inserir_senha :: Senhas -> String -> Int -> Senhas
inserir_senha [] letra numero = [(letra, numero)]
inserir_senha  lista letra numero = lista ++ [(letra, numero)]

--Verificar se existe
existe_senha :: Senhas -> String -> Int -> Bool
existe_senha [] _ _ = False
existe_senha ((letra, numero):xs) letra_ numero_ | ((letra, numero)==(letra_, numero_)) = True
                                                 | otherwise = existe_senha xs (letra_) (numero_)

existe_numero_conta :: Utentes -> Int -> Bool
existe_numero_conta [] numero2 = False
existe_numero_conta ((numero, nome, data_, bi, telefone, saldo, data_cadastro):xs) numero2 | (show numero2 ==  show numero) || (numero2 == numero) = True
                                                                                           | (length xs == 0) = False
                                                                                           | otherwise = existe_numero_conta xs numero2

existe_telefone :: Utentes -> Int -> Bool
existe_telefone [] telefone_ = False
existe_telefone ((numero, nome, data_, bi, telefone, saldo, data_cadastro):xs) telefone_ | (show telefone ==  show telefone_) || (telefone_ == telefone) = True
                                                                                         | (length xs == 0) = False
                                                                                         | otherwise = existe_telefone xs telefone_

existe_bi :: Utentes -> String -> Bool
existe_bi [] n_bi = False
existe_bi ((numero, nome, data_, bi, telefone, saldo, data_cadastro):xs) n_bi | (show bi ==  show n_bi) || (n_bi == bi) = True
                                                                              | (length xs == 0) = False
                                                                              | otherwise = existe_bi xs n_bi

--Pesquisar
conta_nome :: Utentes -> Int -> String
conta_nome ((numero, nome, data_, bi, telefone, saldo, data_cadastro) : xs) numero_ | ((show numero) == (show numero_))= nome :: String
                                                                                    | otherwise = conta_nome xs numero_

numero_conta :: Utentes -> Int -> Int
numero_conta ((numero, nome, data_, bi, telefone, saldo, data_cadastro): xs) numero_ | ((show numero) == (show numero_)) = numero :: Int
                                                                                     | otherwise = numero_conta xs numero_

data_nascimento :: Utentes -> Int -> String
data_nascimento ((numero, nome, data_, bi, telefone, saldo, data_cadastro): xs) numero_ | ((show numero) == (show numero_)) = data_ :: String
                                                                                        | otherwise = data_nascimento xs numero_

num_bi :: Utentes -> Int -> String
num_bi ((numero, nome, data_, bi, telefone, saldo, data_cadastro):xs) numero_ |((show numero) == (show numero_)) = bi :: String
                                                                              |otherwise = num_bi xs numero_

n_telefone :: Utentes -> Int-> Int
n_telefone ((numero, nome, data_, bi, telefone, saldo, data_cadastro):xs) numero_ |((show numero) == (show numero_)) = telefone :: Int
                                                                                  |otherwise = n_telefone xs numero_

saldo_conta :: Utentes -> Int -> Float
saldo_conta ((numero, nome, data_, bi, telefone, saldo, data_cadastro):xs) numero_ |((show numero) == (show numero_) || (numero == numero_)) = saldo :: Float
                                                                                   |otherwise = saldo_conta xs numero_

cadastro_conta :: Utentes -> Int -> String
cadastro_conta ((numero, nome, data_, bi, telefone, saldo, data_cadastro):xs) numero_ | ((show numero) == (show numero_)) = data_cadastro :: String
                                                                                      |otherwise = cadastro_conta xs numero_

--Número de cadastramento por dia
n_cadastro_dia:: Utentes -> String -> Int
n_cadastro_dia [] data_ = 0
n_cadastro_dia ((numero, nome, data_1, bi, telelefone, saldo, data_cadastro):xs) data_| (show data_ == show data_cadastro) || (data_ == data_cadastro) = 1 + n_cadastro_dia xs data_
                                                                                      | otherwise = n_cadastro_dia xs data_
--Número de transferências por dia
n_transferencias_dia :: Transferencias -> String -> Int
n_transferencias_dia [] data_= 0
n_transferencias_dia ((numero, iban, saldo, data_1, hora): xs) data_ | (data_== data_1) = 1 + n_transferencias_dia xs data_
                                                                     | otherwise = n_transferencias_dia xs data_

--Número de depositos por dia
n_depositos_dia :: Depositos -> String -> Int
n_depositos_dia [] data_= 0
n_depositos_dia ((numero, saldo, data_1, hora): xs) data_ | (data_ == data_1) = 1 + n_depositos_dia xs data_
                                                         | otherwise = n_depositos_dia xs data_
--Número de levantamentos por dia
n_levantamentos_dia :: Levantamentos -> String -> Int
n_levantamentos_dia [] data_= 0
n_levantamentos_dia ((numero, saldo, data_1 ,hora): xs) data_ | (data_ == data_1) = 1 + n_levantamentos_dia xs data_
                                                              | otherwise = n_levantamentos_dia xs data_

--Número de levantamentos por mês
n_levantamentos_mes :: Levantamentos -> Int -> String -> String
n_levantamentos_mes [] numero data_= ""
n_levantamentos_mes ((contador, saldo, data_1,hora):xs) numero data_ | ((drop 5 (take 7(data_))==  (drop 5 (take 7(data_1)))) || (drop 6 (take 7(data_))== drop 6(take 7 (data_1)))) && ((show contador == show numero) || (contador == numero)) = " \t Montante: -"++(show saldo)++" \t Data: "++ data_1 ++" \t Hora: "++ hora ++" \n"++ n_levantamentos_mes xs numero data_
                                                                     | otherwise = n_levantamentos_mes xs numero data_                                                                     

--Número de depositos por mês
n_depositos_mes :: Depositos->Int->String->String
n_depositos_mes [] numero data_= ""
n_depositos_mes ((contador, saldo, data_1,hora): xs) numero data_ | ((drop 5 (take 7(data_)) == (drop 5 (take 7(data_1)))) || (drop 6 (take 7(data_)) == drop 6(take 7 (data_1)))) && ((show contador == show numero) || (contador == numero)) = " \t Montante: +"++ (show saldo) ++" \t Data: "++ data_1 ++" \t Hora: "++ hora ++"  \n"++ n_depositos_mes xs numero data_
                                                                  | otherwise = n_depositos_mes xs numero data_

--Número de transferências por mês
n_transferencias_mes :: Transferencias -> Int -> String -> String
n_transferencias_mes [] id_t data_ = ""
n_transferencias_mes ((contador, iban, saldo, data_1, hora): xs) id_t data_ | ((drop 5 (take 7(data_)) ==  (drop 5 (take 7(data_1)))) || (drop 6 (take 7(data_)) == drop 6(take 7 (data_1)))) && ((show contador == show id_t) || (contador == id_t)) = " \t Montante: -"++ (show saldo) ++" \t Data: "++ data_1 ++" \t Hora: "++ hora ++" \n"++ n_transferencias_mes xs id_t data_
                                                                            | otherwise = n_transferencias_mes xs id_t data_

--Todas transferências
todas_transferencias :: Transferencias -> Int -> String
todas_transferencias [] numero = ""
todas_transferencias ((contador, iban, saldo, data_, hora): xs) numero |  ((show contador == show numero) || (contador == numero)) = " \t Montante: -"++ (show saldo) ++" \t Data: "++ data_ ++" \t Hora: "++ hora ++" \n"++ todas_transferencias xs numero
                                                                       | otherwise = todas_transferencias xs numero 

--Todos depositos
todas_depositos :: Depositos -> Int -> String
todas_depositos [] numero = ""
todas_depositos ((contador, saldo,data_, hora): xs) numero | ((show contador == show numero) || (contador == numero)) = " \t Montante: +"++ (show saldo) ++" \t Data: "++ data_ ++" \t Hora: "++ hora ++"  \n"++ todas_depositos xs numero
                                                           | otherwise = todas_depositos xs numero
    
--Todos levantamentos
todos_levantamentos :: Levantamentos -> Int -> String
todos_levantamentos [] numero = ""
todos_levantamentos ((contador, saldo, data_, hora): xs) numero | ((show contador == show numero) || (contador == numero)) = " \t Montante: +"++ (show saldo) ++" \t Data: "++ data_ ++" \t Hora: "++ hora ++"  \n"++ todos_levantamentos xs numero
                                                                | otherwise = todos_levantamentos xs numero

--Listar Utente
listar_utente :: Utentes -> Int -> String
listar_utente [] _ = ""
listar_utente ((numero, nome, data_, bi, telefone, saldo, data_cadastro) : xs) numero_ | ((show numero_) == (show numero) || (numero_) == (numero)) = "\tConta: "++ show(numero) ++"\tNome: "++ (remove_chars_2 nome) ++" \tData: "++ data_ ++"\tBI: "++ bi ++"\tTelefone: "++ (show telefone) ++"\tSaldo Atual: "++ (show(saldo))
                                                                                         | otherwise = listar_utente xs numero

--Listar Levantamento
listar_levantamento :: Utentes -> Float -> Int -> String
listar_levantamento [] _ _ = ""
listar_levantamento ((numero, nome, data_, bi, telefone, saldo, data_cadastro):xs) valor numero_ |((show numero_) == (show numero)) = "\tConta: "++ show(numero) ++" Nome: "++ (remove_chars_2 nome) ++" Data: "++ data_ ++" BI: "++ bi ++" Telefone: "++ (show telefone) ++" Saldo atual: "++ (show(saldo - valor)) ++"\n"
                                                                                                 | otherwise = listar_levantamento xs valor numero_

--Listar Utentes
listar_utentes :: Utentes -> String
listar_utentes [] = ""
listar_utentes ((numero, nome, data_, bi, telefone, saldo, data_cadastro):xs) | (xs/=[] || (length xs) >= 0) = show(numero) ++","++ (remove_chars_2 nome) ++","++ data_ ++","++ bi ++","++ (show telefone) ++","++(show(saldo))++","++ data_cadastro ++"|"++ listar_utentes xs
                                                                              | otherwise = listar_utentes xs

--Todos Utentes
todos_utentes :: Utentes -> String
todos_utentes []= ""
todos_utentes ((numero, nome, data_, bi, telefone, saldo, data_cadastro):xs) | (xs/=[] || (length xs)>=0) = "\tConta: "++ show(numero) ++" Nome: "++ (remove_chars_2 nome) ++" Data de Nascimento: "++ data_ ++" BI: "++ bi ++" Telefone: "++ (show telefone) ++" Saldo Atual: "++ (show(saldo)) ++" Data de Cadastro: "++ data_cadastro ++"\n"++ todos_utentes xs
                                                                             | otherwise = todos_utentes xs

--Listar Utentes por dia
utentes_dia :: Utentes -> String -> String                                         
utentes_dia [] data_= ""
utentes_dia ((numero, nome, data_1, bi, telefone, saldo, data_cadastro):xs)  data_ | (data_ == data_cadastro) ="\tConta: "++ show(numero) ++" Nome: "++ (remove_chars_2 nome) ++" Data de Nascimento: "++ data_1 ++" BI: "++ bi ++" Telefone: "++ (show telefone)++" Saldo Atual: "++ (show(saldo)) ++" Data de Cadastro: "++ data_cadastro ++"\n"++ utentes_dia xs data_
                                                                                   | otherwise = utentes_dia xs data_

--Listar Senhas por dia
listar_senhas :: Senhas -> String
listar_senhas [] = ""
listar_senhas ((letra, numero):xs) | (xs/=[] || (length xs)>=0) = letra ++","++ (show numero) ++"|"++ listar_senhas xs
                                   |(xs==[] || (length xs)==0) = letra ++","++ (show numero)
                                   | otherwise = listar_senhas xs

--Listar Transferências por dia
listar_transferencia_dia :: Transferencias -> String -> String
listar_transferencia_dia [] _ = ""
listar_transferencia_dia ((numero, iban, saldo, data_1, hora):xs) data_ | (data_ == data_1) = "\tConta: "++(show numero)++"\tIBAN: "++(show iban)++"\tValor: "++(show saldo)++"\tData: "++ data_1 ++"\tHora: "++ hora ++"\n"++ listar_transferencia_dia  xs data_
                                                                        | otherwise = listar_transferencia_dia xs data_

--Listar Todas Transferências
listar_todas_transferencia :: Transferencias -> String
listar_todas_transferencia [] = ""
listar_todas_transferencia ((numero, iban, saldo, data_, hora):xs) | (xs/=[]) || (length xs>=0) = "\tConta: "++(show numero)++"\tIBAN: "++(show iban)++"\tValor: "++(show saldo)++"\tData: "++ data_ ++"\tHora: "++ hora ++"\n"++ listar_todas_transferencia xs
                                                                   | otherwise = listar_todas_transferencia xs

--Listar Depositos por dia
listar_depositos_dia :: Depositos -> String -> String
listar_depositos_dia [] data_ = ""
listar_depositos_dia ((numero, saldo, data_1, hora):xs) data_ | ((show data_)==(show data_1))= "\tConta: "++(show numero)++"\tValor: "++(show saldo)++"\tData: "++ data_1 ++"\tHora: "++ hora ++"\n"++ listar_depositos_dia xs data_
                                                              | otherwise = listar_depositos_dia xs data_

--Listar Todos Depositos
listar_todos_depositos :: Depositos -> String
listar_todos_depositos [] = ""
listar_todos_depositos ((numero, saldo, data_1, hora):xs) | (xs/=[]) || (length xs>=0) = "\tConta: "++(show numero)++"\tValor: "++(show saldo)++"\tData: "++ data_1 ++"\tHora: "++ hora ++"\n"++ listar_todos_depositos xs
                                                          | otherwise = listar_todos_depositos xs

--Listar Levantamentos por dia
listar_levantamentos_dia :: Levantamentos -> String -> String
listar_levantamentos_dia [] _ = ""
listar_levantamentos_dia ((numero, saldo, data_1, hora):xs) data_ | (data_ == data_1) = "\tConta: "++(show numero)++"\tValor: "++(show saldo)++"\tData: "++ data_1 ++"\tHora: "++ hora ++"\n"++ listar_levantamentos_dia xs data_
                                                    | otherwise = listar_levantamentos_dia xs data_

--Listar Todos Levantamentos
listar_todos_levantamentos :: Levantamentos -> String
listar_todos_levantamentos [] = ""
listar_todos_levantamentos ((numero, saldo, data_, hora):xs) | (xs/=[]) || (length xs>=0) = "\tConta: "++(show numero)++"\tValor: "++(show saldo)++"\tData: "++ data_ ++"\tHora: "++ hora ++"\n"++ listar_todos_levantamentos xs
                                                      | otherwise = listar_todos_levantamentos xs
--Utentes
utentes :: [[String]] -> Utentes
utentes [] = []
utentes ([numero, nome, data_, bi, telefone, saldo, data_cadastro]:xs) = let numero_ = (read numero) :: Int
                                                                             nome_    = (nome) :: String
                                                                             data_1   =(data_) :: String
                                                                             bi_    =(bi) :: String
                                                                             telefone_   =(read telefone) :: Int
                                                                             saldo_   =(read saldo) :: Float
                                                                             data_cadastro_ = (data_cadastro) :: String
                                                  in (numero_, nome_, data_1, bi_, telefone_, saldo_, data_cadastro_):(utentes xs)

--Senhas
senhas :: [[String]] -> Senhas
senhas [] = []
senhas ([letra,numero]:xs) = let letra_senha = (letra) :: String
                                 numero_senha = (read numero) :: Int
                          in (letra_senha, numero_senha):(senhas xs)

--Transferências
tranferencias ::[[String]] -> Transferencias
tranferencias [] = [] 
tranferencias ([numero, iban, saldo, data_, hora]:xs) = let numero_conta = (read numero) :: Int
                                                            iban_ = (read iban) :: Int
                                                            saldo_ = (read saldo) :: Float
                                                            data_1 = (data_) :: String
                                                            horas = (hora) :: String
                                              in (numero_conta, iban_, saldo_, data_1, horas):(tranferencias xs)

--Depositos
depositos ::[[String]]->Depositos
depositos [] = [] 
depositos ([numero, saldo, data_, hora]:xs) = let numero_conta = (read numero) :: Int
                                                  saldo_ = (read saldo) :: Float
                                                  data_1 = (data_) :: String
                                                  horas = (hora) :: String
                                              in (numero_conta, saldo_, data_1, horas) : (depositos xs)

--Levantamentos
levantamentos :: [[String]] -> Levantamentos
levantamentos [] = [] 
levantamentos ([numero, saldo, data_, hora]:xs) = let numero_conta = (read numero) :: Int
                                                      saldo_ = (read saldo) :: Float
                                                      data_1 = (data_) :: String
                                                      horas = (hora) :: String
                                                  in (numero_conta, saldo_, data_1, horas) : (levantamentos xs)

validar_data :: String -> Int -> Bool
validar_data numero cod = if(eh_numero numero) then
          if((read numero)>0 && (read numero)<13 && cod==2) then True 
               else if((read numero)>0 && (read numero)<32 && cod==1) then True 
          else if((read numero)>1980 && (read numero)<2021 && cod==3 && (2021-(read numero)>17)) then True else False
       else False
