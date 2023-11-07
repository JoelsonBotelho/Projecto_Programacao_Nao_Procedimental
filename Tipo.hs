module Tipo where
-- Dados da conta
type Utente = (Int, String, String, String, Int, Float, String)
type Utentes = [Utente]

-- Senha
type Senha = (String, Int)
type Senhas = [Senha]

-- Operções
type Deposito = (Int, Float, String, String)
type Depositos = [Deposito]

type Levantamento = (Int, Float, String, String)
type Levantamentos = [Levantamento]

type Transferencia = (Int, Int, Float, String, String)
type Transferencias = [Transferencia]