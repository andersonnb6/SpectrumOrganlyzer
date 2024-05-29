#' @export
best_mz <- function(df_raw, mz_inicial, mz_final) {
# Armazenando nomes das amostras e sequencia mz
nome_amostra <- df_raw$Amostras
seqIons <- round(as.numeric(colnames(df_raw[-1])), 1)

# criando dataset com apenas os dados de abundancia relativa das amostras
df_relative <- data.frame(t(df_raw[-1]))

# Dataframe para armazenar os resultados
df_result1 <- data.frame(rep("-", 10))

for (i in 1:ncol(df_relative)) {
  # Criando dataset temporario com dados de uma unica amostra e sequencia mz
  # convertendo sequencia mz em inteiro para gerar numeros repetidos
  db_temp <- data.frame(as.integer(seqIons),df_relative[i])
  colnames(db_temp) <- c("mz", "relative")

  # Removendo as linhas que tenham zero na X2 "coluna2"
  db_temp <- db_temp[db_temp$relative != 0, ]

  # Encontrando linhas com valores duplicados na primeira coluna
  indices_duplicados <- duplicated(db_temp$mz, fromLast = TRUE)

  # Selecionando apenas linha com a ultima ocorrencia de mz repetido
  db_temp <- db_temp[!indices_duplicados, , drop = FALSE]

  # Inserir a nova coluna entre "coluna1" e "coluna2"
  db_temp <- cbind(db_temp[, 1], rep("-", nrow(db_temp)), db_temp[, -1])
  db_temp <- data.frame(db_temp)
  colnames(db_temp) <- c("mz", "intensity", "relative")

  # Combinando resultados com o dataframe df_result1
  # Obtendo dimencao necessaria para o novo dataframe
  max_rows <- max(nrow(df_result1), nrow(db_temp))
  max_cols <- max(ncol(df_result1), ncol(db_temp))

  # Criar um dataframe vazio com o tamanho máximo
  df_combined <- data.frame(matrix(NA, nrow = max_rows, ncol = max_cols))

  # Atribuir os valores dos dataframes originais ao dataframe combinado
  df_combined[1:nrow(df_result1), 1:ncol(df_result1)] <- df_result1
  df_combined[1:nrow(db_temp), (ncol(df_result1) + 1):(ncol(df_result1) + ncol(db_temp))] <- db_temp

  # Atualizando df_result1
  df_result1 <- df_combined
}

# removando coluna 01 do df_result1
# este dataframe tera a trinca (mz, intensity e relative) por amostra,
# de acordo com a ordem de subissao
df_result1 <- df_result1[-1]


###############################################################################
# Criando sequencia de 100:1000 considerando casas decimais
###############################################################################

# Criando sequencia mz com numeros inteiros
seq_mz <- mz_inicial:mz_final

# criando dataframe com coluna 1 contendo lista relacao massa-carga
# este dataframe sera utilizada como dicionario durante organizacao dos perfis
db_seqMz <- data.frame(mz = seq_mz)

###############################################################################
# Organizando Perfis
###############################################################################

# criando dataset para receber resultado da organizacao dos perfis
db_result <- data.frame(mz = db_seqMz$mz)

# indicador da trinca durante o loop
num_trinca <- 1

# Loop para organizar ada trinca de colunas dos perfis
for (i in 1:((ncol(df_result1)/3))) {
  # Selecionando trinca de colunas
  db_trinca <- df_result1[,c(num_trinca,num_trinca+1,num_trinca+2)]

  # Coletando e salvando da amostra
  #nomeAmostra <- colnames(db_trinca[3])
  #nome_amostra <- append(nome_amostra, nomeAmostra)

  # Removendo linhas com NA
  db_trinca <- db_trinca[complete.cases(db_trinca), ]

  # Renomeando nomes das colunas
  colnames(db_trinca) <- c("mz", "intensity", "relative")

  # Encontrar as linhas com valores duplicados na primeira coluna
  indices_duplicados <- duplicated(db_trinca$mz, fromLast = TRUE)

  # Remover as linhas duplicadas, mantendo apenas a ultima ocorrencia
  # Ou seja, se tivermos 102.1 duplicado, apenas o ultimo sera mantido
  db_trinca <- db_trinca[!indices_duplicados, , drop = FALSE]

  # criando indice de mz que correspondem entre os dataframes db_seqMz e db_trinca
  indices <- match(db_seqMz$mz, db_trinca$mz)

  # Inserindo valores de abundancia relativa em db_seqMz e zerando os que nao tem correpondencia
  db_result[ncol(db_result)+1] <- ifelse(!is.na(indices), db_trinca$relative[indices], 0)

  # atualizando indicador da trinca
  num_trinca <- num_trinca + 3
}

###############################################################################
# Organizando Resultados
#  - Note que foi feita uma adaptacao para que os ios fossem repreentados apenas
#    por numeros.
###############################################################################

# Transpoem dataset
db_result <- data.frame(t(db_result[-1]))

# Converte cada elemento em numerico
db_result <- as.data.frame(lapply(db_result, as.numeric))

# Adiciona nomes das amostras na primeira coluna
db_result <- cbind(c(nome_amostra), db_result)

# Adiciona nomes das colunas garantindo que sejam caracteres
names(db_result) <- as.character(c("Amostras", seq_mz))

# Corrije nomes das amostras
db_result$Amostras <- sub("\\.\\.\\..*", "", db_result$Amostras)

return(db_result)
}
