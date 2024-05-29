#' @export
organize_mz <- function(df_raw, mz_inicial, mz_final) {
  # definindo range
  num_inicial <- mz_inicial
  num_final <- mz_final

  # Crie vetores para os números inteiros de 10 a 100 e os decimais de 0.1 a 0.9
  inteiros <- num_inicial:num_final
  decimais <- seq(0.0, 0.9, by = 0.1)

  # Use a função outer() para criar todas as combinações entre os números inteiros e os decimais
  seq_mz <- c(outer(inteiros, decimais, `+`))

  # ordena sequencia do menor para maior
  seq_mz <- sort(seq_mz, decreasing = FALSE)

  # remove 9 numeros finais da sequencia que sao desnecessarios
  seq_mz <- seq_mz[1:(length(seq_mz)-9)]

  # criando dataframe com coluna 1 contendo lista relacao massa-carga
  # este dataframe sera utilizada como dicionario durante organizacao dos perfis
  db_seqMz <- data.frame(mz = seq_mz)

  ###############################################################################
  # Organizando Perfis
  ###############################################################################

  # criando dataset para receber resultado da organizacao dos perfis
  db_result <- data.frame(mz = db_seqMz$mz)

  # vetor que armazenara nomes das amostras
  nome_amostra <- c()

  # indicador da trinca durante o loop
  num_trinca <- 1

  # Loop para organizar ada trinca de colunas dos perfis
  for (i in 1:((ncol(df_raw)/3))) {
    # Selecionando trinca de colunas
    db_trinca <- df_raw[,c(num_trinca,num_trinca+1,num_trinca+2)]

    # Coletando e salvando da amostra
    nomeAmostra <- colnames(db_trinca[3])
    nome_amostra <- append(nome_amostra, nomeAmostra)

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
