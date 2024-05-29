#' @export
subtract_mz <- function(df, nome_amostra) {
# nome_amostra = nome da amostra a ser usada como Meio

# checando se a mostra informada contem no dataframe
if (any(df$Amostra == nome_amostra)) {

  # Obtendo indice da linha onde esta o nome da amostra a ser usada como Meio
  meio_row <- which(df$Amostra == nome_amostra)

  # Criando dataframe apenas com a linha da amostra a ser usada como Meio
  meio_values <- df[meio_row, -1]

  # Loop subtraindo valores das amostras pela amostra a ser usada como Meio
  for (i in 2:ncol(df)) {
    df[, i] <- df[, i] - meio_values[[i - 1]]
  }

  # Removendo linha contendo amostra a ser usada como Meio
  df <- df[-meio_row, ]

  # Convertendo valores negativos em zero
  df[df < 0] <- 0

  return(df)

} else {

  return(print("ERRO: O nome informado não consta no conjunto de dados!!"))

}
}
