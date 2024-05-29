#' @export
mean_mz <- function(df_raw) {
# Garantindo que o nome da primeira coluna seja Amostras
colnames(df_raw)[1] <- "Amostras"

# Calcular medias agrupadas pelo nome das amostras
df_media <- aggregate(. ~ Amostras, data = df_raw, FUN = mean)

return(df_media)
}
