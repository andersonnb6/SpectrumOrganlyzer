#' @export
cutoff_mz <- function(df, cutoff_value) {
# Convertendo valores <cutoff em zero
df[df < cutoff_value] <- 0

return(df)
}
