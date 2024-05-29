#' @export
prepare_environment <- function() {
  # Package names
  packages <- c("openxlsx")

  # Install packages not yet installed
  installed_packages <- packages %in% rownames(installed.packages())
  if (any(installed_packages == FALSE)) {
    install.packages(packages[!installed_packages])
  }

  # Packages loading
  invisible(lapply(packages, library, character.only = TRUE))

  # Limpando ambiente e terminal
  rm(list=ls())
  cat("\014")

  # Mensagem
  print("-------------------------------------------------------")
  print("  Seu ambiente está pronto para realizar as análises!!")
  print("-------------------------------------------------------")
}
