# Fungsi Uji Linear
uji_linear <- function(data, response, predictor) {
  # Validasi input
  if (!all(c(response, predictor) %in% colnames(data))) {
    stop("Kolom tidak ditemukan dalam data.")
  }

  # Formula model
  formula <- as.formula(paste(response, "~", predictor))

  # Regresi linear
  model <- lm(formula, data = data)

  # Ringkasan hasil
  return(summary(model))
}
#' Uji Linear
#'
#' Fungsi ini melakukan uji regresi linear pada data yang diberikan.
#'
#' @param data Data frame yang digunakan untuk analisis.
#' @param response Nama kolom respons (variabel dependen).
#' @param predictor Nama kolom prediktor (variabel independen).
#' @return Ringkasan hasil regresi linear.
#' @examples
#' @export
# Contoh penggunaan:
# data <- data.frame(y = rnorm(50), x = runif(50))
# uji_linear(data, "y", "x")
