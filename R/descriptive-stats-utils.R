#' compute the normality of a data column with the shapiro test
#'
#' @param data Tibble from spreadsheet
#' @param var The column name in data to look at
#' @param data_type String, "continuous", "ordinal", "categorical", "binary"
#' @param output something to do with the shapiro test
#'
#' @import dplyr
#'
#' @return return something
compute_normality <- function(data, var, data_type, output) {
  if (data_type == "continuous") {
    test_results <- data %>%
      dplyr::pull(!!var) %>%
      shapiro.test()
    if (output == "p") {
      return(test_results$p.value)
    } else if (output == "stat") {
      return(test_results$statistic)
    }
  }
  return(NA)
}
compute_normality <- Vectorize(compute_normality)
