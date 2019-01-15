#' Load file from Quilt
#'
#' @param pkg package name
#' @param file file name
#'
#' @return dataframe
#' @export
#'
#' @import reticulate
#' @examples
#' qload("akarve/examples", "README")
#' qload("examples/wine", "quality")
#' qload("examples/wine", "quality/red")
#' qload("akarve/seattle_911", "responses")
qload <- function(pkg, file) {
    pkg_pythonic <- stringr::str_replace_all(pkg, "/", "\\.")
    pkg_name <- paste0("quilt.data.", pkg_pythonic)
    data <- reticulate::import(module = pkg_name)
    file <- stringr::str_replace_all(file, "/", "$")

    df <- eval(parse(text = paste0("data$", file, "()")))
    df
}
