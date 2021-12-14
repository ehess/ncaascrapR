# https://stackoverflow.com/a/53290748
Mode <- function(x) {
  if ( length(x) <= 2 ) return(x[1])
  if ( anyNA(x) ) x = x[!is.na(x)]
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

my_time <- function() strftime(Sys.time(), format = "%H:%M:%S")


#' @import rvest
check_status <- function(res) {

  x = httr::status_code(res)

  if(x != 200) stop("The API returned an error", call. = FALSE)

}

#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

#' @import utils
utils::globalVariables(c("where"))

# check if a package is installed
is_installed <- function(pkg) requireNamespace(pkg, quietly = TRUE)



#' @keywords internal
"_PACKAGE"
