#' recode_data
#'
#' recodes "Yes" = 1 and "No" = 0 "Don't know" = NA
#'
#' @param dataframe dataframe usually read from the exported RedCap database
#'
#' @return dataframe
#' @export
#' @examples
#' # create dummy dataframe
#' dat <- data.frame(x = letters[1:10],
#'                   y = sample(c("Yes", "No"), 10, replace = TRUE))
#' 
#' recode_data(dat)
recode_data <- function(dat){
  # retains them as characters
  dat[dat == "Yes"] <- "1"
  dat[dat == "No"] <- "0"
  dat[dat == "Don't know"] <- NA
  return(dat)
}
