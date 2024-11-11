#' get_variables
#'
#' return a data frame with just the variable of interest. Contains PID and
#' timepoint and creates a new ID based on these data
#'
#' @param dataframe input metadata dataframe
#' @param variables a vector of variable names that you want to 
#' retrieve from the dataframe
#' @param format the format of the returned dataframe. Long or wide.
#' @return dataframe
#' @import DYNAMHIX
#' @import purrr
#' @import dplyr
#' @export
#' @examples
#'
#' No examples yet
get_variables <- function(df, variables=NULL, format=NULL){
  
  # a couple of checks
  if (is.null(variables)){
    stop("Must provide a variable to extract using the 'variables' argument")
  }
  
  variables_na <- unlist(lapply(variables, function(x) match(x, colnames(df))))
  variables_na <- match(NA, variables_na)
  
  if (!(is.na(variables_na))){
    stop(paste0(variables[variables_na], " is/are not in colnames(df)"))
  }
  
  if (is.null(format)){
    stop("Must provide an output format: 'long' or 'wide'")
  }
  
  if (is.na(match(format, c("long", "wide")))){
    stop("Format argument must be one of 'long' or 'wide'")
  }

  # get a list of dataframes with each variable of interest
  dfs <- lapply(variables, function(x) get_variable(df, variable = x))
    
  # output long or wide formatted data
  if (format == "wide"){
    ret <- reduce(dfs, left_join, by = c("UID", "PID", "KID", "Timepoint", "Diagnosis"))
  }else if (format == "long"){
    ret <- bind_rows(dfs)
  }else{
    stop("format argument needs to be one of 'wide' or 'long'")
  }
  return(ret)
}  
  
  