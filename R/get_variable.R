#' get_variable
#'
#' return a data frame with just the variable of interest. Contains PID and
#' timepoint and creates a new ID based on these data
#'
#' @param dataframe input metadata dataframe
#' @param variable name of the variable you want to retrieve from the dataframe
#'
#' @return dataframe
#' @export
#' @examples
#'
#' No examples yet
get_variable <- function(df, variable=NULL){
  
  # a couple of checks
  if (is.null(variable)){
    stop("Must provide a variable to extract using the 'variable' argument")
  }
  
  if (is.na(match(variable, colnames(df)))){
    stop("variable not found in data, check the spelling against 'colnames(df)'")
  }
  
  # rename variables
  if (variable == "BRISTOL.Stool.Scale"){
    new_var_name <- "Bristol_stool_scale"
    df[,variable] <- as.numeric(gsub("Type ", "", df[,variable]))
  }else{
    new_var_name <- variable
  }
  
  var_data <- data.frame(paste0(df$Participant.ID, ":", df$Event.Name),
                         df$Participant.ID,
                         df$KIT.ID,
                         df$Event.Name,
                         df$Diagnosis,
                         variable,
                         df[,variable])
  
  
  colnames(var_data) <- c("UID", "PID", "KID", "Timepoint", "Diagnosis", "Variable", "Value")
  var_data[var_data == ""] <- NA
  var_data <- as.data.frame(na.omit(var_data))
  return(var_data)
}
