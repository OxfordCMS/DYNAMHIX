#' check_variable
#'
#' A set of checks for each patient and associated variable with get_variable
#'
#' @param dataframe input metadata dataframe
#'
#' @return dataframe
#' @export
#' @examples
#'
#' No examples yet
library(futile.logger)
check_variable <- function(var_data){

  # grab the variable name
  variable <- var_data[1,5]
  
  # check patient ids and corresponding variables
  pid_var <- list()
  for (pid in unique(var_data$PID)){
    flog.info(paste0("Checking associated ",
                     variable,
                     " for PID = ",
                     pid))
    to_check <- var_data[var_data$PID == pid,]
    to_check <- to_check[to_check[,"Value"] != "",]
    if (nrow(to_check) == 0){
      flog.info(paste0("Could not find ", variable, " for ", pid))
    }else{
      var_value <- unique(to_check[,"Value"])
      if (length(var_value) > 1){
        var_value <- paste(var_value, collapse = ":")
      }
      pid_var[[pid]] <- var_value
    }
  }
  
  final_check <- data.frame(PID = names(pid_var),
                            X = unlist(pid_var, use.names = FALSE))
  colnames(final_check) <- c("PID", variable)
  rownames(final_check) <- final_check$PID
  return(final_check)
}


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
library(futile.logger)
get_variable <- function(df, variable=NULL, is_temporal = FALSE){
  
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
                         variable,
                         df[,variable])
  
  
  colnames(var_data) <- c("UID", "PID", "KID", "Timepoint", "Variable", "Value")

  # perform the checks
  check <- check_variable(var_data)
    
  # add the correct data in
  common <- intersect(unique(var_data$PID), check$PID)
  
  # subset
  if (is_temporal == FALSE){
      var_data <- var_data[var_data$PID %in% common,]
      var_data$Value <- check[var_data$PID, variable]
      var_data <- distinct(var_data)
  }
  return(var_data[var_data$Value != "",])
}
