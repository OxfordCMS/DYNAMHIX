get_diet_data <- function(df){
  
  diet_columns <- colnames(df)[74:86]
    
  diet_dat <- list()
  for (i in 1:length(diet_columns)){
    variable <- diet_columns[i]
    flog.info(paste0("Collecting data for diet component ", variable))
    var_data <- data.frame(paste0(df$Participant.ID, ":", df$Event.Name),
                           df$Participant.ID,
                           df$KIT.ID,
                           df$Event.Name,
                           variable,
                           df[,variable])
        
    colnames(var_data) <- c("UID", "PID", "KID", "Timepoint", "Variable", "Value")
    var_data <- var_data[,c("UID", "PID", "Timepoint", "Variable", "Value")]
    
    diet_dat[[i]] <- distinct(var_data[var_data$Value != "",])
    
  }
  flog.info("FINISHED collecting data")
  diet_dat <- Reduce(function(dtf1, dtf2) merge(dtf1, dtf2, by = "UID", all.x = TRUE), diet_dat)
  diet_dat <- diet_dat[,grep("Timepoint.", colnames(diet_dat), invert=TRUE)]
  diet_dat <- diet_dat[,grep("PID.", colnames(diet_dat), invert=TRUE)]
  diet_dat <- diet_dat[,grep("Variable", colnames(diet_dat), invert=TRUE)]
  
  rownames(diet_dat) <- diet_dat$UID
  diet_dat <- diet_dat[,2:ncol(diet_dat)]
  
  # rename columns and reorder
  colnames(diet_dat) <- c(
                          "vegetables",
                          "whole_grains",
                          "fruits",
                          "red_meat",
                          "white_meat",
                          "fish",
                          "shellfish",
                          "dairy",
                          "live_yoghurt",
                          "probiotics",
                          "sugar",
                          "sweetener",
                          "PID",
                          "timepoint",
                          "alcohol")
  
  return(diet_dat)
}
