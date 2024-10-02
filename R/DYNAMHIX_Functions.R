recode_dat <- function(dat){
  
  # recode the yes's as 1
  dat[dat == "Yes"] <- 1
  dat[dat == "No"] <- 0
  return(dat)
}


# function to get locations of aliquots
get_aliquot_data <- function(dat){
  dat <- dat[dat$Aliquot.ID != "",]
  new_df <- data.frame(Patient_ID = dat$Participant.ID,
                       Sample_ID = dat$Aliquot.ID,
                       Freezer = dat$Freezer.1,
                       Drawer = dat$Freezer.Drawer,
                       Box = dat$Box,
                       Position = dat$Position.in.box.)
  print(new_df)
  
  #new_df <- new_df[match(gtools::mixedsort(new_df$Box, decreasing=FALSE), new_df$Box),]
  new_df <- dplyr::distinct(new_df)
  new_df$Box <- gsub("Box", "box", new_df$Box)
  new_df <- new_df[order(new_df$Box, new_df$Position),]
  
  return(new_df)
}



# functionj to get locations of aliquots
get_biopsy_data <- function(dat){
  dat <- dat[grep("Biopsy", dat$Sample.type),]
  
  new_df <- data.frame(Patient_ID = dat$Participant.ID,
                       Sample_ID = dat$Sample.ID,
                       Freezer = dat$Freezer.2,
                       Drawer = dat$Freezer.Drawer,
                       Box = dat$Box,
                       Position = dat$Position.in.box.)
  
  #new_df <- new_df[match(gtools::mixedsort(new_df$Box, decreasing=FALSE), new_df$Box),]
  new_df <- dplyr::distinct(new_df)
  new_df$Box <- gsub("Box", "box", new_df$Box)
  new_df <- new_df[order(new_df$Box, new_df$Position),]
  
  return(new_df)
}

## Function to get data associated with a particular variable
get_variable_data <- function(dat, variable="BRISTOL.Stool.Scale"){
  
  # rename variables
  if (variable == "BRISTOL.Stool.Scale"){
    new_var_name <- "Bristol_stool_scale"
    dat[,variable] <- as.numeric(gsub("Type ", "", dat[,variable]))
  }else{
    new_var_name <- variable
  }
  var_data <- data.frame(dat$Participant.ID,
                         dat[,variable],
                         dat$Event.Name,
                         dat$Diagnosis)
  colnames(var_data) <- c("PID", new_var_name, "Timepoint", "Diagnosis")
  var_data[var_data == ""] <- NA
  var_data <- as.data.frame(na.omit(var_data))
  return(var_data)
  
}

# Diet information
get_diet_data <- function(dat){
  
  diet_columns <- colnames(dat)[68:80]
  
  diet_dat <- list()
  for (i in 1:length(diet_columns)){
    variable <- diet_columns[i]
    variable_dat <- get_variable_data(dat, variable=variable)
    
    # Create new ID for timepoint-patient
    variable_dat$PID_TP <- paste0(variable_dat$PID, "_", variable_dat$Timepoint)
    diet_dat[[i]] <- variable_dat
  }
  diet_dat <- Reduce(function(dtf1, dtf2) merge(dtf1, dtf2, by = "PID_TP", all.x = TRUE), diet_dat)
  diet_dat <- diet_dat[,grep("Timepoint.", colnames(diet_dat), invert=TRUE)]
  diet_dat <- diet_dat[,grep("PID.", colnames(diet_dat), invert=TRUE)]
  diet_dat <- diet_dat[,grep("Diagnosis.", colnames(diet_dat), invert=TRUE)]
  
  # rename columns and reorder
  colnames(diet_dat) <- c("vegetables",
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
                          "alcohol",
                          "timepoint",
                          "diagnosis")
  
  return(diet_dat)
}
