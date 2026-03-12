#' get_variable_average
#'
#' Take the average across timepoints for a dataframe
#' that has been returned by get_variable function
#'
#' @param dataframe input dataframe (from get_variable)
#'
#' @return dataframe
#' @export
#' @examples
#'
#' No examples yet

get_variable_average <- function(df){
  df %>% 
    group_by(PID) %>%
    reframe(PID = PID, Mean = mean(Value), Diagnosis = Diagnosis) %>%
    distinct() %>%
    as.data.frame()
}
  