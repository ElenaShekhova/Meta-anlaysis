incidence_cal <- function (data, only)
{
  noIA <- data$n_noIA
  IA <- data$n_IA
  incidence <- IA/(noIA+IA)*100
  
  result <- ifelse(data$study_class == only, incidence, NA)
  
  return (result)
}

incidence_cal(data_mean_rg, "cohort")
newdf <- data_mean_rg
newdf$incidence <- incidence_cal(data_mean_rg, "cohort")
