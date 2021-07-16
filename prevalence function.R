prevalence_cal <- function (data, only)
{
  noIA <- data$n_noIA
  IA <- data$n_IA
  prevalence <- IA/(noIA+IA)*100
  
  result <- ifelse(data$study_class == only, prevalence, NA)
  
  return (result)
}

prevalence_cal(data_mean_rg, "cohort")
newdf <- data_mean_rg
newdf$prevalence <- prevalence_cal(data_mean_rg, "cohort")
