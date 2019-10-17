save_and_display_table <- function(tabl, filename){
  saveRDS(tabl, file.path("supp_tables", filename))
  pander(tabl, split.cell = 40, split.table = Inf)
}

get_fixed_effects_with_p_values <- function(brms_model){
  fixed_effects <- data.frame(summary(brms_model)$fixed) %>%
    rownames_to_column("Parameter")
  fixed_effects$p <- (100 - as.data.frame(bayestestR::p_direction(brms_model))$pd) / 100
  fixed_effects %>% select(Parameter, everything()) %>%
    rename(lower_95_CI = l.95..CI, upper_95_CI = u.95..CI) %>%
    mutate(` ` = ifelse(p < 0.05, "\\*", " "))
}

get_random_effects <- function(brms_model){
  random_effects <- data.frame(do.call("rbind", summary(brms_model)$random)) %>%
    rownames_to_column("Parameter")
  random_effects$p <- NA
  random_effects %>% select(Parameter, everything())
}
