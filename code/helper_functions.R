save_and_display_table <- function(tabl, filename){
  saveRDS(tabl, file.path("supp_tables", filename))
  pander(tabl, split.cell = 40, split.table = Inf)
}

get_fixed_effects_with_p_values <- function(brms_model){
  fixed_effects <- data.frame(summary(brms_model)$fixed) %>%
    rownames_to_column("Parameter")
  p <- as.data.frame(bayestestR::p_direction(brms_model)) %>%
    filter(Component == "conditional") %>% pull(pd)
  fixed_effects$p <- 1 - p
  fixed_effects %>% select(Parameter, everything()) %>%
    rename(lower_95_CI = l.95..CI, upper_95_CI = u.95..CI)  %>%
    mutate(` ` = ifelse(p < 0.1, "~", ""),
           ` ` = replace(` `, p < 0.05, "\\*"),
           ` ` = replace(` `, p < 0.01, "**"),
           ` ` = replace(` `, p < 0.001, "***"),
           ` ` = replace(` `, p == " ", ""))
}

get_random_effects <- function(brms_model){
  random_effects <- data.frame(do.call("rbind", summary(brms_model)$random)) %>%
    rownames_to_column("Parameter")
  random_effects$p <- NA
  random_effects %>% select(Parameter, everything())
}

