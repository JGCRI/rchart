library(rchart); library(dplyr); library(ggplot2)

rTable_i <- readRDS("C:/Z/projects/current/00_IM3/tests/xanthosGlobalRuns/runoff_GCMs_5trail_delta/rTable_i_runoff_GCMs_5trail_delta.rds")


# Plot Line Plots for GDP and Pop
rchart::chart(data=rTable_i %>%
                dplyr::filter(param %in% c("pop","gdp","elecFinalBySecTWh","elecByTechTWh",
                                           "energyFinalConsumBySecEJ","energyFinalByFuelBySectorEJ",
                                           "watWithdrawBySec", "watConsumBySec",
                                           "agProdByCrop","landAlloc")),
              scenRef = "Ref_RCP8p5_NORESM_5trail_delta_applied2015",
              append = "_all_summary",
              chart_type = "param_absolute")

data=rTable_i %>%
  dplyr::filter(param %in% c("pop","gdp","elecFinalBySecTWh","elecByTechTWh",
                             "energyFinalConsumBySecEJ","energyFinalByFuelBySectorEJ",
                             "watWithdrawBySec", "watConsumBySec",
                             "agProdByCrop","landAlloc"))
scenRef = "Ref_RCP8p5_NORESM_5trail_delta_applied2015"
append = "_all_summary"
chart_type = "param_absolute"

rchart::chart(data=rTable_i %>%
                dplyr::filter(param %in% c("pop","gdp")),
              scenRef = "Ref_RCP8p5_NORESM_5trail_delta_applied2015",
              append = "_socioecon",
              chart_type = "param_diff_percent")

# Plot Bar Charts for other chosen params
rchart::chart(data=rTable_i %>%
                dplyr::filter(param %in% c("elecFinalBySecTWh","elecByTechTWh",
                                           "energyFinalConsumBySecEJ","energyFinalByFuelBySectorEJ",
                                           "watWithdrawBySec", "watConsumBySec",
                                           "agProdByCrop","landAlloc")) %>%
                dplyr::mutate(class=class1),
              scenRef = "Ref_RCP8p5_NORESM_5trail_delta_applied2015",
              append = "_otherParams",
              chart_type = c("class_absolute"))

# data=rTable_i %>%
#   dplyr::filter(param %in% c("elecFinalBySecTWh","elecByTechTWh",
#                              "energyFinalConsumBySecEJ","energyFinalByFuelBySectorEJ",
#                              "watWithdrawBySec", "watConsumBySec",
#                              "agProdByCrop","landAlloc"))
# scenRef = "Ref_RCP8p5_NORESM_5trail_delta_applied2015"
# append = "_otherParams"
# chart_type = "class_absolute"
