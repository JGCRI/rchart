#...........................
# Test IM3 Wrapped Charts
#............................

library(rchart); library(dplyr); library(ggplot2)
data_chart <- readRDS("C:/Z/projects/current/00_IM3/pic_checks/rchart/data_chart_test.rds")

# Line Chart
# Add interactions for 8 scenarios
data_chart <- data_chart %>%
  dplyr::mutate(rcp = dplyr::if_else(grepl("^rcp45",scenario),"rcp45","rcp85")) %>%
  dplyr::mutate(scen = dplyr::case_when(grepl("hotter_ssp3",scenario)~"hotter_ssp3",
                                        grepl("hotter_ssp5",scenario)~"hotter_ssp5",
                                        grepl("cooler_ssp3",scenario)~"cooler_ssp3",
                                        grepl("cooler_ssp5",scenario)~"cooler_ssp5",
                                        grepl("^ssp3",scenario)~"ssp3",
                                        grepl("^ssp5",scenario)~"ssp5",
                                        TRUE ~ "scen"))

# Aggregate all US states to National
tblAggsums <- data_chart %>%
  dplyr::select(-class1) %>%
  dplyr::filter(aggregate == "sum") %>%
  dplyr::mutate(region = "USA",
                subRegion = "USA") %>%
  dplyr::group_by_at(dplyr::vars(-value)) %>%
  dplyr::summarize_at(c("value"), list(~ sum(.)))
tblAggmeans <- data_chart %>%
  dplyr::select(-class1) %>%
  dplyr::filter(aggregate == "mean") %>%
  dplyr::mutate(region = "USA", subRegion = "USA") %>%
  dplyr::group_by_at(dplyr::vars(-value)) %>%
  dplyr::summarize_at(c("value"), list(~ mean(.)))

data_chart_agg <- dplyr::bind_rows(tblAggsums, tblAggmeans) %>% dplyr::ungroup()

palette_all_scens <- c("rcp45cooler_ssp3" =  "dodgerblue1",
                        "rcp45cooler_ssp5" =  "dodgerblue4",
                        "rcp45hotter_ssp3" =  "firebrick1",
                        "rcp45hotter_ssp5" =  "firebrick4",
                        "rcp85cooler_ssp3" =  "darkolivegreen3",
                        "rcp85cooler_ssp5" =  "darkolivegreen",
                        "rcp85hotter_ssp3" =  "orchid",
                        "rcp85hotter_ssp5" =  "orchid4"); palette_all_scens

# Plot Bar Charts for other energy
charta <- rchart::chart(data=data_chart_agg %>%
                          dplyr::filter(param %in% c("pop","gdp"),
                                        #scenario == ((data_chart_agg$scenario)%>%unique())[1]
                                        ),
                        #interaction_col_lty = "rcp",
                        #interaction_col_color = "scen",
                        palette = palette_all_scens,
                        #scenRef = ((data_chart_agg$scenario)%>%unique())[1],
                        #append = paste0("_energy_",runName),
                        #folder = dirOutputs,
                        #chart_type = c("param_absolute"),
                        width = 40, height = 30, ncol=4,
                        save = F)

charta$chart_param
charta$chart_param_diff_absolute

# Plot Bar Charts for other energy
chartb <- rchart::chart(data=data_chart %>%
                dplyr::filter(param %in% c("energyFinalConsumBySecEJ","energyFinalByFuelBySectorEJ")) %>%
                dplyr::mutate(class=class1),
                scenRef = "rcp85hotter_ssp5",
                #append = paste0("_energy_",runName),
                #folder = dirOutputs,
                chart_type = c("class_absolute","class_diff_absolute","class_diff_percent"),
                width = 40, height = 30, ncol=4,
                save = F, scales="fixed")
chartb$chart_class
chartb$chart_class_diff_absolute


