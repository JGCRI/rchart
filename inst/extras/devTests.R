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
              chart_type = "param_absolute",
              folder = "charts_output")

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




######################### testing breaks arguments ###########################

test_classes <- read.csv("C:/Users/wait467/OneDrive - PNNL/Desktop/SEAsia_local/tests/rchart_class_test.csv")
test_classes_chart <- rchart::chart(test_classes, scenRef = "Ref", save = F)
#test_classes_chart <- rchart::chart(test_classes, breaks = 3,save = F)

test_classes_chart$chart_class_Thailand
test_classes_chart$chart_region_absolute
test_classes_chart$chart_param_Thailand
test_classes_chart$chart_param_diff_absolute_Thailand
test_classes_chart$chart_param_diff_percent_Thailand
test_classes_chart$chart_class_diff_absolute_Thailand


test_params <- read.csv("C:/Users/wait467/OneDrive - PNNL/Desktop/SEAsia_local/tests/all_socioeconomics.csv")
test_params_chart <- rchart::chart(test_params, save = F, aspect_ratio = 0.4)

test_params_chart$chart_region_absolute +
  ggplot2::theme(legend.position = "right",
                 strip.background = ggplot2::element_blank(),
                 strip.text = ggplot2::element_blank())



# Example for workflows
library(rchart); library(dplyr); library(ggplot2); library(gcamextractor)

# Extract Data
data_extracted <- readgcam(gcamdatabase = "C:/Z/projects/current/00_IM3/pic_checks/databases/database_rcp85hotter_ssp5_runoff",
                 paramsSelect = c("pop","elecByTechTWh"),
                 folder = "test_folder",
                 saveData = F)

# View extracted data
names(data_extracted) # View all available data tables
head(data_extracted$dataAggParam) # View data aggregated by parameter
head(data_extracted$dataAggClass1) # View data aggregated by class 1
head(data_extracted$data) # View all data

# Filter population data to specific countries and years
data_plot <- data_extracted$dataAggClass1 %>%
  dplyr::filter(param %in% c("pop","elecByTechTWh"),
                subRegion %in% c("Argentina", "Colombia"))

# Plot data with rchart
charts <- rchart::chart(data_plot,
                        save = T, show = T, scales = "free", folder="test_charts")


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

# Plot line plots for all
data_chart_agg$inter <- interaction(data_chart_agg[["rcp"]],data_chart_agg[["scen"]])
ggplot(data_chart_agg %>%
         dplyr::filter(param %in% c("elecFinalBySecTWh","elecByTechTWh",
                                    "energyFinalConsumBySecEJ",
                                    "watWithdrawBySec", "watConsumBySec",
                                    "agProdByCrop","pop","gdp")), aes_string(x="x", y="value", group="inter")) +
  geom_line(aes_string(linetype="rcp", color="scen")) +
  facet_wrap(param~., scale="free_y")
#ggsave(paste0(dirOutputs,"/summary_",runName,".png"), width = 10, height = 10)

# Plot Bar Charts for other energy
charta <- rchart::chart(data=data_chart_agg %>%
                          dplyr::filter(param %in% c("pop","gdp","energyFinalConsumBySecEJ"),
                                        #scenario == ((data_chart_agg$scenario)%>%unique())[1]
                                        ),
                        interaction_col_lty = "rcp",
                        interaction_col_color = "scen",
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
                #scenRef = scenRef,
                #append = paste0("_energy_",runName),
                #folder = dirOutputs,
                chart_type = c("class_absolute","class_diff_absolute","class_diff_percent"),
                width = 40, height = 30, ncol=4,
                save = F)




