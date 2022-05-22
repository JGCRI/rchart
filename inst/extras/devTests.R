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
                        save = T, show = T, scales = "free")

