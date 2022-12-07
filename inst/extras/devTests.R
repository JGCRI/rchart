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

#....................
# Emission Plots
#...................

library(rchart); library(dplyr); library(ggplot2)
data_gcam <- readRDS("C:/Z/projects/current/00_IM3/pic_checks/rchart/dataGCAM_im3.rds")

palette_all_scens <- c("rcp45cooler_ssp3" =  "dodgerblue1",
                       "rcp45cooler_ssp5" =  "dodgerblue4",
                       "rcp45hotter_ssp3" =  "firebrick1",
                       "rcp45hotter_ssp5" =  "firebrick4",
                       "rcp85cooler_ssp3" =  "darkolivegreen3",
                       "rcp85cooler_ssp5" =  "darkolivegreen",
                       "rcp85hotter_ssp3" =  "orchid",
                       "rcp85hotter_ssp5" =  "orchid4"); palette_all_scens



datax <- data_gcam

datax$param %>% unique()
datax$scenario %>% unique()
gridRegions <- c("California grid","Central East grid", "Central Northeast grid","Central Northwest grid","Central Southwest grid","Florida grid",
                 "Hawaii grid","Mid-Atlantic grid", "New England grid","New York grid","Northwest grid", "Southeast grid",
                 "Southwest grid", "Texas grid")

# Compare NonCO2 USA
dfx <- datax %>%
  dplyr::filter(param=="emissGHGByGasGWPAR5",
                region %in% c(as.character(unique(rmap::mapUS52$subRegion)),"USA",gridRegions)) %>%
  dplyr::select(-region) %>%
  dplyr::group_by(scenario,class1,x) %>%
  dplyr::summarize(value=sum(value,na.rm=T)); dfx

# Add interactions for 8 scenarios
dfx <- dfx %>%
  dplyr::mutate(rcp = dplyr::if_else(grepl("^rcp45",scenario),"rcp45","rcp85")) %>%
  dplyr::mutate(scen = dplyr::case_when(grepl("hotter_ssp3",scenario)~"hotter_ssp3",
                                        grepl("hotter_ssp5",scenario)~"hotter_ssp5",
                                        grepl("cooler_ssp3",scenario)~"cooler_ssp3",
                                        grepl("cooler_ssp5",scenario)~"cooler_ssp5",
                                        grepl("^ssp3",scenario)~"ssp3",
                                        grepl("^ssp5",scenario)~"ssp5",
                                        TRUE ~ "scen"))

dfx$interaction <- interaction(dfx[["rcp"]],dfx[["scen"]])


p1 <- ggplot2::ggplot(dfx %>% filter(x>2010),(aes_string(x="x",y="value", color = "scen",
                                                         lty = "rcp"))) +
  geom_line(size=1.5)  + expand_limits(y=c(0,0)) +
  ggplot2::scale_color_manual(values=palette_all_scens%>%as.vector()) +
  facet_wrap(.~class1, scale="free_y") + theme_bw() + ggtitle("USA Non-CO2 emissions (Variable Units)")+xlab(NULL)+ylab(NULL)+
  scale_x_continuous(labels=seq(2010,2100,5), breaks=seq(2010,2100,5))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)); p1

ggsave(paste0(dirOutputs,"/compareNonCO2USA_",runName,".png"),width=15,height=10, units="in",device="png")
print(paste0("Saved as: ",dirOutputs,"/compareNonCO2USA_",runName,".png"))

# Compare Plot
dfx <- datax %>%
  filter(param=="emissGHGByGasGWPAR5")%>%
  dplyr::select(-region) %>%
  dplyr::group_by(scenario,class1,x) %>%
  dplyr::summarize(value=sum(value,na.rm=T)); dfx

# Add interactions for 8 scenarios
dfx <- dfx %>%
  dplyr::mutate(rcp = dplyr::if_else(grepl("^rcp45",scenario),"rcp45","rcp85")) %>%
  dplyr::mutate(scen = dplyr::case_when(grepl("hotter_ssp3",scenario)~"hotter_ssp3",
                                        grepl("hotter_ssp5",scenario)~"hotter_ssp5",
                                        grepl("cooler_ssp3",scenario)~"cooler_ssp3",
                                        grepl("cooler_ssp5",scenario)~"cooler_ssp5",
                                        grepl("^ssp3",scenario)~"ssp3",
                                        grepl("^ssp5",scenario)~"ssp5",
                                        TRUE ~ "scen"))

dfx$interaction <- interaction(dfx[["rcp"]],dfx[["scen"]])

p1 <- ggplot2::ggplot(dfx %>% filter(x>2010), aes_string(x="x",y="value", color = "scen",lty = "rcp")) +
  geom_line(size=1.5)  + expand_limits(y=c(0,0)) +
  ggplot2::scale_color_manual(values=palette_all_scens%>%as.vector()) +
  facet_wrap(.~class1, scale="free_y") + theme_bw() + ggtitle("Global Non-CO2 emissions (Variable Units)")+xlab(NULL)+ylab(NULL)+
  scale_x_continuous(labels=seq(2010,2100,5), breaks=seq(2010,2100,5))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)); p1

ggsave(paste0(dirOutputs,"/compareNonCO2global_",runName,".png"),width=15,height=10, units="in",device="png")
print(paste0("Saved as: ",dirOutputs,"/compareNonCO2global_",runName,".png"))

# USA CO2 Emissions annual
dfx <- datax %>%
  dplyr::filter(
    param =="emissCO2BySector",
    region %in% c(as.character(unique(rmap::mapUS52$subRegion)),"USA"))%>%
  dplyr::select(-region) %>%
  dplyr::group_by(scenario,x) %>%
  dplyr::summarize(value=sum(value,na.rm=T)); dfx

# Add interactions for 8 scenarios
dfx <- dfx %>%
  dplyr::mutate(rcp = dplyr::if_else(grepl("^rcp45",scenario),"rcp45","rcp85")) %>%
  dplyr::mutate(scen = dplyr::case_when(grepl("hotter_ssp3",scenario)~"hotter_ssp3",
                                        grepl("hotter_ssp5",scenario)~"hotter_ssp5",
                                        grepl("cooler_ssp3",scenario)~"cooler_ssp3",
                                        grepl("cooler_ssp5",scenario)~"cooler_ssp5",
                                        grepl("^ssp3",scenario)~"ssp3",
                                        grepl("^ssp5",scenario)~"ssp5",
                                        TRUE ~ "scen"))

dfx$interaction <- interaction(dfx[["rcp"]],dfx[["scen"]])


p1 <- ggplot2::ggplot(dfx %>% filter(x>2005), aes_string(x="x",y="value", color = "scen",lty = "rcp")) +
  geom_line(size=1.5) + expand_limits(y=c(0,0)) +
  ggplot2::scale_color_manual(values=palette_all_scens%>%as.vector()) +
  theme_bw() + ylab("CO2 (MTC)") + ggtitle("USA CO2 emissions (MTC)")+xlab(NULL)+
  scale_x_continuous(labels=seq(2010,2100,5), breaks=seq(2010,2100,5))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)); p1

ggsave(paste0(dirOutputs,"/compareCO2USA_",runName,".png"),width=9,height=7, units="in",device="png")
print(paste0("Saved as: ",dirOutputs,"/compareCO2USA_",runName,".png"))


# Global CO2 Cum with RCP ranges
# CMIP5 Table SPM.3, pg 27 https://www.ipcc.ch/site/assets/uploads/2018/02/WG1AR5_SPM_FINAL.pdf

dfx <- datax %>% filter(param=="emissCO2CumGlobal2010to2100", region=="Global"); dfx
dfxrcp <- datax %>% filter(param=="emissCO2CumGlobal2010to2100RCP") %>%
  dplyr::select(param, scenario, region, subRegion, x, class1, class2,value) %>%
  dplyr::mutate(scenario=class1) %>%
  unique()%>%
  tidyr::spread(key="class2", value="value"); dfxrcp

# Add interactions for 8 scenarios
dfx <- dfx %>%
  dplyr::mutate(rcp = dplyr::if_else(grepl("^rcp45",scenario),"rcp45","rcp85")) %>%
  dplyr::mutate(scen = dplyr::case_when(grepl("hotter_ssp3",scenario)~"hotter_ssp3",
                                        grepl("hotter_ssp5",scenario)~"hotter_ssp5",
                                        grepl("cooler_ssp3",scenario)~"cooler_ssp3",
                                        grepl("cooler_ssp5",scenario)~"cooler_ssp5",
                                        grepl("^ssp3",scenario)~"ssp3",
                                        grepl("^ssp5",scenario)~"ssp5",
                                        TRUE ~ "scen"))

dfx$interaction <- interaction(dfx[["rcp"]],dfx[["scen"]])

p1 <- ggplot2::ggplot() +
  geom_ribbon(data=dfxrcp %>% filter(x>2010),aes(x=x,ymin=min, ymax=max, fill=scenario), alpha=0.2) +
  scale_fill_manual(values=c("rcp8.5"="cadetblue","rcp6.0"="red","rcp4.5"="green","rcp2.6"="gold"), name=NULL) +
  geom_line(data=dfx ,size=1.5, aes_string(x="x",y="value", color = "scen",lty = "rcp")) + expand_limits(y=c(0,0)) + xlab(NULL)+
  ggplot2::scale_color_manual(values=palette_all_scens%>%as.vector()) +
  scale_x_continuous(breaks=seq(2010,2100,5))+
  facet_grid(.~rcp)+
  theme_bw() + ylab("CO2 (GTC)") + ggtitle("Global cumulative (2010 to 2100) CO2 emissions (GTC) - RCP data from CMIP5 Table SPM.3")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)); p1

ggsave(paste0(dirOutputs,"/compareCO2GlobalCumRCP_",runName,".png"),width=15,height=10, units="in",device="png")
print(paste0("Saved as: ",dirOutputs,"/compareCO2GlobalCumRCP_",runName,".png"))


#...........................................
# Testing rchart with gcam_training material
#..........................................

library(rchart)
#data <- data.table(url("https://github.com/JGCRI/gcam_training/blob/main/examples/gcamDataTable_aggClass1.csv")) # Read Data
data <- read.csv("C:/Z/metarepos/gcam_training/examples/gcamDataTable_aggClass1.csv") # Read Data
charts <- rchart::chart(data, save=F, scenRef = "GCAM_SSP2") # Save all charts into a list. scenRef is optional.
names(charts) # To see list of charts
charts$chart_param_Argentina
charts$chart_class_Argentina
