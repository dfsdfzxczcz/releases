setwd("D:/oa/Figure1")


library(ggmap)

library(maps)
library(dplyr)
library(cowplot)
library(patchwork)
library(purrr)
library(stringr)
library(sf)

library(tmaptools)
library(cols4all)


Countries_1990to2021 <- read.csv('204counties_1990to2021.csv', header=T)

load('GBD_maps.RData')


Prevalence2021_ASR  <- Countries_1990to2021 %>%
  filter(cause_name == "Osteoarthritis hip",
         year == "2021",
         sex_name == "Both",
         age_name == "Age-standardized",
         measure_name == "Prevalence",
         metric_name == "Rate") %>%
  select("location_id", "location_name","val")


summary_table <- Prevalence2021_ASR %>%
  rename(ASPR_per_100k = val) %>%
  arrange(desc(ASPR_per_100k))

print("--- 2021年髋关节骨关节炎年龄标化患病率最高的10个国家 ---")
print(head(summary_table, 10))


write.csv(summary_table, "./2.输出/Prevalence_hip_2021_ASR_summary.csv", row.names = FALSE)
print("汇总表已保存到 ./2.输出/Prevalence_hip_2021_ASR_summary.csv")

hist(Prevalence2021_ASR$val,
     main = "Histogram of Prevalence 2021 ASR Values",
     xlab = "Values",
     col = "#1f77b4",
     border = "white")


quantiles <- quantile(Prevalence2021_ASR$val, probs = c(0,0.25, 0.5, 0.75,1), na.rm = TRUE)


num_classes <- 8
breaks <- quantile(Prevalence2021_ASR$val,
                   probs = seq(0, 1, length.out = num_classes + 1),
                   na.rm = TRUE)
breaks <- unique(breaks)


breaks_labels <- purrr::imap_chr(breaks[-length(breaks)],
                                 ~ paste0(round(.x), "-", round(breaks[.y+1])))

if(length(breaks) > 1) {
  last_break_start <- round(breaks[length(breaks)-1])
  breaks_labels[length(breaks_labels)] <- paste0("≥", last_break_start)
}


pal <- cols4all::c4a("YlGnBu", n = length(breaks_labels))


Prevalence2021_ASR_map <- left_join(Prevalence2021_ASR, world_GBD, by = c('location_id' = 'Location.ID')) %>%
  mutate(val2 = cut(val,
                    breaks = breaks,
                    labels = breaks_labels,
                    include.lowest = TRUE, right = FALSE))


Prevalence2021_ASR_map_plot <- ggplot(data = Prevalence2021_ASR_map) +
  geom_sf(aes(geometry = geometry, fill = val2), linewidth = 0.1) +
  scale_fill_manual(
    values = pal,
    breaks = breaks_labels,
    labels = breaks_labels,
    drop = FALSE) +
  theme_void(base_size = 6)+
  labs(x="", y="")+
  guides(fill = guide_legend(title='ASPR per 100,000 (2021)', ncol =2, override.aes = list(linewidth = 0)))+
  theme(legend.position.inside = c(0.06, 0.15),
        legend.key.height = unit(0.4, "cm"),
        legend.key.width  = unit(0.4, "cm")
  )+
  coord_sf(xlim = c(-180,208), expand = FALSE)


a <- Prevalence2021_ASR_map[Prevalence2021_ASR_map$location_id %in% subregions_shp[['Caribbean and central America']]$Location.ID,]


theme_map_sub <- theme_void()+labs(x="", y="")+theme_bw()+
  theme(text = element_text(size = 4),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'none',
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(vjust = 1, hjust = 0.5))

x_location = c(-90,-59)
y_location = c(7,28)


sub1 <- ggplot(data = a, aes(fill = val2)) +
  geom_sf(aes(geometry = geometry)) +
  scale_fill_manual(
    values = pal,
    breaks = breaks_labels,
    labels = breaks_labels,
    drop = FALSE) +
  coord_sf(xlim = x_location, ylim = y_location, expand = FALSE)+
  ggtitle('Caribbean and central America')+
  theme_map_sub


a <- Prevalence2021_ASR_map[Prevalence2021_ASR_map$location_id %in% subregions_shp[['Persian Gulf']]$Location.ID,]


x_location = c(45,55)
y_location = c(18.5,31.5)


sub2 <- ggplot(data = a, aes(fill = val2)) +
  geom_sf(aes(geometry = geometry)) +
  scale_fill_manual(
    values = pal,
    breaks = breaks_labels,
    labels = breaks_labels,
    drop = FALSE) +
  coord_sf(xlim = x_location, ylim = y_location, expand = FALSE)+
  ggtitle('Persian Gulf')+
  theme_map_sub


a <- Prevalence2021_ASR_map[Prevalence2021_ASR_map$location_id %in% subregions_shp[['Balkan Peninsula']]$Location.ID,]

x_location = c(12.5,32)
y_location = c(35,53)

sub3 <- ggplot(data = a, aes(fill = val2)) +
  geom_sf(aes(geometry = geometry)) +
  scale_fill_manual(
    values = pal,
    breaks = breaks_labels,
    labels = breaks_labels,
    drop = FALSE) +
  coord_sf(xlim = x_location, ylim = y_location, expand = FALSE)+
  ggtitle('Balkan Peninsula')+
  theme_map_sub


a <- Prevalence2021_ASR_map[Prevalence2021_ASR_map$location_id %in% subregions_shp[['Southeast Asia']]$Location.ID,]

x_location = c(97.5,119.7)
y_location = c(-9.2,9)

sub4 <- ggplot(data = a, aes(fill = val2)) +
  geom_sf(aes(geometry = geometry)) +
  scale_fill_manual(
    values = pal,
    breaks = breaks_labels,
    labels = breaks_labels,
    drop = FALSE) +
  coord_sf(xlim = x_location, ylim = y_location, expand = FALSE)+
  ggtitle('Southeast Asia')+
  theme_map_sub


a <- Prevalence2021_ASR_map[Prevalence2021_ASR_map$location_id %in% subregions_shp[['West Africa']]$Location.ID,]

x_location = c(-17.5,-7)
y_location = c(6.8,16.7)

sub5 <- ggplot(data = a, aes(fill = val2)) +
  geom_sf(aes(geometry = geometry)) +
  scale_fill_manual(
    values = pal,
    breaks = breaks_labels,
    labels = breaks_labels,
    drop = FALSE) +
  coord_sf(xlim = x_location, ylim = y_location, expand = FALSE)+
  ggtitle('West Africa')+
  theme_map_sub


a <- Prevalence2021_ASR_map[Prevalence2021_ASR_map$location_id %in% subregions_shp[['Eastern Mediterranean']]$Location.ID,]

x_location = c(30.5,38.5)
y_location = c(29.1,34.9)

sub6 <- ggplot(data = a, aes(fill = val2)) +
  geom_sf(aes(geometry = geometry)) +
  scale_fill_manual(
    values = pal,
    breaks = breaks_labels,
    labels = breaks_labels,
    drop = FALSE) +
  coord_sf(xlim = x_location, ylim = y_location, expand = FALSE)+
  ggtitle('Eastern Mediterranean')+
  theme_map_sub


a <- Prevalence2021_ASR_map[Prevalence2021_ASR_map$location_id %in% subregions_shp[['Nothern Europe']]$Location.ID,]

x_location = c(2.5,27)
y_location = c(48,59)
sub7 <- ggplot(data = a, aes(fill = val2)) +
  geom_sf(aes(geometry = geometry)) +
  scale_fill_manual(
    values = pal,
    breaks = breaks_labels,
    labels = breaks_labels,
    drop = FALSE) +
  coord_sf(xlim = x_location, ylim = y_location, expand = FALSE)+
  ggtitle('Northern Europe')+
  theme_map_sub


plot1 <- (sub1 + sub2 + sub3 + sub4) + plot_layout(nrow = 1)

plot2 <- (sub5 | sub6) / sub7 + plot_layout(height = c(1, 1.2))

plot3 <- plot1|plot2  + plot_layout(widths = c(1, 15))

Prevalence2021_ASR_map_plot <- Prevalence2021_ASR_map_plot / plot3 + plot_layout(height = c(2,1),widths = c(2,1))


ggsave(Prevalence2021_ASR_map_plot, file = './2.输出/Prevalence_hip_2021_ASR_map.pdf', units = 'cm', width = 24, height = 17.8)


regions_1990to2021 <- read.csv('Global_21Regions_1990to2021.csv', header = T)
super_1990to2021   <- read.csv('7big+1global.csv', header = T)

countries_change_raw <- read.csv('204counties_change.csv', header = T)
regions_change_raw   <- read.csv('Global_21Regions_change.csv', header = T)
super_change_raw     <- read.csv('7big+1global change.csv', header = T)


extract_aspr_2021 <- function(df) {
  tmp <- df
  if ("cause_name"   %in% names(tmp)) tmp <- dplyr::filter(tmp, cause_name == "Osteoarthritis hip")
  if ("year"         %in% names(tmp)) tmp <- dplyr::filter(tmp, year == 2021 | year == "2021")
  if ("sex_name"     %in% names(tmp)) tmp <- dplyr::filter(tmp, sex_name == "Both")
  if ("age_name"     %in% names(tmp)) tmp <- dplyr::filter(tmp, age_name == "Age-standardized")
  if ("measure_name" %in% names(tmp)) tmp <- dplyr::filter(tmp, measure_name == "Prevalence")
  if ("metric_name"  %in% names(tmp)) tmp <- dplyr::filter(tmp, metric_name == "Rate")
  dplyr::select(tmp, location_id, location_name, ASPR_per_100k = val)
}

extract_aspr_change <- function(df) {
  tmp <- df
  if ("cause_name"   %in% names(tmp)) tmp <- dplyr::filter(tmp, cause_name == "Osteoarthritis hip")
  if ("sex_name"     %in% names(tmp)) tmp <- dplyr::filter(tmp, sex_name == "Both")
  if ("age_name"     %in% names(tmp)) tmp <- dplyr::filter(tmp, age_name == "Age-standardized")
  if ("measure_name" %in% names(tmp)) tmp <- dplyr::filter(tmp, measure_name == "Prevalence")
  if ("metric_name"  %in% names(tmp)) tmp <- dplyr::filter(tmp, metric_name == "Rate")
  if ("change" %in% names(tmp)) {
    dplyr::select(tmp, location_id, location_name, ASPR_change_1990_2021 = change)
  } else {
    dplyr::select(tmp, location_id, location_name, ASPR_change_1990_2021 = val)
  }
}


countries_2021 <- Prevalence2021_ASR %>%
  dplyr::select(location_id, location_name, ASPR_per_100k = val)
regions_2021 <- extract_aspr_2021(regions_1990to2021)
super_2021   <- extract_aspr_2021(super_1990to2021)


countries_change <- extract_aspr_change(countries_change_raw)
regions_change   <- extract_aspr_change(regions_change_raw)
super_change     <- extract_aspr_change(super_change_raw)


countries_join <- dplyr::left_join(countries_2021, countries_change, by = c("location_id","location_name")) %>%
  dplyr::mutate(level = "Country")
regions_join <- dplyr::left_join(regions_2021, regions_change, by = c("location_id","location_name")) %>%
  dplyr::mutate(level = "GBD21Region")
super_join <- dplyr::left_join(super_2021, super_change, by = c("location_id","location_name")) %>%
  dplyr::mutate(level = "SuperRegion/Global")


ASPR_2021_with_change_all <- dplyr::bind_rows(super_join, regions_join, countries_join) %>%
  dplyr::select(level, location_id, location_name, ASPR_per_100k, ASPR_change_1990_2021)

write.csv(ASPR_2021_with_change_all, "./2.输出/ASPR_2021_with_change_7super_21region_204countries.csv", row.names = FALSE)
print("合并后的ASPR与变化表已保存到 ./2.输出/ASPR_2021_with_change_7super_21region_204countries.csv")


setwd("D:/oa/Figure3")


library(tidyverse)
library(ggplot2)
library(scales)


Global_2021_AgeandSex <- read.csv('Global_2021_AgeandSex.csv')


Global_2021_AgeandSex$age_name <- str_split(Global_2021_AgeandSex$age_name, ' ', simplify = T)[,1]


Global_2021_AgeandSex_Prevalance <- Global_2021_AgeandSex %>%
  select("location_name","year","sex_name","age_name","measure_name","metric_name","cause_name","val","upper","lower") %>%
  filter(location_name == "Global",
         year == "2021",
         cause_name == "Osteoarthritis hip",
         age_name %in% c("<5","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+"),
         sex_name %in% c("Male","Female"),
         measure_name == 'Prevalence',
         metric_name %in% c('Number', 'Rate')) %>%
  mutate(val = if_else(metric_name == 'Number', val/1000000,val/(1000*10/3)),
         upper = if_else(metric_name == 'Number', upper/1000000,upper/(1000*10/3)),
         lower = if_else(metric_name == 'Number', lower/1000000,lower/(1000*10/3)))


Global_2021_AgeandSex_Prevalance$age_name <- factor(Global_2021_AgeandSex_Prevalance$age_name, levels = c('<5','5-9','10-14','15-19','20-24','25-29','30-34','35-39','40-44','45-49','50-54','55-59','60-64','65-69','70-74','75-79','80-84','85-89','90-94','95+'))
Global_2021_AgeandSex_Prevalance$sex_name <- factor(Global_2021_AgeandSex_Prevalance$sex_name, levels = c('Male', 'Female'))


summary_table_age_sex <- Global_2021_AgeandSex_Prevalance %>%

  mutate(
    val = if_else(metric_name == 'Number', val * 1000000, val * (1000 * 10/3)),
    upper = if_else(metric_name == 'Number', upper * 1000000, upper * (1000 * 10/3)),
    lower = if_else(metric_name == 'Number', lower * 1000000, lower * (1000 * 10/3))
  ) %>%

  select(age_name, sex_name, metric_name, val, lower, upper) %>%
  pivot_wider(
    names_from = metric_name,
    values_from = c(val, lower, upper),
    names_sep = "_"
  ) %>%

  select(age_name, sex_name, val_Number, lower_Number, upper_Number, val_Rate, lower_Rate, upper_Rate)


print("--- 按年龄和性别分列的髋关节骨关节炎全球患病率汇总表 (2021) ---")
print(head(summary_table_age_sex))


if (!dir.exists('./2.输出')) {
  dir.create('./2.输出')
}

write.csv(summary_table_age_sex, "./2.输出/Prevalence_hip_age_sex_summary_2021.csv", row.names = FALSE)
print("汇总表已保存到 ./2.输出/Prevalence_hip_age_sex_summary_2021.csv")


plot <- ggplot() +

  geom_col(data = subset(Global_2021_AgeandSex_Prevalance, metric_name == "Number"), aes(x = age_name, y = val, fill = sex_name),  width = 0.6,position = position_dodge(width = 0.6)) +

  geom_errorbar(data = subset(Global_2021_AgeandSex_Prevalance, metric_name == "Number"), aes(x = age_name, ymin = lower, ymax = upper, group = sex_name),width = 0.3,position = position_dodge(width = 0.6)) +

  geom_line(data = subset(Global_2021_AgeandSex_Prevalance, metric_name == "Rate"), aes(x = age_name, y = val, group = sex_name, color = sex_name),linetype = "dashed",size = 1) +
  geom_line(data = subset(Global_2021_AgeandSex_Prevalance, metric_name == "Rate"), aes(x = age_name, y = upper, group = sex_name, color = sex_name), linetype = "dashed") +
  geom_line(data = subset(Global_2021_AgeandSex_Prevalance, metric_name == "Rate"), aes(x = age_name, y = lower, group = sex_name, color = sex_name),linetype = "dashed") +

  scale_fill_manual(name = "Number", values = c("Male" = "#6B58A6", "Female" = "#FCAF17"), guide = "legend") +

  scale_color_manual(name = "Rate", values = c("Male" = "#D30F8C", "Female" = "#0871B9"), guide = "legend") +

  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = 'top',
        legend.title = element_blank(),
        legend.key.size = unit(0.3, "cm"),
        axis.text = element_text(size = 6),
        axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1),
        axis.text.y.left = element_text(margin = margin(r = 0)),
        axis.text.y.right = element_text(margin = margin(r = 0)),
        axis.title = element_text(size = 8),
        axis.ticks = element_blank()) +


  scale_y_continuous(
    name = "Total prevalent cases (millions)",
    sec.axis = sec_axis(~ .*10/3, name = "Prevalence per 100,000 \n population (000s)"),
    expand = c(0,0))  +
  xlab('Age group (years)')


if (!dir.exists('./2.输出')) {
  dir.create('./2.输出')
}
ggsave(plot, file = './2.输出/Prevalence_Osteoarthritis_hip_age_and_sex_2021.pdf',units = 'cm', height = 6, width = 10)


setwd("D:/oa/Figure4")


library(readxl)
library(tidyverse)
library(ggplot2)


SDI_value <- read.csv('SDI_1990-2021.csv', header = TRUE, check.names = FALSE)

Global_21Regions_1990to2021 <- read.csv('Global_21Regions_1990to2021.csv', header=T)


order_globalandregions <- read.csv("order_globalandregions.csv", header = F)


SDI_value_Global_21Regions <- SDI_value %>%
  filter(Location %in% order_globalandregions$V1)

SDI_value_Global_21Regions_long <- pivot_longer(SDI_value_Global_21Regions,
                                                cols = colnames(SDI_value_Global_21Regions)[c(-1,-2)],
                                                names_to = 'Year',
                                                values_to = 'SDI') %>%
  mutate(Year = as.character(Year)) %>%
  mutate(ID = paste0(Location,"_", Year))


Global_21regions_DALYs_ASR <- Global_21Regions_1990to2021 %>%
  select("location_name","year","sex_name","age_name","measure_name","metric_name","val", "cause_name") %>%
  filter(cause_name == "Osteoarthritis hip",
         sex_name == "Both",
         age_name == "Age-standardized",
         measure_name == "DALYs (Disability-Adjusted Life Years)",
         metric_name == "Rate") %>%
  rename("DALYs_ASR" = "val") %>%
  mutate(DALYs_ASR = round(DALYs_ASR/1000,5),
         ID = paste0(location_name,"_",year))


Global_21regions_DALYs_ASR_LocationandYear <- left_join(SDI_value_Global_21Regions_long,Global_21regions_DALYs_ASR, by = 'ID') %>%
  mutate(SDI = as.numeric(str_replace(SDI,'\\·','.')))


summary_table_sdi <- Global_21regions_DALYs_ASR_LocationandYear %>%

  filter(Year %in% c("1990", "2021")) %>%

  select(Location, Year, SDI, DALYs_ASR) %>%

  pivot_wider(
    names_from = Year,
    values_from = c(SDI, DALYs_ASR),
    names_sep = "_"
  ) %>%

  mutate(
    SDI_Change = SDI_2021 - SDI_1990,
    DALYs_ASR_Change = DALYs_ASR_2021 - DALYs_ASR_1990
  ) %>%

  arrange(desc(SDI_2021))


print("--- 1990-2021年各区域SDI与DALYs ASR变化汇总表 ---")
print(summary_table_sdi)


write.csv(summary_table_sdi, "SDI_vs_DALY_summary_1990_2021.csv", row.names = FALSE)
print("汇总表已保存到 SDI_vs_DALY_summary_1990_2021.csv")


cor_test <- cor.test(Global_21regions_DALYs_ASR_LocationandYear[,"SDI",drop = T], Global_21regions_DALYs_ASR_LocationandYear[, "DALYs_ASR", drop = TRUE])

cor_r <- cor_test$estimate
cor_int<- cor_test$conf.int
cor_p <- cor_test$p.value


Corr_Global_Regions_SDI <- ggplot(Global_21regions_DALYs_ASR_LocationandYear,
                                  aes(Global_21regions_DALYs_ASR_LocationandYear[,"SDI",drop = T],
                                      Global_21regions_DALYs_ASR_LocationandYear[,"DALYs_ASR",drop = T])) +
  geom_point(aes(color = Location, shape= Location))+
  scale_shape_manual(values = 1:22) +
  geom_smooth(colour='black',stat = "smooth",method='loess',se=F,span=0.5) +
  geom_text(x = min(Global_21regions_DALYs_ASR_LocationandYear$SDI + 0.2, na.rm = TRUE),
            y = max(Global_21regions_DALYs_ASR_LocationandYear[,"DALYs_ASR",drop = T], na.rm = TRUE)*0.8,
            label = paste("R =", round(cor_r, 2),"(", round(cor_int[1],2), "to", round(cor_int[2],2), ")","\n", "p =", ifelse(cor_p<0.001,"< 0.001",round(cor_p,2))),
            hjust = 1, vjust = 0,
            size = 4)+
  ylab(paste0("DALYs_ASR",'\n (100,000 persons)'))+
  xlab("Sociodemographic index")+
  theme_bw(base_size = 8)+
  theme(panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent"),
        legend.position = 'top',
        legend.key.size = unit(0.3, "cm"),
        legend.title = element_blank(),
        axis.text.x = element_text(face = "bold",size = 8),
        axis.text.y = element_text(face = "bold",size = 8),
        axis.title.x = element_text(face = "bold",size = 10),
        axis.title.y = element_text(face = "bold",size = 10))


ggsave(Corr_Global_Regions_SDI, file = 'OA_hip_DALYs_rate_22region_SDI.pdf', width = 7, height = 4)


setwd("D:/oa/FigureS10")


library(readxl)
library(tidyverse)
library(ggplot2)


SDI_value <- read.csv('SDI_1990-2021.csv', header = TRUE, check.names = FALSE)

Global_21Regions_1990to2021 <- read.csv('Global_21Regions_1990to2021.csv', header=T)
countries_204 <- read.csv('204counties_1990to2021.csv', header=T)
Global_21Regions_204countries_1990to2021 <- rbind(Global_21Regions_1990to2021,countries_204)


list_global_21regions_204countries <- read_xlsx("list_global_21regions_204countries.xlsx", sheet=1)


list_204countries <- list_global_21regions_204countries %>%
  filter(!subregion %in% c("Global","region21")) %>%
  rename("Location" = "Country")


SDI_value_204countries <- SDI_value %>%
  select("Location","location_id","2021") %>%
  filter(location_id %in% list_204countries$location_id) %>%
  rename("SDI_2021"="2021")


Countries204_DALYs_ASR <- Global_21Regions_204countries_1990to2021 %>%
  select("location_name","location_id","year","sex_name","age_name","measure_name","metric_name","val", "cause_name") %>%
  filter(cause_name == "Osteoarthritis hip",
         year == "2021",
         sex_name == "Both",
         age_name == "Age-standardized",
         measure_name == "DALYs (Disability-Adjusted Life Years)",
         metric_name == "Rate") %>%
  rename("DALYs_ASR_2021" = "val",
         "Location" = "location_name")


Countries204_DALYs_ASR_SDI_1 <- left_join(SDI_value_204countries,Countries204_DALYs_ASR, by = 'location_id')
Countries204_DALYs_ASR_SDI_2 <- left_join(Countries204_DALYs_ASR_SDI_1, list_204countries, by = 'location_id') %>%
  mutate(SDI_2021 = as.numeric(str_replace(SDI_2021,'\\·','.')))


summary_table_204 <- Countries204_DALYs_ASR_SDI_2 %>%

  select(
    Country = Location.x,
    Region = subregion,
  ) %>%

  arrange(desc(DALYs_ASR_2021))


print("--- 2021年各国家SDI与DALYs ASR汇总表 (按DALYs ASR降序) ---")
print(head(summary_table_204, 10))


write.csv(summary_table_204, "SDI_vs_DALY_204countries_summary_2021.csv", row.names = FALSE)
print("汇总表已保存到 SDI_vs_DALY_204countries_summary_2021.csv")


cor_test <- cor.test(Countries204_DALYs_ASR_SDI_2[,"SDI_2021",drop = T], Countries204_DALYs_ASR_SDI_2[, "DALYs_ASR_2021", drop = TRUE])

cor_r <- cor_test$estimate
cor_int<- cor_test$conf.int
cor_p <- cor_test$p.value


Corr_204countries_SDI <- ggplot(Countries204_DALYs_ASR_SDI_2,
                                aes(Countries204_DALYs_ASR_SDI_2[,"SDI_2021",drop = T],
                                    Countries204_DALYs_ASR_SDI_2[,"DALYs_ASR_2021",drop = T])) +
  geom_point(aes(color = subregion),size = 0.5)+
  scale_shape_manual(values = 1:22) +
  geom_smooth(colour='black',stat = "smooth",method='loess',se=F,span=0.5) +
  geom_text(x = min(Countries204_DALYs_ASR_SDI_2$SDI_2021 + 0.25, na.rm = TRUE),
            y = max(Countries204_DALYs_ASR_SDI_2[,"DALYs_ASR_2021",drop = T], na.rm = TRUE)*0.8,
            label = paste("R =", round(cor_r, 2),"(", round(cor_int[1],2), "to", round(cor_int[2],2), ")","\n", "p =", ifelse(cor_p<0.001,"< 0.001",round(cor_p,2))),
            hjust = 1, vjust = 0,
            size = 4)+
  geom_text(aes(label = Location,color = subregion),
            hjust = 0.7, vjust = 0,
            size = 1) +
  ylab(paste0("DALYs_ASR",'\n (100,000 persons)'))+
  xlab("Socio-demographic index")+
  theme_bw(base_size = 8)+
  theme(panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent"),
        legend.position = 'top',
        legend.key.size = unit(0.3, "cm"),
        legend.title = element_blank(),
        axis.text.x = element_text(face = "bold",size = 8),
        axis.text.y = element_text(face = "bold",size = 8),
        axis.title.x = element_text(face = "bold",size = 10),
        axis.title.y = element_text(face = "bold",size = 10))


ggsave(Corr_204countries_SDI, file = 'OA_hip_DALYs_rate_204countries_SDI_2021.pdf', width = 8, height = 4)


  setwd("D:/oa/FigureS11")


library(tidyverse)
library(ggplot2)


Risk_factor_2021 <- read.csv("Risk_factor_2021.csv",header = T)
order_globalandregions <- read.csv("order_globalandregions.csv",header = F)
setequal(unique(Risk_factor_2021$location_name), order_globalandregions$V1)


unique(Risk_factor_2021$rei_name)


Risk_factor_2021_DALYs_Male <- Risk_factor_2021 %>%
  select("location_name","year","sex_name","age_name","measure_name","metric_name","rei_name","val","upper", "lower", "cause_name" ) %>%
  filter(cause_name == "Osteoarthritis hip",
         year == "2021",
         sex_name == "Male",
         age_name == "Age-standardized",
         measure_name == "DALYs (Disability-Adjusted Life Years)",
         metric_name == "Percent")


Risk_factor_2021_DALYs_Male <- Risk_factor_2021_DALYs_Male %>%
  mutate(val = round(val*100,1),
         upper_1 = round(upper*100,1),
         lower = round(lower*100,1),
         rei_name = str_wrap(Risk_factor_2021_DALYs_Male$rei_name, width = 30))


Risk_factor_2021_DALYs_Male <- Risk_factor_2021_DALYs_Male %>%
  mutate(location_name = fct_relevel(location_name,rev(c('Global','High-income Asia Pacific','High-income North America', 'Western Europe','Australasia', 'Andean Latin America','Tropical Latin America','Central Latin America','Southern Latin America', 'Caribbean', 'Central Europe', 'Eastern Europe','Central Asia', 'North Africa and Middle East', 'South Asia', 'Southeast Asia', 'East Asia','Oceania', 'Western Sub-Saharan Africa', 'Eastern Sub-Saharan Africa', 'Central Sub-Saharan Africa', 'Southern Sub-Saharan Africa')))) %>%
  arrange(location_name, rei_name)


p1 <- ggplot()+
  geom_col(data = Risk_factor_2021_DALYs_Male,aes(x = location_name,y = val, fill = rei_name),color = 'black',width = .7,position = 'dodge',size = .3)+

  scale_y_continuous(breaks = c(0,20,40,60,80),limits = c(0, 90),expand = c(0,0))+
  coord_flip() +
  geom_text(data = Risk_factor_2021_DALYs_Male,
            aes(x= location_name, label=val, y=val+1),
            position=position_dodge(width=0.7), vjust=0.5,hjust = -0.1, size = 2) +
  facet_wrap(~rei_name, scales = "free_x",nrow = 1)+
  ylab('DALYs attributable to risk factors (%)')+
  xlab("GBD regions")+
  theme_light()+
  theme(panel.background = element_rect(fill = "transparent"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 6, colour = 'black'),
        axis.title.x = element_text(size = 8, colour = 'black'),
        axis.title.y = element_text(size = 8, colour = 'black'),
        strip.background = element_rect(fill = '#B6D1FA'),
        strip.text = element_text(colour = 'black',size = 6,lineheight = 1),
        legend.position = 'none')


ggsave(p1,file = "OA_hip_risk_factor_male_2021.pdf", units = 'cm', height = 15, width = 25 )


Risk_factor_2021_DALYs_Female <- Risk_factor_2021 %>%
  select("location_name","year","sex_name","age_name","measure_name","metric_name","rei_name","val","upper", "lower", "cause_name" ) %>%
  filter(cause_name == "Osteoarthritis hip",
         year == "2021",
         sex_name == "Female",
         age_name == "Age-standardized",
         measure_name == "DALYs (Disability-Adjusted Life Years)",
         metric_name == "Percent")


Risk_factor_2021_DALYs_Female <- Risk_factor_2021_DALYs_Female %>%
  mutate(val = round(val*100,1),
         upper_1 = round(upper*100,1),
         lower = round(lower*100,1),
         rei_name = str_wrap(rei_name, width = 30))


Risk_factor_2021_DALYs_Female <- Risk_factor_2021_DALYs_Female %>%
  mutate(location_name = fct_relevel(location_name,rev(c('Global','High-income Asia Pacific','High-income North America', 'Western Europe','Australasia', 'Andean Latin America','Tropical Latin America','Central Latin America','Southern Latin America', 'Caribbean', 'Central Europe', 'Eastern Europe','Central Asia', 'North Africa and Middle East', 'South Asia', 'Southeast Asia', 'East Asia','Oceania', 'Western Sub-Saharan Africa', 'Eastern Sub-Saharan Africa', 'Central Sub-Saharan Africa', 'Southern Sub-Saharan Africa')))) %>%
  arrange(location_name, rei_name)


p2 <- ggplot()+
  geom_col(data = Risk_factor_2021_DALYs_Female,aes(x = location_name,y = val, fill = rei_name),color = 'black',width = .7,position = 'dodge',size = .3)+

  scale_y_continuous(breaks = c(0,20,40,60,80),limits = c(0, 90),expand = c(0,0))+
  coord_flip() +
  geom_text(data = Risk_factor_2021_DALYs_Female,
            aes(x= location_name, label=val, y=val+1),
            position=position_dodge(width=0.7), vjust=0.5,hjust = -0.1, size = 2) +
  facet_wrap(~rei_name, scales = "free_x",nrow = 1)+
  ylab('DALYs attributable to risk factors (%)')+
  xlab("GBD regions")+
  theme_light()+
  theme(panel.background = element_rect(fill = "transparent"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 6, colour = 'black'),
        axis.title.x = element_text(size = 8, colour = 'black'),
        axis.title.y = element_text(size = 8, colour = 'black'),
        strip.background = element_rect(fill = '#B6D1FA'),
        strip.text = element_text(colour = 'black',size = 6,lineheight = 1),
        legend.position = 'none')


ggsave(p2,file = "OA_hip_risk_factor_female_2021.pdf", units = 'cm', height = 15, width = 25 )


Risk_factor_2021_DALYs_Both <- Risk_factor_2021 %>%
  select("location_name","year","sex_name","age_name","measure_name","metric_name","rei_name","val","upper", "lower", "cause_name" ) %>%
  filter(cause_name == "Osteoarthritis hip",
         year == "2021",
         sex_name == "Both",
         age_name == "Age-standardized",
         measure_name == "DALYs (Disability-Adjusted Life Years)",
         metric_name == "Percent")


Risk_factor_2021_DALYs_Both <- Risk_factor_2021_DALYs_Both %>%
  mutate(val = round(val*100,1),
         upper_1 = round(upper*100,1),
         lower = round(lower*100,1),
         rei_name = str_wrap(rei_name, width = 30))


Risk_factor_2021_DALYs_Both <- Risk_factor_2021_DALYs_Both %>%
  mutate(location_name = fct_relevel(location_name,rev(c('Global','High-income Asia Pacific','High-income North America', 'Western Europe','Australasia', 'Andean Latin America','Tropical Latin America','Central Latin America','Southern Latin America', 'Caribbean', 'Central Europe', 'Eastern Europe','Central Asia', 'North Africa and Middle East', 'South Asia', 'Southeast Asia', 'East Asia','Oceania', 'Western Sub-Saharan Africa', 'Eastern Sub-Saharan Africa', 'Central Sub-Saharan Africa', 'Southern Sub-Saharan Africa')))) %>%
  arrange(location_name, rei_name)


p3 <- ggplot()+
  geom_col(data = Risk_factor_2021_DALYs_Both,aes(x = location_name,y = val, fill = rei_name),color = 'black',width = .7,position = 'dodge',size = .3)+

  scale_y_continuous(breaks = c(0,20,40,60,80),limits = c(0, 90),expand = c(0,0))+
  coord_flip() +
  geom_text(data = Risk_factor_2021_DALYs_Both,
            aes(x= location_name, label=val, y=val+1),
            position=position_dodge(width=0.7), vjust=0.5,hjust = -0.1, size = 2) +
  facet_wrap(~rei_name, scales = "free_x",nrow = 1)+
  ylab('DALYs attributable to risk factors (%)')+
  xlab("GBD regions")+
  theme_light()+
  theme(panel.background = element_rect(fill = "transparent"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 6, colour = 'black'),
        axis.title.x = element_text(size = 8, colour = 'black'),
        axis.title.y = element_text(size = 8, colour = 'black'),
        strip.background = element_rect(fill = '#B6D1FA'),
        strip.text = element_text(colour = 'black',size = 6,lineheight = 1),
        legend.position = 'none')


ggsave(p3,file = "OA_hip_risk_factor_both_2021.pdf", units = 'cm', height = 15, width = 25 )


library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(ggtext)
library(patchwork)
library(scales)


set.seed(123)


das_gupta_decomposition <- function(P1_age, P2_age, D1_age, D2_age) {

  P1_age[P1_age == 0] <- 1e-9
  P2_age[P2_age == 0] <- 1e-9

  R1_age <- D1_age / P1_age
  R2_age <- D2_age / P2_age
  C1_age <- P1_age / sum(P1_age)
  C2_age <- P2_age / sum(P2_age)

  term1 <- sum(0.5 * (R1_age + R2_age) * (C1_age + C2_age)) * (sum(P2_age) - sum(P1_age))
  term2 <- sum(0.5 * (R1_age + R2_age) * (C2_age - C1_age)) * (sum(P1_age) + sum(P2_age))
  term3 <- sum(0.5 * (C1_age + C2_age) * (R2_age - R1_age)) * (sum(P1_age) + sum(P2_age))

  return(c("Population Growth" = term1, "Population Aging" = term2, "Epidemiological Change" = term3))
}


generate_log_normal_draws <- function(n_draws, means, lowers, uppers) {

  means[means <= 0] <- 1e-9
  lowers[lowers <= 0] <- 1e-9
  uppers[uppers <= 0] <- 1e-9


  log_sds <- (log(uppers) - log(lowers)) / 3.92


  log_means <- log(means) - 0.5 * log_sds^2


  draws <- map(1:length(means), ~ rlnorm(n_draws, meanlog = log_means[.x], sdlog = log_sds[.x]))


  return(as.data.frame(do.call(cbind, draws)))
}


run_decomposition_with_ci <- function(data_1990, data_2021, n_draws = 1000) {

  daly_draws_1990 <- generate_log_normal_draws(n_draws, data_1990$dalys_val, data_1990$dalys_lower, data_1990$dalys_upper)
  daly_draws_2021 <- generate_log_normal_draws(n_draws, data_2021$dalys_val, data_2021$dalys_lower, data_2021$dalys_upper)
  pop_draws_1990 <- generate_log_normal_draws(n_draws, data_1990$pop_val, data_1990$pop_lower, data_1990$pop_upper)
  pop_draws_2021 <- generate_log_normal_draws(n_draws, data_2021$pop_val, data_2021$pop_lower, data_2021$pop_upper)


  sim_results <- map_dfr(1:n_draws, ~ {
    D1_age_draw <- as.numeric(daly_draws_1990[.x, ])
    D2_age_draw <- as.numeric(daly_draws_2021[.x, ])
    P1_age_draw <- as.numeric(pop_draws_1990[.x, ])
    P2_age_draw <- as.numeric(pop_draws_2021[.x, ])

    das_gupta_decomposition(P1_age_draw, P2_age_draw, D1_age_draw, D2_age_draw)
  })


  summary_results <- sim_results %>%
    pivot_longer(everything(), names_to = "component", values_to = "contribution") %>%
    group_by(component) %>%
    summarise(
      contribution_median = median(contribution),
      contribution_lower = quantile(contribution, 0.025),
      contribution_upper = quantile(contribution, 0.975),
      .groups = 'drop'
    )

  return(summary_results)
}


setwd("D:/oa/anynasis")
daly_data <- read.csv("Das Gupta 7region+5sdi+global DALYs.csv")
pop_data <- read.csv("Das Gupta 7region+5sdi+global populations.csv")

master_data <- daly_data %>%
  filter(cause_name == "Osteoarthritis hip", metric_name == "Number", sex_name == "Both") %>%
  select(location = location_name, age = age_name, year, dalys_val = val, dalys_upper = upper, dalys_lower = lower) %>%
  left_join(
      filter(sex_name == "Both") %>%
      select(location = location_name, age = age_name, year, pop_val = val, pop_upper = upper, pop_lower = lower),
    by = c("location", "age", "year")
  ) %>%
  filter(!grepl("All ages|standardized", age, ignore.case = TRUE)) %>%
  na.omit()


all_decomp_results_ci <- master_data %>%
  group_by(location) %>%
  nest() %>%
  mutate(decomp = map(data, ~{
    data_1990 <- .x %>% filter(year == 1990) %>% arrange(age)
    data_2021 <- .x %>% filter(year == 2021) %>% arrange(age)
    run_decomposition_with_ci(data_1990, data_2021, n_draws = 1000)
  })) %>%
  select(location, decomp) %>%
  unnest(cols = decomp)

plot_data_prep <- all_decomp_results_ci %>%
  mutate(
    component = factor(component, levels = c("Epidemiological Change", "Population Aging", "Population Growth"))
  )

location_levels <- c(
  sort(unique(plot_data_prep$location[!grepl("Global|SDI", plot_data_prep$location)]))
)
plot_data_prep$location <- factor(plot_data_prep$location, levels = location_levels)

create_single_plot_ci <- function(region_name, show_y_axis = TRUE) {
  region_data <- plot_data_prep %>% filter(location == region_name)

  plot_title <- paste0("<b>", region_name, "</b>")

  annotation_info <- region_data %>%
    summarise(
      total_change = sum(contribution_median),
      x_position = max(contribution_upper),
      .groups = 'drop'
    )

  p <- ggplot(region_data, aes(x = contribution_median, y = component)) +
    geom_errorbarh(aes(xmin = contribution_lower, xmax = contribution_upper), height = 0.2, color = "grey70", linewidth = 0.8) +
    geom_segment(aes(x = 0, xend = contribution_median, y = component, yend = component), color = "grey60") +
    geom_point(aes(color = component), size = 4) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    geom_text(
      data = annotation_info,
      aes(y = 3.5, x = x_position, label = paste("Total Change:", scales::comma(total_change, accuracy = 1))),
      hjust = 1, vjust = 1, size = 3.5, color = "black", fontface = "bold", inherit.aes = FALSE
    ) +
    scale_color_manual(values = c(
      "Epidemiological Change" = "#2E8B57",
      "Population Aging" = "#E46C0A",
      "Population Growth" = "#3B75AF"
    )) +
    scale_y_discrete(limits = rev(levels(region_data$component))) +
    scale_x_continuous(labels = scales::comma) +
    labs(title = plot_title, x = NULL, y = NULL, color = "Decomposition Component") +
    theme_bw(base_size = 12) +
    theme(
      plot.title = element_markdown(size = 11, face = "bold", margin = margin(t = 5, b = 5)),
      plot.margin = margin(5, 5, 15, 5)
    )

  if (!show_y_axis) {
    p <- p + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
  } else {
    p <- p + theme(axis.text.y = element_text(face = "bold"))
  }
  return(p)
}


NCOL <- 4
plot_list <- imap(location_levels, ~{
  show_axis <- (.y - 1) %% NCOL == 0
  create_single_plot_ci(.x, show_y_axis = show_axis)
})


final_plot_ci <- wrap_plots(plot_list, ncol = NCOL, guides = "collect") +
  plot_annotation(
    title = "Decomposition of Change in Hip OA DALYs with 95% Uncertainty Intervals (1990-2021)",
    subtitle = "Analysis for Global, 7 Super-regions and 5 SDI Quintiles based on 1000 Monte Carlo draws",
    caption = "Contribution to Change in DALYs (Median and 95% UI)",
    theme = theme(
      plot.title = element_text(face = "bold", size = 22, hjust = 0.5),
      plot.subtitle = element_text(size = 15, color = "grey20", hjust = 0.5, margin = margin(b = 10)),
      plot.caption = element_text(face = "bold", size = 14, hjust = 0.5, vjust = -1)
    )
  ) &
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 14),
    legend.text = element_text(size = 12)
  )

print(final_plot_ci)


ggsave(
  plot = final_plot_ci,
  width = 18,
  height = 15,
  units = "in",
  device = "pdf"
)

write.csv(
  row.names = FALSE
)

print("--- Analysis with Uncertainty Complete (Final Scientifically Corrected Version) ---")
print("Plot saved to das_gupta_decomposition_lollipop_plot_with_CI_FINAL.pdf")
print("Summary table with 95% CI saved to das_gupta_decomposition_summary_with_CI_FINAL.csv")


library(dplyr)
library(ggplot2)
library(tidyr)
library(purrr)
library(stringr)
library(strucchange)

rm(list = ls())


setwd("D:/oa/anynasis")
full_gbd_data <- read.csv("7big+1global.csv")


asr_data <- full_gbd_data %>%
  filter(
    cause_name == "Osteoarthritis hip",
    measure_name == "DALYs (Disability-Adjusted Life Years)",
    metric_name == "Rate",
    age_name == "Age-standardized",
    sex_name == "Both"
  ) %>%
  rename(location = location_name, asr = val) %>%
  select(location, year, asr) %>%
  distinct(location, year, .keep_all = TRUE) %>%
  arrange(location, year)


print("--- Filtered Data for Main Analysis ---")
if(nrow(asr_data) == 0) stop("Filtering resulted in zero rows.")


run_joinpoint_analysis <- function(df, max_breakpoints = 2, h = 3) {


  df <- df %>% arrange(year)


  best_model <- NULL
  best_bic <- Inf
  best_bp_obj <- NULL


  mod0 <- lm(log(asr) ~ year, data = df)
  best_model <- mod0
  best_bic <- BIC(mod0)


  for (k in 1:max_breakpoints) {

    bp_k_obj <- try(breakpoints(log(asr) ~ year, data = df, h = h, breaks = k), silent = TRUE)

    if (inherits(bp_k_obj, "try-error") || is.na(bp_k_obj$breakpoints[1])) {
    }


    mod_k <- lm(log(asr) ~ breakfactor(bp_k_obj) / year - 1, data = df)

    bic_k <- BIC(mod_k)


    if (bic_k < best_bic - 2) {
      best_bic <- bic_k
      best_model <- mod_k
      best_bp_obj <- bp_k_obj
    }
  }


  if (is.null(best_bp_obj)) {

    slope_summary <- summary(best_model)$coefficients
    slope_est <- slope_summary["year", "Estimate"]
    slope_se <- slope_summary["year", "Std. Error"]
    apc <- (exp(slope_est) - 1) * 100
    apc_lwr <- (exp(slope_est - 1.96 * slope_se) - 1) * 100
    apc_upr <- (exp(slope_est + 1.96 * slope_se) - 1) * 100

    results <- data.frame(
      location = unique(df$location), num_breakpoints = 0, breakpoint_year = NA_character_,
      aapc = apc, aapc_lwr = apc_lwr, aapc_upr = apc_upr,
      segment = 1, apc = apc, apc_lwr = apc_lwr, apc_upr = apc_upr,
      start_year = min(df$year), end_year = max(df$year)
    )
  } else {

    breakpoints <- df$year[best_bp_obj$breakpoints]
    coef_summary <- summary(best_model)$coefficients

    slope_indices <- grep(":year$", rownames(coef_summary))
    slope_estimates <- coef_summary[slope_indices, "Estimate"]
    slope_ses <- coef_summary[slope_indices, "Std. Error"]

    apc_df <- data.frame(
      apc = (exp(slope_estimates) - 1) * 100,
      apc_lwr = (exp(slope_estimates - 1.96 * slope_ses) - 1) * 100,
      apc_upr = (exp(slope_estimates + 1.96 * slope_ses) - 1) * 100
    )

    segment_years <- c(min(df$year), sort(breakpoints), max(df$year))
    years_per_segment <- diff(segment_years)


    avg_slope_est <- weighted.mean(slope_estimates, years_per_segment)
    aapc <- (exp(avg_slope_est) - 1) * 100


    vcov_mat <- vcov(best_model)
    slope_cov_mat <- vcov_mat[slope_indices, slope_indices, drop = FALSE]
    weights <- years_per_segment / sum(years_per_segment)
    var_avg_slope <- t(weights) %*% slope_cov_mat %*% weights
    se_avg_slope <- sqrt(var_avg_slope[1, 1])

    aapc_lwr <- (exp(avg_slope_est - 1.96 * se_avg_slope) - 1) * 100
    aapc_upr <- (exp(avg_slope_est + 1.96 * se_avg_slope) - 1) * 100

    results <- data.frame(
      location = unique(df$location), num_breakpoints = length(breakpoints),
      breakpoint_year = paste(round(sort(breakpoints)), collapse = ", "),
      aapc = aapc, aapc_lwr = aapc_lwr, aapc_upr = aapc_upr,
      segment = 1:nrow(apc_df),
      start_year = segment_years[1:nrow(apc_df)],
      end_year = segment_years[2:(nrow(apc_df) + 1)]
    )
    results <- cbind(results, apc_df)
  }

  return(list(model = best_model, results = results))
}


analysis_results <- asr_data %>%
  split(.$location) %>%
  map(run_joinpoint_analysis) %>%
  compact()

summary_table <- map_df(analysis_results, "results") %>%
  dplyr::select(
  ) %>%
  mutate(across(where(is.numeric), ~round(., 2))) %>%
  arrange(location, start_year)

print("--- Joinpoint Analysis Summary Table (Main) ---")
print(summary_table)


plot_data <- asr_data %>%
  split(.$location) %>%
  map2_df(analysis_results[names(.)], ~{
    if (!is.null(.y$model)) {
      pred <- predict(.y$model, newdata = .x, se.fit = TRUE)
        mutate(
          fit = exp(pred$fit),
          lwr = exp(pred$fit - 1.96 * pred$se.fit),
          upr = exp(pred$fit + 1.96 * pred$se.fit)
        )
    }
  }, .id = "location")

location_order <- c("Global", sort(unique(plot_data$location[plot_data$location != "Global"])))
plot_data$location <- factor(plot_data$location, levels = location_order)

summary_for_plot <- summary_table %>%
  filter(num_breakpoints > 0) %>%
  separate_rows(breakpoint_year, sep = ", ", convert = TRUE) %>%
  mutate(location = factor(location, levels = location_order))

joinpoint_plot <- ggplot(plot_data, aes(x = year, y = asr)) +
  geom_point(color = "grey60", alpha = 0.6) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "steelblue", alpha = 0.2) +
  geom_line(aes(y = fit), color = "steelblue", linewidth = 1) +
  geom_vline(data = summary_for_plot, aes(xintercept = breakpoint_year), linetype = "dashed", color = "red") +
  facet_wrap(~location, scales = "free_y", ncol = 4, labeller = labeller(location = label_wrap_gen(width = 25))) +
  labs(
    title = "Main Analysis: Trends in Age-Standardized DALY Rates of Hip Osteoarthritis (1990-2021)",
    subtitle = "Joinpoint analysis for Global and 7 Super-regions (strucchange method)",
    x = "Year", y = "Age-Standardized DALY Rate (per 100,000)"
  ) +
  theme_bw() +
  theme(
    strip.text = element_text(face = "bold", size = 9),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(joinpoint_plot)
ggsave("joinpoint_trends_plot_main.pdf", plot = joinpoint_plot, width = 12, height = 8, units = "in")
write.csv(summary_table, "joinpoint_analysis_summary_R_main.csv", row.names = FALSE)
print("--- Main Analysis Visualization and Summary Saved ---")


21区Das Gupta及joinpoint的分析均与7区的一样
