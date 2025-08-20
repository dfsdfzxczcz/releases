library(dplyr)
library(purrr)
library(sf)
library(ggplot2)
library(cols4all)
library(patchwork)
library(stringr)
library(tidyr)


normalize_key <- function(x) {
    stringr::str_to_lower() %>%
    stringr::str_replace_all("&", "and") %>%
    stringr::str_replace_all("[,---]", " ") %>%
    stringr::str_squish()
}


setwd("D:/oa/anynasis")


risk_data_static <- read.csv("./risk/all time risk 204 country.csv")
risk_data_change <- read.csv("./risk/all time risk 204 country change.csv")
load("./risk/GBD_maps.RData")


region7_static_raw  <- read.csv("./risk/90 21 risk 7+1region.csv")
region21_static_raw <- read.csv("./risk/90 21 risk 21region.csv")
region7_change_raw  <- read.csv("./risk/all time risk 7+1region change.csv")
region21_change_raw <- read.csv("./risk/risk 21region change.csv")


paf_2021_data <- risk_data_static %>%
  filter(
    cause_name == "Osteoarthritis hip",
    rei_name == "High body-mass index",
    sex_name == "Both",
    str_detect(metric_name, regex("Percent", TRUE)),
    str_detect(measure_name, regex("DALY", TRUE)),
    age_name == "Age-standardized",
    year == 2021
  ) %>%
  mutate(val = val * 100) %>%
  select(location_id, location_name, val)


paf_change_data <- risk_data_change %>%
  filter(
    cause_name == "Osteoarthritis hip",
    rei_name == "High body-mass index",
    sex_name == "Both",
    str_detect(metric_name, regex("Percent", TRUE)),
    str_detect(measure_name, regex("DALY", TRUE)),
    age_name == "Age-standardized"
  ) %>%
  mutate(val = val * 100) %>%
  select(location_id, location_name, val)


paf_2021_7p1 <- region7_static_raw %>%
  filter(
    cause_name == "Osteoarthritis hip",
    rei_name == "High body-mass index",
    sex_name == "Both",
    str_detect(metric_name, regex("Percent", TRUE)),
    str_detect(measure_name, regex("DALY", TRUE)),
    age_name == "Age-standardized",
    year == 2021
  ) %>%
  mutate(val = val * 100) %>%
  select(location_name, val) %>%
  mutate(name_norm = normalize_key(location_name))


paf_2021_21 <- region21_static_raw %>%
  filter(
    cause_name == "Osteoarthritis hip",
    rei_name == "High body-mass index",
    sex_name == "Both",
    str_detect(metric_name, regex("Percent", TRUE)),
    str_detect(measure_name, regex("DALY", TRUE)),
    age_name == "Age-standardized",
    year == 2021
  ) %>%
  mutate(val = val * 100) %>%
  select(location_name, val) %>%
  mutate(name_norm = normalize_key(location_name))


paf_change_7p1 <- region7_static_raw %>%
  filter(
    cause_name == "Osteoarthritis hip",
    rei_name == "High body-mass index",
    sex_name %in% c("Both", "Both sexes"),
    str_detect(metric_name, regex("Percent", TRUE)),
    str_detect(measure_name, regex("DALY", TRUE)),
    age_name == "Age-standardized",
    year %in% c(1990, 2021)
  ) %>%
  mutate(
    val = val * 100,
    name_norm = normalize_key(location_name)
  ) %>%
  select(location_name, name_norm, year, val) %>%
  pivot_wider(names_from = year, values_from = val, names_prefix = "y_") %>%
  mutate(val = (y_2021 - y_1990) / y_1990 * 100) %>%
  select(location_name, name_norm, val)

paf_change_21 <- region21_static_raw %>%
  filter(
    cause_name == "Osteoarthritis hip",
    rei_name == "High body-mass index",
    sex_name %in% c("Both", "Both sexes"),
    str_detect(metric_name, regex("Percent", TRUE)),
    str_detect(measure_name, regex("DALY", TRUE)),
    age_name == "Age-standardized",
    year %in% c(1990, 2021)
  ) %>%
  mutate(
    val = val * 100,
    name_norm = normalize_key(location_name)
  ) %>%
  select(location_name, name_norm, year, val) %>%
  pivot_wider(names_from = year, values_from = val, names_prefix = "y_") %>%
  mutate(val = (y_2021 - y_1990) / y_1990 * 100) %>%
  select(location_name, name_norm, val)


paf_summary_table_countries <- paf_2021_data %>%
  inner_join(paf_change_data, by = c("location_id", "location_name"), suffix = c("_2021", "_change")) %>%
  rename(
    paf_in_2021 = val_2021,
    paf_change_1990_2021 = val_change
  ) %>%
  select(location_name, paf_in_2021, paf_change_1990_2021)


paf_summary_7p1 <- paf_2021_7p1 %>%
  inner_join(paf_change_7p1, by = "name_norm", suffix = c("_2021", "_change")) %>%
  transmute(
    location_name = location_name_2021,
    paf_in_2021 = val_2021,
    paf_change_1990_2021 = val_change
  )


paf_summary_21 <- paf_2021_21 %>%
  inner_join(paf_change_21, by = "name_norm", suffix = c("_2021", "_change")) %>%
  transmute(
    location_name = location_name_2021,
    paf_in_2021 = val_2021,
    paf_change_1990_2021 = val_change
  )


paf_summary_table <- bind_rows(
) %>%
  distinct(location_name, .keep_all = TRUE) %>%
  arrange(desc(paf_change_1990_2021))


print("--- Top 10 Countries with the Largest Increase in PAF (1990-2021) [Countries only] ---")
print(head(paf_summary_table_countries %>% arrange(desc(paf_change_1990_2021)), 10))


write.csv(paf_summary_table, "./risk/PAF_summary_table_hip_bmi.csv", row.names = FALSE)
print("Summary table (countries + Global/regions) saved to ./risk/PAF_summary_table_hip_bmi.csv")


create_paf_map <- function(data, value_col, palette, title, subtitle, legend_title) {

  value_col_sym <- sym(value_col)


  num_classes <- 7
  breaks <- quantile(data[[value_col]], probs = seq(0, 1, length.out = num_classes + 1), na.rm = TRUE)

  if (min(breaks) < 0 && max(breaks) > 0) {
    max_abs <- max(abs(breaks))
    breaks <- seq(-max_abs, max_abs, length.out = num_classes + 1)
  }
  breaks <- unique(breaks)

  breaks_labels <- purrr::imap_chr(breaks[-length(breaks)], ~ paste0(round(.x, 1), " to ", round(breaks[.y+1], 1)))

  pal <- cols4all::c4a(palette, n = length(breaks_labels))


  map_data <- left_join(data, world_GBD, by = c('location_id' = 'Location.ID')) %>%
    mutate(category = cut(!!value_col_sym,
                          breaks = breaks,
                          labels = breaks_labels,
                          include.lowest = TRUE, right = TRUE)) %>%
    st_as_sf()


  main_plot <- ggplot(data = map_data) +
    geom_sf(aes(geometry = geometry, fill = category), linewidth = 0.1, color = "white") +
    scale_fill_manual(values = pal, name = legend_title, drop = FALSE, na.value = "grey80") +
    theme_void(base_size = 12) +
    labs(title = title, subtitle = subtitle) +
    guides(fill = guide_legend(title.position = "top", direction = "horizontal")) +
    theme(
      legend.position = "bottom",
      legend.key.width = unit(1.5, "cm"),
      plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
      plot.subtitle = element_text(hjust = 0.5, size = 12, color = "grey30")
    ) +
    coord_sf(expand = FALSE)

  return(main_plot)
}


map_a <- create_paf_map(
  data = paf_2021_data,
  value_col = "val",
  palette = "YlOrRd",
  title = "A: PAF of Hip OA DALYs due to High BMI, 2021",
  subtitle = "Contribution of risk factor to disease burden",
  legend_title = "PAF in 2021 (%)"
)


map_b <- create_paf_map(
  data = paf_change_data,
  value_col = "val",
  palette = "RdBu",
  title = "B: Change in PAF of Hip OA DALYs due to High BMI",
  subtitle = "Percentage point change from 1990 to 2021",
  legend_title = "Change in PAF (%)"
)


final_plot <- map_a + map_b


print(final_plot)


ggsave(
  file = './risk/PAF_and_Change_hip_bmi_maps.pdf',
  units = 'cm',
  width = 32,
  height = 18
)

print("--- Risk Factor Analysis Maps Saved ---")
print("Plot saved to ./risk/PAF_and_Change_hip_bmi_maps.pdf")


cat("Counts (distinct locations):\n")
cat("Countries - 2021:", n_distinct(paf_2021_data$location_id),
    "| Change:", n_distinct(paf_change_data$location_id), "\n")
cat("7+1 regions - 2021:", n_distinct(paf_2021_7p1$name_norm),
    "| Change (after restrict):", n_distinct(paf_change_7p1$name_norm), "\n")
cat("21 regions - 2021:", n_distinct(paf_2021_21$name_norm),
    "| Change (after restrict):", n_distinct(paf_change_21$name_norm), "\n")

missing_countries <- anti_join(paf_2021_data, paf_change_data, by = c("location_id","location_name")) %>%
  select(location_name)
if (nrow(missing_countries) > 0) {
  cat("Countries missing change values (showing up to 20):\n")
  print(head(missing_countries$location_name, 20))
}

cat("Summary table sizes:\n")
cat("Countries:", nrow(paf_summary_table_countries),
    "| 7+1:", nrow(paf_summary_7p1),
    "| 21:", nrow(paf_summary_21), "\n")


setwd("D:/oa/anynasis/risk")

library(readxl)
library(tidyverse)
library(ggplot2)


SDI_value <- read.csv('IHME_GBD_SDI_2021_SDI_1950_2021_Y2024M05D16.csv', header = TRUE, check.names = FALSE)
Global_21Regions_PAF <- read.csv('all time risk 21+global region.csv', header=T)
order_globalandregions <- read.csv("order_globalandregions.csv", header = F)


SDI_2021 <- SDI_value %>%
  filter(location_name %in% order_globalandregions$V1,
         year_id == 2021) %>%
  select(location_name, mean_value) %>%
  rename(Location = location_name, SDI_2021 = mean_value) %>%
  group_by(Location) %>%
  slice(1) %>%
  ungroup()


PAF_2021 <- Global_21Regions_PAF %>%
  filter(cause_name == "Osteoarthritis hip",
         rei_name == "High body-mass index",
         year == 2021,
         sex_name == "Both",
         age_name == "Age-standardized",
         metric_name == "Percent") %>%
  mutate(PAF_percent = val * 100) %>%
  select(location_name, PAF_percent) %>%
  rename(Location = location_name) %>%
  group_by(Location) %>%
  slice(1) %>%
  ungroup()


merged_data_21regions <- inner_join(SDI_2021, PAF_2021, by = "Location")


cor_test <- cor.test(merged_data_21regions$SDI_2021, merged_data_21regions$PAF_percent)
cor_r <- cor_test$estimate
cor_int <- cor_test$conf.int
cor_p <- cor_test$p.value


PAF_SDI_21regions_plot <- ggplot(merged_data_21regions, aes(x = SDI_2021, y = PAF_percent)) +
  geom_point(aes(color = Location, shape = Location), size = 2) +
  scale_shape_manual(values = 1:22) +
  geom_smooth(colour='black', stat = "smooth", method='loess', se=FALSE, span=0.5) +
  geom_text(x = min(merged_data_21regions$SDI_2021, na.rm = TRUE),
            y = max(merged_data_21regions$PAF_percent, na.rm = TRUE) * 0.8,
            label = paste("R =", round(cor_r, 2),"(", round(cor_int[1],2), "to", round(cor_int[2],2), ")","\n",
                          "p =", ifelse(cor_p<0.001,"< 0.001",round(cor_p,2))),
            hjust = 0, vjust = 1, size = 4) +
  geom_text(aes(label = Location, color = Location), size = 1, hjust = 0.7, vjust = 0, show.legend = FALSE) +
  ylab("Population Attributable Fraction (%)") +
  xlab("Socio-demographic Index (SDI)") +
  theme_bw(base_size = 8) +
  theme(panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent"),
        legend.position = 'top',
        legend.key.size = unit(0.3, "cm"),
        legend.title = element_blank(),
        axis.text.x = element_text(face = "bold", size = 8),
        axis.text.y = element_text(face = "bold", size = 8),
        axis.title.x = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 10))

print(PAF_SDI_21regions_plot)
ggsave("PAF_vs_SDI_21regions_2021.pdf", plot = PAF_SDI_21regions_plot, width = 11, height = 7)


print(PAF_SDI_21regions_plot)
ggsave("PAF_vs_SDI_21regions_2021.pdf", plot = PAF_SDI_21regions_plot, width = 11, height = 7)


print("--- 21区域+Global的PAF vs SDI分析完成 ---")


library(dplyr)
library(ggplot2)
library(tidyr)
library(purrr)
library(stringr)
library(strucchange)

rm(list = ls())


setwd("D:/oa/anynasis/risk")
full_paf_data <- read.csv("all time risk 7+1region.csv")


paf_data <- full_paf_data %>%
  filter(
    cause_name == "Osteoarthritis hip",
    rei_name == "High body-mass index",
    metric_name == "Percent",
    sex_name == "Both",
    age_name == "Age-standardized"
  ) %>%
  mutate(paf_percent = val * 100) %>%
  rename(location = location_name, paf = paf_percent) %>%
  select(location, year, paf) %>%
  filter(!is.na(paf), paf > 0) %>%
  distinct(location, year, .keep_all = TRUE) %>%
  arrange(location, year)


print("--- Filtered PAF Data for 7 Super-regions Analysis ---")
if(nrow(paf_data) == 0) stop("Filtering resulted in zero rows.")


run_paf_joinpoint_analysis <- function(df, max_breakpoints = 2, h = 3) {

  df <- df %>% arrange(year)
  best_model <- NULL
  best_bic <- Inf
  best_bp_obj <- NULL


  mod0 <- lm(paf ~ year, data = df)
  best_model <- mod0
  best_bic <- BIC(mod0)

  for (k in 1:max_breakpoints) {
    bp_k_obj <- try(breakpoints(paf ~ year, data = df, h = h, breaks = k), silent = TRUE)
    if (inherits(bp_k_obj, "try-error") || is.na(bp_k_obj$breakpoints[1])) next
    mod_k <- lm(paf ~ breakfactor(bp_k_obj) / year - 1, data = df)
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
    apc <- slope_est
    apc_lwr <- slope_est - 1.96 * slope_se
    apc_upr <- slope_est + 1.96 * slope_se

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
      apc = slope_estimates,
      apc_lwr = slope_estimates - 1.96 * slope_ses,
      apc_upr = slope_estimates + 1.96 * slope_ses
    )

    segment_years <- c(min(df$year), sort(breakpoints), max(df$year))
    years_per_segment <- diff(segment_years)
    avg_slope_est <- weighted.mean(slope_estimates, years_per_segment)
    aapc <- avg_slope_est

    vcov_mat <- vcov(best_model)
    slope_cov_mat <- vcov_mat[slope_indices, slope_indices, drop = FALSE]
    weights <- years_per_segment / sum(years_per_segment)
    var_avg_slope <- t(weights) %*% slope_cov_mat %*% weights
    se_avg_slope <- sqrt(var_avg_slope[1, 1])

    aapc_lwr <- avg_slope_est - 1.96 * se_avg_slope
    aapc_upr <- avg_slope_est + 1.96 * se_avg_slope

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


analysis_results <- paf_data %>%
  split(.$location) %>%
  map(run_paf_joinpoint_analysis) %>%
  compact()

summary_table <- map_df(analysis_results, "results") %>%
  dplyr::select(
  ) %>%
  mutate(across(where(is.numeric), ~round(., 3))) %>%
  arrange(location, start_year)

print("--- PAF Joinpoint Analysis Summary Table (7 Super-regions) ---")
print(summary_table)


plot_data <- paf_data %>%
  split(.$location) %>%
  map2_df(analysis_results[names(.)], ~{
    if (!is.null(.y$model)) {
      pred <- predict(.y$model, newdata = .x, se.fit = TRUE)
        mutate(
          fit = pred$fit,
          lwr = pred$fit - 1.96 * pred$se.fit,
          upr = pred$fit + 1.96 * pred$se.fit
        )
    }
  }, .id = "location")

location_order <- c("Global", sort(unique(plot_data$location[plot_data$location != "Global"])))
plot_data$location <- factor(plot_data$location, levels = location_order)
summary_for_plot <- summary_table %>%
  filter(num_breakpoints > 0) %>%
  separate_rows(breakpoint_year, sep = ", ", convert = TRUE) %>%
  mutate(location = factor(location, levels = location_order))

paf_joinpoint_plot <- ggplot(plot_data, aes(x = year, y = paf)) +
  geom_point(color = "grey60", alpha = 0.6) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "steelblue", alpha = 0.2) +
  geom_line(aes(y = fit), color = "steelblue", linewidth = 1) +
  geom_vline(data = summary_for_plot, aes(xintercept = breakpoint_year), linetype = "dashed", color = "red") +
  facet_wrap(~location, scales = "free_y", ncol = 4, labeller = labeller(location = label_wrap_gen(width = 25))) +
  labs(
    title = "PAF Trends: Population Attributable Fraction of Hip OA due to High BMI (1990-2021)",
    subtitle = "Joinpoint analysis for Global and 7 Super-regions (strucchange method)",
    x = "Year",
    y = "Population Attributable Fraction (%)"
  ) +
  theme_bw() +
  theme(
    strip.text = element_text(face = "bold", size = 9),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(paf_joinpoint_plot)
ggsave("PAF_joinpoint_trends_7superregions.pdf", plot = paf_joinpoint_plot, width = 12, height = 8, units = "in")
write.csv(summary_table, "PAF_joinpoint_analysis_7superregions.csv", row.names = FALSE)
print("--- PAF Joinpoint Analysis (7 Super-regions) Complete ---")


if (!requireNamespace("readr", quietly = TRUE)) install.packages("readr", repos = "https://cloud.r-project.org")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr", repos = "https://cloud.r-project.org")
if (!requireNamespace("tidyr", quietly = TRUE)) install.packages("tidyr", repos = "https://cloud.r-project.org")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2", repos = "https://cloud.r-project.org")
if (!requireNamespace("ggdist", quietly = TRUE)) install.packages("ggdist", repos = "https://cloud.r-project.org")
if (!requireNamespace("flextable", quietly = TRUE)) install.packages("flextable", repos = "https://cloud.r-project.org")
if (!requireNamespace("officer", quietly = TRUE)) install.packages("officer", repos = "https://cloud.r-project.org")
if (!requireNamespace("truncnorm", quietly = TRUE)) install.packages("truncnorm", repos = "https://cloud.r-project.org")

library(readr); library(dplyr); library(tidyr); library(ggplot2)
library(ggdist); library(flextable); library(officer); library(stringr); library(truncnorm)

setwd("D:/oa/anynasis/risk")


z975 <- qnorm(0.975)
norm_txt <- function(x) { x <- as.character(x); x <- str_replace_all(x, "[\u2013\u2014\u2212]", "-"); str_squish(x) }
logit <- function(p) log(p/(1-p))
expit <- function(z) 1/(1+exp(-z))
sd_from_ui_norm <- function(lower, upper) (upper - lower) / (2 * z975)
lognorm_from_ui <- function(lower, upper) {
  if (is.na(lower) || is.na(upper) || lower <= 0 || upper <= 0) return(list(mu=NA, sigma=NA))
  mu    <- (log(lower) + log(upper)) / 2
  sigma <- (log(upper) - log(lower)) / (2 * z975)
  list(mu = mu, sigma = sigma)
}
qsum <- function(x) c(med = unname(quantile(x, 0.5,  na.rm=TRUE)),
                      lo  = unname(quantile(x, 0.025, na.rm=TRUE)),
                      hi  = unname(quantile(x, 0.975, na.rm=TRUE)))
fit_paf <- function(lo_pp, hi_pp) {
  eps <- 1e-6
  if (!is.finite(lo_pp) || !is.finite(hi_pp)) return(list(type="normal", mu=NA, sd=NA))
  if (lo_pp <= 0 + 1e-8 || hi_pp >= 100 - 1e-8) {
    mu <- (lo_pp + hi_pp)/2
    sd <- (hi_pp - lo_pp)/(2*z975)
    list(type="normal", mu=mu, sd=sd)
  } else {
    plo <- pmin(1 - eps, pmax(eps, lo_pp/100))
    phi <- pmin(1 - eps, pmax(eps, hi_pp/100))
    mu  <- (logit(plo) + logit(phi))/2
    sd  <- (logit(phi) - logit(plo))/(2*z975)
    list(type="logit", mu=mu, sd=sd)
  }
}


daly_1_5_7 <- readr::read_csv("1+5+7 number.csv", show_col_types = FALSE) %>%
  dplyr::rename_with(tolower) %>%
  dplyr::mutate(across(c(location_name,cause_name,measure_name,metric_name,sex_name,age_name), norm_txt)) %>%
  dplyr::filter(
    year == 2021,
    str_detect(cause_name,   regex("^osteoarthritis\\s*hip$", TRUE)),
    str_detect(measure_name, regex("^dalys", TRUE)),
    str_detect(metric_name,  regex("^number$", TRUE)),
    sex_name %in% c("Both","Both sexes"),
    age_name == "All ages"
  ) %>%
  dplyr::group_by(location_name) %>%
  dplyr::summarise(
    n_rows = dplyr::n(),
    total_dalys_2021 = first(as.numeric(val)),
    daly_lo          = first(as.numeric(lower)),
    daly_hi          = first(as.numeric(upper)),
    .groups = "drop"
  )
stopifnot(all(daly_1_5_7$n_rows == 1))
daly_1_5_7 <- dplyr::select(daly_1_5_7, -n_rows)


paf_1_5_7 <- readr::read_csv("2021 risk global+7region+5sdi.csv", show_col_types = FALSE) %>%
  dplyr::rename_with(tolower) %>%
  dplyr::mutate(across(c(location_name,cause_name,measure_name,metric_name,sex_name,age_name,rei_name), norm_txt)) %>%
  dplyr::filter(
    str_detect(measure_name, regex("^dalys", TRUE)),
    str_detect(metric_name,  regex("^percent$", TRUE)),
    str_detect(rei_name,     regex("high.*bmi|body-?mass", TRUE)),
    str_detect(cause_name,   regex("^osteoarthritis\\s*hip$", TRUE)),
    age_name == "All ages",
    sex_name %in% c("Both","Both sexes"),
    year == 2021
  ) %>%
  dplyr::transmute(
    paf_val = as.numeric(val),
    paf_lo  = as.numeric(lower),
    paf_hi  = as.numeric(upper)
  ) %>%
  dplyr::mutate(
    scale_flag = ifelse(max(abs(c(paf_val, paf_lo, paf_hi)), na.rm = TRUE) <= 1.5, 100, 1),
    paf_val = paf_val * scale_flag,
    paf_lo  = paf_lo  * scale_flag,
    paf_hi  = paf_hi  * scale_flag
  ) %>% dplyr::select(-scale_flag)


policy_base <- daly_1_5_7 %>% dplyr::inner_join(paf_1_5_7, by = "location_name")
if (nrow(policy_base) == 0) {
  cat("Join empty. Check unmatched:\n")
  cat("DALY only: ", paste(setdiff(daly_1_5_7$location_name, paf_1_5_7$location_name), collapse=" | "), "\n")
  cat("PAF only:  ", paste(setdiff(paf_1_5_7$location_name, daly_1_5_7$location_name), collapse=" | "), "\n")
  stop("policy_base is empty after join")
}
stopifnot(nrow(policy_base) >= 13)
policy_base <- policy_base %>% arrange(location_name)


policy_scenarios <- tibble(
  scenario_name = c("Conservative (5%)","Moderate (10%)","Ambitious (15%)"),
  bmr_reduction = c(0.05, 0.10, 0.15)
)


simulate_policy_mc <- function(df, scenarios, n = 1000) {
  purrr::map_dfr(seq_len(nrow(df)), function(i) {
    row <- df[i,]

    lp <- lognorm_from_ui(row$daly_lo, row$daly_hi)
    daly_draw <- if (is.na(lp$mu)) rep(row$total_dalys_2021, n) else rlnorm(n, lp$mu, lp$sigma)

    pf <- fit_paf(row$paf_lo, row$paf_hi)
    if (pf$type == "normal") {
      paf_draw <- truncnorm::rtruncnorm(n, a = 0, b = 100, mean = pf$mu, sd = pf$sd)
    } else {
      paf_draw <- expit(rnorm(n, mean = pf$mu, sd = pf$sd)) * 100
    }
    purrr::map_dfr(1:nrow(scenarios), function(k) {
      r <- scenarios$bmr_reduction[k]
      pct_reduction <- paf_draw * r
      avoidable     <- daly_draw * (paf_draw/100) * r
      s1 <- qsum(pct_reduction); s2 <- qsum(avoidable)
      tibble(
        location_name = row$location_name,
        scenario_name = scenarios$scenario_name[k],
        pct_med = s1["med"], pct_lo = s1["lo"], pct_hi = s1["hi"],
        av_med  = s2["med"], av_lo  = s2["lo"], av_hi  = s2["hi"]
      )
    })
  })
}

set.seed(2025)
policy_results_mc <- simulate_policy_mc(policy_base, policy_scenarios, n = 1000) %>%
  mutate(scenario_name = factor(scenario_name,
                                levels = c("Conservative (5%)","Moderate (10%)","Ambitious (15%)")))


order_units <- policy_results_mc %>%
  group_by(location_name) %>%
  summarise(rank_key = mean(pct_med, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(rank_key)) %>%
  pull(location_name)


pos <- position_dodge(width = 0.7)
interval_plot <- ggplot(policy_results_mc,
                        aes(y = factor(location_name, levels = rev(order_units)),
                            x = pct_med, xmin = pct_lo, xmax = pct_hi,
                            color = scenario_name)) +
  ggdist::stat_pointinterval(point_size = 3, linewidth = 1.1, .width = 0.95, position = pos) +
  scale_color_manual(
    name   = "Policy Scenario (BMI Risk Reduction)",
    values = c("Conservative (5%)"="#FDBF6F","Moderate (10%)"="#FF7F00","Ambitious (15%)"="#E31A1C"),
    breaks = c("Conservative (5%)","Moderate (10%)","Ambitious (15%)")
  ) +
  scale_x_continuous(labels = function(x) paste0(x, "%")) +
  labs(title = "Potential reduction in hip OA DALYs under BMI risk-reduction scenarios (1+5+7, All ages)",
       x = "Reduction in total DALYs (%) - median and 95% UI", y = NULL) +
  theme_bw(base_size = 12) +
  theme(legend.position = "top", panel.grid.major.y = element_blank(),
        legend.title = element_text(face = "bold"))

print(interval_plot)
ggsave("Policy_Scenario_1+5+7_IntervalPoint.pdf", interval_plot, width = 12, height = 8.5, units = "in", dpi = 300)
ggsave("Policy_Scenario_1+5+7_IntervalPoint.png", interval_plot, width = 12, height = 8.5, units = "in", dpi = 300)


tbl_long <- policy_results_mc %>%
  mutate(
    Reduct_pct = sprintf("%.2f (%.2f-%.2f)", pct_med, pct_lo, pct_hi),
    Avoid_k    = sprintf("%.1f (%.1f-%.1f)", av_med/1000, av_lo/1000, av_hi/1000)
  ) %>%
  select(location_name, scenario_name, Reduct_pct, Avoid_k)

base_fmt <- policy_base %>%
  mutate(
    DALYs_2021_k_95UI = sprintf("%.0f (%.0f-%.0f)", total_dalys_2021/1000, daly_lo/1000, daly_hi/1000),
    PAF_2021_pct_95UI = sprintf("%.1f (%.1f-%.1f)", paf_val, paf_lo, paf_hi)
  ) %>%
  select(location_name, DALYs_2021_k_95UI, PAF_2021_pct_95UI)

tbl_pub <- tbl_long %>%
  tidyr::pivot_wider(
    names_from = scenario_name,
    values_from = c(Reduct_pct, Avoid_k),
    names_sep = " | "
  ) %>%
  left_join(base_fmt, by = "location_name") %>%
  select(
    Location = location_name,
    `Total DALYs 2021 (000s) (95% UI)` = DALYs_2021_k_95UI,
    `PAF 2021 (%) (95% UI)`            = PAF_2021_pct_95UI,
    `Reduction (%) | Conservative (5%)` = `Reduct_pct | Conservative (5%)`,
    `Reduction (%) | Moderate (10%)`    = `Reduct_pct | Moderate (10%)`,
    `Reduction (%) | Ambitious (15%)`   = `Reduct_pct | Ambitious (15%)`,
    `Avoidable DALYs (000s) | Conservative (5%)` = `Avoid_k | Conservative (5%)`,
    `Avoidable DALYs (000s) | Moderate (10%)`    = `Avoid_k | Moderate (10%)`,
    `Avoidable DALYs (000s) | Ambitious (15%)`   = `Avoid_k | Ambitious (15%)`
  ) %>%
  arrange(factor(Location, levels = order_units))


ft <- flextable(tbl_pub)
ft <- theme_vanilla(ft)
ft <- font(ft, fontname = "Times New Roman", part = "all")
ft <- fontsize(ft, size = 9.5, part = "all")
ft <- align(ft, j = setdiff(colnames(tbl_pub), "Location"), align = "right", part = "all")
ft <- border_remove(ft)
b_top_bot <- fp_border(color = "black", width = 1.5)
b_mid     <- fp_border(color = "black", width = 1.0)
ft <- hline_top(ft,    part = "header", border = b_top_bot)
ft <- hline(ft,        i = 1, part = "header", border = b_mid)
ft <- hline_bottom(ft, part = "header", border = b_top_bot)
ft <- hline_bottom(ft, part = "body",   border = b_top_bot)
ft <- width(ft, j = "Location", width = 3.0)
ft <- width(ft, j = setdiff(colnames(tbl_pub), "Location"), width = 1.4)
ft <- set_table_properties(ft, layout = "autofit", width = 1)
ft <- autofit(ft)

notes_txt <- paste(
  "Medians with 95% uncertainty intervals from Monte Carlo propagation (N=1,000).",
  "PAF (%) modeled per unit: Normal if UI touches 0/100; otherwise Logit-normal; DALYs Number Lognormal; N=1,000.",
  sep = " "
)
ps <- officer::prop_section(
  page_size    = officer::page_size(orient = "landscape"),
  page_margins = officer::page_mar(top = 0.5, bottom = 0.5, left = 0.6, right = 0.6)
)
flextable::save_as_docx(
  "{caption}" = "Table. Potential reduction in hip OA DALYs (1+5+7, All ages) - medians and 95% UI.",
  "{table}"   = ft,
  "{notes}"   = qflextable(data.frame(Notes = notes_txt)),
  path        = "Policy_Scenario_1+5+7_Table.docx",
  pr_section  = ps
)
write.csv(tbl_pub, "Policy_Scenario_1+5+7_Table.csv", row.names = FALSE)


eps <- 1e-6
is_logit_row <- with(paf_1_5_7, is.finite(paf_lo) & is.finite(paf_hi) &
                       paf_lo > 0 + 1e-8 & paf_hi < 100 - 1e-8)
if (any(is_logit_row)) {
  p_lo <- pmin(1 - eps, pmax(eps, paf_1_5_7$paf_lo[is_logit_row]/100))
  p_hi <- pmin(1 - eps, pmax(eps, paf_1_5_7$paf_hi[is_logit_row]/100))
  mu_p  <- (logit(p_lo) + logit(p_hi)) / 2
  sg_p  <- (logit(p_hi) - logit(p_lo)) / (2 * z975)
  recon_lo <- expit(mu_p - z975*sg_p) * 100
  recon_hi <- expit(mu_p + z975*sg_p) * 100
  max_abs_diff_paf <- max(
    abs(recon_lo - paf_1_5_7$paf_lo[is_logit_row]),
    abs(recon_hi - paf_1_5_7$paf_hi[is_logit_row]),
    na.rm = TRUE
  )
  cat(sprintf("[VALID] PAF logit-normal UI max abs diff (pp): %.3f\n", max_abs_diff_paf))
  stopifnot(max_abs_diff_paf <= 0.5 + 1e-6)
} else {
  cat("[NOTE] No rows eligible for pure logit-normal check (some UIs touch 0 or 100).\n")
}


recon <- paf_1_5_7 %>%
  rowwise() %>%
  mutate(
    type = ifelse(paf_lo <= 0 + 1e-8 | paf_hi >= 100 - 1e-8, "normal", "logit"),
    rec_lo = if (type=="normal") {
      mu <- (paf_lo + paf_hi)/2; sd <- (paf_hi - paf_lo)/(2*z975); mu - z975*sd
    } else {
      plo <- pmin(1-eps, pmax(eps, paf_lo/100)); phi <- pmin(1-eps, pmax(eps, paf_hi/100))
      mu <- (logit(plo)+logit(phi))/2; sd <- (logit(phi)-logit(plo))/(2*z975)
      expit(mu - z975*sd) * 100
    },
    rec_hi = if (type=="normal") {
      mu <- (paf_lo + paf_hi)/2; sd <- (paf_hi - paf_lo)/(2*z975); mu + z975*sd
    } else {
      plo <- pmin(1-eps, pmax(eps, paf_lo/100)); phi <- pmin(1-eps, pmax(eps, paf_hi/100))
      mu <- (logit(plo)+logit(phi))/2; sd <- (logit(phi)-logit(plo))/(2*z975)
      expit(mu + z975*sd) * 100
    }
  ) %>% ungroup()

max_abs_diff_paf <- max(abs(recon$rec_lo - recon$paf_lo), abs(recon$rec_hi - recon$paf_hi), na.rm = TRUE)
cat(sprintf("[VALID] PAF per-model UI max abs diff (pp): %.3f\n", max_abs_diff_paf))
stopifnot(max_abs_diff_paf <= 1.0 + 1e-6)


paf_fit <- policy_base %>% select(location_name, paf_lo, paf_hi, paf_val, daly_lo, daly_hi)
det_base <- paf_fit %>%
  rowwise() %>%
  mutate(
    type = ifelse(paf_lo <= 0 + 1e-8 | paf_hi >= 100 - 1e-8, "normal", "logit"),
    paf_med = if (type == "normal") {
      mu <- (paf_lo + paf_hi)/2; sd <- (paf_hi - paf_lo)/(2*z975)
      a <- (0   - mu)/sd; b <- (100 - mu)/sd
      p_med <- pnorm(a) + 0.5*(pnorm(b) - pnorm(a))
      qnorm(p_med, mean = mu, sd = sd)
    } else {
      plo <- pmin(1 - eps, pmax(eps, paf_lo/100))
      phi <- pmin(1 - eps, pmax(eps, paf_hi/100))
      mu  <- (logit(plo) + logit(phi))/2
      expit(mu) * 100
    },
    daly_med = exp((log(daly_lo) + log(daly_hi))/2)
  ) %>% ungroup() %>% select(location_name, paf_med, daly_med)

det_ref <- det_base %>%
  tidyr::crossing(policy_scenarios) %>%
  mutate(
    det_pct = paf_med * bmr_reduction,
    det_avk = daly_med * (paf_med/100) * bmr_reduction / 1000
  ) %>% select(location_name, scenario_name, det_pct, det_avk)

cmp <- policy_results_mc %>%
  group_by(location_name, scenario_name) %>%
  summarise(pct_med = first(pct_med), av_medk = first(av_med/1000), .groups = "drop") %>%
  left_join(det_ref, by = c("location_name","scenario_name")) %>%
  mutate(
    rel_err_pct = abs(pct_med - det_pct) / pmax(1e-9, abs(det_pct)),
    rel_err_avk = abs(av_medk - det_avk) / pmax(1e-9, abs(det_avk))
  )

max_rel_err_pct <- max(cmp$rel_err_pct, na.rm = TRUE)
max_rel_err_avk <- max(cmp$rel_err_avk, na.rm = TRUE)
cat(sprintf("[VALID] MC median vs deterministic-median: pct=%.3f, avk=%.3f\n",
            max_rel_err_pct, max_rel_err_avk))
stopifnot(max_rel_err_pct <= 0.10 + 1e-9, max_rel_err_avk <= 0.10 + 1e-9)
