# Runs with R 4.5.1


### Table 1 ###

library(readxl)
library(dplyr)
library(tidyr)
library(did)

set.seed(123)

setwd("your path")

df <- read_excel("data.xlsx") %>%
  distinct() %>%
  mutate(Commune = paste(Prefecture, City, sep = "_"))

sector_cols <- c(
  "Paper","Stationery","OfficeFurniture","MobilePhones","HomeAppliances",
  "AirConditioners","WaterHeaters","Lighting","Vehicles","FireExtinguishers",
  "Uniforms","InteriorBedding","WorkGloves","OtherTextiles","Equipment","DisasterSupplies"
)

years_window <- 2012:2020

T_years <- length(years_window)

panel_green <- df %>%
  select(
    Prefecture, City, Commune,
    matches(paste0("^(", paste(sector_cols, collapse = "|"), ")_\\d{4}$"))
  ) %>%
  pivot_longer(
    cols = matches("_(19|20)\\d{2}$"),
    names_to = c("Segment", "Year"),
    names_pattern = "^(.*)_(\\d{4})$",
    values_to = "Green"
  ) %>%
  mutate(
    Year    = as.integer(Year),
    Segment = factor(Segment, levels = sector_cols),
    Green   = as.numeric(Green)
  ) %>%
  filter(Year %in% years_window, !is.na(Green))

communes_keep <- panel_green %>%
  distinct(Commune, Segment, Year) %>%
  count(Commune, name = "n_cells") %>%
  filter(n_cells == length(sector_cols) * T_years) %>%
  pull(Commune)

panel_green <- panel_green %>%
  filter(Commune %in% communes_keep)

panel_eq <- df %>%
  select(Prefecture, City, Commune, matches("^earthquake_score_\\d{4}$")) %>%
  pivot_longer(
    cols = matches("^earthquake_score_\\d{4}$"),
    names_to = "Year",
    names_pattern = "^earthquake_score_(\\d{4})$",
    values_to = "earthquake_score"
  ) %>%
  mutate(
    Year = as.integer(Year),
    earthquake_score = as.numeric(earthquake_score),
    earthquake_score = ifelse(is.na(earthquake_score), 0, earthquake_score)
  ) %>%
  filter(Year %in% years_window, Commune %in% communes_keep)

thresholds <- c(1L, 2L, 3L)

threshold_label <- function(th) {
  dplyr::case_when(
    th == 1L ~ "Shindo ≥ 5-",
    th == 2L ~ "Shindo ≥ 5+",
    th == 3L ~ "Shindo ≥ 6-",
    TRUE     ~ paste0("score ≥ ", th)
  )
}

run_att_global_for_threshold <- function(th) {
  timing_tbl <- panel_eq %>%
    mutate(eq_treat = as.integer(earthquake_score >= th)) %>%
    group_by(Commune) %>%
    summarise(
      first_eq_year = ifelse(any(eq_treat == 1L), min(Year[eq_treat == 1L]), 0L),
      .groups = "drop"
    )
  n_treated <- sum(timing_tbl$first_eq_year > 0L)
  n_control <- sum(timing_tbl$first_eq_year == 0L)
  panel <- panel_green %>%
    left_join(timing_tbl, by = "Commune") %>%
    mutate(
      G = as.integer(first_eq_year),
      Commune_Segment = interaction(Commune, Segment, drop = TRUE),
      Year_Segment    = interaction(Year, Segment, drop = TRUE),
      id_cs = as.integer(factor(Commune_Segment)),
      Year_Segment_fe = factor(Year_Segment)
    ) %>%
    as.data.frame()
  att_cs <- did::att_gt(
    yname   = "Green",
    tname   = "Year",
    idname  = "id_cs",
    gname   = "G",
    data    = panel,
    panel   = TRUE,
    control_group = "nevertreated",
    clustervars   = "Commune",
    xformla = ~ Year_Segment_fe
  )
  att_global <- did::aggte(att_cs, type = "simple")
  att_hat <- as.numeric(att_global$overall.att)
  se_hat  <- as.numeric(att_global$overall.se)
  ci_low  <- att_hat - 1.96 * se_hat
  ci_high <- att_hat + 1.96 * se_hat
  t_stat  <- att_hat / se_hat
  p_value <- 2 * (1 - pnorm(abs(t_stat)))
  tibble(
    threshold = th,
    threshold_lab = threshold_label(th),
    ATT = att_hat,
    SE  = se_hat,
    CI_low = ci_low,
    CI_high = ci_high,
    p_value = p_value,
    n_treated_communes = n_treated,
    n_control_communes = n_control
  )
}

results_df <- bind_rows(lapply(thresholds, run_att_global_for_threshold))
print(results_df)


### Figure 3 ###

library(readxl); library(dplyr); library(tidyr); library(did); library(ggplot2)
set.seed(123)

sector_cols <- c("Paper","Stationery","OfficeFurniture","MobilePhones","HomeAppliances",
                 "AirConditioners","WaterHeaters","Lighting","Vehicles","FireExtinguishers",
                 "Uniforms","InteriorBedding","WorkGloves","OtherTextiles","Equipment","DisasterSupplies")
years_window <- 2012:2020

df <- read_excel("data.xlsx") %>%
  distinct() %>%
  mutate(Commune = paste(Prefecture, City, sep = "_"))

panel_green <- df %>%
  select(Commune, matches(paste0("^(", paste(sector_cols, collapse="|"), ")_\\d{4}$"))) %>%
  pivot_longer(matches("_(19|20)\\d{2}$"), names_to=c("Segment","Year"),
               names_pattern="^(.*)_(\\d{4})$", values_to="Green") %>%
  mutate(Year=as.integer(Year), Green=as.numeric(Green)) %>%
  filter(Year %in% years_window, !is.na(Green))

communes_keep <- panel_green %>%
  distinct(Commune, Segment, Year) %>% count(Commune) %>%
  filter(n == length(sector_cols) * length(years_window)) %>% pull(Commune)
panel_green <- filter(panel_green, Commune %in% communes_keep)

panel_eq <- df %>%
  select(Commune, matches("^earthquake_score_\\d{4}$")) %>%
  pivot_longer(matches("^earthquake_score_\\d{4}$"), names_to="Year",
               names_pattern="^earthquake_score_(\\d{4})$", values_to="earthquake_score") %>%
  mutate(Year=as.integer(Year),
         earthquake_score=as.numeric(ifelse(is.na(earthquake_score), 0, earthquake_score))) %>%
  filter(Year %in% years_window, Commune %in% communes_keep)

th <- 1L

timing <- panel_eq %>%
  group_by(Commune) %>%
  summarise(G = ifelse(any(earthquake_score >= th), min(Year[earthquake_score >= th]), 0L),
            .groups = "drop")

panel <- panel_green %>%
  left_join(timing, by="Commune") %>%
  mutate(id_cs = as.integer(factor(interaction(Commune, Segment, drop=TRUE))),
         Year_Segment_fe = factor(interaction(Year, Segment, drop=TRUE))) %>%
  as.data.frame()

att_cs <- did::att_gt(
  yname="Green", tname="Year", idname="id_cs", gname="G",
  data=panel, panel=TRUE, control_group="nevertreated",
  clustervars="Commune", xformla=~Year_Segment_fe
)

att_dyn <- did::aggte(att_cs, type = "dynamic", min_e = -5, max_e = 5)

es_df <- data.frame(
  e      = att_dyn$egt,
  ATT    = att_dyn$att.egt,
  SE     = att_dyn$se.egt,
  CI_low  = att_dyn$att.egt - 1.96 * att_dyn$se.egt,
  CI_high = att_dyn$att.egt + 1.96 * att_dyn$se.egt
)

ggplot(es_df, aes(x=e, y=ATT)) +
  geom_hline(yintercept=0, linetype="dashed", color="gray40") +
  geom_vline(xintercept=-0.5, linetype="dotted") +
  geom_errorbar(aes(ymin=CI_low, ymax=CI_high), width=0.2) +
  geom_point(size=2) +
  scale_x_continuous(breaks = seq(min(es_df$e), max(es_df$e), by = 1)) +
  labs(x="Event time (years relative to treatment)",
       y="ATT on Green (95% CI)") +
  theme_bw()


### Figure 4 ###

library(readxl)
library(dplyr)
library(tidyr)
library(did)
library(ggplot2)

set.seed(123)

setwd("your path")

df <- read_excel("data.xlsx") %>%
  distinct() %>%
  mutate(Commune = paste(Prefecture, City, sep = "_"))

sector_cols <- c(
  "Paper","Stationery","OfficeFurniture","MobilePhones","HomeAppliances",
  "AirConditioners","WaterHeaters","Lighting","Vehicles","FireExtinguishers",
  "Uniforms","InteriorBedding","WorkGloves","OtherTextiles","Equipment","DisasterSupplies"
)

years_window <- 2012:2020
T_years <- length(years_window)

panel_green <- df %>%
  select(
    Prefecture, City, Commune,
    matches(paste0("^(", paste(sector_cols, collapse = "|"), ")_\\d{4}$"))
  ) %>%
  pivot_longer(
    cols = matches("_(19|20)\\d{2}$"),
    names_to = c("Segment", "Year"),
    names_pattern = "^(.*)_(\\d{4})$",
    values_to = "Green"
  ) %>%
  mutate(
    Year    = as.integer(Year),
    Segment = factor(Segment, levels = sector_cols),
    Green   = as.numeric(Green)
  ) %>%
  filter(Year %in% years_window, !is.na(Green))

communes_keep <- panel_green %>%
  distinct(Commune, Segment, Year) %>%
  count(Commune, name = "n_cells") %>%
  filter(n_cells == length(sector_cols) * T_years) %>%
  pull(Commune)

panel_green <- panel_green %>%
  filter(Commune %in% communes_keep)

panel_eq <- df %>%
  select(Prefecture, City, Commune, matches("^earthquake_score_\\d{4}$")) %>%
  pivot_longer(
    cols = matches("^earthquake_score_\\d{4}$"),
    names_to = "Year",
    names_pattern = "^earthquake_score_(\\d{4})$",
    values_to = "earthquake_score"
  ) %>%
  mutate(
    Year = as.integer(Year),
    earthquake_score = as.numeric(earthquake_score),
    earthquake_score = ifelse(is.na(earthquake_score), 0, earthquake_score)
  ) %>%
  filter(Year %in% years_window, Commune %in% communes_keep)

th <- 1L

timing_tbl <- panel_eq %>%
  mutate(eq_treat = as.integer(earthquake_score >= th)) %>%
  group_by(Commune) %>%
  summarise(
    first_eq_year = ifelse(any(eq_treat == 1L), min(Year[eq_treat == 1L]), 0L),
    .groups = "drop"
  )

run_att_for_sector <- function(seg) {
  message("Estimating: ", seg)
  
  panel_seg <- panel_green %>%
    filter(Segment == seg) %>%
    left_join(timing_tbl, by = "Commune") %>%
    mutate(
      G     = as.integer(first_eq_year),
      id_cs = as.integer(factor(Commune))
    ) %>%
    as.data.frame()
  
  att_cs <- tryCatch(
    did::att_gt(
      yname         = "Green",
      tname         = "Year",
      idname        = "id_cs",
      gname         = "G",
      data          = panel_seg,
      panel         = TRUE,
      control_group = "nevertreated",
      clustervars   = "Commune"
    ),
    error = function(e) { message("  ERROR att_gt: ", e$message); NULL }
  )
  
  if (is.null(att_cs)) return(NULL)
  
  att_agg <- tryCatch(
    did::aggte(att_cs, type = "simple"),
    error = function(e) { message("  ERROR aggte: ", e$message); NULL }
  )
  
  if (is.null(att_agg)) return(NULL)
  
  att_hat <- as.numeric(att_agg$overall.att)
  se_hat  <- as.numeric(att_agg$overall.se)
  
  tibble(
    Segment = as.character(seg),
    ATT     = att_hat,
    SE      = se_hat,
    CI_low  = att_hat - 1.96 * se_hat,
    CI_high = att_hat + 1.96 * se_hat,
    p_value = 2 * (1 - pnorm(abs(att_hat / se_hat)))
  )
}

sector_results <- bind_rows(lapply(sector_cols, run_att_for_sector))

plot_df <- sector_results %>%
  arrange(desc(ATT)) %>%
  mutate(
    Segment  = factor(Segment, levels = Segment),
    color_lbl = "black"
  )

ggplot(plot_df, aes(x = ATT, y = Segment)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.6) +
  geom_errorbarh(
    aes(xmin = CI_low, xmax = CI_high),
    height = 0.25, color = "gray55", linewidth = 0.6
  ) +
  geom_point(size = 2.2, color = "black") +
  scale_x_continuous(
    breaks = seq(-0.20, 0.00, by = 0.05),
    labels = function(x) sprintf("%.2f", x)
  ) +
  labs(
    x       = "ATT on Green (95% CI)",
    y       = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.major.x = element_line(color = "gray88"),
    panel.grid.major.y = element_line(color = "gray92"),
    panel.grid.minor   = element_blank(),
    axis.text.y = element_text(
      size   = 9,
      colour = plot_df$color_lbl
    ),
    plot.caption = element_text(hjust = 0.5, size = 10, margin = margin(t = 8))
  )


### Table F3 ###

library(readxl)
library(dplyr)
library(tidyr)
library(did)

set.seed(123)

setwd("your path")

df <- read_excel("data.xlsx") %>%
  distinct() %>%
  mutate(Commune = paste(Prefecture, City, sep = "_"))

sector_cols <- c(
  "Paper","Stationery","OfficeFurniture","MobilePhones","HomeAppliances",
  "AirConditioners","WaterHeaters","Lighting","Vehicles","FireExtinguishers",
  "Uniforms","InteriorBedding","WorkGloves","OtherTextiles","Equipment","DisasterSupplies"
)

years_window <- 2012:2020
T_years      <- length(years_window)

panel_green <- df %>%
  select(
    Prefecture, City, Commune,
    matches(paste0("^(", paste(sector_cols, collapse = "|"), ")_\\d{4}$"))
  ) %>%
  pivot_longer(
    cols         = matches("_(19|20)\\d{2}$"),
    names_to     = c("Segment", "Year"),
    names_pattern = "^(.*)_(\\d{4})$",
    values_to    = "Green"
  ) %>%
  mutate(
    Year    = as.integer(Year),
    Segment = factor(Segment, levels = sector_cols),
    Green   = as.numeric(Green)
  ) %>%
  filter(Year %in% years_window, !is.na(Green))

communes_keep <- panel_green %>%
  distinct(Commune, Segment, Year) %>%
  count(Commune, name = "n_cells") %>%
  filter(n_cells == length(sector_cols) * T_years) %>%
  pull(Commune)

panel_green <- panel_green %>% filter(Commune %in% communes_keep)

panel_eq <- df %>%
  select(Prefecture, City, Commune, matches("^earthquake_score_\\d{4}$")) %>%
  pivot_longer(
    cols          = matches("^earthquake_score_\\d{4}$"),
    names_to      = "Year",
    names_pattern = "^earthquake_score_(\\d{4})$",
    values_to     = "earthquake_score"
  ) %>%
  mutate(
    Year             = as.integer(Year),
    earthquake_score = as.numeric(earthquake_score),
    earthquake_score = ifelse(is.na(earthquake_score), 0, earthquake_score)
  ) %>%
  filter(Year %in% years_window, Commune %in% communes_keep)

th <- 1L

timing_tbl <- panel_eq %>%
  mutate(eq_treat = as.integer(earthquake_score >= th)) %>%
  group_by(Commune) %>%
  summarise(
    G = ifelse(any(eq_treat == 1L), min(Year[eq_treat == 1L]), 0L),
    .groups = "drop"
  )

n_treated <- sum(timing_tbl$G > 0L)
n_control <- sum(timing_tbl$G == 0L)

run_att_sector <- function(seg) {
  message("Estimating: ", seg)
  
  panel_seg <- panel_green %>%
    filter(Segment == seg) %>%
    left_join(timing_tbl, by = "Commune") %>%
    mutate(id_cs = as.integer(factor(Commune))) %>%
    as.data.frame()
  
  att_cs <- did::att_gt(
    yname         = "Green",
    tname         = "Year",
    idname        = "id_cs",
    gname         = "G",
    data          = panel_seg,
    panel         = TRUE,
    control_group = "nevertreated",
    clustervars   = "Commune"
  )
  
  att_agg <- did::aggte(att_cs, type = "simple")
  
  att_hat <- as.numeric(att_agg$overall.att)
  se_hat  <- as.numeric(att_agg$overall.se)
  
  tibble(
    Outcome   = as.character(seg),
    ATT       = att_hat,
    SE        = se_hat,
    CI_low    = att_hat - 1.96 * se_hat,
    CI_high   = att_hat + 1.96 * se_hat,
    p_value   = 2 * (1 - pnorm(abs(att_hat / se_hat))),
    Treated   = n_treated,
    Control   = n_control
  )
}

results_by_sector <- bind_rows(lapply(sector_cols, run_att_sector))

print(results_by_sector, n = Inf)


### Figure G9 ###

library(readxl)
library(dplyr)
library(tidyr)
library(did)
library(ggplot2)
library(ggrepel)

set.seed(123)

setwd("your path")

df <- read_excel("data.xlsx") %>%
  distinct() %>%
  mutate(Commune = paste(Prefecture, City, sep = "_"))

sector_cols <- c(
  "Paper","Stationery","OfficeFurniture","MobilePhones","HomeAppliances",
  "AirConditioners","WaterHeaters","Lighting","Vehicles","FireExtinguishers",
  "Uniforms","InteriorBedding","WorkGloves","OtherTextiles","Equipment","DisasterSupplies"
)

years_window <- 2012:2020
T_years      <- length(years_window)

panel_green <- df %>%
  select(
    Prefecture, City, Commune,
    matches(paste0("^(", paste(sector_cols, collapse = "|"), ")_\\d{4}$"))
  ) %>%
  pivot_longer(
    cols          = matches("_(19|20)\\d{2}$"),
    names_to      = c("Segment", "Year"),
    names_pattern = "^(.*)_(\\d{4})$",
    values_to     = "Green"
  ) %>%
  mutate(
    Year    = as.integer(Year),
    Segment = factor(Segment, levels = sector_cols),
    Green   = as.numeric(Green)
  ) %>%
  filter(Year %in% years_window, !is.na(Green))

communes_keep <- panel_green %>%
  distinct(Commune, Segment, Year) %>%
  count(Commune, name = "n_cells") %>%
  filter(n_cells == length(sector_cols) * T_years) %>%
  pull(Commune)

panel_green <- panel_green %>% filter(Commune %in% communes_keep)

panel_eq <- df %>%
  select(Prefecture, City, Commune, matches("^earthquake_score_\\d{4}$")) %>%
  pivot_longer(
    cols          = matches("^earthquake_score_\\d{4}$"),
    names_to      = "Year",
    names_pattern = "^earthquake_score_(\\d{4})$",
    values_to     = "earthquake_score"
  ) %>%
  mutate(
    Year             = as.integer(Year),
    earthquake_score = as.numeric(earthquake_score),
    earthquake_score = ifelse(is.na(earthquake_score), 0, earthquake_score)
  ) %>%
  filter(Year %in% years_window, Commune %in% communes_keep)

th <- 1L

timing_tbl <- panel_eq %>%
  mutate(eq_treat = as.integer(earthquake_score >= th)) %>%
  group_by(Commune) %>%
  summarise(
    G = ifelse(any(eq_treat == 1L), min(Year[eq_treat == 1L]), 0L),
    .groups = "drop"
  )

run_att_sector <- function(seg) {
  message("Estimating: ", seg)
  
  panel_seg <- panel_green %>%
    filter(Segment == seg) %>%
    left_join(timing_tbl, by = "Commune") %>%
    mutate(id_cs = as.integer(factor(Commune))) %>%
    as.data.frame()
  
  att_cs <- tryCatch(
    did::att_gt(
      yname         = "Green",
      tname         = "Year",
      idname        = "id_cs",
      gname         = "G",
      data          = panel_seg,
      panel         = TRUE,
      control_group = "nevertreated",
      clustervars   = "Commune"
    ),
    error = function(e) { message("  ERROR att_gt: ", e$message); NULL }
  )
  if (is.null(att_cs)) return(NULL)
  
  att_agg <- tryCatch(
    did::aggte(att_cs, type = "simple"),
    error = function(e) { message("  ERROR aggte: ", e$message); NULL }
  )
  if (is.null(att_agg)) return(NULL)
  
  att_hat <- as.numeric(att_agg$overall.att)
  se_hat  <- as.numeric(att_agg$overall.se)
  
  tibble(
    Outcome = as.character(seg),
    ATT     = att_hat,
    SE      = se_hat,
    p_value = 2 * (1 - pnorm(abs(att_hat / se_hat)))
  )
}

results_by_sector <- bind_rows(lapply(sector_cols, run_att_sector))

baseline_2011 <- df %>%
  filter(Commune %in% communes_keep) %>%
  select(Commune,
         matches(paste0("^(", paste(sector_cols, collapse = "|"), ")_2011$"))) %>%
  pivot_longer(
    cols          = matches("_2011$"),
    names_to      = "Segment",
    names_pattern = "^(.*)_2011$",
    values_to     = "Green_2011"
  ) %>%
  mutate(Green_2011 = as.numeric(Green_2011)) %>%
  filter(!is.na(Green_2011)) %>%
  group_by(Segment) %>%
  summarise(mean_Green_2011 = mean(Green_2011, na.rm = TRUE), .groups = "drop")

plot_df <- results_by_sector %>%
  left_join(baseline_2011, by = c("Outcome" = "Segment")) %>%
  mutate(
    sig_col = case_when(
      p_value < 0.05 ~ "p < 0.05",
      p_value < 0.10 ~ "p < 0.10",
      TRUE           ~ "n.s."
    )
  )

ggplot(plot_df, aes(x = mean_Green_2011, y = ATT)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50",
             linewidth = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "black", fill = "gray82",
              linewidth = 0.8, alpha = 0.4) +
  geom_point(size = 2.2, color = "black") +
  geom_text_repel(
    aes(label = Outcome),
    color         = "black",
    size          = 2.9,
    max.overlaps  = 20,
    segment.size  = 0.3,
    segment.color = "gray60"
  ) +
  scale_x_continuous(breaks = seq(0.4, 0.9, by = 0.1)) +
  scale_y_continuous(breaks = seq(-0.10, 0.00, by = 0.025)) +
  labs(
    x = "Sectoral average of Green in 2011",
    y = "Sector-level ATT on Green"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90")
  )


### Figure H10 ###

library(readxl)
library(dplyr)
library(tidyr)
library(did)
library(ggplot2)
library(knitr)

setwd("your path")
set.seed(123)

DATA_PATH <- "data.xlsx"

years_window <- 2012:2020
T_years <- length(years_window)

scores <- c(1, 2, 3)

score_labels <- c(
  "1" = "Intensity ≥ Shindo 5-",
  "2" = "Intensity ≥ Shindo 5+",
  "3" = "Intensity ≥ Shindo 6-"
)

MIN_E <- -5
MAX_E <-  5

df <- read_excel(DATA_PATH) %>%
  distinct() %>%
  mutate(Commune = paste(Prefecture, City, sep = "_"))

panel_y <- df %>%
  select(Prefecture, City, Commune, matches("^repairs_per_capita_\\d{4}$")) %>%
  pivot_longer(
    cols = matches("^repairs_per_capita_\\d{4}$"),
    names_to = "Year",
    names_pattern = "^repairs_per_capita_(\\d{4})$",
    values_to = "repairs_per_capita"
  ) %>%
  mutate(
    Year = as.integer(Year),
    repairs_per_capita = as.numeric(repairs_per_capita)
  ) %>%
  filter(Year %in% years_window, !is.na(repairs_per_capita))

communes_keep <- panel_y %>%
  distinct(Commune, Year) %>%
  count(Commune, name = "n_years") %>%
  filter(n_years == T_years) %>%
  pull(Commune)

panel_y <- panel_y %>% filter(Commune %in% communes_keep)

panel_score <- df %>%
  select(Prefecture, City, Commune, matches("^earthquake_score_\\d{4}$")) %>%
  pivot_longer(
    cols = matches("^earthquake_score_\\d{4}$"),
    names_to = "Year",
    names_pattern = "^earthquake_score_(\\d{4})$",
    values_to = "earthquake_score"
  ) %>%
  mutate(
    Year = as.integer(Year),
    earthquake_score = as.numeric(earthquake_score),
    earthquake_score = ifelse(is.na(earthquake_score), 0, earthquake_score)
  ) %>%
  filter(Year %in% years_window) %>%
  filter(Commune %in% communes_keep)

run_score_pure <- function(k) {
  excluded_communes <- panel_score %>%
    filter(earthquake_score %in% scores, earthquake_score < k) %>%
    distinct(Commune) %>%
    pull(Commune)
  
  communes_k <- setdiff(communes_keep, excluded_communes)
  
  y_k <- panel_y %>% filter(Commune %in% communes_k)
  s_k <- panel_score %>% filter(Commune %in% communes_k)
  
  s_k <- s_k %>%
    mutate(treated = as.integer(earthquake_score >= k))
  
  timing_tbl <- s_k %>%
    group_by(Commune) %>%
    summarise(
      first_k_year = ifelse(any(treated == 1L), min(Year[treated == 1L]), 0L),
      .groups = "drop"
    )
  
  panel <- y_k %>%
    left_join(s_k %>% select(Commune, Year, earthquake_score, treated),
              by = c("Commune", "Year")) %>%
    left_join(timing_tbl, by = "Commune") %>%
    mutate(
      G  = as.integer(first_k_year),
      id = as.integer(factor(Commune))
    ) %>%
    as.data.frame()
  
  comm_status <- panel %>%
    distinct(Commune, G) %>%
    mutate(is_treated_commune = (G > 0))
  n_treated_communes <- sum(comm_status$is_treated_commune)
  n_control_communes <- sum(!comm_status$is_treated_commune)
  
  att_cs <- att_gt(
    yname   = "repairs_per_capita",
    tname   = "Year",
    idname  = "id",
    gname   = "G",
    data    = panel,
    panel   = TRUE,
    control_group = "nevertreated",
    clustervars   = "Commune",
    xformla = ~ 1,
    allow_unbalanced_panel = FALSE
  )
  
  att_global <- aggte(att_cs, type = "simple")
  att_hat <- att_global$overall.att
  se_hat  <- att_global$overall.se
  t_stat  <- att_hat / se_hat
  p_value <- 2 * (1 - pnorm(abs(t_stat)))
  
  att_row <- data.frame(
    score = k,
    label = score_labels[as.character(k)],
    ATT = att_hat,
    SE  = se_hat,
    p_value = p_value,
    n_treated_communes = n_treated_communes,
    n_control_communes = n_control_communes,
    n_communes_kept = length(unique(panel$Commune))
  )

  es <- aggte(att_cs, type = "dynamic", min_e = MIN_E, max_e = MAX_E)
  
  es_df <- data.frame(
    score      = k,
    label      = score_labels[as.character(k)],
    event_time = es$egt,
    att        = es$att.egt,
    se         = es$se.egt,
    crit       = es$crit.val.egt
  ) %>%
    mutate(
      ci_low  = att - crit * se,
      ci_high = att + crit * se
    )
  
  list(att_row = att_row, es_df = es_df, att_cs = att_cs, panel = panel)
}

res <- lapply(scores, run_score_pure)
names(res) <- paste0("score", scores)

att_table <- bind_rows(lapply(res, `[[`, "att_row")) %>%
  mutate(
    ATT = round(ATT, 4),
    SE  = round(SE, 4),
    p_value = round(p_value, 4)
  ) %>%
  arrange(score)

es_all <- bind_rows(lapply(res, `[[`, "es_df")) %>%
  mutate(
    label = factor(label, levels = score_labels[c("1","2","3")])
  )

p_facet <- ggplot(es_all, aes(x = event_time, y = att)) +
  geom_point(size = 2.4) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high),
                width = 0.2, linewidth = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_x_continuous(breaks = sort(unique(es_all$event_time))) +
  facet_wrap(~ label) +
  labs(
    x = "Event time",
    y = "ATT on repairs_per_capita (95% CI)"
  ) +
  theme_minimal(base_size = 13)

print(p_facet)


### Figure K16 ###

library(dplyr)
library(tidyr)
library(fixest)
library(ggplot2)
library(stringr)
library(readxl)
library(patchwork)

setwd("your path")
set.seed(123)

df_raw <- read_excel("data.xlsx") %>%
  distinct() %>%
  mutate(Commune = paste(Prefecture, City, sep = "_"))

years_window <- 2012:2020
sector_cols  <- c("Paper","Stationery","OfficeFurniture","MobilePhones","HomeAppliances",
                  "AirConditioners","WaterHeaters","Lighting","Vehicles","FireExtinguishers",
                  "Uniforms","InteriorBedding","WorkGloves","OtherTextiles","Equipment","DisasterSupplies")

panel_green <- df_raw %>%
  select(Prefecture, City, Commune, Bridges_per_capita,
         matches(paste0("^(", paste(sector_cols, collapse="|"), ")_\\d{4}$"))) %>%
  pivot_longer(matches("_(19|20)\\d{2}$"), names_to=c("Segment","Year"),
               names_pattern="^(.*)_(\\d{4})$", values_to="Green") %>%
  mutate(Year=as.integer(Year), Segment=factor(Segment, levels=sector_cols), Green=as.numeric(Green)) %>%
  filter(Year %in% years_window, !is.na(Green))

communes_keep <- panel_green %>%
  distinct(Commune, Segment, Year) %>% count(Commune) %>%
  filter(n == length(sector_cols) * length(years_window)) %>% pull(Commune)
panel_green <- filter(panel_green, Commune %in% communes_keep)

repair_cols <- names(df_raw)[str_detect(names(df_raw), "^repairs_per_capita_\\d{4}$")]
panel_repair <- df_raw %>%
  select(Prefecture, City, Commune, all_of(repair_cols)) %>%
  pivot_longer(all_of(repair_cols), names_to="Year",
               names_pattern="^repairs_per_capita_(\\d{4})$", values_to="repair_pc") %>%
  mutate(Year=as.integer(Year), repair_pc=as.numeric(repair_pc)) %>%
  filter(Year %in% years_window, Commune %in% communes_keep)

threshold_label <- function(th) dplyr::case_when(
  th==1L ~ "Intensity \u2265 Shindo 5-",
  th==2L ~ "Intensity \u2265 Shindo 5+",
  th==3L ~ "Intensity \u2265 Shindo 6-",
  TRUE   ~ paste0("score \u2265 ", th)
)

fs_list <- results_list <- vector("list", 3)

for (k in 1:3) {
  th <- as.integer(k)
  
  panel_eq_th <- df_raw %>%
    select(Prefecture, City, Commune, matches("^earthquake_score_\\d{4}$")) %>%
    pivot_longer(matches("^earthquake_score_\\d{4}$"), names_to="Year",
                 names_pattern="^earthquake_score_(\\d{4})$", values_to="earthquake_score") %>%
    mutate(Year=as.integer(Year),
           earthquake_score=as.numeric(ifelse(is.na(earthquake_score), 0, earthquake_score)),
           eq_treat=as.integer(earthquake_score >= th)) %>%
    filter(Year %in% years_window, Commune %in% communes_keep)
  
  timing_tbl <- panel_eq_th %>% group_by(Commune) %>%
    summarise(first_eq_year=ifelse(any(eq_treat==1L), min(Year[eq_treat==1L]), 0L), .groups="drop")
  
  panel_th <- panel_green %>%
    left_join(panel_eq_th %>% select(Prefecture, City, Commune, Year, eq_treat), by=c("Prefecture","City","Commune","Year")) %>%
    left_join(timing_tbl, by="Commune") %>%
    left_join(panel_repair, by=c("Prefecture","City","Commune","Year")) %>%
    mutate(treated=first_eq_year>0L, post_eq=as.integer(treated & Year>=first_eq_year),
           Year_Segment=interaction(Year, Segment, drop=TRUE),
           Z=Bridges_per_capita*post_eq) %>%
    filter(!is.na(repair_pc), !is.na(Z), !is.na(Green)) %>% as.data.frame()
  
  make_row <- function(model, yvar, spec) {
    ct <- tryCatch(coeftable(model), error=function(e) NULL)
    val <- if (!is.null(ct) && yvar %in% rownames(ct))
      list(b=ct[yvar,"Estimate"], se=ct[yvar,"Std. Error"]) else list(b=NA_real_, se=NA_real_)
    tibble(threshold=th, threshold_lab=threshold_label(th), spec=spec,
           b=val$b, se=val$se,
           ci_low=val$b-1.96*val$se, ci_high=val$b+1.96*val$se)
  }
  
  fs_list[[k]] <- bind_rows(
    make_row(feols(repair_pc ~ Z | Commune+Year, panel_th, cluster=~Commune), "Z", "post-earthquake controlled"),
    make_row(feols(repair_pc ~ Z+post_eq | Commune+Year, panel_th, cluster=~Commune), "Z", "post-earthquake not controlled")
  )
  
  results_list[[k]] <- bind_rows(
    make_row(feols(Green ~ 1 | Commune+Year_Segment | repair_pc ~ Z, panel_th, cluster=~Commune), "fit_repair_pc", "post-earthquake controlled"),
    make_row(feols(Green ~ post_eq | Commune+Year_Segment | repair_pc ~ Z, panel_th, cluster=~Commune), "fit_repair_pc", "post-earthquake not controlled")
  )
}

fs_df  <- bind_rows(fs_list)
ss_df  <- bind_rows(results_list)

make_plot <- function(df, y, ylab, subtitle) {
  ggplot(df, aes(x=spec, y=.data[[y]])) +
    geom_point(size=3) +
    geom_errorbar(aes(ymin=ci_low, ymax=ci_high), width=0.15, linewidth=0.8) +
    geom_hline(yintercept=0, linetype="dashed") +
    facet_wrap(~threshold_lab, ncol=1, scales="free_y") +
    labs(subtitle=subtitle, x=NULL, y=ylab) +
    theme_minimal(base_size=12) +
    theme(axis.text.x=element_text(hjust=0.5, size=8),
          strip.text=element_text(hjust=0.5),
          plot.subtitle=element_text(hjust=0.5))
}

p_fs <- make_plot(fs_df, "b",
                  "First-stage effect of Z on repairs_per_capita (95% CI)",
                  "First stage model by treatment threshold")
p_ss <- make_plot(ss_df, "b",
                  "Second-stage effect of repairs_per_capita on Green (95% CI)",
                  "Second stage model by treatment threshold")

p_both <- p_fs | p_ss
print(p_both)


### Figure L17 ###

library(dplyr)
library(tidyr)
library(fixest)
library(ggplot2)
library(stringr)
library(readxl)
library(patchwork)

setwd("your path")
set.seed(123)

df_raw <- read_excel("data.xlsx") %>%
  distinct() %>%
  mutate(Commune = paste(Prefecture, City, sep = "_"))

years_window <- 2012:2020
sector_cols  <- c("Paper","Stationery","OfficeFurniture","MobilePhones","HomeAppliances",
                  "AirConditioners","WaterHeaters","Lighting","Vehicles","FireExtinguishers",
                  "Uniforms","InteriorBedding","WorkGloves","OtherTextiles","Equipment","DisasterSupplies")

panel_green <- df_raw %>%
  select(Prefecture, City, Commune, Tunnels_per_capita,
         matches(paste0("^(", paste(sector_cols, collapse="|"), ")_\\d{4}$"))) %>%
  pivot_longer(matches("_(19|20)\\d{2}$"), names_to=c("Segment","Year"),
               names_pattern="^(.*)_(\\d{4})$", values_to="Green") %>%
  mutate(Year=as.integer(Year), Segment=factor(Segment, levels=sector_cols), Green=as.numeric(Green)) %>%
  filter(Year %in% years_window, !is.na(Green))

communes_keep <- panel_green %>%
  distinct(Commune, Segment, Year) %>% count(Commune) %>%
  filter(n == length(sector_cols) * length(years_window)) %>% pull(Commune)
panel_green <- filter(panel_green, Commune %in% communes_keep)

repair_cols <- names(df_raw)[str_detect(names(df_raw), "^repairs_per_capita_\\d{4}$")]
panel_repair <- df_raw %>%
  select(Prefecture, City, Commune, all_of(repair_cols)) %>%
  pivot_longer(all_of(repair_cols), names_to="Year",
               names_pattern="^repairs_per_capita_(\\d{4})$", values_to="repair_pc") %>%
  mutate(Year=as.integer(Year), repair_pc=as.numeric(repair_pc)) %>%
  filter(Year %in% years_window, Commune %in% communes_keep)

threshold_label <- function(th) dplyr::case_when(
  th==1L ~ "Intensity \u2265 Shindo 5-",
  th==2L ~ "Intensity \u2265 Shindo 5+",
  th==3L ~ "Intensity \u2265 Shindo 6-",
  TRUE   ~ paste0("score \u2265 ", th)
)

fs_list <- results_list <- vector("list", 3)

for (k in 1:3) {
  th <- as.integer(k)
  
  panel_eq_th <- df_raw %>%
    select(Prefecture, City, Commune, matches("^earthquake_score_\\d{4}$")) %>%
    pivot_longer(matches("^earthquake_score_\\d{4}$"), names_to="Year",
                 names_pattern="^earthquake_score_(\\d{4})$", values_to="earthquake_score") %>%
    mutate(Year=as.integer(Year),
           earthquake_score=as.numeric(ifelse(is.na(earthquake_score), 0, earthquake_score)),
           eq_treat=as.integer(earthquake_score >= th)) %>%
    filter(Year %in% years_window, Commune %in% communes_keep)
  
  timing_tbl <- panel_eq_th %>% group_by(Commune) %>%
    summarise(first_eq_year=ifelse(any(eq_treat==1L), min(Year[eq_treat==1L]), 0L), .groups="drop")
  
  panel_th <- panel_green %>%
    left_join(panel_eq_th %>% select(Prefecture, City, Commune, Year, eq_treat), by=c("Prefecture","City","Commune","Year")) %>%
    left_join(timing_tbl, by="Commune") %>%
    left_join(panel_repair, by=c("Prefecture","City","Commune","Year")) %>%
    mutate(treated=first_eq_year>0L, post_eq=as.integer(treated & Year>=first_eq_year),
           Year_Segment=interaction(Year, Segment, drop=TRUE),
           Z=Tunnels_per_capita*post_eq) %>%
    filter(!is.na(repair_pc), !is.na(Z), !is.na(Green)) %>% as.data.frame()
  
  make_row <- function(model, yvar, spec) {
    ct <- tryCatch(coeftable(model), error=function(e) NULL)
    val <- if (!is.null(ct) && yvar %in% rownames(ct))
      list(b=ct[yvar,"Estimate"], se=ct[yvar,"Std. Error"]) else list(b=NA_real_, se=NA_real_)
    tibble(threshold=th, threshold_lab=threshold_label(th), spec=spec,
           b=val$b, se=val$se,
           ci_low=val$b-1.96*val$se, ci_high=val$b+1.96*val$se)
  }
  
  fs_list[[k]] <- bind_rows(
    make_row(feols(repair_pc ~ Z | Commune+Year, panel_th, cluster=~Commune), "Z", "post-earthquake controlled"),
    make_row(feols(repair_pc ~ Z+post_eq | Commune+Year, panel_th, cluster=~Commune), "Z", "post-earthquake not controlled")
  )
  
  results_list[[k]] <- bind_rows(
    make_row(feols(Green ~ 1 | Commune+Year_Segment | repair_pc ~ Z, panel_th, cluster=~Commune), "fit_repair_pc", "post-earthquake controlled"),
    make_row(feols(Green ~ post_eq | Commune+Year_Segment | repair_pc ~ Z, panel_th, cluster=~Commune), "fit_repair_pc", "post-earthquake not controlled")
  )
}

fs_df  <- bind_rows(fs_list)
ss_df  <- bind_rows(results_list)

make_plot <- function(df, y, ylab, subtitle) {
  ggplot(df, aes(x=spec, y=.data[[y]])) +
    geom_point(size=3) +
    geom_errorbar(aes(ymin=ci_low, ymax=ci_high), width=0.15, linewidth=0.8) +
    geom_hline(yintercept=0, linetype="dashed") +
    facet_wrap(~threshold_lab, ncol=1, scales="free_y") +
    labs(subtitle=subtitle, x=NULL, y=ylab) +
    theme_minimal(base_size=12) +
    theme(axis.text.x=element_text(hjust=0.5, size=8),
          strip.text=element_text(hjust=0.5),
          plot.subtitle=element_text(hjust=0.5))
}

p_fs <- make_plot(fs_df, "b",
                  "First-stage effect of Z on repairs_per_capita (95% CI)",
                  "First stage model by treatment threshold")
p_ss <- make_plot(ss_df, "b",
                  "Second-stage effect of repairs_per_capita on Green (95% CI)",
                  "Second stage model by treatment threshold")

p_both <- p_fs | p_ss
print(p_both)


### Figure M18 and Table M4 ###

library(dplyr)
library(tidyr) 
library(fixest)
library(ggplot2)
library(stringr)
library(readxl)

setwd("your path")

df_raw <- read_excel("data.xlsx") %>%
  distinct() %>%
  mutate(Commune = paste(Prefecture, City, sep = "_"))

years_window <- 2012:2020
sector_cols  <- c("Paper","Stationery","OfficeFurniture","MobilePhones","HomeAppliances",
                  "AirConditioners","WaterHeaters","Lighting","Vehicles","FireExtinguishers",
                  "Uniforms","InteriorBedding","WorkGloves","OtherTextiles","Equipment","DisasterSupplies")
tau_min <- -5; tau_max <- 5; tau_ref <- -1
taus     <- tau_min:tau_max
taus_use <- setdiff(taus, tau_ref)

panel_green <- df_raw %>%
  select(Commune, Bridges_per_capita,
         matches(paste0("^(", paste(sector_cols, collapse="|"), ")_\\d{4}$"))) %>%
  pivot_longer(matches("_(19|20)\\d{2}$"), names_to=c("Segment","Year"),
               names_pattern="^(.*)_(\\d{4})$", values_to="Green") %>%
  mutate(Year=as.integer(Year), Green=as.numeric(Green)) %>%
  filter(Year %in% years_window, !is.na(Green)) %>%
  distinct(Commune, Segment, Year, .keep_all=TRUE)

repair_cols <- names(df_raw)[str_detect(names(df_raw), "^repairs_per_capita_\\d{4}$")]
panel_repair <- df_raw %>%
  select(Commune, all_of(repair_cols)) %>%
  pivot_longer(all_of(repair_cols), names_to="Year",
               names_pattern="^repairs_per_capita_(\\d{4})$", values_to="repair_pc") %>%
  mutate(Year=as.integer(Year), repair_pc=as.numeric(repair_pc)) %>%
  filter(Year %in% years_window) %>%
  group_by(Commune, Year) %>%
  summarise(repair_pc=mean(repair_pc, na.rm=TRUE), .groups="drop")

timing_tbl <- df_raw %>%
  select(Commune, matches("^earthquake_score_\\d{4}$")) %>%
  pivot_longer(matches("^earthquake_score_\\d{4}$"), names_to="Year",
               names_pattern="^earthquake_score_(\\d{4})$", values_to="earthquake_score") %>%
  mutate(Year=as.integer(Year),
         earthquake_score=as.numeric(ifelse(is.na(earthquake_score), 0, earthquake_score)),
         treated=as.integer(earthquake_score >= 1)) %>%
  filter(Year %in% years_window) %>%
  group_by(Commune) %>%
  summarise(first_eq_year=ifelse(any(treated==1), min(Year[treated==1]), 0L), .groups="drop")

panel <- panel_green %>%
  left_join(timing_tbl, by="Commune") %>%
  left_join(panel_repair, by=c("Commune","Year")) %>%
  mutate(treated=first_eq_year > 0,
         event_time=ifelse(treated, Year - first_eq_year, NA_integer_),
         Year_Segment=interaction(Year, Segment, drop=TRUE)) %>%
  filter(!is.na(repair_pc))

for (tau in taus_use) {
  sfx <- paste0(ifelse(tau < 0, "m", "p"), abs(tau))
  panel[[paste0("D_", sfx)]] <- as.integer(!is.na(panel$event_time) & panel$event_time == tau)
  panel[[paste0("Z_", sfx)]] <- panel$Bridges_per_capita * panel[[paste0("D_", sfx)]]
}

panel_es <- filter(panel, is.na(event_time) | (event_time >= tau_min & event_time <= tau_max))

z_terms <- paste0("Z_", ifelse(taus_use < 0, "m", "p"), abs(taus_use), collapse=" + ")

m_rep <- feols(as.formula(paste0("repair_pc ~ ", z_terms, " | Commune + Year")),
               data=panel_es, cluster=~Commune)
m_gr  <- feols(as.formula(paste0("Green ~ ",    z_terms, " | Commune + Year_Segment")),
               data=panel_es, cluster=~Commune)

extract_coefs <- function(model, outcome) {
  ct <- as.data.frame(coeftable(model))
  ct$term <- rownames(ct)
  ct %>%
    filter(grepl("^Z_[mp]\\d+$", term)) %>%
    mutate(tau     = ifelse(grepl("^Z_m", term), -1L, 1L) * as.integer(gsub("^Z_[mp]", "", term)),
           outcome = outcome,
           ci_low  = Estimate - 1.96 * `Std. Error`,
           ci_high = Estimate + 1.96 * `Std. Error`) %>%
    select(outcome, tau, Estimate, ci_low, ci_high) %>%
    bind_rows(tibble(outcome=outcome, tau=tau_ref, Estimate=0, ci_low=0, ci_high=0)) %>%
    arrange(tau)
}

es_coefs <- bind_rows(extract_coefs(m_rep, "repairs_per_capita"),
                      extract_coefs(m_gr,  "Green")) %>%
  mutate(outcome = factor(outcome, levels=c("repairs_per_capita","Green")))

p <- ggplot(es_coefs, aes(x=tau, y=Estimate)) +
  geom_point(size=2.5) +
  geom_errorbar(aes(ymin=ci_low, ymax=ci_high), width=0.2) +
  geom_hline(yintercept=0, linetype="dashed") +
  facet_wrap(~outcome, scales="free_y") +
  scale_x_continuous(breaks=tau_min:tau_max) +
  labs(x="Event time t",
       y="Coefficient of Bridges_per_capita x 1{event time = t} (95% CI)") +
  theme_minimal(base_size=12)

print(p)

taus_pre    <- -5:-2
z_pre_names <- paste0("Z_", ifelse(taus_pre < 0, "m", "p"), abs(taus_pre))

pre_results <- bind_rows(
  lapply(list(list(m_rep, "repairs_per_capita"), list(m_gr, "Green")), function(x) {
    w <- wald(x[[1]], keep = z_pre_names)
    tibble(outcome = x[[2]], wald_F = w$stat, p_wald = w$p, dof = w$df1)
  })
)

print(pre_results)



### Figure N19 and Table N5 ###

library(dplyr)
library(tidyr)
library(fixest)
library(ggplot2)
library(stringr)
library(readxl)

setwd("your path")

df_raw <- read_excel("data.xlsx") %>%
  distinct() %>%
  mutate(Commune = paste(Prefecture, City, sep = "_"))

years_window <- 2012:2020
sector_cols  <- c("Paper","Stationery","OfficeFurniture","MobilePhones","HomeAppliances",
                  "AirConditioners","WaterHeaters","Lighting","Vehicles","FireExtinguishers",
                  "Uniforms","InteriorBedding","WorkGloves","OtherTextiles","Equipment","DisasterSupplies")
tau_min <- -5; tau_max <- 5; tau_ref <- -1
taus     <- tau_min:tau_max
taus_use <- setdiff(taus, tau_ref)

panel_green <- df_raw %>%
  select(Commune, Tunnels_per_capita,
         matches(paste0("^(", paste(sector_cols, collapse="|"), ")_\\d{4}$"))) %>%
  pivot_longer(matches("_(19|20)\\d{2}$"), names_to=c("Segment","Year"),
               names_pattern="^(.*)_(\\d{4})$", values_to="Green") %>%
  mutate(Year=as.integer(Year), Green=as.numeric(Green)) %>%
  filter(Year %in% years_window, !is.na(Green)) %>%
  distinct(Commune, Segment, Year, .keep_all=TRUE)

repair_cols <- names(df_raw)[str_detect(names(df_raw), "^repairs_per_capita_\\d{4}$")]
panel_repair <- df_raw %>%
  select(Commune, all_of(repair_cols)) %>%
  pivot_longer(all_of(repair_cols), names_to="Year",
               names_pattern="^repairs_per_capita_(\\d{4})$", values_to="repair_pc") %>%
  mutate(Year=as.integer(Year), repair_pc=as.numeric(repair_pc)) %>%
  filter(Year %in% years_window) %>%
  group_by(Commune, Year) %>%
  summarise(repair_pc=mean(repair_pc, na.rm=TRUE), .groups="drop")

timing_tbl <- df_raw %>%
  select(Commune, matches("^earthquake_score_\\d{4}$")) %>%
  pivot_longer(matches("^earthquake_score_\\d{4}$"), names_to="Year",
               names_pattern="^earthquake_score_(\\d{4})$", values_to="earthquake_score") %>%
  mutate(Year=as.integer(Year),
         earthquake_score=as.numeric(ifelse(is.na(earthquake_score), 0, earthquake_score)),
         treated=as.integer(earthquake_score >= 1)) %>%
  filter(Year %in% years_window) %>%
  group_by(Commune) %>%
  summarise(first_eq_year=ifelse(any(treated==1), min(Year[treated==1]), 0L), .groups="drop")

panel <- panel_green %>%
  left_join(timing_tbl, by="Commune") %>%
  left_join(panel_repair, by=c("Commune","Year")) %>%
  mutate(treated=first_eq_year > 0,
         event_time=ifelse(treated, Year - first_eq_year, NA_integer_),
         Year_Segment=interaction(Year, Segment, drop=TRUE)) %>%
  filter(!is.na(repair_pc))

for (tau in taus_use) {
  sfx <- paste0(ifelse(tau < 0, "m", "p"), abs(tau))
  panel[[paste0("D_", sfx)]] <- as.integer(!is.na(panel$event_time) & panel$event_time == tau)
  panel[[paste0("Z_", sfx)]] <- panel$Tunnels_per_capita * panel[[paste0("D_", sfx)]]
}

panel_es <- filter(panel, is.na(event_time) | (event_time >= tau_min & event_time <= tau_max))

z_terms <- paste0("Z_", ifelse(taus_use < 0, "m", "p"), abs(taus_use), collapse=" + ")

m_rep <- feols(as.formula(paste0("repair_pc ~ ", z_terms, " | Commune + Year")),
               data=panel_es, cluster=~Commune)
m_gr  <- feols(as.formula(paste0("Green ~ ",    z_terms, " | Commune + Year_Segment")),
               data=panel_es, cluster=~Commune)

extract_coefs <- function(model, outcome) {
  ct <- as.data.frame(coeftable(model))
  ct$term <- rownames(ct)
  ct %>%
    filter(grepl("^Z_[mp]\\d+$", term)) %>%
    mutate(tau     = ifelse(grepl("^Z_m", term), -1L, 1L) * as.integer(gsub("^Z_[mp]", "", term)),
           outcome = outcome,
           ci_low  = Estimate - 1.96 * `Std. Error`,
           ci_high = Estimate + 1.96 * `Std. Error`) %>%
    select(outcome, tau, Estimate, ci_low, ci_high) %>%
    bind_rows(tibble(outcome=outcome, tau=tau_ref, Estimate=0, ci_low=0, ci_high=0)) %>%
    arrange(tau)
}

es_coefs <- bind_rows(extract_coefs(m_rep, "repairs_per_capita"),
                      extract_coefs(m_gr,  "Green")) %>%
  mutate(outcome = factor(outcome, levels=c("repairs_per_capita","Green")))

p <- ggplot(es_coefs, aes(x=tau, y=Estimate)) +
  geom_point(size=2.5) +
  geom_errorbar(aes(ymin=ci_low, ymax=ci_high), width=0.2) +
  geom_hline(yintercept=0, linetype="dashed") +
  facet_wrap(~outcome, scales="free_y") +
  scale_x_continuous(breaks=tau_min:tau_max) +
  labs(x="Event time t",
       y="Coefficient of Tunnels_per_capita x 1{event time = t} (95% CI)") +
  theme_minimal(base_size=12)

print(p)

taus_pre    <- -5:-2
z_pre_names <- paste0("Z_", ifelse(taus_pre < 0, "m", "p"), abs(taus_pre))

pre_results <- bind_rows(
  lapply(list(list(m_rep, "repairs_per_capita"), list(m_gr, "Green")), function(x) {
    w <- wald(x[[1]], keep = z_pre_names)
    tibble(outcome = x[[2]], wald_F = w$stat, p_wald = w$p, dof = w$df1)
  })
)

print(pre_results)



### Figure 020 ###

library(dplyr)
library(tidyr)
library(fixest)
library(ggplot2)
library(stringr)
library(readxl)

setwd("your path")

df_raw <- read_excel("data.xlsx") %>%
  distinct() %>%
  mutate(Commune = paste(Prefecture, City, sep = "_"))

years_window <- 2012:2020
sector_cols  <- c("Paper","Stationery","OfficeFurniture","MobilePhones","HomeAppliances",
                  "AirConditioners","WaterHeaters","Lighting","Vehicles","FireExtinguishers",
                  "Uniforms","InteriorBedding","WorkGloves","OtherTextiles","Equipment","DisasterSupplies")
tau_min <- -5; tau_max <- 5; tau_ref <- -1
taus_use <- setdiff(tau_min:tau_max, tau_ref)

panel_green <- df_raw %>%
  select(Commune, Bridges_per_capita,
         matches(paste0("^(", paste(sector_cols, collapse="|"), ")_\\d{4}$"))) %>%
  pivot_longer(matches("_(19|20)\\d{2}$"), names_to=c("Segment","Year"),
               names_pattern="^(.*)_(\\d{4})$", values_to="Green") %>%
  mutate(Year=as.integer(Year), Green=as.numeric(Green)) %>%
  filter(Year %in% years_window, !is.na(Green)) %>%
  distinct(Commune, Segment, Year, .keep_all=TRUE)

repair_cols <- names(df_raw)[str_detect(names(df_raw), "^repairs_per_capita_\\d{4}$")]
panel_repair <- df_raw %>%
  select(Commune, all_of(repair_cols)) %>%
  pivot_longer(all_of(repair_cols), names_to="Year",
               names_pattern="^repairs_per_capita_(\\d{4})$", values_to="repair_pc") %>%
  mutate(Year=as.integer(Year), repair_pc=as.numeric(repair_pc)) %>%
  filter(Year %in% years_window) %>%
  group_by(Commune, Year) %>%
  summarise(repair_pc=mean(repair_pc, na.rm=TRUE), .groups="drop")

timing_tbl <- df_raw %>%
  select(Commune, matches("^earthquake_score_\\d{4}$")) %>%
  pivot_longer(matches("^earthquake_score_\\d{4}$"), names_to="Year",
               names_pattern="^earthquake_score_(\\d{4})$", values_to="earthquake_score") %>%
  mutate(Year=as.integer(Year),
         earthquake_score=as.numeric(ifelse(is.na(earthquake_score), 0, earthquake_score)),
         treated=as.integer(earthquake_score >= 1)) %>%
  filter(Year %in% years_window) %>%
  group_by(Commune) %>%
  summarise(first_eq_year=ifelse(any(treated==1), min(Year[treated==1]), 0L), .groups="drop")

panel <- panel_green %>%
  left_join(timing_tbl, by="Commune") %>%
  left_join(panel_repair, by=c("Commune","Year")) %>%
  left_join(df_raw %>% select(Commune, Population_2011) %>% rename(Size_i=Population_2011) %>%
              mutate(Size_i=as.numeric(Size_i)) %>% distinct(), by="Commune") %>%
  mutate(treated=first_eq_year > 0,
         event_time=ifelse(treated, Year - first_eq_year, NA_integer_),
         Year_Segment=interaction(Year, Segment, drop=TRUE),
         SizeBin=cut(Size_i, breaks=quantile(Size_i, probs=seq(0,1,0.25), na.rm=TRUE),
                     include.lowest=TRUE, labels=c("Q1_small","Q2","Q3","Q4_large"))) %>%
  filter(!is.na(repair_pc))

for (tau in taus_use) {
  sfx <- paste0(ifelse(tau < 0, "m", "p"), abs(tau))
  panel[[paste0("D_", sfx)]] <- as.integer(!is.na(panel$event_time) & panel$event_time == tau)
  panel[[paste0("Z_", sfx)]] <- panel$Bridges_per_capita * panel[[paste0("D_", sfx)]]
}

panel_es <- filter(panel, is.na(event_time) | (event_time >= tau_min & event_time <= tau_max))
z_terms  <- paste0("Z_", ifelse(taus_use < 0, "m", "p"), abs(taus_use), collapse=" + ")

m_rep_base <- feols(as.formula(paste0("repair_pc ~ ", z_terms, " | Commune + Year")),
                    data=panel_es, cluster=~Commune)
m_rep_size <- feols(as.formula(paste0("repair_pc ~ ", z_terms, " | Commune + Year + SizeBin^Year")),
                    data=panel_es, cluster=~Commune)
m_gr_base  <- feols(as.formula(paste0("Green ~ ", z_terms, " | Commune + Year_Segment")),
                    data=panel_es, cluster=~Commune)
m_gr_size  <- feols(as.formula(paste0("Green ~ ", z_terms, " | Commune + Year_Segment + SizeBin^Year")),
                    data=panel_es, cluster=~Commune)

extract_coefs <- function(model, outcome, spec) {
  ct <- as.data.frame(coeftable(model)); ct$term <- rownames(ct)
  ct %>%
    filter(grepl("^Z_[mp]\\d+$", term)) %>%
    mutate(tau    = ifelse(grepl("^Z_m", term), -1L, 1L) * as.integer(gsub("^Z_[mp]", "", term)),
           outcome=outcome, spec=spec,
           ci_low =Estimate - 1.96*`Std. Error`,
           ci_high=Estimate + 1.96*`Std. Error`) %>%
    select(outcome, spec, tau, Estimate, ci_low, ci_high) %>%
    bind_rows(tibble(outcome=outcome, spec=spec, tau=tau_ref, Estimate=0, ci_low=0, ci_high=0)) %>%
    arrange(tau)
}

es_comp <- bind_rows(
  extract_coefs(m_rep_base, "repairs_per_capita", "Baseline"),
  extract_coefs(m_rep_size, "repairs_per_capita", "Add SizeBin×Year"),
  extract_coefs(m_gr_base,  "Green",              "Baseline"),
  extract_coefs(m_gr_size,  "Green",              "Add SizeBin×Year")
) %>% mutate(outcome=factor(outcome, levels=c("repairs_per_capita","Green")),
             spec=factor(spec, levels=c("Add SizeBin×Year","Baseline")))

p <- ggplot(es_comp, aes(x=tau, y=Estimate, shape=spec)) +
  geom_point(position=position_dodge(0.35), size=2.2) +
  geom_errorbar(aes(ymin=ci_low, ymax=ci_high),
                position=position_dodge(0.35), width=0.2) +
  geom_hline(yintercept=0, linetype="dashed") +
  facet_wrap(~outcome, scales="free_y") +
  scale_x_continuous(breaks=tau_min:tau_max) +
  labs(x="Event time t",
       y="Coefficient of bridges_per_capita × 1{event_time = t} (95% CI)",
       shape=NULL) +
  theme_minimal(base_size=12)

print(p)



### Figure P21 ###

library(dplyr)
library(tidyr)
library(fixest)
library(ggplot2)
library(stringr)
library(readxl)

setwd("your path")

df_raw <- read_excel("data.xlsx") %>%
  distinct() %>%
  mutate(Commune = paste(Prefecture, City, sep = "_"))

years_window <- 2012:2020
sector_cols  <- c("Paper","Stationery","OfficeFurniture","MobilePhones","HomeAppliances",
                  "AirConditioners","WaterHeaters","Lighting","Vehicles","FireExtinguishers",
                  "Uniforms","InteriorBedding","WorkGloves","OtherTextiles","Equipment","DisasterSupplies")
tau_min <- -5; tau_max <- 5; tau_ref <- -1
taus_use <- setdiff(tau_min:tau_max, tau_ref)

panel_green <- df_raw %>%
  select(Commune, Tunnels_per_capita,
         matches(paste0("^(", paste(sector_cols, collapse="|"), ")_\\d{4}$"))) %>%
  pivot_longer(matches("_(19|20)\\d{2}$"), names_to=c("Segment","Year"),
               names_pattern="^(.*)_(\\d{4})$", values_to="Green") %>%
  mutate(Year=as.integer(Year), Green=as.numeric(Green)) %>%
  filter(Year %in% years_window, !is.na(Green)) %>%
  distinct(Commune, Segment, Year, .keep_all=TRUE)

repair_cols <- names(df_raw)[str_detect(names(df_raw), "^repairs_per_capita_\\d{4}$")]
panel_repair <- df_raw %>%
  select(Commune, all_of(repair_cols)) %>%
  pivot_longer(all_of(repair_cols), names_to="Year",
               names_pattern="^repairs_per_capita_(\\d{4})$", values_to="repair_pc") %>%
  mutate(Year=as.integer(Year), repair_pc=as.numeric(repair_pc)) %>%
  filter(Year %in% years_window) %>%
  group_by(Commune, Year) %>%
  summarise(repair_pc=mean(repair_pc, na.rm=TRUE), .groups="drop")

timing_tbl <- df_raw %>%
  select(Commune, matches("^earthquake_score_\\d{4}$")) %>%
  pivot_longer(matches("^earthquake_score_\\d{4}$"), names_to="Year",
               names_pattern="^earthquake_score_(\\d{4})$", values_to="earthquake_score") %>%
  mutate(Year=as.integer(Year),
         earthquake_score=as.numeric(ifelse(is.na(earthquake_score), 0, earthquake_score)),
         treated=as.integer(earthquake_score >= 1)) %>%
  filter(Year %in% years_window) %>%
  group_by(Commune) %>%
  summarise(first_eq_year=ifelse(any(treated==1), min(Year[treated==1]), 0L), .groups="drop")

panel <- panel_green %>%
  left_join(timing_tbl, by="Commune") %>%
  left_join(panel_repair, by=c("Commune","Year")) %>%
  left_join(df_raw %>% select(Commune, Population_2011) %>% rename(Size_i=Population_2011) %>%
              mutate(Size_i=as.numeric(Size_i)) %>% distinct(), by="Commune") %>%
  mutate(treated=first_eq_year > 0,
         event_time=ifelse(treated, Year - first_eq_year, NA_integer_),
         Year_Segment=interaction(Year, Segment, drop=TRUE),
         SizeBin=cut(Size_i, breaks=quantile(Size_i, probs=seq(0,1,0.25), na.rm=TRUE),
                     include.lowest=TRUE, labels=c("Q1_small","Q2","Q3","Q4_large"))) %>%
  filter(!is.na(repair_pc))

for (tau in taus_use) {
  sfx <- paste0(ifelse(tau < 0, "m", "p"), abs(tau))
  panel[[paste0("D_", sfx)]] <- as.integer(!is.na(panel$event_time) & panel$event_time == tau)
  panel[[paste0("Z_", sfx)]] <- panel$Tunnels_per_capita * panel[[paste0("D_", sfx)]]
}

panel_es <- filter(panel, is.na(event_time) | (event_time >= tau_min & event_time <= tau_max))
z_terms  <- paste0("Z_", ifelse(taus_use < 0, "m", "p"), abs(taus_use), collapse=" + ")

m_rep_base <- feols(as.formula(paste0("repair_pc ~ ", z_terms, " | Commune + Year")),
                    data=panel_es, cluster=~Commune)
m_rep_size <- feols(as.formula(paste0("repair_pc ~ ", z_terms, " | Commune + Year + SizeBin^Year")),
                    data=panel_es, cluster=~Commune)
m_gr_base  <- feols(as.formula(paste0("Green ~ ", z_terms, " | Commune + Year_Segment")),
                    data=panel_es, cluster=~Commune)
m_gr_size  <- feols(as.formula(paste0("Green ~ ", z_terms, " | Commune + Year_Segment + SizeBin^Year")),
                    data=panel_es, cluster=~Commune)

extract_coefs <- function(model, outcome, spec) {
  ct <- as.data.frame(coeftable(model)); ct$term <- rownames(ct)
  ct %>%
    filter(grepl("^Z_[mp]\\d+$", term)) %>%
    mutate(tau    = ifelse(grepl("^Z_m", term), -1L, 1L) * as.integer(gsub("^Z_[mp]", "", term)),
           outcome=outcome, spec=spec,
           ci_low =Estimate - 1.96*`Std. Error`,
           ci_high=Estimate + 1.96*`Std. Error`) %>%
    select(outcome, spec, tau, Estimate, ci_low, ci_high) %>%
    bind_rows(tibble(outcome=outcome, spec=spec, tau=tau_ref, Estimate=0, ci_low=0, ci_high=0)) %>%
    arrange(tau)
}

es_comp <- bind_rows(
  extract_coefs(m_rep_base, "repairs_per_capita", "Baseline"),
  extract_coefs(m_rep_size, "repairs_per_capita", "Add SizeBin×Year"),
  extract_coefs(m_gr_base,  "Green",              "Baseline"),
  extract_coefs(m_gr_size,  "Green",              "Add SizeBin×Year")
) %>% mutate(outcome=factor(outcome, levels=c("repairs_per_capita","Green")),
             spec=factor(spec, levels=c("Add SizeBin×Year","Baseline")))

p <- ggplot(es_comp, aes(x=tau, y=Estimate, shape=spec)) +
  geom_point(position=position_dodge(0.35), size=2.2) +
  geom_errorbar(aes(ymin=ci_low, ymax=ci_high),
                position=position_dodge(0.35), width=0.2) +
  geom_hline(yintercept=0, linetype="dashed") +
  facet_wrap(~outcome, scales="free_y") +
  scale_x_continuous(breaks=tau_min:tau_max) +
  labs(x="Event time t",
       y="Coefficient of tunnels_per_capita × 1{event_time = t} (95% CI)",
       shape=NULL) +
  theme_minimal(base_size=12)

print(p)



### Table Q6 ###

library(readxl)
library(dplyr)
library(tidyr)
library(did)
library(knitr)
library(kableExtra)

setwd("your path")
set.seed(123)

df <- read_excel("data.xlsx") %>%
  distinct() %>%
  mutate(Commune = paste(Prefecture, City, sep = "_"))

years_window <- 2012:2020

build_panel <- function(df, var, years_window) {
  df %>%
    select(Commune, matches(paste0("^", var, "_\\d{4}$"))) %>%
    pivot_longer(matches(paste0("^", var, "_\\d{4}$")), names_to="Year",
                 names_pattern=paste0("^", var, "_(\\d{4})$"), values_to=var) %>%
    mutate(Year=as.integer(Year), !!var := as.numeric(.data[[var]])) %>%
    filter(Year %in% years_window, !is.na(.data[[var]])) %>%
    { x <- .; filter(x, Commune %in% (x %>% distinct(Commune,Year) %>% count(Commune) %>%
                                        filter(n==length(years_window)) %>% pull(Commune))) }
}

eq_long <- df %>%
  select(Commune, matches("^earthquake_score_\\d{4}$")) %>%
  pivot_longer(matches("^earthquake_score_\\d{4}$"), names_to="Year",
               names_pattern="^earthquake_score_(\\d{4})$", values_to="earthquake_score") %>%
  mutate(Year=as.integer(Year),
         earthquake_score=replace_na(as.numeric(earthquake_score), 0)) %>%
  filter(Year %in% years_window)

run_cs <- function(outcome_panel, var, eq_long, thresh) {
  communes_keep <- unique(outcome_panel$Commune)
  timing_tbl <- eq_long %>%
    filter(Commune %in% communes_keep) %>%
    mutate(treated=as.integer(earthquake_score >= thresh)) %>%
    group_by(Commune) %>%
    summarise(G=ifelse(any(treated==1L), min(Year[treated==1L]), 0L), .groups="drop")
  
  panel <- outcome_panel %>%
    left_join(timing_tbl, by="Commune") %>%
    mutate(id=as.integer(factor(Commune))) %>%
    filter(G==0 | G > min(years_window)) %>% as.data.frame()
  
  att_cs <- tryCatch(
    att_gt(yname=var, tname="Year", idname="id", gname="G", data=panel,
           panel=TRUE, control_group="nevertreated", clustervars="Commune",
           xformla=~1, allow_unbalanced_panel=FALSE),
    error=function(e) NULL)
  if (is.null(att_cs)) return(NULL)
  
  ag <- tryCatch(aggte(att_cs, type="simple"), error=function(e) NULL)
  if (is.null(ag)) return(NULL)
  
  att <- ag$overall.att; se <- ag$overall.se
  pv  <- 2*(1-pnorm(abs(att/se)))
  tibble(
    Outcome   = var,
    Threshold = paste0("\u2265 ", thresh),
    ATT=round(att,3), SE=round(se,3),
    CI_low=round(att-1.96*se,3), CI_high=round(att+1.96*se,3),
    p_value=round(pv,3),
    Stars=case_when(pv<0.01~"***",pv<0.05~"**",pv<0.10~"*",TRUE~""),
    N_treated=sum(timing_tbl$G>0), N_control=sum(timing_tbl$G==0)
  )
}

outcomes <- list(Balance=build_panel(df,"Balance",years_window),
                 Debt   =build_panel(df,"Debt",   years_window))

results_all <- bind_rows(lapply(names(outcomes), function(ov)
  bind_rows(lapply(c(1,2,3), function(th) run_cs(outcomes[[ov]], ov, eq_long, th)))
))

tab <- results_all %>%
  mutate(
    Outcome   = recode(Outcome, Balance="Balance"),
    CI        = paste0("[", CI_low, ", ", CI_high, "]"),
    ATT_se    = paste0(ATT, Stars, " (", SE, ")")
  ) %>%
  select(Outcome, Threshold, ATT_se, CI, p_value, N_treated, N_control) %>%
  pivot_wider(names_from=Outcome,
              values_from=c(ATT_se, CI, p_value, N_treated, N_control)) %>%
  select(Threshold,
         ATT_se_Balance, CI_Balance, p_value_Balance, N_treated_Balance, N_control_Balance,
         ATT_se_Debt,    CI_Debt,    p_value_Debt,    N_treated_Debt,    N_control_Debt)

kable(tab, format="latex", booktabs=TRUE,
      col.names=c("Intensity",
                  "ATT (S.E.)","95\\% CI","$p$-value","Treated","Control",
                  "ATT (S.E.)","95\\% CI","$p$-value","Treated","Control"),
      escape=FALSE,
      caption="Callaway \\& Sant'Anna estimates by outcome and earthquake intensity threshold",
      label="tab:cs_results") %>%
  add_header_above(c(" "=1, "Balance"=5, "Debt"=5)) %>%
  kable_styling(latex_options="hold_position") %>%
  footnote(general="Stars: * $p<0.10$, ** $p<0.05$, *** $p<0.01$. Standard errors clustered at commune level.",
           escape=FALSE, general_title="")



### Table R7 ###

library(readxl)
library(dplyr)
library(tidyr)
library(did)

setwd("your path")
set.seed(123)

df <- read_excel("data.xlsx") %>%
  distinct() %>%
  mutate(
    Commune      = paste(Prefecture, City, sep = "_"),
    Balance_2011 = suppressWarnings(as.numeric(Balance_2011))
  )

sector_cols <- c(
  "Paper","Stationery","OfficeFurniture","MobilePhones","HomeAppliances",
  "AirConditioners","WaterHeaters","Lighting","Vehicles","FireExtinguishers",
  "Uniforms","InteriorBedding","WorkGloves","OtherTextiles","Equipment","DisasterSupplies"
)
years_window <- 2012:2020
T_years      <- length(years_window)

panel_green <- df %>%
  select(Commune, matches(paste0("^(", paste(sector_cols, collapse = "|"), ")_\\d{4}$"))) %>%
  pivot_longer(matches("_(19|20)\\d{2}$"), names_to = c("Segment", "Year"),
               names_pattern = "^(.*)_(\\d{4})$", values_to = "Green") %>%
  mutate(Year = as.integer(Year), Segment = factor(Segment, levels = sector_cols),
         Green = as.numeric(Green)) %>%
  filter(Year %in% years_window, !is.na(Green))

communes_keep <- panel_green %>%
  distinct(Commune, Segment, Year) %>% count(Commune) %>%
  filter(n == length(sector_cols) * T_years) %>% pull(Commune)
panel_green <- filter(panel_green, Commune %in% communes_keep)

panel_eq <- df %>%
  select(Commune, matches("^earthquake_score_\\d{4}$")) %>%
  pivot_longer(matches("^earthquake_score_\\d{4}$"), names_to = "Year",
               names_pattern = "^earthquake_score_(\\d{4})$", values_to = "earthquake_score") %>%
  mutate(Year = as.integer(Year),
         earthquake_score = replace_na(suppressWarnings(as.numeric(earthquake_score)), 0)) %>%
  filter(Year %in% years_window, Commune %in% communes_keep)

cut_med   <- median(df$Balance_2011, na.rm = TRUE)
city_size <- df %>%
  filter(Commune %in% communes_keep) %>%
  select(Commune, Balance_2011) %>% distinct() %>%
  mutate(big_city = as.integer(Balance_2011 >= cut_med)) %>%
  select(Commune, big_city)

run_att_sector <- function(dat, min_group_n = 5) {
  keep_G <- dat %>% filter(G > 0) %>% distinct(Commune, G) %>%
    count(G) %>% filter(n >= min_group_n) %>% pull(G)
  dat2 <- dat %>% filter(G == 0 | G %in% keep_G) %>%
    mutate(id_cs = as.integer(factor(Commune_Segment)))
  ag <- did::aggte(
    did::att_gt(yname = "Green", tname = "Year", idname = "id_cs", gname = "G",
                xformla = ~Year_Segment_fe, data = dat2, panel = TRUE,
                control_group = "nevertreated", bstrap = FALSE, est_method = "dr"),
    type = "simple")
  as.numeric(ag$overall.att)
}

count_used_units_sector <- function(dat, min_group_n = 5) {
  keep_G <- dat %>% filter(G > 0) %>% distinct(Commune, G) %>%
    count(G) %>% filter(n >= min_group_n) %>% pull(G)
  dat %>% filter(G == 0 | G %in% keep_G) %>%
    distinct(Commune, G) %>%
    summarise(n_control = sum(G == 0), n_treated = sum(G > 0), .groups = "drop")
}

thresholds   <- c(1, 2, 3)
B            <- 500
results_list <- list()

for (thresh in thresholds) {
  cat("\n=== Threshold >=", thresh, "===\n")
  
  timing_tbl <- panel_eq %>%
    mutate(treated = as.integer(earthquake_score >= thresh)) %>%
    group_by(Commune) %>%
    summarise(G = ifelse(any(treated == 1L), min(Year[treated == 1L]), 0L), .groups = "drop")
  
  panel_ddd <- panel_green %>%
    left_join(timing_tbl, by = "Commune") %>%
    left_join(city_size,  by = "Commune") %>%
    filter(!is.na(big_city), G == 0 | G > min(Year, na.rm = TRUE)) %>%
    mutate(Commune_Segment = paste0(Commune, "_", as.character(Segment)),
           Year_Segment_fe = factor(paste0(Year, "_", as.character(Segment))))
  
  att_small <- tryCatch(run_att_sector(filter(panel_ddd, big_city == 0)), error = function(e) NA_real_)
  att_big   <- tryCatch(run_att_sector(filter(panel_ddd, big_city == 1)), error = function(e) NA_real_)
  ddd_hat   <- att_big - att_small
  
  communes <- sort(unique(panel_ddd$Commune))
  nC       <- length(communes)
  boot_ddd <- vapply(seq_len(B), function(bb) {
    s  <- sample(communes, nC, replace = TRUE)
    bd <- bind_rows(lapply(seq_along(s), function(k)
      panel_ddd %>% filter(Commune == s[k]) %>%
        mutate(Commune         = paste0(s[k], "__", k),
               Commune_Segment = paste0(s[k], "__", k, "_", as.character(Segment)),
               Year_Segment_fe = factor(paste0(Year, "_", as.character(Segment))))))
    b_small <- tryCatch(run_att_sector(filter(bd, big_city == 0)), error = function(e) NA_real_)
    b_big   <- tryCatch(run_att_sector(filter(bd, big_city == 1)), error = function(e) NA_real_)
    b_big - b_small
  }, numeric(1))
  
  ci         <- quantile(boot_ddd, c(0.025, 0.975), na.rm = TRUE)
  size_small <- count_used_units_sector(filter(panel_ddd, big_city == 0))
  size_big   <- count_used_units_sector(filter(panel_ddd, big_city == 1))
  
  results_list[[thresh]] <- tibble(
    threshold       = thresh,
    DDD             = ddd_hat,
    se_boot         = sd(boot_ddd, na.rm = TRUE),
    ci_low          = ci[[1]],
    ci_high         = ci[[2]],
    n_control_small = size_small$n_control,
    n_treated_small = size_small$n_treated,
    n_control_big   = size_big$n_control,
    n_treated_big   = size_big$n_treated
  )
}

results_all <- bind_rows(results_list) %>%
  mutate(z     = DDD / se_boot,
         p     = 2 * (1 - pnorm(abs(z))),
         star  = case_when(p < 0.01 ~ "***", p < 0.05 ~ "**", p < 0.10 ~ "*", TRUE ~ ""),
         threshold_label = paste0("score \u2265 ", threshold))

print(results_all)


### Table S8 ###

library(readxl)
library(dplyr)
library(tidyr)
library(did)

setwd("your path")
set.seed(123)

df <- read_excel("data.xlsx") %>%
  distinct() %>%
  mutate(
    Commune   = paste(Prefecture, City, sep = "_"),
    Debt_2011 = suppressWarnings(as.numeric(Debt_2011))
  )

sector_cols <- c(
  "Paper","Stationery","OfficeFurniture","MobilePhones","HomeAppliances",
  "AirConditioners","WaterHeaters","Lighting","Vehicles","FireExtinguishers",
  "Uniforms","InteriorBedding","WorkGloves","OtherTextiles","Equipment","DisasterSupplies"
)
years_window <- 2012:2020
T_years      <- length(years_window)

panel_green <- df %>%
  select(Commune, matches(paste0("^(", paste(sector_cols, collapse = "|"), ")_\\d{4}$"))) %>%
  pivot_longer(matches("_(19|20)\\d{2}$"), names_to = c("Segment", "Year"),
               names_pattern = "^(.*)_(\\d{4})$", values_to = "Green") %>%
  mutate(Year = as.integer(Year), Segment = factor(Segment, levels = sector_cols),
         Green = as.numeric(Green)) %>%
  filter(Year %in% years_window, !is.na(Green))

communes_keep <- panel_green %>%
  distinct(Commune, Segment, Year) %>% count(Commune) %>%
  filter(n == length(sector_cols) * T_years) %>% pull(Commune)
panel_green <- filter(panel_green, Commune %in% communes_keep)

panel_eq <- df %>%
  select(Commune, matches("^earthquake_score_\\d{4}$")) %>%
  pivot_longer(matches("^earthquake_score_\\d{4}$"), names_to = "Year",
               names_pattern = "^earthquake_score_(\\d{4})$", values_to = "earthquake_score") %>%
  mutate(Year = as.integer(Year),
         earthquake_score = replace_na(suppressWarnings(as.numeric(earthquake_score)), 0)) %>%
  filter(Year %in% years_window, Commune %in% communes_keep)

cut_med   <- median(df$Debt_2011, na.rm = TRUE)
city_size <- df %>%
  filter(Commune %in% communes_keep) %>%
  select(Commune, Debt_2011) %>% distinct() %>%
  mutate(big_city = as.integer(Debt_2011 >= cut_med)) %>%
  select(Commune, big_city)

run_att_sector <- function(dat, min_group_n = 5) {
  keep_G <- dat %>% filter(G > 0) %>% distinct(Commune, G) %>%
    count(G) %>% filter(n >= min_group_n) %>% pull(G)
  dat2 <- dat %>% filter(G == 0 | G %in% keep_G) %>%
    mutate(id_cs = as.integer(factor(Commune_Segment)))
  ag <- did::aggte(
    did::att_gt(yname = "Green", tname = "Year", idname = "id_cs", gname = "G",
                xformla = ~Year_Segment_fe, data = dat2, panel = TRUE,
                control_group = "nevertreated", bstrap = FALSE, est_method = "dr"),
    type = "simple")
  as.numeric(ag$overall.att)
}

count_used_units_sector <- function(dat, min_group_n = 5) {
  keep_G <- dat %>% filter(G > 0) %>% distinct(Commune, G) %>%
    count(G) %>% filter(n >= min_group_n) %>% pull(G)
  dat %>% filter(G == 0 | G %in% keep_G) %>%
    distinct(Commune, G) %>%
    summarise(n_control = sum(G == 0), n_treated = sum(G > 0), .groups = "drop")
}

thresholds   <- c(1, 2, 3)
B            <- 500
results_list <- list()

for (thresh in thresholds) {
  cat("\n=== Threshold >=", thresh, "===\n")
  
  timing_tbl <- panel_eq %>%
    mutate(treated = as.integer(earthquake_score >= thresh)) %>%
    group_by(Commune) %>%
    summarise(G = ifelse(any(treated == 1L), min(Year[treated == 1L]), 0L), .groups = "drop")
  
  panel_ddd <- panel_green %>%
    left_join(timing_tbl, by = "Commune") %>%
    left_join(city_size,  by = "Commune") %>%
    filter(!is.na(big_city), G == 0 | G > min(Year, na.rm = TRUE)) %>%
    mutate(Commune_Segment = paste0(Commune, "_", as.character(Segment)),
           Year_Segment_fe = factor(paste0(Year, "_", as.character(Segment))))
  
  att_small <- tryCatch(run_att_sector(filter(panel_ddd, big_city == 0)), error = function(e) NA_real_)
  att_big   <- tryCatch(run_att_sector(filter(panel_ddd, big_city == 1)), error = function(e) NA_real_)
  ddd_hat   <- att_big - att_small
  
  communes <- sort(unique(panel_ddd$Commune))
  nC       <- length(communes)
  boot_ddd <- vapply(seq_len(B), function(bb) {
    s  <- sample(communes, nC, replace = TRUE)
    bd <- bind_rows(lapply(seq_along(s), function(k)
      panel_ddd %>% filter(Commune == s[k]) %>%
        mutate(Commune         = paste0(s[k], "__", k),
               Commune_Segment = paste0(s[k], "__", k, "_", as.character(Segment)),
               Year_Segment_fe = factor(paste0(Year, "_", as.character(Segment))))))
    b_small <- tryCatch(run_att_sector(filter(bd, big_city == 0)), error = function(e) NA_real_)
    b_big   <- tryCatch(run_att_sector(filter(bd, big_city == 1)), error = function(e) NA_real_)
    b_big - b_small
  }, numeric(1))
  
  ci         <- quantile(boot_ddd, c(0.025, 0.975), na.rm = TRUE)
  size_small <- count_used_units_sector(filter(panel_ddd, big_city == 0))
  size_big   <- count_used_units_sector(filter(panel_ddd, big_city == 1))
  
  results_list[[thresh]] <- tibble(
    threshold       = thresh,
    DDD             = ddd_hat,
    se_boot         = sd(boot_ddd, na.rm = TRUE),
    ci_low          = ci[[1]],
    ci_high         = ci[[2]],
    n_control_small = size_small$n_control,
    n_treated_small = size_small$n_treated,
    n_control_big   = size_big$n_control,
    n_treated_big   = size_big$n_treated
  )
}

results_all <- bind_rows(results_list) %>%
  mutate(z     = DDD / se_boot,
         p     = 2 * (1 - pnorm(abs(z))),
         star  = case_when(p < 0.01 ~ "***", p < 0.05 ~ "**", p < 0.10 ~ "*", TRUE ~ ""),
         threshold_label = paste0("score \u2265 ", threshold))

print(results_all)


### Table T9 ###

library(readxl)
library(dplyr)
library(tidyr)
library(did)

setwd("your path")
set.seed(123)

df <- read_excel("data.xlsx") %>%
  distinct() %>%
  mutate(
    Commune         = paste(Prefecture, City, sep = "_"),
    Population_2011 = suppressWarnings(as.numeric(Population_2011))
  )

sector_cols <- c(
  "Paper","Stationery","OfficeFurniture","MobilePhones","HomeAppliances",
  "AirConditioners","WaterHeaters","Lighting","Vehicles","FireExtinguishers",
  "Uniforms","InteriorBedding","WorkGloves","OtherTextiles","Equipment","DisasterSupplies"
)
years_window <- 2012:2020
T_years      <- length(years_window)

panel_green <- df %>%
  select(Commune, matches(paste0("^(", paste(sector_cols, collapse = "|"), ")_\\d{4}$"))) %>%
  pivot_longer(matches("_(19|20)\\d{2}$"), names_to = c("Segment", "Year"),
               names_pattern = "^(.*)_(\\d{4})$", values_to = "Green") %>%
  mutate(Year = as.integer(Year), Segment = factor(Segment, levels = sector_cols),
         Green = as.numeric(Green)) %>%
  filter(Year %in% years_window, !is.na(Green))

communes_keep <- panel_green %>%
  distinct(Commune, Segment, Year) %>% count(Commune) %>%
  filter(n == length(sector_cols) * T_years) %>% pull(Commune)
panel_green <- filter(panel_green, Commune %in% communes_keep)

panel_eq <- df %>%
  select(Commune, matches("^earthquake_score_\\d{4}$")) %>%
  pivot_longer(matches("^earthquake_score_\\d{4}$"), names_to = "Year",
               names_pattern = "^earthquake_score_(\\d{4})$", values_to = "earthquake_score") %>%
  mutate(Year = as.integer(Year),
         earthquake_score = replace_na(suppressWarnings(as.numeric(earthquake_score)), 0)) %>%
  filter(Year %in% years_window, Commune %in% communes_keep)

cut_med   <- median(df$Population_2011, na.rm = TRUE)
city_size <- df %>%
  filter(Commune %in% communes_keep) %>%
  select(Commune, Population_2011) %>% distinct() %>%
  mutate(big_city = as.integer(Population_2011 >= cut_med)) %>%
  select(Commune, big_city)

run_att_sector <- function(dat, min_group_n = 5) {
  keep_G <- dat %>% filter(G > 0) %>% distinct(Commune, G) %>%
    count(G) %>% filter(n >= min_group_n) %>% pull(G)
  dat2 <- dat %>% filter(G == 0 | G %in% keep_G) %>%
    mutate(id_cs = as.integer(factor(Commune_Segment)))
  ag <- did::aggte(
    did::att_gt(yname = "Green", tname = "Year", idname = "id_cs", gname = "G",
                xformla = ~Year_Segment_fe, data = dat2, panel = TRUE,
                control_group = "nevertreated", bstrap = FALSE, est_method = "dr"),
    type = "simple")
  as.numeric(ag$overall.att)
}

count_used_units_sector <- function(dat, min_group_n = 5) {
  keep_G <- dat %>% filter(G > 0) %>% distinct(Commune, G) %>%
    count(G) %>% filter(n >= min_group_n) %>% pull(G)
  dat %>% filter(G == 0 | G %in% keep_G) %>%
    distinct(Commune, G) %>%
    summarise(n_control = sum(G == 0), n_treated = sum(G > 0), .groups = "drop")
}

thresholds   <- c(1, 2, 3)
B            <- 500
results_list <- list()

for (thresh in thresholds) {
  cat("\n=== Threshold >=", thresh, "===\n")
  
  timing_tbl <- panel_eq %>%
    mutate(treated = as.integer(earthquake_score >= thresh)) %>%
    group_by(Commune) %>%
    summarise(G = ifelse(any(treated == 1L), min(Year[treated == 1L]), 0L), .groups = "drop")
  
  panel_ddd <- panel_green %>%
    left_join(timing_tbl, by = "Commune") %>%
    left_join(city_size,  by = "Commune") %>%
    filter(!is.na(big_city), G == 0 | G > min(Year, na.rm = TRUE)) %>%
    mutate(Commune_Segment = paste0(Commune, "_", as.character(Segment)),
           Year_Segment_fe = factor(paste0(Year, "_", as.character(Segment))))
  
  att_small <- tryCatch(run_att_sector(filter(panel_ddd, big_city == 0)), error = function(e) NA_real_)
  att_big   <- tryCatch(run_att_sector(filter(panel_ddd, big_city == 1)), error = function(e) NA_real_)
  ddd_hat   <- att_big - att_small
  
  communes <- sort(unique(panel_ddd$Commune))
  nC       <- length(communes)
  boot_ddd <- vapply(seq_len(B), function(bb) {
    s  <- sample(communes, nC, replace = TRUE)
    bd <- bind_rows(lapply(seq_along(s), function(k)
      panel_ddd %>% filter(Commune == s[k]) %>%
        mutate(Commune         = paste0(s[k], "__", k),
               Commune_Segment = paste0(s[k], "__", k, "_", as.character(Segment)),
               Year_Segment_fe = factor(paste0(Year, "_", as.character(Segment))))))
    b_small <- tryCatch(run_att_sector(filter(bd, big_city == 0)), error = function(e) NA_real_)
    b_big   <- tryCatch(run_att_sector(filter(bd, big_city == 1)), error = function(e) NA_real_)
    b_big - b_small
  }, numeric(1))
  
  ci         <- quantile(boot_ddd, c(0.025, 0.975), na.rm = TRUE)
  size_small <- count_used_units_sector(filter(panel_ddd, big_city == 0))
  size_big   <- count_used_units_sector(filter(panel_ddd, big_city == 1))
  
  results_list[[thresh]] <- tibble(
    threshold       = thresh,
    DDD             = ddd_hat,
    se_boot         = sd(boot_ddd, na.rm = TRUE),
    ci_low          = ci[[1]],
    ci_high         = ci[[2]],
    n_control_small = size_small$n_control,
    n_treated_small = size_small$n_treated,
    n_control_big   = size_big$n_control,
    n_treated_big   = size_big$n_treated
  )
}

results_all <- bind_rows(results_list) %>%
  mutate(z     = DDD / se_boot,
         p     = 2 * (1 - pnorm(abs(z))),
         star  = case_when(p < 0.01 ~ "***", p < 0.05 ~ "**", p < 0.10 ~ "*", TRUE ~ ""),
         threshold_label = paste0("score \u2265 ", threshold))

print(results_all)



### Table U10 ###

library(readxl)
library(dplyr)
library(tidyr)
library(did)

setwd("your path")
set.seed(123)

df <- read_excel("data.xlsx") %>%
  distinct() %>%
  mutate(
    Commune         = paste(Prefecture, City, sep = "_"),
    Staff_per_capita_2011 = suppressWarnings(as.numeric(Staff_per_capita_2011))
  )

sector_cols <- c(
  "Paper","Stationery","OfficeFurniture","MobilePhones","HomeAppliances",
  "AirConditioners","WaterHeaters","Lighting","Vehicles","FireExtinguishers",
  "Uniforms","InteriorBedding","WorkGloves","OtherTextiles","Equipment","DisasterSupplies"
)
years_window <- 2012:2020
T_years      <- length(years_window)

panel_green <- df %>%
  select(Commune, matches(paste0("^(", paste(sector_cols, collapse = "|"), ")_\\d{4}$"))) %>%
  pivot_longer(matches("_(19|20)\\d{2}$"), names_to = c("Segment", "Year"),
               names_pattern = "^(.*)_(\\d{4})$", values_to = "Green") %>%
  mutate(Year = as.integer(Year), Segment = factor(Segment, levels = sector_cols),
         Green = as.numeric(Green)) %>%
  filter(Year %in% years_window, !is.na(Green))

communes_keep <- panel_green %>%
  distinct(Commune, Segment, Year) %>% count(Commune) %>%
  filter(n == length(sector_cols) * T_years) %>% pull(Commune)
panel_green <- filter(panel_green, Commune %in% communes_keep)

panel_eq <- df %>%
  select(Commune, matches("^earthquake_score_\\d{4}$")) %>%
  pivot_longer(matches("^earthquake_score_\\d{4}$"), names_to = "Year",
               names_pattern = "^earthquake_score_(\\d{4})$", values_to = "earthquake_score") %>%
  mutate(Year = as.integer(Year),
         earthquake_score = replace_na(suppressWarnings(as.numeric(earthquake_score)), 0)) %>%
  filter(Year %in% years_window, Commune %in% communes_keep)

cut_med   <- median(df$Staff_per_capita_2011, na.rm = TRUE)
city_size <- df %>%
  filter(Commune %in% communes_keep) %>%
  select(Commune, Staff_per_capita_2011) %>% distinct() %>%
  mutate(big_city = as.integer(Staff_per_capita_2011 >= cut_med)) %>%
  select(Commune, big_city)

run_att_sector <- function(dat, min_group_n = 5) {
  keep_G <- dat %>% filter(G > 0) %>% distinct(Commune, G) %>%
    count(G) %>% filter(n >= min_group_n) %>% pull(G)
  dat2 <- dat %>% filter(G == 0 | G %in% keep_G) %>%
    mutate(id_cs = as.integer(factor(Commune_Segment)))
  ag <- did::aggte(
    did::att_gt(yname = "Green", tname = "Year", idname = "id_cs", gname = "G",
                xformla = ~Year_Segment_fe, data = dat2, panel = TRUE,
                control_group = "nevertreated", bstrap = FALSE, est_method = "dr"),
    type = "simple")
  as.numeric(ag$overall.att)
}

count_used_units_sector <- function(dat, min_group_n = 5) {
  keep_G <- dat %>% filter(G > 0) %>% distinct(Commune, G) %>%
    count(G) %>% filter(n >= min_group_n) %>% pull(G)
  dat %>% filter(G == 0 | G %in% keep_G) %>%
    distinct(Commune, G) %>%
    summarise(n_control = sum(G == 0), n_treated = sum(G > 0), .groups = "drop")
}

thresholds   <- c(1, 2, 3)
B            <- 500
results_list <- list()

for (thresh in thresholds) {
  cat("\n=== Threshold >=", thresh, "===\n")
  
  timing_tbl <- panel_eq %>%
    mutate(treated = as.integer(earthquake_score >= thresh)) %>%
    group_by(Commune) %>%
    summarise(G = ifelse(any(treated == 1L), min(Year[treated == 1L]), 0L), .groups = "drop")
  
  panel_ddd <- panel_green %>%
    left_join(timing_tbl, by = "Commune") %>%
    left_join(city_size,  by = "Commune") %>%
    filter(!is.na(big_city), G == 0 | G > min(Year, na.rm = TRUE)) %>%
    mutate(Commune_Segment = paste0(Commune, "_", as.character(Segment)),
           Year_Segment_fe = factor(paste0(Year, "_", as.character(Segment))))
  
  att_small <- tryCatch(run_att_sector(filter(panel_ddd, big_city == 0)), error = function(e) NA_real_)
  att_big   <- tryCatch(run_att_sector(filter(panel_ddd, big_city == 1)), error = function(e) NA_real_)
  ddd_hat   <- att_big - att_small
  
  communes <- sort(unique(panel_ddd$Commune))
  nC       <- length(communes)
  boot_ddd <- vapply(seq_len(B), function(bb) {
    s  <- sample(communes, nC, replace = TRUE)
    bd <- bind_rows(lapply(seq_along(s), function(k)
      panel_ddd %>% filter(Commune == s[k]) %>%
        mutate(Commune         = paste0(s[k], "__", k),
               Commune_Segment = paste0(s[k], "__", k, "_", as.character(Segment)),
               Year_Segment_fe = factor(paste0(Year, "_", as.character(Segment))))))
    b_small <- tryCatch(run_att_sector(filter(bd, big_city == 0)), error = function(e) NA_real_)
    b_big   <- tryCatch(run_att_sector(filter(bd, big_city == 1)), error = function(e) NA_real_)
    b_big - b_small
  }, numeric(1))
  
  ci         <- quantile(boot_ddd, c(0.025, 0.975), na.rm = TRUE)
  size_small <- count_used_units_sector(filter(panel_ddd, big_city == 0))
  size_big   <- count_used_units_sector(filter(panel_ddd, big_city == 1))
  
  results_list[[thresh]] <- tibble(
    threshold       = thresh,
    DDD             = ddd_hat,
    se_boot         = sd(boot_ddd, na.rm = TRUE),
    ci_low          = ci[[1]],
    ci_high         = ci[[2]],
    n_control_small = size_small$n_control,
    n_treated_small = size_small$n_treated,
    n_control_big   = size_big$n_control,
    n_treated_big   = size_big$n_treated
  )
}

results_all <- bind_rows(results_list) %>%
  mutate(z     = DDD / se_boot,
         p     = 2 * (1 - pnorm(abs(z))),
         star  = case_when(p < 0.01 ~ "***", p < 0.05 ~ "**", p < 0.10 ~ "*", TRUE ~ ""),
         threshold_label = paste0("score \u2265 ", threshold))

print(results_all)



### Tables V11 and V12 ###

library(readxl)
library(dplyr)
library(tidyr)
library(did)
library(fixest)

setwd("your path")
set.seed(123)

sector_cols  <- c("Paper","Stationery","OfficeFurniture","MobilePhones","HomeAppliances",
                  "AirConditioners","WaterHeaters","Lighting","Vehicles","FireExtinguishers",
                  "Uniforms","InteriorBedding","WorkGloves","OtherTextiles","Equipment","DisasterSupplies")
years_window <- 2012:2020
T_years      <- length(years_window)
radii_use    <- c(10, 30, 50, 70, 90)
score_thr    <- 1L

df_raw <- read_excel("data.xlsx") %>%
  distinct() %>%
  mutate(Commune = paste(Prefecture, City, sep = "_"))

panel_green_all <- df_raw %>%
  select(Prefecture, City, Commune,
         matches(paste0("^(", paste(sector_cols, collapse="|"), ")_\\d{4}$"))) %>%
  pivot_longer(matches("_(19|20)\\d{2}$"), names_to=c("Segment","Year"),
               names_pattern="^(.*)_(\\d{4})$", values_to="Green") %>%
  mutate(Year=as.integer(Year), Segment=factor(Segment, levels=sector_cols),
         Green=as.numeric(Green)) %>%
  filter(Year %in% years_window)

communes_keep <- panel_green_all %>%
  group_by(Commune) %>%
  summarise(n_nonmiss=sum(!is.na(Green)), .groups="drop") %>%
  filter(n_nonmiss == length(sector_cols) * T_years) %>% pull(Commune)

panel_green <- filter(panel_green_all, Commune %in% communes_keep, !is.na(Green))
df_bal      <- filter(df_raw, Commune %in% communes_keep)

run_radius <- function(rk) {
  message("\n=== Rayon ", rk, "km ===")
  
  # --- Binary ---
  panel_nbmax <- df_bal %>%
    select(Prefecture, City, Commune,
           matches(paste0("^earthquake_max_", rk, "km_score_\\d{4}$"))) %>%
    pivot_longer(matches(paste0("^earthquake_max_", rk, "km_score_\\d{4}$")),
                 names_to="Year",
                 names_pattern=paste0("^earthquake_max_", rk, "km_score_(\\d{4})$"),
                 values_to="nb_max_score") %>%
    mutate(Year=as.integer(Year),
           nb_max_score=replace_na(as.numeric(nb_max_score), 0),
           treat=as.integer(nb_max_score >= score_thr)) %>%
    filter(Year %in% years_window)
  
  timing <- panel_nbmax %>%
    group_by(Commune) %>%
    summarise(G=as.integer(ifelse(any(treat==1L), min(Year[treat==1L]), 0L)),
              .groups="drop")
  
  panel_s <- panel_green %>%
    left_join(panel_nbmax %>% select(Prefecture,City,Commune,Year,nb_max_score,treat),
              by=c("Prefecture","City","Commune","Year")) %>%
    left_join(timing, by="Commune") %>%
    mutate(id_cs=as.integer(factor(interaction(Commune, Segment, drop=TRUE))),
           Year_Segment_fe=factor(interaction(Year, Segment, drop=TRUE))) %>%
    as.data.frame()
  
  commG     <- distinct(panel_s, Commune, G)
  n_treated <- sum(commG$G > 0); n_control <- sum(commG$G == 0)
  
  att_out <- list(att=NA_real_, se=NA_real_)
  if (n_treated > 0 && n_control > 0) {
    fit <- tryCatch(
      aggte(att_gt(yname="Green", tname="Year", idname="id_cs", gname="G",
                   data=panel_s, panel=TRUE, control_group="nevertreated",
                   clustervars="Commune", xformla=~Year_Segment_fe),
            type="simple", na.rm=TRUE),
      error=function(e) NULL)
    if (!is.null(fit)) att_out <- list(att=fit$overall.att, se=fit$overall.se)
  }
  
  # --- Count ---
  cnt_pat   <- paste0("^earthquake_n_", rk, "km_ge6minus_\\d{4}$")
  count_out <- list(beta=NA_real_, se=NA_real_, p=NA_real_, n_obs=NA_integer_)
  if (any(grepl(cnt_pat, names(df_bal)))) {
    panel_cnt <- df_bal %>%
      select(Prefecture, City, Commune, matches(cnt_pat)) %>%
      pivot_longer(matches(cnt_pat), names_to="Year",
                   names_pattern=paste0("^earthquake_n_", rk, "km_ge6minus_(\\d{4})$"),
                   values_to="n_neighbors_ge6m") %>%
      mutate(Year=as.integer(Year),
             n_neighbors_ge6m=replace_na(as.numeric(n_neighbors_ge6m), 0)) %>%
      filter(Year %in% years_window) %>%
      left_join(panel_green, ., by=c("Prefecture","City","Commune","Year")) %>%
      mutate(Commune_Segment=interaction(Commune,Segment,drop=TRUE),
             Year_Segment=interaction(Year,Segment,drop=TRUE))
    
    fit_cnt <- tryCatch(
      feols(Green ~ n_neighbors_ge6m | Commune_Segment + Year_Segment,
            cluster=~Commune, data=panel_cnt),
      error=function(e) NULL)
    if (!is.null(fit_cnt)) {
      ct <- summary(fit_cnt)$coeftable["n_neighbors_ge6m",]
      count_out <- list(beta=ct["Estimate"], se=ct["Std. Error"],
                        p=ct["Pr(>|t|)"], n_obs=as.integer(nobs(fit_cnt)))
    }
  }
  
  tibble(radius_km=rk,
         att_binary=att_out$att, se_binary=att_out$se,
         p_binary=2*(1-pnorm(abs(att_out$att/att_out$se))),
         n_obs_att=nrow(panel_s), n_treated=n_treated, n_control=n_control,
         beta_count=count_out$beta, se_count=count_out$se,
         p_count=count_out$p, n_obs_cnt=count_out$n_obs)
}

results <- bind_rows(lapply(radii_use, run_radius))
print(results)

fmt  <- function(x, d=4) ifelse(is.na(x), "", formatC(x, format="f", digits=d))
fmti <- function(x) ifelse(is.na(x), "", as.character(x))
df2  <- results %>% mutate(across(c(att_binary,se_binary,beta_count,se_count), fmt),
                           across(c(p_binary,p_count), ~fmt(.x, d=3)),
                           across(c(n_obs_att,n_treated,n_control,n_obs_cnt), fmti))
cat(
  "\\begin{tabular}{lccccccccccc}\n\\hline\n",
  "Radius & ATT & SE & p & $N$ (ATT) & Treated & Control & $\\beta$ (count) & SE & p & $N$ (count) \\\\\n\\hline\n",
  paste(apply(df2, 1, function(r) paste(r[c("radius_km","att_binary","se_binary","p_binary",
                                            "n_obs_att","n_treated","n_control","beta_count","se_count","p_count","n_obs_cnt")],
                                        collapse=" & ")), collapse=" \\\\\n"),
  "\n\\hline\n\\end{tabular}\n"
)



### Figure W22 ###

library(readxl)
library(dplyr)
library(tidyr)
library(did)
library(ggplot2)

setwd("your path")
set.seed(123)

sector_cols  <- c("Paper","Stationery","OfficeFurniture","MobilePhones","HomeAppliances",
                  "AirConditioners","WaterHeaters","Lighting","Vehicles","FireExtinguishers",
                  "Uniforms","InteriorBedding","WorkGloves","OtherTextiles","Equipment","DisasterSupplies")
years_window <- 2012:2020
T_years      <- length(years_window)

df <- read_excel("data.xlsx") %>%
  distinct() %>% mutate(Commune = paste(Prefecture, City, sep = "_"))

panel_green <- df %>%
  select(Prefecture, City, Commune,
         matches(paste0("^(", paste(sector_cols, collapse="|"), ")_\\d{4}$"))) %>%
  pivot_longer(matches("_(19|20)\\d{2}$"), names_to=c("Segment","Year"),
               names_pattern="^(.*)_(\\d{4})$", values_to="Green") %>%
  mutate(Year=as.integer(Year), Segment=factor(Segment, levels=sector_cols),
         Green=as.numeric(Green)) %>%
  filter(Year %in% years_window, !is.na(Green))

communes_keep <- panel_green %>%
  distinct(Commune, Segment, Year) %>% count(Commune, name="n_cells") %>%
  filter(n_cells == length(sector_cols) * T_years) %>% pull(Commune)
panel_green <- filter(panel_green, Commune %in% communes_keep)

timing_tbl <- df %>%
  select(Prefecture, City, Commune, matches("^earthquake_score_\\d{4}$")) %>%
  pivot_longer(matches("^earthquake_score_\\d{4}$"), names_to="Year",
               names_pattern="^earthquake_score_(\\d{4})$", values_to="earthquake_score") %>%
  mutate(Year=as.integer(Year),
         earthquake_score=replace_na(as.numeric(earthquake_score), 0),
         earthquake_5p=as.integer(earthquake_score >= 1)) %>%
  filter(Year %in% years_window, Commune %in% communes_keep) %>%
  group_by(Commune) %>%
  summarise(G=as.integer(ifelse(any(earthquake_5p==1L), min(Year[earthquake_5p==1L]), 0L)),
            .groups="drop")

panel_placebo <- panel_green %>%
  left_join(timing_tbl, by="Commune") %>%
  mutate(
    G_placebo = case_when(G == 0L ~ 0L, G >= 2014L ~ G - 2L, TRUE ~ NA_integer_),
    Commune_Segment = interaction(Commune, Segment, drop=TRUE),
    Year_Segment_fe = factor(interaction(Year, Segment, drop=TRUE))
  ) %>%
  filter(!is.na(G_placebo)) %>%
  mutate(id_cs_p = as.integer(factor(Commune_Segment))) %>%
  as.data.frame()

att_placebo <- att_gt(yname="Green", tname="Year", idname="id_cs_p", gname="G_placebo",
                      data=panel_placebo, panel=TRUE, control_group="nevertreated",
                      clustervars="Commune", xformla=~Year_Segment_fe)

es_placebo  <- aggte(att_placebo, type="dynamic", min_e=-4, max_e=5)
summary(aggte(att_placebo, type="simple"))

plot_placebo <- data.frame(event_time=es_placebo$egt, att=es_placebo$att.egt,
                           se=es_placebo$se.egt, crit=es_placebo$crit.val.egt) %>%
  mutate(ci_low=att - crit*se, ci_high=att + crit*se)

ggplot(plot_placebo, aes(x=event_time, y=att)) +
  geom_hline(yintercept=0, linetype="dashed") +
  geom_errorbar(aes(ymin=ci_low, ymax=ci_high), width=0.2, linewidth=0.8) +
  geom_point(size=3) +
  scale_x_continuous(breaks=sort(unique(plot_placebo$event_time))) +
  labs(x="Event time (years relative to anticipated treatment = t \u2212 2)",
       y="ATT on Green (95% CI)") +
  theme_minimal(base_size=15)



### Figure X23 ###

library(readxl)
library(dplyr)
library(tidyr)
library(did)
library(ggplot2)

setwd("your path")
set.seed(123)

sector_cols  <- c("Paper","Stationery","OfficeFurniture","MobilePhones","HomeAppliances",
                  "AirConditioners","WaterHeaters","Lighting","Vehicles","FireExtinguishers",
                  "Uniforms","InteriorBedding","WorkGloves","OtherTextiles","Equipment","DisasterSupplies")
years_window <- 2012:2020
T_years      <- length(years_window)
B            <- 500
event_grid   <- -5:5

df <- read_excel("data.xlsx") %>%
  distinct() %>% mutate(Commune = paste(Prefecture, City, sep = "_"))

panel_green <- df %>%
  select(Prefecture, City, Commune,
         matches(paste0("^(", paste(sector_cols, collapse="|"), ")_\\d{4}$"))) %>%
  pivot_longer(matches("_(19|20)\\d{2}$"), names_to=c("Segment","Year"),
               names_pattern="^(.*)_(\\d{4})$", values_to="Green") %>%
  mutate(Year=as.integer(Year), Segment=factor(Segment, levels=sector_cols),
         Green=as.numeric(Green)) %>%
  filter(Year %in% years_window, !is.na(Green))

communes_keep <- panel_green %>%
  distinct(Commune, Segment, Year) %>% count(Commune, name="n_cells") %>%
  filter(n_cells == length(sector_cols) * T_years) %>% pull(Commune)
panel_green <- filter(panel_green, Commune %in% communes_keep)

timing_tbl <- df %>%
  select(Prefecture, City, Commune, matches("^earthquake_score_\\d{4}$")) %>%
  pivot_longer(matches("^earthquake_score_\\d{4}$"), names_to="Year",
               names_pattern="^earthquake_score_(\\d{4})$", values_to="earthquake_score") %>%
  mutate(Year=as.integer(Year),
         earthquake_score=replace_na(as.numeric(earthquake_score), 0),
         earthquake_5p=as.integer(earthquake_score >= 1)) %>%
  filter(Year %in% years_window, Commune %in% communes_keep) %>%
  group_by(Commune) %>%
  summarise(G=as.integer(ifelse(any(earthquake_5p==1L), min(Year[earthquake_5p==1L]), 0L)),
            .groups="drop")

panel <- panel_green %>%
  left_join(timing_tbl, by="Commune") %>%
  mutate(id_cs=as.integer(factor(interaction(Commune, Segment, drop=TRUE))),
         Year_Segment_fe=factor(interaction(Year, Segment, drop=TRUE))) %>%
  as.data.frame()

communes_info     <- distinct(panel, Commune, G) %>% mutate(ever_treated=(G > 0))
n_treated         <- sum(communes_info$ever_treated)
n_never           <- sum(!communes_info$ever_treated)
real_treat_years  <- sort(unique(communes_info$G[communes_info$G > 0]))

mc_overall <- data.frame(rep=seq_len(B), overall_att=NA_real_)
mc_event   <- vector("list", B)

set.seed(123)
for (b in seq_len(B)) {
  res <- tryCatch({
    pan_b <- panel %>%
      left_join(
        communes_info %>%
          mutate(G = ifelse(sample(c(rep(TRUE,n_treated),rep(FALSE,n_never))),
                            sample(real_treat_years, n(), replace=TRUE), 0L)) %>%
          select(Commune, G),
        by="Commune", suffix=c("_old","")
      ) %>% select(-G_old) %>% as.data.frame()
    ag_s <- aggte(att_gt(yname="Green", tname="Year", idname="id_cs", gname="G",
                         data=pan_b, panel=TRUE, control_group="nevertreated",
                         clustervars="Commune", xformla=~Year_Segment_fe),
                  type="simple")
    ag_d <- aggte(att_gt(yname="Green", tname="Year", idname="id_cs", gname="G",
                         data=pan_b, panel=TRUE, control_group="nevertreated",
                         clustervars="Commune", xformla=~Year_Segment_fe),
                  type="dynamic", min_e=-5, max_e=5)
    list(ok=TRUE, att=ag_s$overall.att,
         es=data.frame(event_time=ag_d$egt, att=ag_d$att.egt))
  }, error=function(e) list(ok=FALSE))
  if (!res$ok) next
  mc_overall$overall_att[b] <- res$att
  mc_event[[b]] <- right_join(mutate(res$es, rep=b),
                              data.frame(event_time=event_grid), by="event_time") %>%
    mutate(rep=b)
}

mc_es <- bind_rows(mc_event) %>%
  group_by(event_time) %>%
  summarise(mean_att=mean(att,na.rm=TRUE),
            q025=quantile(att,.025,na.rm=TRUE), q975=quantile(att,.975,na.rm=TRUE),
            .groups="drop")

ggplot(mc_es, aes(x=event_time, y=mean_att)) +
  geom_ribbon(aes(ymin=q025, ymax=q975), alpha=0.2) +
  geom_line(linewidth=0.9) + geom_point(size=2.6) +
  geom_hline(yintercept=0, linetype="dashed") +
  geom_vline(xintercept=0, linetype="dashed") +
  scale_x_continuous(breaks=event_grid) +
  labs(x="Event time (years relative to random treatment)",
       y="Mean ATT on Green (shaded: 95% CI)") +
  theme_minimal(base_size=14)



### Table Y13 and Figure Y24 ###

library(readxl)
library(dplyr)
library(tidyr)
library(did)
library(ggplot2)

setwd("your path")
set.seed(123)

sector_cols  <- c("Paper","Stationery","OfficeFurniture","MobilePhones","HomeAppliances",
                  "AirConditioners","WaterHeaters","Lighting","Vehicles","FireExtinguishers",
                  "Uniforms","InteriorBedding","WorkGloves","OtherTextiles","Equipment","DisasterSupplies")
years_window <- 2012:2020
T_years      <- length(years_window)
EQ_THRESHOLD <- 1

df <- read_excel("data.xlsx") %>%
  distinct() %>% mutate(Commune = paste(Prefecture, City, sep = "_"))

build_panel <- function(df, censor_2011=FALSE, censor_second=FALSE) {
  panel_green <- df %>%
    select(Prefecture, City, Commune,
           matches(paste0("^(", paste(sector_cols, collapse="|"), ")_\\d{4}$"))) %>%
    pivot_longer(matches("_(19|20)\\d{2}$"), names_to=c("Segment","Year"),
                 names_pattern="^(.*)_(\\d{4})$", values_to="Green") %>%
    mutate(Year=as.integer(Year), Segment=factor(Segment, levels=sector_cols),
           Green=as.numeric(Green)) %>%
    filter(Year %in% years_window, !is.na(Green))
  
  communes_keep <- panel_green %>%
    distinct(Commune, Segment, Year) %>% count(Commune, name="n_cells") %>%
    filter(n_cells == length(sector_cols) * T_years) %>% pull(Commune)
  panel_green <- filter(panel_green, Commune %in% communes_keep)
  
  panel_eq_all <- df %>%
    select(Prefecture, City, Commune, matches("^earthquake_score_\\d{4}$")) %>%
    pivot_longer(matches("^earthquake_score_\\d{4}$"), names_to="Year",
                 names_pattern="^earthquake_score_(\\d{4})$", values_to="earthquake_score") %>%
    mutate(Year=as.integer(Year),
           earthquake_score=replace_na(as.numeric(earthquake_score), 0),
           earthquake_5p=as.integer(earthquake_score >= EQ_THRESHOLD)) %>%
    filter(Commune %in% communes_keep)
  
  if (censor_2011) {
    exc <- panel_eq_all %>% filter(Year==2011, earthquake_5p==1L) %>%
      distinct(Commune) %>% pull(Commune)
    communes_keep <- setdiff(communes_keep, exc)
    panel_green  <- filter(panel_green,  Commune %in% communes_keep)
    panel_eq_all <- filter(panel_eq_all, Commune %in% communes_keep)
  }
  
  panel_eq <- filter(panel_eq_all, Year %in% years_window)
  
  timing_tbl <- panel_eq %>%
    group_by(Commune) %>%
    summarise(G=as.integer(ifelse(any(earthquake_5p==1L), min(Year[earthquake_5p==1L]), 0L)),
              .groups="drop")
  
  if (censor_second) {
    second_tbl <- panel_eq_all %>%
      filter(Year %in% years_window, earthquake_5p==1L) %>%
      group_by(Commune) %>%
      summarise(second_eq_year=ifelse(n_distinct(Year)>=2, sort(unique(Year))[2], NA_integer_),
                .groups="drop")
    panel_green <- panel_green %>%
      left_join(second_tbl, by="Commune") %>%
      filter(is.na(second_eq_year) | Year < second_eq_year) %>% select(-second_eq_year)
    panel_eq <- panel_eq %>%
      left_join(second_tbl, by="Commune") %>%
      filter(is.na(second_eq_year) | Year < second_eq_year) %>% select(-second_eq_year)
    timing_tbl <- panel_eq %>%
      group_by(Commune) %>%
      summarise(G=as.integer(ifelse(any(earthquake_5p==1L), min(Year[earthquake_5p==1L]), 0L)),
                .groups="drop")
  }
  
  panel_green %>%
    left_join(panel_eq %>% select(Prefecture,City,Commune,Year,earthquake_score,earthquake_5p),
              by=c("Prefecture","City","Commune","Year")) %>%
    left_join(timing_tbl, by="Commune") %>%
    mutate(id_cs=as.integer(factor(interaction(Commune, Segment, drop=TRUE))),
           Year_Segment_fe=factor(interaction(Year, Segment, drop=TRUE))) %>%
    as.data.frame()
}

estimate_did <- function(panel) {
  commG     <- distinct(panel, Commune, G)
  n_treated <- sum(commG$G > 0); n_control <- sum(commG$G == 0)
  fit <- att_gt(yname="Green", tname="Year", idname="id_cs", gname="G",
                data=panel, panel=TRUE, control_group="nevertreated",
                clustervars="Commune", xformla=~Year_Segment_fe)
  ag_s <- aggte(fit, type="simple")
  ag_d <- aggte(fit, type="dynamic", min_e=-5, max_e=5)
  att  <- ag_s$overall.att; se <- ag_s$overall.se
  list(
    att_summary = data.frame(ATT=att, SE=se,
                             ci_low=att-qnorm(.975)*se, ci_high=att+qnorm(.975)*se,
                             p=2*(1-pnorm(abs(att/se))),
                             n_treated=n_treated, n_control=n_control),
    es_df = data.frame(event_time=ag_d$egt, att=ag_d$att.egt, se=ag_d$se.egt) %>%
      mutate(ci_low=att-qnorm(.975)*se, ci_high=att+qnorm(.975)*se)
  )
}

scenarios <- list(
  baseline = list(label="Baseline",                                    censor_2011=FALSE, censor_second=FALSE),
  drop2011 = list(label="Censoring 2011",                              censor_2011=TRUE,  censor_second=FALSE),
  censor2  = list(label="Censoring from second treatment",             censor_2011=FALSE, censor_second=TRUE),
  both     = list(label="Censoring 2011 and second treatment",         censor_2011=TRUE,  censor_second=TRUE)
)

results <- lapply(scenarios, function(sc) {
  cat("Running:", sc$label, "\n")
  est <- estimate_did(build_panel(df, sc$censor_2011, sc$censor_second))
  list(scenario=sc$label, est=est)
})

att_table <- bind_rows(lapply(results, function(x)
  x$est$att_summary %>%
    mutate(Scenario=x$scenario,
           CI=paste0("[", round(ci_low,4), ", ", round(ci_high,4), "]")) %>%
    select(Scenario, ATT, SE, CI, p, n_treated, n_control))) %>%
  mutate(across(c(ATT,SE,p), ~round(.x,4)))

cat(knitr::kable(att_table, format="latex", booktabs=TRUE,
                 col.names=c("Scenario","ATT","SE","95\\% CI","p-value",
                             "\\# treated","\\# control")))

es_all <- bind_rows(lapply(results, function(x)
  mutate(x$est$es_df, Scenario=x$scenario)))

ggplot(es_all, aes(x=event_time, y=att)) +
  geom_hline(yintercept=0, linetype="dashed") +
  geom_vline(xintercept=0, linetype="dashed") +
  geom_errorbar(aes(ymin=ci_low, ymax=ci_high), width=0.2, linewidth=0.7) +
  geom_point(size=2.2) +
  scale_x_continuous(breaks=sort(unique(es_all$event_time))) +
  facet_wrap(~Scenario) +
  labs(x="Event time", y="ATT on Green (95% CI)") +
  theme_minimal(base_size=13)



### Table Z14 ###

library(readxl)
library(dplyr)
library(tidyr)
library(did)
library(ggplot2)

set.seed(123)
setwd("your path")

df <- read_excel("data.xlsx") %>%
  distinct() %>%
  mutate(Commune = paste(Prefecture, City, sep = "_"))

sector_cols <- c(
  "Paper","Stationery","OfficeFurniture","MobilePhones","HomeAppliances",
  "AirConditioners","WaterHeaters","Lighting","Vehicles","FireExtinguishers",
  "Uniforms","InteriorBedding","WorkGloves","OtherTextiles","Equipment","DisasterSupplies"
)

years_window <- 2012:2020
T_years <- length(years_window)

panel_green_long <- df %>%
  select(
    Prefecture, City, Commune,
    matches(paste0("^(", paste(sector_cols, collapse="|"), ")_\\d{4}$"))
  ) %>%
  pivot_longer(
    cols = matches("_(19|20)\\d{2}$"),
    names_to = c("Segment", "Year"),
    names_pattern = "^(.*)_(\\d{4})$",
    values_to = "Green"
  ) %>%
  mutate(
    Year    = as.integer(Year),
    Segment = factor(Segment, levels = sector_cols),
    Green   = as.numeric(Green)
  ) %>%
  filter(Year %in% years_window, !is.na(Green))

communes_keep <- panel_green_long %>%
  distinct(Commune, Segment, Year) %>%
  count(Commune, name = "n_cells") %>%
  filter(n_cells == length(sector_cols) * T_years) %>%
  pull(Commune)

panel_green_long <- panel_green_long %>% filter(Commune %in% communes_keep)

panel_green_mean <- panel_green_long %>%
  group_by(Prefecture, City, Commune, Year) %>%
  summarise(Green_mean = mean(Green, na.rm = TRUE), .groups = "drop")

panel_eq_base <- df %>%
  select(Prefecture, City, Commune, matches("^earthquake_score_\\d{4}$")) %>%
  pivot_longer(
    cols = matches("^earthquake_score_\\d{4}$"),
    names_to = "Year",
    names_pattern = "^earthquake_score_(\\d{4})$",
    values_to = "earthquake_score"
  ) %>%
  mutate(
    Year = as.integer(Year),
    earthquake_score = as.numeric(earthquake_score),
    earthquake_score = ifelse(is.na(earthquake_score), 0, earthquake_score)
  ) %>%
  filter(Year %in% years_window, Commune %in% communes_keep)

thresholds <- c(1, 2, 3)

results_att   <- list()
results_es    <- list()

for (thresh in thresholds) {
  
  cat("\n========== Seuil score >=", thresh, "==========\n")
  
  panel_eq <- panel_eq_base %>%
    mutate(earthquake_5p = as.integer(earthquake_score >= thresh))
  
  timing_tbl <- panel_eq %>%
    group_by(Commune) %>%
    summarise(
      first_eq_year = ifelse(any(earthquake_5p == 1L), min(Year[earthquake_5p == 1L]), 0L),
      .groups = "drop"
    )
  
  panel <- panel_green_mean %>%
    left_join(
      panel_eq %>% select(Prefecture, City, Commune, Year, earthquake_score, earthquake_5p),
      by = c("Prefecture", "City", "Commune", "Year")
    ) %>%
    left_join(timing_tbl, by = "Commune") %>%
    mutate(
      G  = as.integer(first_eq_year),
      id = as.integer(factor(Commune))
    ) %>%
    as.data.frame()
  
  att_cs <- att_gt(
    yname         = "Green_mean",
    tname         = "Year",
    idname        = "id",
    gname         = "G",
    data          = panel,
    panel         = TRUE,
    control_group = "nevertreated",
    clustervars   = "Commune",
    xformla       = ~ 1,
    allow_unbalanced_panel = FALSE
  )
  
  es_cs <- aggte(att_cs, type = "dynamic", min_e = -5, max_e = 5)
  summary(es_cs)
  
  results_es[[as.character(thresh)]] <- data.frame(
    threshold  = thresh,
    event_time = es_cs$egt,
    att        = es_cs$att.egt,
    se         = es_cs$se.egt,
    crit       = es_cs$crit.val.egt
  ) %>%
    mutate(
      ci_low  = att - crit * se,
      ci_high = att + crit * se,
      label   = paste0("score >= ", thresh)
    )

  att_global <- aggte(att_cs, type = "simple")
  summary(att_global)
  
  att_hat <- att_global$overall.att
  se_hat  <- att_global$overall.se
  t_stat  <- att_hat / se_hat
  p_value <- 2 * (1 - pnorm(abs(t_stat)))
  
  results_att[[as.character(thresh)]] <- tibble(
    threshold = thresh,
    ATT       = att_hat,
    SE        = se_hat,
    t         = t_stat,
    p_value   = p_value,
    star      = case_when(p_value < 0.01 ~ "***", p_value < 0.05 ~ "**",
                          p_value < 0.10 ~ "*", TRUE ~ "")
  )
  pre <- which(es_cs$egt <= -2)
  any_pre_sig <- any(abs(es_cs$att.egt[pre]) > es_cs$crit.val.egt * es_cs$se.egt[pre])
}

att_table <- bind_rows(results_att)
print(att_table)
