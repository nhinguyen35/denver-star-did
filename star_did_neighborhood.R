# Denver STAR Program — Neighborhood-Level Difference-in-Differences
# Research question: Did STAR's launch in the 8 downtown pilot neighborhoods
# (June 2020) reduce STAR-targeted offense counts relative to all other
# Denver neighborhoods?
#
# Unit of analysis : neighborhood × month
# Treated units    : 8 D6 pilot neighborhoods (cbd, union-station, five-points,
#                    civic-center, capitol-hill, north-capitol-hill,
#                    cheesman-park, city-park-west)
# Control units    : all other Denver neighborhoods
# Sample window    : Jan 2018 – Aug 2021 (44 months)
# Outcome (DV)     : monthly count of STAR-targeted offenses per neighborhood

# --- Packages -----------------------------------------------------------------
library(tidyverse)  # data manipulation + ggplot2
library(lubridate)  # date parsing: mdy_hms(), floor_date(), year(), month()
library(scales)     # comma() formatter for plot axes
library(broom)      # tidy() — extracts regression coefficients as a data frame

# ==============================================================================
# SECTION 1: Describe the Raw Data
# ==============================================================================

# --- Load raw CSV and convert date columns ------------------------------------
crime_raw <- read_csv(
  "data/crime.csv",
  locale = locale(encoding = "latin1"),
  show_col_types = FALSE
)

crime <- crime_raw |>
  mutate(
    first_occurrence_date = mdy_hms(first_occurrence_date),
    reported_date         = mdy_hms(reported_date),
    last_occurrence_date  = mdy_hms(last_occurrence_date),
    year                  = year(first_occurrence_date)
  )

cat("=== Dataset dimensions ===\n")
cat("Rows:", nrow(crime), "| Columns:", ncol(crime), "\n\n")

cat("=== Time period covered ===\n")
crime |>
  summarise(min_date = min(first_occurrence_date, na.rm = TRUE),
            max_date = max(first_occurrence_date, na.rm = TRUE)) |>
  print()
cat("\n")

# --- Define the 13 STAR-eligible offense types --------------------------------
# Non-violent substance use, trespassing, and public disorder calls that
# match Denver STAR's documented eligibility criteria.
star_types <- c(
  "criminal-trespassing",
  "disturbing-the-peace",
  "criminal-mischief-other",
  "drug-poss-paraphernalia",
  "drug-methampetamine-possess",
  "drug-pcs-other-drug",
  "liquor-possession",
  "drug-heroin-possess",
  "drug-cocaine-possess",
  "drug-opium-or-deriv-possess",
  "drug-marijuana-possess",
  "drug-synth-narcotic-possess",
  "drug-hallucinogen-possess"
)

# --- Define the 8 treated neighborhoods (STAR pilot zone, June 2020) ----------
# Source: Denver Department of Public Safety STAR pilot zone documentation.
# All 8 are in Denver Police District 6 (downtown).
treated_nbhds <- c(
  "cbd",
  "union-station",
  "five-points",
  "civic-center",
  "capitol-hill",
  "north-capitol-hill",
  "cheesman-park",
  "city-park-west"
)

# --- Count STAR-targeted incidents by offense type (all years) ----------------
cat("=== STAR-targeted offense types (count + share) ===\n")
crime |>
  filter(offense_type_id %in% star_types) |>
  count(offense_type_id, sort = TRUE) |>
  mutate(pct = round(100 * n / sum(n), 1)) |>
  print()
cat("Total STAR-targeted incidents (all years):",
    sum(crime$offense_type_id %in% star_types), "\n\n")

# --- Per-neighborhood summary: total + STAR-targeted incidents ----------------
cat("=== Top 10 neighborhoods by STAR-targeted offense count ===\n")
crime |>
  filter(offense_type_id %in% star_types,
         year %in% 2018:2021) |>
  count(neighborhood_id, sort = TRUE) |>
  head(10) |>
  print()
cat("\n")

# --- Verify the 8 treated neighborhoods exist in the data ---------------------
cat("=== Treated neighborhoods: row counts in raw data ===\n")
crime |>
  filter(neighborhood_id %in% treated_nbhds) |>
  count(neighborhood_id) |>
  arrange(neighborhood_id) |>
  print()
cat("\n")

# --- Missing-value audit ------------------------------------------------------
cat("=== Missing-value audit ===\n")
missing_tbl <- crime |>
  summarise(across(everything(), ~ sum(is.na(.)))) |>
  pivot_longer(everything(), names_to = "variable", values_to = "n_missing") |>
  mutate(pct_missing = round(100 * n_missing / nrow(crime), 2)) |>
  arrange(desc(n_missing))
print(missing_tbl, n = Inf)
cat("\n")

star_date <- as.Date("2020-06-01")

# --- Figure B: STAR-targeted offenses — 8 treated avg vs control avg ---------
# The "money shot": did treated neighborhoods fall faster than controls after launch?
# Both lines are PER-NEIGHBORHOOD averages so the comparison is on equal footing.
p_figB <- crime |>
  filter(offense_type_id %in% star_types,
         first_occurrence_date >= as.POSIXct("2018-01-02"),
         first_occurrence_date <  as.POSIXct("2021-09-01"),
         !is.na(neighborhood_id)) |>
  mutate(
    year_month = floor_date(first_occurrence_date, "month"),
    group      = if_else(neighborhood_id %in% treated_nbhds,
                         "Treated (avg per pilot nbhd)",
                         "Control (avg per other nbhd)")
  ) |>
  count(group, neighborhood_id, year_month) |>
  group_by(group, year_month) |>
  summarise(mean_n = mean(n), .groups = "drop") |>
  ggplot(aes(x = as.Date(year_month), y = mean_n, colour = group)) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = star_date, linetype = "dashed", colour = "black") +
  annotate("text", x = star_date, y = Inf,
           label = "STAR launch\nJun 2020", hjust = -0.05, vjust = 1.3, size = 3) +
  scale_colour_manual(values = c("Treated (avg per pilot nbhd)"  = "#2166ac",
                                 "Control (avg per other nbhd)"  = "#d73027")) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
  scale_y_continuous(labels = comma) +
  labs(title    = "Figure B: Monthly STAR-targeted offenses — 8 treated nbhds vs control avg",
       subtitle = "The 'money shot': per-neighborhood averages, treated vs control",
       x = NULL, y = "Avg monthly STAR-targeted incidents per neighborhood", colour = NULL) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    legend.text     = element_text(size = 9, colour = "black"),
    axis.text.x     = element_text(angle = 45, hjust = 1),
    plot.title      = element_text(size = 11, colour = "black", face = "bold"),
    plot.subtitle   = element_text(size = 9,  colour = "black"),
    plot.margin     = margin(t = 14, r = 10, b = 10, l = 10)
  )

ggsave("output/fig1b_money_shot_nbhd.png", p_figB, width = 9, height = 5.5, dpi = 150)

# ==============================================================================
# SECTION 2: Sample Construction
# ==============================================================================

cat("=== Filter log ===\n")
cat(sprintf("Raw data:                        %d rows\n", nrow(crime)))

# Step 1: keep Jan 2, 2018 – Aug 31, 2021
crime_step1 <- crime |>
  filter(first_occurrence_date >= as.POSIXct("2018-01-02"),
         first_occurrence_date <  as.POSIXct("2021-09-01"))
cat(sprintf("After date filter (step 1):      %d rows  (removed %d)\n",
            nrow(crime_step1), nrow(crime) - nrow(crime_step1)))

# Step 2: drop rows with missing neighborhood_id
crime_step2 <- crime_step1 |>
  filter(!is.na(neighborhood_id))
cat(sprintf("After dropping missing nbhd (step 2): %d rows  (removed %d)\n\n",
            nrow(crime_step2), nrow(crime_step1) - nrow(crime_step2)))

# --- Step 3: build the neighborhood × month panel (STAR-targeted only) -------
# We restrict to neighborhoods that appear in the dataset during the window.
# Every neighborhood in the data is either treated or control.
crime_clean <- crime_step2 |>
  mutate(
    year_month    = floor_date(first_occurrence_date, "month") |> as.Date(),
    treated       = as.integer(neighborhood_id %in% treated_nbhds),
    post_star     = as.integer(year_month >= star_date),
    did_term      = treated * post_star,
    star_targeted = as.integer(offense_type_id %in% star_types)
  )

# Count unique neighborhoods in each group
n_treated_nbhds <- crime_clean |>
  filter(treated == 1) |> distinct(neighborhood_id) |> nrow()
n_control_nbhds <- crime_clean |>
  filter(treated == 0) |> distinct(neighborhood_id) |> nrow()
cat(sprintf("\nTreated neighborhoods in data: %d\n", n_treated_nbhds))
cat(sprintf("Control neighborhoods in data: %d\n\n", n_control_nbhds))

# Aggregate to (neighborhood × month), STAR-targeted only.
# IMPORTANT: use complete() to fill in zeros — a neighborhood-month with NO
# STAR-targeted offenses should be a row with incidents = 0, not a missing row.
analysis_panel <- crime_clean |>
  filter(star_targeted == 1) |>
  count(neighborhood_id, treated, year_month, name = "incidents") |>
  complete(
    neighborhood_id,
    year_month = seq(min(year_month), max(year_month), by = "month"),
    fill = list(incidents = 0)
  ) |>
  # re-attach treated indicator (lost on neighborhoods with 0 obs in some months)
  mutate(treated = as.integer(neighborhood_id %in% treated_nbhds),
         post_star = as.integer(year_month >= star_date),
         did_term  = treated * post_star)

cat(sprintf("Step 3: Aggregate to neighborhood × month (STAR-targeted)\n"))
cat(sprintf("  STAR-targeted offenses in window: %d\n",
            sum(crime_clean$star_targeted)))
cat(sprintf("  Panel cells: %d (= %d nbhds × %d months)\n\n",
            nrow(analysis_panel),
            n_distinct(analysis_panel$neighborhood_id),
            n_distinct(analysis_panel$year_month)))

# --- Section 2 summary stats --------------------------------------------------
cat("=== Panel summary ===\n")
analysis_panel |>
  group_by(treated) |>
  summarise(
    n_nbhds          = n_distinct(neighborhood_id),
    n_cells          = n(),
    mean_incidents   = round(mean(incidents), 2),
    sd_incidents     = round(sd(incidents), 2),
    min_incidents    = min(incidents),
    max_incidents    = max(incidents),
    pct_zero         = round(100 * mean(incidents == 0), 1),
    .groups = "drop"
  ) |>
  print()
cat("\n")

# --- Baseline comparability: treated vs control pre-STAR means ----------------
# Concern: downtown treated neighborhoods are high-volume and structurally
# different from most control neighborhoods. DID does NOT require equal levels —
# it requires parallel TRENDS. This table documents the level difference so
# readers can assess whether trend parallelism is plausible.
cat("=== Baseline comparability (pre-STAR period: Jan 2018 – May 2020) ===\n")
analysis_panel |>
  filter(post_star == 0) |>
  group_by(treated) |>
  summarise(
    n_nbhds        = n_distinct(neighborhood_id),
    mean_incidents = round(mean(incidents), 2),
    median_inc     = round(median(incidents), 2),
    sd_incidents   = round(sd(incidents), 2),
    .groups = "drop"
  ) |>
  mutate(group = if_else(treated == 1, "Treated (8 pilot nbhds)", "Control (70 nbhds)")) |>
  select(group, n_nbhds, mean_incidents, median_inc, sd_incidents) |>
  print()
cat("Note: DID requires parallel trends, not equal baseline levels.\n")
cat("The large level difference motivates Robustness 4 (high-volume controls only).\n\n")

# --- Save panel to disk -------------------------------------------------------
saveRDS(analysis_panel, "output/analysis_panel_nbhd.rds")
write_csv(analysis_panel, "output/analysis_panel_nbhd.csv")

# ==============================================================================
# SECTION 3: Analysis — Neighborhood-Level DID
# ==============================================================================

# --- Figure 1: Main parallel-trends plot — treated avg vs control avg ---------
p_fig1 <- analysis_panel |>
  group_by(treated, year_month) |>
  summarise(mean_inc = mean(incidents), .groups = "drop") |>
  mutate(Group = if_else(treated == 1,
                         "Treated (avg per pilot nbhd)",
                         "Control (avg per other nbhd)")) |>
  ggplot(aes(x = as.Date(year_month), y = mean_inc, colour = Group)) +
  geom_line(linewidth = 1.1) +
  geom_vline(xintercept = star_date, linetype = "dashed", colour = "black") +
  annotate("text", x = star_date, y = Inf,
           label = "STAR launch\nJun 2020", hjust = -0.05, vjust = 1.3, size = 3) +
  scale_colour_manual(values = c("Treated (avg per pilot nbhd)" = "#2166ac",
                                 "Control (avg per other nbhd)" = "#d73027")) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
  scale_y_continuous(labels = comma) +
  labs(
    title    = "Figure 1: Monthly STAR-targeted offenses — 8 treated nbhds vs control avg",
    subtitle = "Both lines are per-neighborhood averages so comparison is on equal footing",
    x = NULL, y = "Avg monthly STAR-targeted incidents per neighborhood", colour = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    legend.text     = element_text(size = 9, colour = "black"),
    axis.text.x     = element_text(angle = 45, hjust = 1),
    plot.title      = element_text(size = 11, colour = "black", face = "bold"),
    plot.subtitle   = element_text(size = 9,  colour = "black"),
    plot.margin     = margin(t = 14, r = 10, b = 10, l = 10)
  )

ggsave("output/fig3_1_main_timeseries_nbhd.png", p_fig1, width = 9, height = 5.5, dpi = 150)

# --- Figure 2: Faceted view — one panel per treated neighborhood --------------
# Shows whether the post-STAR drop appears in ALL 8 treated neighborhoods,
# or whether the average is being driven by a subset of them.
p_fig2 <- analysis_panel |>
  filter(treated == 1) |>
  ggplot(aes(x = as.Date(year_month), y = incidents)) +
  geom_line(linewidth = 0.7, colour = "#2166ac") +
  geom_vline(xintercept = star_date, linetype = "dashed", colour = "black") +
  facet_wrap(~ neighborhood_id, scales = "free_y", ncol = 4) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = comma) +
  labs(
    title    = "Figure 2: STAR-targeted offenses per treated neighborhood, 2018–2021",
    subtitle = "Black dashed = STAR launch (Jun 2020). Free y-axis per panel.",
    x = NULL, y = "Monthly incidents"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    axis.text.x     = element_text(angle = 45, hjust = 1),
    plot.title      = element_text(size = 11, colour = "black", face = "bold"),
    plot.subtitle   = element_text(size = 9,  colour = "black"),
    plot.margin     = margin(t = 14, r = 10, b = 10, l = 10),
    strip.text      = element_text(size = 9, face = "bold")
  )

ggsave("output/fig3_2_faceted_nbhd.png", p_fig2, width = 10, height = 6, dpi = 150)

# --- Main DID regression: incidents ~ neighborhood FE + month FE + did_term ---
# Key assumption (parallel trends): in the absence of STAR, STAR-targeted
# incidents in the 8 pilot neighborhoods would have followed the same monthly
# trend as the control neighborhoods. Neighborhood FE absorb permanent level
# differences (downtown always has more crime); month FE absorb city-wide shocks
# (COVID, seasonality). The did_term coefficient isolates the treated neighborhoods'
# deviation from that common trend after June 2020 — our causal estimate of STAR.
did_mod    <- lm(incidents ~ factor(neighborhood_id) + factor(year_month) + did_term,
                 data = analysis_panel)
did_result <- tidy(did_mod, conf.int = TRUE) |> filter(term == "did_term")

cat("=== Main DID Regression Results (neighborhood-level) ===\n\n")
cat(sprintf("DID estimate:  %.3f incidents/nbhd/month\n",   did_result$estimate))
cat(sprintf("95%% CI:       [%.3f, %.3f]\n",                did_result$conf.low, did_result$conf.high))
cat(sprintf("Std. error:    %.3f\n",                        did_result$std.error))
cat(sprintf("t-statistic:   %.2f\n",                        did_result$statistic))
cat(sprintf("p-value:       %.4f\n",                        did_result$p.value))
cat(sprintf("Observations:  %d\n",                          nobs(did_mod)))
cat(sprintf("R-squared:     %.3f\n\n",                      summary(did_mod)$r.squared))

# --- Figure 3: DID coefficient plot ------------------------------------------
p_fig3 <- ggplot(did_result,
                 aes(x = "DID estimate", y = estimate,
                     ymin = conf.low, ymax = conf.high)) +
  geom_pointrange(size = 1, colour = "#2166ac") +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
  scale_y_continuous(labels = comma) +
  labs(
    title    = "Figure 3: Neighborhood-level DID estimate with 95% CI",
    subtitle = "Model: incidents ~ neighborhood FE + month FE + (treated × post-STAR)\nNegative = treated nbhds fell more than controls after STAR",
    x = NULL, y = "Differential monthly STAR-targeted incidents (treated vs control)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title    = element_text(size = 11, colour = "black", face = "bold"),
    plot.subtitle = element_text(size = 9,  colour = "black"),
    plot.margin   = margin(t = 14, r = 10, b = 10, l = 10)
  )

ggsave("output/fig3_3_did_coef_nbhd.png", p_fig3, width = 5, height = 5, dpi = 150)

# --- Figure 4: Event-study plot ----------------------------------------------
# Treated-avg minus control-avg by month, normalized so May 2020 (month -1) = 0.
# A flat pre-period line supports parallel trends.
event_gaps <- analysis_panel |>
  group_by(treated, year_month) |>
  summarise(mean_inc = mean(incidents), .groups = "drop") |>
  pivot_wider(names_from = treated, values_from = mean_inc, names_prefix = "g") |>
  rename(treated_avg = g1, control_avg = g0) |>
  mutate(
    months_to_star = 12L * (year(year_month) - 2020L) + (month(year_month) - 6L),
    gap            = treated_avg - control_avg
  )

ref_gap    <- event_gaps$gap[event_gaps$months_to_star == -1]  # May 2020 baseline
event_gaps <- event_gaps |> mutate(norm_gap = gap - ref_gap)
pre_sd     <- sd(event_gaps$norm_gap[event_gaps$months_to_star < 0])

p_fig4 <- ggplot(event_gaps, aes(x = months_to_star, y = norm_gap)) +
  annotate("rect", xmin = min(event_gaps$months_to_star), xmax = 0,
           ymin = -Inf, ymax = Inf, fill = "grey90", alpha = 0.5) +
  geom_ribbon(aes(ymin = norm_gap - 2 * pre_sd, ymax = norm_gap + 2 * pre_sd),
              alpha = 0.15, fill = "#2166ac") +
  geom_line(colour = "#2166ac", linewidth = 0.9) +
  geom_point(colour = "#2166ac", size = 1.8) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "black") +
  annotate("text", x = 0,   y = Inf, label = "STAR launch",
           hjust = -0.05, vjust = 1.3, size = 3) +
  annotate("text", x = -14, y = Inf, label = "Pre-treatment\n(shaded)",
           hjust = 0.5, vjust = 1.3, size = 3, colour = "grey40") +
  scale_y_continuous(labels = comma) +
  labs(
    title    = "Figure 4: Event-study plot (neighborhood-level)",
    subtitle = "Treated avg minus control avg, normalised to May 2020 = 0\nBand = ±2 SD of pre-treatment variation",
    x        = "Months relative to STAR launch  (0 = June 2020)",
    y        = "Normalised gap (treated avg − control avg)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title    = element_text(size = 11, colour = "black", face = "bold"),
    plot.subtitle = element_text(size = 9,  colour = "black"),
    plot.margin   = margin(t = 14, r = 10, b = 10, l = 10)
  )

ggsave("output/fig3_4_event_study_nbhd.png", p_fig4, width = 9, height = 5.5, dpi = 150)

# --- Robustness 1: Pre-treatment parallel trends visual -----------------------
p_rob1 <- analysis_panel |>
  filter(year_month < star_date) |>
  group_by(treated, year_month) |>
  summarise(mean_inc = mean(incidents), .groups = "drop") |>
  mutate(Group = if_else(treated == 1, "Treated (avg)", "Control (avg)")) |>
  ggplot(aes(x = as.Date(year_month), y = mean_inc, colour = Group, group = Group)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  scale_colour_manual(values = c("Treated (avg)" = "#2166ac",
                                 "Control (avg)" = "#d73027")) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  scale_y_continuous(labels = comma) +
  labs(
    title    = "Robustness Check 1: Pre-treatment parallel trends (Jan 2018 – May 2020)",
    subtitle = "Lines should move in parallel. A common slope supports the DID assumption.",
    x = NULL, y = "Avg monthly STAR-targeted incidents per nbhd", colour = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    legend.text     = element_text(size = 9, colour = "black"),
    axis.text.x     = element_text(angle = 45, hjust = 1),
    plot.title      = element_text(size = 11, colour = "black", face = "bold"),
    plot.subtitle   = element_text(size = 9,  colour = "black"),
    plot.margin     = margin(t = 14, r = 10, b = 10, l = 10)
  )

ggsave("output/fig3_5_parallel_trends_nbhd.png", p_rob1, width = 9, height = 4.5, dpi = 150)

# --- Robustness 2: Placebo — fake treatment date backdated to June 2019 -------
placebo_panel <- crime_clean |>
  filter(star_targeted == 1,
         year_month < star_date) |>
  mutate(post_placebo = as.integer(year_month >= as.Date("2019-06-01")),
         did_placebo  = treated * post_placebo) |>
  count(neighborhood_id, treated, post_placebo, did_placebo, year_month,
        name = "incidents") |>
  complete(
    neighborhood_id,
    year_month = seq(min(year_month), max(year_month), by = "month"),
    fill = list(incidents = 0)
  ) |>
  mutate(treated      = as.integer(neighborhood_id %in% treated_nbhds),
         post_placebo = as.integer(year_month >= as.Date("2019-06-01")),
         did_placebo  = treated * post_placebo)

did_mod_placebo <- lm(incidents ~ factor(neighborhood_id) + factor(year_month) + did_placebo,
                      data = placebo_panel)
rob_placebo <- tidy(did_mod_placebo, conf.int = TRUE) |> filter(term == "did_placebo")

cat("=== Robustness 2: Placebo test (fake treatment June 2019) ===\n")
cat(sprintf("Placebo DID: %.3f  (95%% CI: [%.3f, %.3f])  p = %.4f\n",
            rob_placebo$estimate, rob_placebo$conf.low,
            rob_placebo$conf.high, rob_placebo$p.value))
if (rob_placebo$p.value > 0.05) {
  cat("Result: NOT significant — no pre-existing break detected. Parallel trends supported.\n\n")
} else {
  cat("WARNING: significant — pre-existing trend break detected.\n\n")
}

# --- Robustness 3: Alternative outcome — all offenses (not just STAR-targeted)
panel_all <- crime_clean |>
  count(neighborhood_id, treated, post_star, did_term, year_month, name = "incidents") |>
  complete(
    neighborhood_id,
    year_month = seq(min(year_month), max(year_month), by = "month"),
    fill = list(incidents = 0)
  ) |>
  mutate(treated   = as.integer(neighborhood_id %in% treated_nbhds),
         post_star = as.integer(year_month >= star_date),
         did_term  = treated * post_star)

did_mod_all <- lm(incidents ~ factor(neighborhood_id) + factor(year_month) + did_term,
                  data = panel_all)
rob_all     <- tidy(did_mod_all, conf.int = TRUE) |> filter(term == "did_term")

cat("=== Robustness 3: Alternative outcome — all offenses ===\n")
cat("Tests: if STAR diverted calls rather than suppressing all crime, the effect\n")
cat("should be stronger for STAR-targeted offenses than for all offenses combined.\n")
cat(sprintf("DID estimate: %.3f  (95%% CI: [%.3f, %.3f])  p = %.4f\n\n",
            rob_all$estimate, rob_all$conf.low, rob_all$conf.high, rob_all$p.value))

# --- Robustness 4: Alternative control — high-volume neighborhoods only -------
# Concern: treated neighborhoods are downtown/high-volume; comparing them to
# all 70 Denver neighborhoods (including low-crime residential areas) may
# violate comparability. Restriction: use only control neighborhoods whose
# pre-STAR average monthly incident rate falls in the top quartile — making
# the control group structurally more similar to the treated neighborhoods.
pre_means <- analysis_panel |>
  filter(post_star == 0) |>
  group_by(neighborhood_id, treated) |>
  summarise(pre_mean = mean(incidents), .groups = "drop")

q75 <- quantile(pre_means$pre_mean[pre_means$treated == 0], 0.75)

comparable_controls <- pre_means |>
  filter(treated == 0, pre_mean >= q75) |>
  pull(neighborhood_id)

panel_comparable <- analysis_panel |>
  filter(treated == 1 | neighborhood_id %in% comparable_controls)

did_mod_comparable <- lm(incidents ~ factor(neighborhood_id) + factor(year_month) + did_term,
                         data = panel_comparable)
rob_comparable <- tidy(did_mod_comparable, conf.int = TRUE) |> filter(term == "did_term")

cat("=== Robustness 4: Alternative control group (high-volume nbhds only) ===\n")
cat(sprintf("Control restricted to %d neighborhoods with pre-STAR avg >= %.1f incidents/month\n",
            length(comparable_controls), q75))
cat("Tests: does the main result hold when controls are more comparable in baseline volume?\n")
cat(sprintf("DID estimate: %.3f  (95%% CI: [%.3f, %.3f])  p = %.4f\n\n",
            rob_comparable$estimate, rob_comparable$conf.low,
            rob_comparable$conf.high, rob_comparable$p.value))

# --- Summary table: all 4 DID estimates --------------------------------------
results_summary <- bind_rows(
  mutate(did_result,     spec = "Main DID (STAR-targeted offenses)"),
  mutate(rob_all,        spec = "Alternative outcome: all offenses"),
  mutate(rob_comparable, spec = "Alternative control: high-volume nbhds only"),
  mutate(rob_placebo,    spec = "Placebo (treatment = Jun 2019)")
) |>
  select(spec, estimate, conf.low, conf.high, p.value) |>
  mutate(across(c(estimate, conf.low, conf.high), \(x) round(x, 3)),
         p.value = round(p.value, 4))

cat("=== All DID estimates — neighborhood specification ===\n")
print(results_summary, n = Inf)

# --- Figure 5: Forest plot ---------------------------------------------------
p_summary <- results_summary |>
  mutate(spec = fct_rev(factor(spec))) |>
  ggplot(aes(x = spec, y = estimate, ymin = conf.low, ymax = conf.high,
             colour = spec == "Main DID (STAR-targeted offenses)")) +
  geom_pointrange(size = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
  coord_flip() +
  scale_colour_manual(values = c("TRUE" = "#2166ac", "FALSE" = "#636363"),
                      guide = "none") +
  scale_y_continuous(labels = comma) +
  labs(
    title    = "Figure 5: Neighborhood DID estimates across all specifications",
    subtitle = "Point = estimate  |  Bar = 95% CI  |  Blue = main specification",
    x = NULL, y = "Differential monthly STAR-targeted incidents per nbhd"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title    = element_text(size = 11, colour = "black", face = "bold"),
    plot.subtitle = element_text(size = 9,  colour = "black"),
    plot.margin   = margin(t = 14, r = 10, b = 10, l = 10)
  )

ggsave("output/fig3_6_robustness_summary_nbhd.png", p_summary, width = 9, height = 4, dpi = 150)

write_csv(results_summary, "output/did_results_nbhd_summary.csv")

# ==============================================================================
# SECTION 4: Managerial Implications
# ==============================================================================
# Translates the DID estimate into actionable numbers for the Denver Department
# of Public Safety to inform funding and expansion decisions.

cat("=== Managerial Implications ===\n\n")

est          <- did_result$estimate
n_pilot      <- 8    # treated neighborhoods in the pilot
months_pilot <- 15   # Jun 2020 – Aug 2021

monthly_avoided_pilot <- abs(est) * n_pilot
total_avoided_pilot   <- monthly_avoided_pilot * months_pilot
annualized_avoided    <- monthly_avoided_pilot * 12

direction <- if_else(est < 0, "reduced", "increased")
cat(sprintf("DID estimate: %.2f incidents per neighborhood per month\n", est))
cat(sprintf("Direction: STAR %s STAR-targeted incidents in pilot neighborhoods.\n\n", direction))

cat(sprintf("Across all 8 pilot neighborhoods:\n"))
cat(sprintf("  Monthly incidents avoided:         %.1f\n", monthly_avoided_pilot))
cat(sprintf("  Total avoided over 15-month pilot: %.0f\n", total_avoided_pilot))
cat(sprintf("  Annualized (projected):            %.0f per year\n\n", annualized_avoided))

n_all_nbhds <- n_distinct(analysis_panel$neighborhood_id)
cat("Implications for citywide expansion:\n")
cat(sprintf("  If the per-neighborhood effect holds city-wide (%d neighborhoods),\n", n_all_nbhds))
cat(sprintf("  STAR could avoid ~%.0f STAR-targeted incidents per month across Denver.\n",
            abs(est) * n_all_nbhds))
cat("  Each avoided incident represents a 911 call handled without police dispatch,\n")
cat("  freeing officer capacity for higher-priority calls.\n")
cat("  Geographic targeting: prioritize high-density districts first, since the\n")
cat("  pilot neighborhoods are disproportionately high-volume — expansion to\n")
cat("  low-volume residential areas may yield a smaller absolute benefit.\n\n")

cat("Caveats for decision-makers:\n")
cat("  - Incident counts reflect reported/recorded events; STAR may also reduce\n")
cat("    unreported incidents not captured here.\n")
cat("  - The pilot ran during COVID (Jun 2020 onward), which independently\n")
cat("    suppressed crime city-wide; month FE absorb this, but the post-period\n")
cat("    is short (15 months) and results should be interpreted cautiously.\n")
cat("  - A cost-effectiveness analysis would require STAR's per-dispatch cost\n")
cat("    vs. average police-response cost — data not available in this dataset.\n\n")

cat("All outputs saved to output/\n")
