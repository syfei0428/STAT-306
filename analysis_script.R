library(dplyr)
library(ggplot2)
library(broom)
library(patchwork)


# --- 1. Data import and cleaning ---

# The raw dataset is stored locally in the project folder as data/EV_cars.csv.
# Using a project-relative path makes the analysis reproducible for all group
# members and avoids machine-specific paths such as ~/Desktop/...

data_path <- "data/EV_cars.csv"
premium_brands <- c("Tesla", "BMW", "Audi", "Mercedes-Benz", "Mercedes", "Porsche")

ev_raw <- read.csv(data_path, header = TRUE, na.strings = c("NA", ""))

ev <- ev_raw %>%
  mutate(
    manufacturer = sub(" .*", "", Car_name),
    manufacturer = ifelse(manufacturer == "Mercedes-Benz", "Mercedes-Benz", manufacturer),
    market_segment = ifelse(manufacturer %in% premium_brands, "Premium", "Non-premium"),
    market_segment = factor(market_segment, levels = c("Non-premium", "Premium")),
    Acceleration_0_100 = acceleration..0.100.
  ) %>%
  select(
    Car_name,
    Car_name_link,
    manufacturer,
    market_segment,
    Price.DE.,
    Battery,
    Range,
    Efficiency,
    Top_speed,
    Acceleration_0_100,
    Fast_charge
  ) %>%
  filter(
    !is.na(Price.DE.),
    !is.na(Battery),
    !is.na(market_segment)
  )

cat("--- Cleaning summary ---\n")
cat("Rows in raw data:", nrow(ev_raw), "\n")
cat("Rows in analysis data:", nrow(ev), "\n")
cat("Missing prices in raw data:", sum(is.na(ev_raw$Price.DE.)), "\n")
cat("Missing Fast_charge in analysis data:", sum(is.na(ev$Fast_charge)), "\n\n")

# This extraction rule was checked manually in the report workflow.
# Multi-word brands such as Mercedes-Benz are corrected explicitly above.


# --- 2. Missing-price check used in the report ---
# Compare observed technical variables between rows with and without listed prices.
# In the report, this supports the claim that the missing-price rows do not look
# dramatically different on the observed technical variables.

ev_missing_check <- ev_raw %>%
  mutate(
    price_missing = is.na(Price.DE.),
    Acceleration_0_100 = acceleration..0.100.
  )

missing_price_comparison <- ev_missing_check %>%
  group_by(price_missing) %>%
  summarise(
    n = n(),
    mean_battery = mean(Battery, na.rm = TRUE),
    mean_range = mean(Range, na.rm = TRUE),
    mean_efficiency = mean(Efficiency, na.rm = TRUE),
    mean_top_speed = mean(Top_speed, na.rm = TRUE),
    mean_accel_0_100 = mean(Acceleration_0_100, na.rm = TRUE),
    mean_fast_charge = mean(Fast_charge, na.rm = TRUE),
    .groups = "drop"
  )

cat("--- Missing-price comparison ---\n")
print(missing_price_comparison)
cat("\n")

# --- Missing Value Deep Dive ---

# 1. Extract all rows with missing Price.DE.
ev_missing_price <- ev_raw %>%
  mutate(
    manufacturer = sub(" .*", "", Car_name),
    manufacturer = ifelse(manufacturer == "Mercedes-Benz", "Mercedes-Benz", manufacturer),
    market_segment = ifelse(manufacturer %in% premium_brands, "Premium", "Non-premium")
  ) %>%
  filter(is.na(Price.DE.)) %>%
  select(Car_name, manufacturer, market_segment, Battery, Range, Top_speed, Fast_charge)

cat("--- Observations with Missing Price (n =", nrow(ev_missing_price), ") ---\n")
print(as.data.frame(ev_missing_price))

# 2. Summary by manufacturer
cat("\n--- Missing Price Count by Manufacturer ---\n")
ev_missing_price %>%
  count(manufacturer, sort = TRUE) %>%
  as.data.frame() %>%
  print()

# 3. Summary by market segment
cat("\n--- Missing Price Count by Market Segment ---\n")
ev_missing_price %>%
  count(market_segment) %>%
  print()

# 4. Extract rows with missing Fast_charge
# These are kept in the cleaned dataset and only dropped from models that use
# Fast_charge. The report notes that these two rows are not extreme on battery
# capacity or price, so excluding them is unlikely to materially change results.
cat("\n--- Observations with Missing Fast_charge (n =",
    sum(is.na(ev_raw$Fast_charge)), ") ---\n")
ev_raw %>%
  filter(is.na(Fast_charge)) %>%
  select(Car_name, Price.DE., Battery, Range) %>%
  print()

# --- 3. Duplicate-name check mentioned in the report ---
# Repeated car names were retained because they are not exact duplicate rows and
# often appear to represent updated listings or distinct configurations.

duplicate_names <- ev %>%
  count(Car_name) %>%
  filter(n > 1) %>%
  arrange(desc(n), Car_name)

cat("--- Duplicate car names retained in the analysis data ---\n")
print(duplicate_names)
cat("\n")


# --- 4. Summary statistics used for Table 1 and grouped context ---

summary_table <- ev %>%
  summarise(
    N = n(),
    mean_price = mean(Price.DE., na.rm = TRUE),
    median_price = median(Price.DE., na.rm = TRUE),
    sd_price = sd(Price.DE., na.rm = TRUE),
    mean_battery = mean(Battery, na.rm = TRUE),
    median_battery = median(Battery, na.rm = TRUE),
    sd_battery = sd(Battery, na.rm = TRUE),
    mean_range = mean(Range, na.rm = TRUE),
    mean_efficiency = mean(Efficiency, na.rm = TRUE)
  )

segment_summary <- ev %>%
  group_by(market_segment) %>%
  summarise(
    n = n(),
    mean_price = mean(Price.DE., na.rm = TRUE),
    mean_battery = mean(Battery, na.rm = TRUE),
    .groups = "drop"
  )

cat("--- Overall summary statistics ---\n")
print(summary_table)
cat("\n--- Segment summaries used in the text ---\n")
print(segment_summary)
cat("\n")


# --- 5. Exploratory plots used in the report ---
# Figure 1 in the report is a histogram rather than a boxplot because it
# displays right-skewness more directly for the price distribution.

price_histogram <- ggplot(ev, aes(x = Price.DE., fill = market_segment)) +
  geom_histogram(binwidth = 5000, color = "black", alpha = 0.7) +
  theme_bw() +
  scale_fill_manual(values = c("Premium" = "#2c3e50", "Non-premium" = "#e74c3c")) +
  facet_wrap(~market_segment, ncol = 2) +
  labs(
    x = "Price (Euro)",
    y = "Frequency",
    fill = "Market Segment"
  ) +
  theme(
    legend.position = "none",
    strip.background = element_rect(fill = "gray90"), 
    strip.text = element_text(face = "bold")
  )
print(price_histogram)


scatter_plot <- ggplot(ev, aes(x = Battery, y = Price.DE., color = market_segment)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Price versus Battery Capacity by Market Segment",
    x = "Battery capacity (kWh)",
    y = "Price (EUR)",
    color = "Segment"
  )
print(scatter_plot)


# --- 6. Simple within-segment regressions used for Table 2 ---
# Big result: the unadjusted battery-price slope is positive in both segments
# and steeper for premium vehicles.

model_nonpremium <- lm(
  Price.DE. ~ Battery,
  data = ev %>% filter(market_segment == "Non-premium")
)

model_premium <- lm(
  Price.DE. ~ Battery,
  data = ev %>% filter(market_segment == "Premium")
)

slope_table <- bind_rows(
  tidy(model_nonpremium) %>% mutate(segment = "Non-premium"),
  tidy(model_premium) %>% mutate(segment = "Premium")
) %>%
  filter(term == "Battery") %>%
  transmute(
    Segment = segment,
    slope_estimate_eur_per_kwh = estimate,
    standard_error = std.error,
    p_value = p.value
  )

cat("--- Table 2: Simple within-segment slopes ---\n")
print(slope_table)
cat("\n")


# --- 7. Correlation matrix used for Table 3 ---
# Big result: Battery and Range are highly correlated, which matters when
# interpreting adjusted regression coefficients.

quant_vars <- ev %>%
  select(
    Price.DE.,
    Battery,
    Range,
    Efficiency,
    Top_speed,
    Acceleration_0_100,
    Fast_charge
  )

correlation_table <- round(cor(quant_vars, use = "complete.obs"), 2)

cat("--- Table 3: Correlation matrix ---\n")
print(correlation_table)
cat("\n")


# --- 8. Appendix Figure A1: Battery and range distributions ---
# This figure shows that premium and non-premium vehicles differ not only in
# center but also in spread for these technical attributes.

p_bat_app <- ggplot(ev, aes(x = Battery, fill = market_segment)) +
  geom_histogram(binwidth = 10, color = "black", alpha = 0.7) +
  scale_fill_manual(values = c("Premium" = "#2c3e50", "Non-premium" = "#e74c3c")) +
  theme_bw() +
  facet_wrap(~market_segment, ncol = 1) +
  labs(title = "(a) Battery Capacity (kWh)", x = "Battery (kWh)", y = "Count") +
  theme(legend.position = "none", plot.title = element_text(size = 10))

p_rng_app <- ggplot(ev, aes(x = Range, fill = market_segment)) +
  geom_histogram(binwidth = 50, color = "black", alpha = 0.7) +
  scale_fill_manual(values = c("Premium" = "#2c3e50", "Non-premium" = "#e74c3c")) +
  theme_bw() +
  facet_wrap(~market_segment, ncol = 1) +
  labs(title = "(b) Driving Range (km)", x = "Range (km)", y = "Count") +
  theme(legend.position = "none", plot.title = element_text(size = 10))

p_bat_app + p_rng_app +
  plot_annotation(
    title = "Appendix Figure A1: Battery Capacity and Driving Range by Segment",
    theme = theme(plot.title = element_text(face = "bold", hjust = 0.5))
  )


# --- 9. Appendix Figure A2: Price versus additional technical variables ---
# These plots extend the exploratory analysis beyond Battery by showing how
# Price relates to Range, Efficiency, Top_speed, and Acceleration_0_100.

p_range <- ggplot(ev, aes(x = Range, y = Price.DE., color = market_segment)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "(a) Price vs. Driving Range", x = "Driving range (km)", y = "Price (EUR)") +
  theme_bw() +
  theme(legend.position = "bottom", plot.title = element_text(size = 10))

p_eff <- ggplot(ev, aes(x = Efficiency, y = Price.DE., color = market_segment)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "(b) Price vs. Energy Efficiency", x = "Efficiency (Wh/km)", y = "Price (EUR)") +
  theme_bw() +
  theme(legend.position = "none", plot.title = element_text(size = 10))

p_speed <- ggplot(ev, aes(x = Top_speed, y = Price.DE., color = market_segment)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "(c) Price vs. Top Speed", x = "Top speed (km/h)", y = "Price (EUR)") +
  theme_bw() +
  theme(legend.position = "none", plot.title = element_text(size = 10))

p_accel <- ggplot(ev, aes(x = Acceleration_0_100, y = Price.DE., color = market_segment)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "(d) Price vs. Acceleration", x = "0-100 km/h time (s)", y = "Price (EUR)") +
  theme_bw() +
  theme(legend.position = "none", plot.title = element_text(size = 10))

(p_range + p_eff) / (p_speed + p_accel)


# --- 10. Appendix Figure A3: Pairwise plot used in the appendix ---
# This supports the report's discussion of overlap among Battery, Range,
# Top_speed, and Fast_charge.

pairs_data <- ev %>%
  select(
    Price.DE.,
    Battery,
    Range,
    Efficiency,
    Top_speed,
    Acceleration_0_100,
    Fast_charge
  )

colnames(pairs_data) <- c(
  "Price",
  "Battery",
  "Range",
  "Efficiency",
  "Top speed",
  "0-100 accel",
  "Fast charge"
)

pairs(
  pairs_data,
  pch = 19,
  cex = 0.5
)


# --- 11. Backward elimination with required research-question terms kept ---
# The research-question terms stay in the model throughout selection. Additional
# candidate variables are checked one at a time with drop1(), and variables with
# large p-values are removed sequentially.

backward_keep_required <- function(data, response, required_terms, optional_terms, alpha = 0.05) {
  current_optional <- optional_terms

  repeat {
    current_terms <- c(required_terms, current_optional)
    current_formula <- as.formula(
      paste(response, "~", paste(current_terms, collapse = " + "))
    )
    current_model <- lm(current_formula, data = data)

    if (length(current_optional) == 0) {
      break
    }

    drop_table <- drop1(current_model, test = "F")
    removable <- intersect(rownames(drop_table), current_optional)

    if (length(removable) == 0) {
      break
    }

    removable_table <- drop_table[removable, , drop = FALSE]
    worst_term <- rownames(removable_table)[which.max(removable_table$`Pr(>F)`)]
    worst_p <- max(removable_table$`Pr(>F)`, na.rm = TRUE)

    if (is.na(worst_p) || worst_p <= alpha) {
      break
    }

    current_optional <- setdiff(current_optional, worst_term)
  }

  final_terms <- c(required_terms, current_optional)
  final_formula <- as.formula(
    paste(response, "~", paste(final_terms, collapse = " + "))
  )

  list(
    final_model = lm(final_formula, data = data),
    kept_optional_terms = current_optional,
    removed_optional_terms = setdiff(optional_terms, current_optional)
  )
}


# --- 12. Initial raw-price model and diagnostics ---
# This is the full model containing all covariates before transformation and
# model simplification.

full_model <- lm(
  Price.DE. ~ Battery * market_segment +
    Range + Efficiency + Top_speed + Acceleration_0_100 + Fast_charge,
  data = ev
)

cat("--- Initial raw-price model ---\n")
print(summary(full_model))
cat("\n")

# Big result: the raw-price model shows increasing residual spread and a heavy
# upper tail in the Q-Q plot. Together with the right-skewed price histogram,
# this motivates the log transformation used in the report.
par(mfrow = c(2, 2))
plot(full_model)
par(mfrow = c(1, 1))


# --- 13. Initial full log model and backward refinement used in the report ---
# The report's model-refinement table is based on the full log-price model
# before any optional technical variables are removed.

required_terms <- c("Battery", "market_segment", "Battery:market_segment")
optional_terms <- c("Range", "Efficiency", "Top_speed", "Acceleration_0_100", "Fast_charge")

initial_log_full_model <- lm(
  log(Price.DE.) ~ Battery * market_segment +
    Range + Efficiency + Top_speed + Acceleration_0_100 + Fast_charge,
  data = ev
)

selection_results <- backward_keep_required(
  data = ev,
  response = "log(Price.DE.)",
  required_terms = required_terms,
  optional_terms = optional_terms,
  alpha = 0.05
)

final_model <- selection_results$final_model

cat("--- Initial full log-price model ---\n")
print(summary(initial_log_full_model))
cat("\n")

cat("--- Final log-price model ---\n")
cat("Kept optional terms:\n")
print(selection_results$kept_optional_terms)
cat("Removed optional terms:\n")
print(selection_results$removed_optional_terms)
cat("\n")
print(summary(final_model))
cat("\n")

# Table 5 in the report summarizes which terms were retained or removed during
# refinement on the log-price scale.
format_p_value <- function(x) {
  ifelse(x < 0.001, "<0.001", sprintf("%.4f", x))
}

initial_log_p_values <- summary(initial_log_full_model)$coefficients[, "Pr(>|t|)"]

term_transition_table <- data.frame(
  Term = c(
    "Battery capacity",
    "Premium segment",
    "Battery capacity x premium segment",
    "Driving range",
    "Energy efficiency",
    "Top speed",
    "Acceleration (0-100 km/h)",
    "Fast-charging speed"
  ),
  initial_full_log_model_p_value = c(
    format_p_value(initial_log_p_values["Battery"]),
    format_p_value(initial_log_p_values["market_segmentPremium"]),
    format_p_value(initial_log_p_values["Battery:market_segmentPremium"]),
    format_p_value(initial_log_p_values["Range"]),
    format_p_value(initial_log_p_values["Efficiency"]),
    format_p_value(initial_log_p_values["Top_speed"]),
    format_p_value(initial_log_p_values["Acceleration_0_100"]),
    format_p_value(initial_log_p_values["Fast_charge"])
  ),
  final_model_status = c(
    "Retained", "Retained", "Retained", "Retained",
    "Retained", "Retained", "Removed", "Retained"
  )
)

cat("--- Table 5: Terms retained or removed during model refinement ---\n")
print(term_transition_table)
cat("\n")

# Big result: after adjustment, the Battery main effect is negative but not
# significant for non-premium vehicles, while the interaction remains strongly
# positive and significant, indicating the battery-price relationship differs
# by segment. With Range in the model, the Battery coefficient reflects only
# the remaining variation in battery capacity beyond what is already explained
# by driving range.
coef_mat <- summary(final_model)$coefficients
final_coef_table <- data.frame(
  Estimate = round(coef_mat[, "Estimate"], 4),
  Std_Error = round(coef_mat[, "Std. Error"], 4),
  t_value = round(coef_mat[, "t value"], 4),
  p_value = ifelse(coef_mat[, "Pr(>|t|)"] < 0.001, "<0.001",
                   sprintf("%.4f", coef_mat[, "Pr(>|t|)"]))
)

row.names(final_coef_table) <- c(
  "(Intercept)",
  "Battery",
  "market_segmentPremium",
  "Range",
  "Efficiency",
  "Top_speed",
  "Fast_charge",
  "Battery:market_segmentPremium"
)

cat("--- Table 6: Final model coefficients ---\n")
print(final_coef_table)
cat("\n")


# --- 14. Diagnostics reported in the appendix ---
# Appendix Figure A4 contains the initial raw-price diagnostics. Appendix
# Figure A5 contains the final log-linear diagnostics, which are improved but
# still show mild remaining departures.

cat("--- Appendix Figure A4: Initial raw-price diagnostics ---\n")
par(mfrow = c(2, 2))
plot(full_model)
par(mfrow = c(1, 1))

cat("--- Appendix Figure A5: Final log-linear diagnostics ---\n")
par(mfrow = c(2, 2))
plot(final_model)
par(mfrow = c(1, 1))

cat("--- Appendix Figure A6: Cook's distance for final model ---\n")
plot(
  cooks.distance(final_model),
  type = "h",
  main = "Cook's Distance for Final Model",
  ylab = "Cook's distance"
)
abline(h = 4 / nrow(ev), col = "red", lty = 2)
