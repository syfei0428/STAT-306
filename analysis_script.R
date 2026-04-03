library(dplyr)
library(ggplot2)
library(broom)


# --- 1. Data import and cleaning ---

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


# --- 2. Missing-price check used in the report ---
# Compare observed technical variables between rows with and without listed prices.

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
cat("\n--- Observations with Missing Fast_charge (n =",
    sum(is.na(ev_raw$Fast_charge)), ") ---\n")
ev_raw %>%
  filter(is.na(Fast_charge)) %>%
  select(Car_name, Price.DE., Battery, Range) %>%
  print()


# --- 3. Exploratory Data Visualization ---

# --- 3. Duplicate-name check mentioned in the report ---

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

price_boxplot <- ggplot(ev, aes(y = Price.DE.)) +
  geom_boxplot(fill = "lightblue") +
  labs(y = "Price (EUR)") +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )
print(price_boxplot)

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


# --- 8. Backward elimination with required research-question terms kept ---

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


# --- 9. Initial raw-price model and diagnostics ---

full_model <- lm(
  Price.DE. ~ Battery * market_segment +
    Range + Efficiency + Top_speed + Acceleration_0_100 + Fast_charge,
  data = ev
)

cat("--- Initial raw-price model ---\n")
print(summary(full_model))
cat("\n")

# Big result: the raw-price model shows heteroscedasticity and a heavy upper tail,
# which motivates the log transformation used in the report.
par(mfrow = c(2, 2))
plot(full_model)
par(mfrow = c(1, 1))


# --- 10. Final log-price model used in the report ---

required_terms <- c("Battery", "market_segment", "Battery:market_segment")
optional_terms <- c("Range", "Efficiency", "Top_speed", "Acceleration_0_100", "Fast_charge")

selection_results <- backward_keep_required(
  data = ev,
  response = "log(Price.DE.)",
  required_terms = required_terms,
  optional_terms = optional_terms,
  alpha = 0.05
)

final_model <- selection_results$final_model

cat("--- Final log-price model ---\n")
cat("Kept optional terms:\n")
print(selection_results$kept_optional_terms)
cat("Removed optional terms:\n")
print(selection_results$removed_optional_terms)
cat("\n")
print(summary(final_model))
cat("\n")

# Big result: after adjustment, the Battery main effect is not significant for
# non-premium vehicles, but the interaction remains strongly significant,
# indicating the battery-price relationship differs by segment.
final_coef_table <- tidy(final_model)

cat("--- Table 4: Final model coefficients ---\n")
print(final_coef_table)
cat("\n")


# --- 11. Final model diagnostics reported in the appendix ---
# Big result: the log transformation improves the diagnostics, although some
# mild upper-tail deviation may remain in the Q-Q plot.

par(mfrow = c(2, 2))
plot(final_model)
par(mfrow = c(1, 1))

plot(
  cooks.distance(final_model),
  type = "h",
  main = "Cook's Distance for Final Model",
  ylab = "Cook's distance"
)
abline(h = 4 / nrow(ev), col = "red", lty = 2)


# --- 12. Pairwise plot used in the appendix ---

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
