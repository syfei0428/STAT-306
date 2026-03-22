library(dplyr)
library(ggplot2)

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

cat("Rows in cleaned analysis dataset:", nrow(ev), "\n")
cat("Missing prices in raw data:", sum(is.na(ev_raw$Price.DE.)), "\n")
cat("Missing fast charge values in raw data:", sum(is.na(ev_raw$Fast_charge)), "\n")
print(table(ev$market_segment))

ev_missing_check <- ev_raw %>%
  mutate(
    manufacturer = sub(" .*", "", Car_name),
    manufacturer = ifelse(manufacturer == "Mercedes-Benz", "Mercedes-Benz", manufacturer),
    market_segment = ifelse(manufacturer %in% premium_brands, "Premium", "Non-premium"),
    price_missing = is.na(Price.DE.),
    Acceleration_0_100 = acceleration..0.100.
  )

missing_price_comparison <- ev_missing_check %>%
  group_by(price_missing) %>%
  summarise(
    mean_battery = mean(Battery, na.rm = TRUE),
    median_battery = median(Battery, na.rm = TRUE),
    mean_range = mean(Range, na.rm = TRUE),
    median_range = median(Range, na.rm = TRUE),
    mean_efficiency = mean(Efficiency, na.rm = TRUE),
    mean_top_speed = mean(Top_speed, na.rm = TRUE),
    mean_acceleration = mean(Acceleration_0_100, na.rm = TRUE),
    mean_fast_charge = mean(Fast_charge, na.rm = TRUE)
  )

cat("\nComparison of models with and without missing prices\n")
print(missing_price_comparison)
cat("\nMarket segment by price missingness\n")
print(table(ev_missing_check$market_segment, ev_missing_check$price_missing))

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
print(summary_table)

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
print(round(cor(quant_vars, use = "complete.obs"), 2))

pairs_data <- quant_vars
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

full_model <- lm(
  Price.DE. ~ Battery * market_segment +
    Range + Efficiency + Top_speed + Acceleration_0_100 + Fast_charge,
  data = ev
)
cat("\nInitial full model\n")
print(summary(full_model))

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

required_terms <- c("Battery", "market_segment", "Battery:market_segment")
optional_terms <- c("Range", "Efficiency", "Top_speed", "Acceleration_0_100", "Fast_charge")

selection_results <- backward_keep_required(
  data = ev,
  response = "Price.DE.",
  required_terms = required_terms,
  optional_terms = optional_terms,
  alpha = 0.05
)

cat("\nOptional variables kept:\n")
print(selection_results$kept_optional_terms)
cat("\nOptional variables removed:\n")
print(selection_results$removed_optional_terms)

final_model <- selection_results$final_model
cat("\nSelected model\n")
print(summary(final_model))

par(mfrow = c(2, 2))
plot(final_model)
par(mfrow = c(1, 1))

plot(cooks.distance(final_model), type = "h",
     main = "Cook's Distance for Final Model",
     ylab = "Cook's distance")
abline(h = 4 / nrow(ev), col = "red", lty = 2)

log_model <- lm(
  log(Price.DE.) ~ Battery * market_segment +
    Range + Efficiency + Top_speed + Acceleration_0_100 + Fast_charge,
  data = ev
)

cat("\nLog-price model\n")
print(summary(log_model))


# improved Diagnostic plots by taking log(y)
model_log <- lm(log(Price.DE.) ~ Battery * market_segment + Efficiency + Top_speed + Fast_charge, data = ev)
summary(model_log)
par(mfrow = c(2, 2))
plot(model_log)
par(mfrow = c(1, 1))
