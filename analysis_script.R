install.packages("here")
library(dplyr)
library(ggplot2)
library(knitr)
library(broom)
library(here)


# --- 1. Data Import and Initial Cleaning ---
# Use here() to build the path from the project root
data_path <- here("data", "EV_cars.csv")
premium_brands <- c("Tesla", "BMW", "Audi", "Mercedes-Benz", "Mercedes", "Porsche")

# Read raw data and handle empty strings as NA
ev_raw <- read.csv(data_path, header = TRUE, na.strings = c("NA", ""))

# Initial data transformation
ev <- ev_raw %>%
  mutate(
    # Extract manufacturer name from the first word of Car_name
    manufacturer = sub(" .*", "", Car_name),
    # Handle specific multi-word brand edge case
    manufacturer = ifelse(manufacturer == "Mercedes-Benz", "Mercedes-Benz", manufacturer),
    # Group into Premium vs Non-premium segments
    market_segment = ifelse(manufacturer %in% premium_brands, "Premium", "Non-premium"),
    market_segment = factor(market_segment, levels = c("Non-premium", "Premium")),
    # Clean variable name for acceleration
    Acceleration_0_100 = acceleration..0.100.,
    # Pre-calculate log price for modeling
    log_price = log(Price.DE.)
  ) %>%
  select(
    Car_name, Car_name_link, manufacturer, market_segment, 
    Price.DE., log_price, Battery, Range, Efficiency, 
    Top_speed, Acceleration_0_100, Fast_charge
  ) %>%
  # Filter out missing values for core analysis variables
  filter(
    !is.na(Price.DE.),
    !is.na(Battery),
    !is.na(market_segment)
  )

# --- 2. Data Quality & Missingness Check ---
cat("--- Data Cleaning Summary ---\n")
cat("Rows in cleaned analysis dataset:", nrow(ev), "\n")
cat("Missing prices in raw data:", sum(is.na(ev_raw$Price.DE.)), "\n")

# Check if missing prices are concentrated in specific car types
ev_missing_check <- ev_raw %>%
  mutate(
    price_missing = is.na(Price.DE.),
    Acceleration_0_100 = acceleration..0.100.
  )

missing_price_comparison <- ev_missing_check %>%
  group_by(price_missing) %>%
  summarise(
    mean_battery = mean(Battery, na.rm = TRUE),
    mean_range = mean(Range, na.rm = TRUE),
    mean_top_speed = mean(Top_speed, na.rm = TRUE)
  )
print(missing_price_comparison)

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

# Plot 1: Original Scale (Helps identify non-linearity and heteroscedasticity)
scatter_plot <- ggplot(ev, aes(x = Battery, y = Price.DE., color = market_segment)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Original Price vs Battery Capacity", 
       x = "Battery (kWh)", 
       y = "Price (EUR)")
print(scatter_plot)

# Plot 2: Log Scale (Shows improved linearity and variance stabilization)
scatter_log_plot <- ggplot(ev, aes(x = Battery, y = log_price, color = market_segment)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Log-Price vs Battery Capacity (More Linear)", 
       x = "Battery (kWh)", 
       y = "log(Price)")
print(scatter_log_plot)

# --- 4. Correlation Analysis ---
quant_vars <- ev %>%
  select(Price.DE., Battery, Range, Efficiency, Top_speed, Acceleration_0_100, Fast_charge)
cat("\n--- Correlation Matrix (Checking for Multicollinearity) ---\n")
print(round(cor(quant_vars, use = "complete.obs"), 2))

# --- 5. Backward Elimination Function ---
# This function performs stepwise selection while keeping research-relevant terms fixed
backward_keep_required <- function(data, response, required_terms, optional_terms, alpha = 0.05) {
  current_optional <- optional_terms
  repeat {
    current_terms <- c(required_terms, current_optional)
    current_formula <- as.formula(paste(response, "~", paste(current_terms, collapse = " + ")))
    current_model <- lm(current_formula, data = data)
    
    if (length(current_optional) == 0) break
    
    # Use F-tests to determine the least significant optional predictor
    drop_table <- drop1(current_model, test = "F")
    removable <- intersect(rownames(drop_table), current_optional)
    if (length(removable) == 0) break
    
    removable_table <- drop_table[removable, , drop = FALSE]
    worst_term <- rownames(removable_table)[which.max(removable_table$`Pr(>F)`)]
    worst_p <- max(removable_table$`Pr(>F)`, na.rm = TRUE)
    
    # Stop if no optional term has a p-value higher than the threshold (alpha)
    if (is.na(worst_p) || worst_p <= alpha) break
    current_optional <- setdiff(current_optional, worst_term)
  }
  final_formula <- as.formula(paste(response, "~", paste(c(required_terms, current_optional), collapse = " + ")))
  return(list(final_model = lm(final_formula, data = data), kept = current_optional))
}

# --- 6. Model Selection (Log-Price Regression) ---
required_terms <- c("Battery", "market_segment", "Battery:market_segment")
optional_terms <- c("Range", "Efficiency", "Top_speed", "Acceleration_0_100", "Fast_charge")

selection_results <- backward_keep_required(
  data = ev,
  response = "log_price",
  required_terms = required_terms,
  optional_terms = optional_terms,
  alpha = 0.05
)

final_model <- selection_results$final_model
cat("\n--- Final Refined Model (Log-Transformed) ---\n")
print(summary(final_model))

# --- 7. Final Model Diagnostics ---
# Reviewing Residuals vs Fitted and Normal Q-Q to verify Gauss-Markov assumptions
par(mfrow = c(2, 2))
plot(final_model, main = "Log-Model Diagnostics")
par(mfrow = c(1, 1))

# --- 8. Influential Observations ---
# Identifying high-leverage points using Cook's Distance
plot(cooks.distance(final_model), type = "h", main = "Cook's Distance (Log-Model)")
# Threshold for influential points: 4/n
abline(h = 4 / nrow(ev), col = "red", lty = 2)