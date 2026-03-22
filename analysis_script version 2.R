library(dplyr)
library(ggplot2)

data_path <- "EV_cars.csv"
premium_brands <- c("Tesla", "BMW", "Audi", "Mercedes-Benz", "Mercedes", "Porsche")

ev_raw <- read.csv(data_path, header = TRUE, na.strings = c("NA", ""))

ev <- ev_raw %>%
  mutate(
    manufacturer = sub(" .*", "", Car_name),
    market_segment = ifelse(manufacturer %in% premium_brands, "Premium", "Non-premium"),
    market_segment = factor(market_segment, levels = c("Non-premium", "Premium")),
    Acceleration_0_100 = acceleration..0.100.,
    log_price = log(Price.DE.)
  ) %>%
  select(
    Car_name, manufacturer, market_segment, Price.DE., log_price,
    Battery, Range, Efficiency, Top_speed, Acceleration_0_100, Fast_charge
  ) %>%
  filter(!is.na(Price.DE.), !is.na(Battery), !is.na(market_segment))


scatter_plot <- ggplot(ev, aes(x = Battery, y = Price.DE., color = market_segment)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Original Price vs Battery Capacity", x = "Battery (kWh)", y = "Price (EUR)")
print(scatter_plot)


scatter_log_plot <- ggplot(ev, aes(x = Battery, y = log_price, color = market_segment)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Log-Price vs Battery Capacity (More Linear)", x = "Battery (kWh)", y = "log(Price)")
print(scatter_log_plot)


backward_keep_required <- function(data, response, required_terms, optional_terms, alpha = 0.05) {
  current_optional <- optional_terms
  repeat {
    current_terms <- c(required_terms, current_optional)
    current_formula <- as.formula(paste(response, "~", paste(current_terms, collapse = " + ")))
    current_model <- lm(current_formula, data = data)
    
    if (length(current_optional) == 0) break
    
    drop_table <- drop1(current_model, test = "F")
    removable <- intersect(rownames(drop_table), current_optional)
    if (length(removable) == 0) break
    
    removable_table <- drop_table[removable, , drop = FALSE]
    worst_term <- rownames(removable_table)[which.max(removable_table$`Pr(>F)`)]
    worst_p <- max(removable_table$`Pr(>F)`, na.rm = TRUE)
    
    if (is.na(worst_p) || worst_p <= alpha) break
    current_optional <- setdiff(current_optional, worst_term)
  }
  
  final_formula <- as.formula(paste(response, "~", paste(c(required_terms, current_optional), collapse = " + ")))
  return(list(final_model = lm(final_formula, data = data), kept = current_optional))
}


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
cat("\nFinal Refined Model (Log-Transformed):\n")
print(summary(final_model))


par(mfrow = c(2, 2))
plot(final_model, main = "Log-Model Diagnostics")
par(mfrow = c(1, 1))

plot(cooks.distance(final_model), type = "h", main = "Cook's Distance (Log-Model)")
abline(h = 4 / nrow(ev), col = "red", lty = 2)