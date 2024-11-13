# Allo-Auto Data ----------------------------------------------------------
# Survival Analysis Ch. 9 -------------------------------------------------


# Packages ----------------------------------------------------------------
library("tidyverse")
library("KMsurv")
library("survival")

aa <- data("alloauto")

# Recreate Figure 9.1 -----------------------------------------------------

# Assume there is a grouping variable (e.g., alloauto$group)
# If no grouping variable exists, replace 'group' with the appropriate column name in your dataset.
# Example uses 'group' as a placeholder; adjust if necessary.

# Fit the survival model for each group and extract cumulative hazards
km_fit_groups <- alloauto %>%
  group_by(type) %>%
  do(
    fit = survfit(Surv(time, delta) ~ 1, data = .)
  ) %>%
  mutate(
    time = list(fit$time),
    cumhaz = list(fit$cumhaz)
  ) %>%
  unnest(c(time, cumhaz)) |> 
  select(type, time, cumhaz) |> mutate(type = as.factor(type))

# Plot the log cumulative hazard for each group
ggplot(km_fit_groups, aes(x = time, y = log(cumhaz), group = type, color = type)) +
  geom_step() +
  labs(x = "Time", y = "Log Cumulative Hazard", 
       title = "Log Estimated Cumulative Hazard by Group") +
  xlim(0, 30) +
  theme_minimal()

# 
# # Fit a Kaplan-Meier survival curve
# km_fit <- survfit(Surv(time, event = delta) ~ type, data = alloauto)
# 
# plot(km_fit)
# 
# # Convert survival function to cumulative hazard
# cumulative_hazard <- log(km_fit$cumhaz)
# 
# # Create a data frame for plotting
# cumhaz_data <- data.frame(
#   time = km_fit$time,
#   cumulative_hazard = cumulative_hazard
# )
# 
# # Plot the cumulative hazard function
# ggplot(cumhaz_data, aes(x = time, y = cumulative_hazard)) +
#   geom_step() +
#   labs(x = "Time", y = "Cumulative Hazard", title = "Estimated Cumulative Hazard Function") +
#   theme_minimal()
# 
