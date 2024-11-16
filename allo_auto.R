# Allo-Auto Data ----------------------------------------------------------
# Survival Analysis Ch. 9 -------------------------------------------------


# Packages ----------------------------------------------------------------
library("tidyverse")
library("KMsurv")
library("survival")
library("scales", warn.conflicts = FALSE) 
library("patchwork")
library("tinytex")
theme_update(panel.grid.minor = element_blank())

baylor_green <- "#154734"
baylor_gold <- "#ffb81c"

aa <- data("alloauto")


# Fit the survival model for each group and extract cumulative hazards
km_fit_groups <- alloauto |> 
  group_by(type) %>%
  do(
    fit = survfit(Surv(time, delta) ~ 1, data = .)
  ) |> 
  mutate(
    time = list(fit$time),
    cumhaz = list(fit$cumhaz)
  ) |> 
  unnest(c(time, cumhaz)) |> 
  select(type, time, cumhaz) |> mutate(type = as.factor(type))

allo_fit <- alloauto %>%
  filter(type == 1) %>%
  survfit(Surv(time, delta) ~ 1, data = .)

auto_fit <- alloauto %>%
  filter(type == 2) %>%
  survfit(Surv(time, delta) ~ 1, data = .)

tb <- tibble(
  "Time" = 1:length(auto_fit$cumhaz[-49]),
  "Allo" = c(allo_fit$cumhaz, NA),
  "Auto" = auto_fit$cumhaz[-49]
) |> pivot_longer(
  cols = c("Allo", "Auto"),
  names_to = "type"
) |> mutate(
  l_time = log(Time),
  l_val = log(value),
  l_norm = qnorm(1 - exp(-value)),
  l_logistic = log(exp(value - 1))
) 

# 1st Plot Attempt --------------------------------------------------------
# Figure 12.1

p1 <- tb |> 
  ggplot(aes(Time, value, colour = type)) +
  geom_step() +
  scale_color_manual(values = c(baylor_gold, baylor_green)) +
  labs(title = "Exponential",
       x = "Time",
       y = "Est. Cumulative Hazard",
       color = "Group") +
  theme_minimal(base_size = 10) +
  theme(legend.position = "none")


# Figure 12.2 -------------------------------------------------------------


p2 <- tb |> 
  ggplot(aes(l_time, l_val, colour = type)) +
  geom_step() +
  scale_color_manual(values = c(baylor_gold, baylor_green)) +
  labs(title = "Weibull",
       x = "Log Time",
       y = "Log Est. Cumulative Hazard",
       color = "Group")  +
  theme_minimal(base_size = 10)



# Figure 12.3 -------------------------------------------------------------

p3 <- tb |> 
  ggplot(aes(l_time, l_norm, colour = type)) +
  geom_step() +
  scale_color_manual(values = c(baylor_gold, baylor_green)) +
  labs(title = "Log-Normal",
       x = "Log Time",
       y = "Probit(1 - exp(H(t)))",
       color = "Group") +
  theme_minimal(base_size = 10) +
  theme(legend.position = "none")


# Figure 12.4 -------------------------------------------------------------

p4 <- tb |> 
  ggplot(aes(l_time, l_logistic, colour = type)) +
  geom_step() +
  scale_color_manual(values = c(baylor_gold, baylor_green)) +
  labs(title = "Log-Logistic",
       x = "Log Time",
       y = "Log Odds",
       color = "Group") +
  theme_minimal(base_size = 10) +
  theme(legend.position = "none")


(p1 + p2) / (p3 + p4)