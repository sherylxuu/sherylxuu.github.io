setwd("/Users/itsxuuu/Desktop/462 Marketing Models/Data")
install.packages("dplyr")
library(dplyr)
install.packages("MatchIt")
library(MatchIt)
install.packages("cobalt")
library(cobalt)
install.packages("plm")  
library(plm)
install.packages("car")
library(car)

# Turn off scientific notation globally:
options(scipen = 999)

# (Optional) Also increase the default number of printed digits if you want more decimal places:
options(digits = 6)

# Filter for data of week 1
data = read.csv("kakao_all.csv")
data_before = data%>%filter(week ==1)

full_panel <- read.csv("kakao_all.csv") %>% 
  filter(week %in% c(1, 2))

### PSM
# Nearest Neighbor Three-to-One PSM With Replacement and With 0.25 Caliper
M1 = matchit(tg ~ age + income + education + gender +
               t_kakao_talk + t_kakao_story + t_kakao_game + n_kakao_game+
               t_non_kakao_talk + t_non_kakao_story + t_non_kakao_game + t_non_kakao + 
               n_non_kakao_talk + n_non_kakao_story + n_non_kakao_game + n_non_kakao, 
             data = data_before, method = "nearest", replace = T, ratio = 3, caliper = 0.25)
summary(M1)
plot(M1, type = "hist")
m1_pre   <- match.data(M1)   # contains only week 1 for matched units
matched_ids1 <- m1_pre %>% select(panel_id, weights)
# Use the full two‐week `data` (not data_before) and inner_join to keep only matched users
matched_panel1 <- data %>%
  filter(week %in% c(1, 2)) %>%   # keep both weeks
  inner_join(matched_ids1, by = "panel_id")

# Nearest Neighbor Three-to-One PSM Without Replacement and With 0.25 Caliper
M2 = matchit(tg ~ age + income + education + gender +
               t_kakao_talk + t_kakao_story + t_kakao_game + n_kakao_game+
               t_non_kakao_talk + t_non_kakao_story + t_non_kakao_game + t_non_kakao + 
               n_non_kakao_talk + n_non_kakao_story + n_non_kakao_game + n_non_kakao, 
             data = data_before, method = "nearest", replace = F, ratio = 3, caliper = 0.25)
summary(M2)
m2_pre   <- match.data(M2)   # contains only week 1 for matched units
matched_ids2 <- m2_pre %>% select(panel_id, weights)
matched_panel2 <- data %>%
  filter(week %in% c(1, 2)) %>%   # keep both weeks
  inner_join(matched_ids2, by = "panel_id")

# Nearest Neighbor Two-to-One PSM With Replacement and With 0.25 Caliper
M3 = matchit(tg ~ age + income + education + gender +
               t_kakao_talk + t_kakao_story + t_kakao_game + n_kakao_game+
               t_non_kakao_talk + t_non_kakao_story + t_non_kakao_game + t_non_kakao + 
               n_non_kakao_talk + n_non_kakao_story + n_non_kakao_game + n_non_kakao, 
             data = data_before, method = "nearest", replace = T, ratio = 2, caliper = 0.25)
summary(M3)
m3_pre   <- match.data(M3)   # contains only week 1 for matched units
matched_ids3 <- m3_pre %>% select(panel_id, weights)
matched_panel3 <- data %>%
  filter(week %in% c(1, 2)) %>%   # keep both weeks
  inner_join(matched_ids3, by = "panel_id")

# Nearest Neighbor One-to-One PSM Without Replacement and With 0.2 Caliper
M4 = matchit(tg ~ age + income + education + gender +
               t_kakao_talk + t_kakao_story + t_kakao_game + n_kakao_game+
               t_non_kakao_talk + t_non_kakao_story + t_non_kakao_game + t_non_kakao + 
               n_non_kakao_talk + n_non_kakao_story + n_non_kakao_game + n_non_kakao, 
             data = data_before, method = "nearest", replace = F, ratio = 1, caliper = 0.2)
summary(M4)
m4_pre   <- match.data(M4)   # contains only week 1 for matched units
matched_ids4 <- m4_pre %>% select(panel_id, weights)
matched_panel4 <- data %>%
  filter(week %in% c(1, 2)) %>%   # keep both weeks
  inner_join(matched_ids2, by = "panel_id")

# Nearest Neighbor Three-to-One PSM With Replacement and With 0.2 Caliper
M5 = matchit(tg ~ age + income + education + gender +
               t_kakao_talk + t_kakao_story + t_kakao_game + n_kakao_game+
               t_non_kakao_talk + t_non_kakao_story + t_non_kakao_game + t_non_kakao + 
               n_non_kakao_talk + n_non_kakao_story + n_non_kakao_game + n_non_kakao, 
             data = data_before, method = "nearest", replace = T, ratio = 3, caliper = 0.2)
summary(M5)
m5_pre   <- match.data(M5)   # contains only week 1 for matched units
matched_ids5 <- m5_pre %>% select(panel_id, weights)
matched_panel5 <- data %>%
  filter(week %in% c(1, 2)) %>%   # keep both weeks
  inner_join(matched_ids5, by = "panel_id")

# Nearest Neighbor One-to-One PSM Without Replacement and With 0.25 Caliper
M6 = matchit(tg ~ age + income + education + gender +
               t_kakao_talk + t_kakao_story + t_kakao_game + n_kakao_game+
               t_non_kakao_talk + t_non_kakao_story + t_non_kakao_game + t_non_kakao + 
               n_non_kakao_talk + n_non_kakao_story + n_non_kakao_game + n_non_kakao, 
             data = data_before, method = "nearest", replace = F, ratio = 1, caliper = 0.25)
summary(M6)
m6_pre   <- match.data(M6)   # contains only week 1 for matched units
matched_ids6 <- m6_pre %>% select(panel_id, weights)
matched_panel6 <- data %>%
  filter(week %in% c(1, 2)) %>%   # keep both weeks
  inner_join(matched_ids6, by = "panel_id")

# Nearest Neighbor Two-to-One PSM Without Replacement and With 0.25 Caliper
M7 = matchit(tg ~ age + income + education + gender +
               t_kakao_talk + t_kakao_story + t_kakao_game + n_kakao_game+
               t_non_kakao_talk + t_non_kakao_story + t_non_kakao_game + t_non_kakao + 
               n_non_kakao_talk + n_non_kakao_story + n_non_kakao_game + n_non_kakao, 
             data = data_before, method = "nearest", replace = F, ratio = 2, caliper = 0.25)
summary(M7)
m7_pre   <- match.data(M7)   # contains only week 1 for matched units
matched_ids7 <- m7_pre %>% select(panel_id, weights)
matched_panel7 <- data %>%
  filter(week %in% c(1, 2)) %>%   # keep both weeks
  inner_join(matched_ids7, by = "panel_id")

# Nearest Neighbor Two-to-One PSM With Replacement and With 0.2 Caliper
M8 = matchit(tg ~ age + income + education + gender +
               t_kakao_talk + t_kakao_story + t_kakao_game + n_kakao_game+
               t_non_kakao_talk + t_non_kakao_story + t_non_kakao_game + t_non_kakao + 
               n_non_kakao_talk + n_non_kakao_story + n_non_kakao_game + n_non_kakao, 
             data = data_before, method = "nearest", replace = T, ratio = 2, caliper = 0.2)
summary(M8)
m8_pre   <- match.data(M8)   # contains only week 1 for matched units
matched_ids8 <- m8_pre %>% select(panel_id, weights)
matched_panel8 <- data %>%
  filter(week %in% c(1, 2)) %>%   # keep both weeks
  inner_join(matched_ids8, by = "panel_id")

# Nearest Neighbor One-to-One PSM Without Replacement and With 0.1 Caliper
M9 = matchit(tg ~ age + income + education + gender +
               t_kakao_talk + t_kakao_story + t_kakao_game + n_kakao_game+
               t_non_kakao_talk + t_non_kakao_story + t_non_kakao_game + t_non_kakao + 
               n_non_kakao_talk + n_non_kakao_story + n_non_kakao_game + n_non_kakao, 
             data = data_before, method = "nearest", replace = F, ratio = 1, caliper = 0.1)
summary(M9)
m9_pre   <- match.data(M9)   # contains only week 1 for matched units
matched_ids9 <- m9_pre %>% select(panel_id, weights)
matched_panel9 <- data %>%
  filter(week %in% c(1, 2)) %>%   # keep both weeks
  inner_join(matched_ids9, by = "panel_id")

# Nearest Neighbor One-to-One PSM Without Replacement and Without Caliper
M10 = matchit(tg ~ age + income + education + gender +
               t_kakao_talk + t_kakao_story + t_kakao_game + n_kakao_game+
               t_non_kakao_talk + t_non_kakao_story + t_non_kakao_game + t_non_kakao + 
               n_non_kakao_talk + n_non_kakao_story + n_non_kakao_game + n_non_kakao, 
             data = data_before, method = "nearest",ratio = 1)
summary(M10)
m10_pre   <- match.data(M10)   # contains only week 1 for matched units
matched_ids10 <- m10_pre %>% select(panel_id, weights)
matched_panel10 <- data %>%
  filter(week %in% c(1, 2)) %>%   # keep both weeks
  inner_join(matched_ids10, by = "panel_id")


# Descriptive stats
data = read.csv("kakao_all.csv")
summary(data)
install.packages("dplyr")
library(dplyr)

data %>%
  summarize(
    N_t_time      = sum(!is.na(t_kakao_game)),
    Mean_t_time   = mean(t_kakao_game, na.rm = TRUE),
    SD_t_time     = sd(t_kakao_game,   na.rm = TRUE),
    Min_t_time    = min(t_kakao_game,  na.rm = TRUE),
    Q1_t_time     = quantile(t_kakao_game, 0.25, na.rm = TRUE),
    Median_t_time = median(t_kakao_game, na.rm = TRUE),
    Q3_t_time     = quantile(t_kakao_game, 0.75, na.rm = TRUE),
    Max_t_time    = max(t_kakao_game,  na.rm = TRUE),
    
    N_n_games      = sum(!is.na(n_kakao_game)),
    Mean_n_games   = mean(n_kakao_game, na.rm = TRUE),
    SD_n_games     = sd(n_kakao_game,   na.rm = TRUE),
    Min_n_games    = min(n_kakao_game,  na.rm = TRUE),
    Q1_n_games     = quantile(n_kakao_game, 0.25, na.rm = TRUE),
    Median_n_games = median(n_kakao_game, na.rm = TRUE),
    Q3_n_games     = quantile(n_kakao_game, 0.75, na.rm = TRUE),
    Max_n_games    = max(n_kakao_game,  na.rm = TRUE)
  )

# Visualization
library(ggplot2)

# 1. Boxplot of t_kakao_game
ggplot(data, aes(x = "", y = t_kakao_game)) +
  geom_boxplot(fill = "lightblue", outlier.color = "red") +
  labs(
    title = "Boxplot of Time Spent on Kakao-Platform Games",
    y     = "t_kakao_game (seconds)",
    x     = NULL
  ) +
  theme_minimal()

# 2. Boxplot of n_kakao_game
ggplot(data, aes(x = "", y = n_kakao_game)) +
  geom_boxplot(fill = "lightgreen", outlier.color = "red") +
  labs(
    title = "Boxplot of Number of Kakao-Platform Games Played",
    y     = "n_kakao_game (count)",
    x     = NULL
  ) +
  theme_minimal()

# 3. Histogram + density for t_kakao_game
ggplot(data, aes(x = t_kakao_game)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", color = "white") +
  geom_density(color = "darkblue", size = 1) +
  labs(
    title = "Histogram & Density of t_kakao_game",
    x     = "t_kakao_game (seconds)",
    y     = "Density"
  ) +
  theme_minimal()

# 4. Histogram + density for n_kakao_game
ggplot(data, aes(x = n_kakao_game)) +
  geom_histogram(aes(y = ..density..), bins = 20, fill = "lightgreen", color = "white") +
  geom_density(color = "darkgreen", size = 1) +
  labs(
    title = "Histogram & Density of n_kakao_game",
    x     = "n_kakao_game (count)",
    y     = "Density"
  ) +
  theme_minimal()

### Panel DID Models
# Dummy‐Variable Regression on the matched sample
did_dummy <- lm(
  formula = n_kakao_game ~ 
    + as.factor(ii)
  + as.factor(week)
  + age + income + education + gender
  + t_kakao_talk + t_kakao_story + t_kakao_game
  + t_non_kakao_talk + t_non_kakao_story + t_non_kakao_game + t_non_kakao
  + n_non_kakao_talk + n_non_kakao_story + n_non_kakao_game + n_non_kakao,       
  data    = matched_panel1)
summary(did_dummy)

#One way Fixed Effects Estimation on the matched sample
# Convert to a pdata.frame so that plm knows how to interpret panel_id & week
pdata1 <- pdata.frame(matched_panel1, index = c("panel_id", "week"))

did_fe <- plm(
  formula = n_kakao_game ~ as.factor(ii) + as.factor(week)
  + age + income + education + gender
  + t_kakao_talk + t_kakao_story + t_kakao_game
  + t_non_kakao_talk + t_non_kakao_story + t_non_kakao_game + t_non_kakao
  + n_non_kakao_talk + n_non_kakao_story + n_non_kakao_game + n_non_kakao,
  data   = pdata1,
  model  = "within")
summary(did_fe)




