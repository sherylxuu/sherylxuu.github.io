# --- Start of HW1.R ---

setwd("/Users/itsxuuu/Desktop/463 Machine Learning/Data")

biz=read.csv("yelp.biz.sample.csv")

#1
linear_model = lm(biz.stars ~ elite_cnt + price_level + metro + biz_age, data = biz)
summary(linear_model)

linear_model1 = lm(biz.stars ~ elite_cnt + factor(price_level) + factor(metro) + biz_age, data = biz)
summary(linear_model1)

#7
plot(biz$elite_cnt, biz$biz_stars,
     xlab = "Number of Elite Users", ylab = "Average Star Rating",
     main = "Scatterplot of Elite Users vs. Average Star Rating")

log_model <- lm(biz.stars ~ log(elite_cnt + 1) + price_level, data = biz)
summary(log_model)

quad_model <- lm(biz.stars ~ elite_cnt + I(elite_cnt^2) + price_level, data = biz)
summary(quad_model)

cubic_model <- lm(biz.stars ~ elite_cnt + I(elite_cnt^2) + I(elite_cnt^3) + price_level, data = biz)
summary(cubic_model)

biz$price_level <- as.factor(biz$price_level)

# plot the models
install.packages(c("ggplot2", "tidyr", "dplyr"))

library(ggplot2)
library(tidyr)
library(dplyr)

# Ensure that price_level is a factor:
biz$price_level <- as.factor(biz$price_level)
print(levels(biz$price_level))  # to see available levels

# Fit the four models:
linear_model <- lm(biz.stars ~ elite_cnt + price_level, data = biz)
quad_model   <- lm(biz.stars ~ elite_cnt + I(elite_cnt^2) + price_level, data = biz)
log_model    <- lm(biz.stars ~ log(elite_cnt + 1) + price_level, data = biz)
cubic_model  <- lm(biz.stars ~ elite_cnt + I(elite_cnt^2) + I(elite_cnt^3) + price_level, data = biz)

# Optionally check the summaries:
summary(linear_model)
summary(quad_model)
summary(log_model)
summary(cubic_model)

# Create a sequence of elite_cnt values, spanning the range in your data:
elite_min <- min(biz$elite_cnt, na.rm = TRUE)
elite_max <- max(biz$elite_cnt, na.rm = TRUE)
elite_seq <- seq(from = elite_min, to = elite_max, length.out = 100)

# Define the fixed price level (using the first level in the factor)
fixed_price <- levels(biz$price_level)[1]

# Create a new data frame for predictions (replicating the fixed price level):
new_data <- data.frame(
  elite_cnt = elite_seq,
  price_level = factor(rep(fixed_price, length(elite_seq)),
                       levels = levels(biz$price_level))
)

# Generate predictions from each model:
new_data$pred_linear <- predict(linear_model, newdata = new_data)
new_data$pred_quad   <- predict(quad_model, newdata = new_data)
new_data$pred_log    <- predict(log_model, newdata = new_data)
new_data$pred_cubic  <- predict(cubic_model, newdata = new_data)

# Define a common y-axis range based on all predictions:
y_range <- range(c(new_data$pred_linear, new_data$pred_quad, 
                   new_data$pred_log, new_data$pred_cubic), na.rm = TRUE)

# Base R plotting:
plot(new_data$elite_cnt, new_data$pred_linear, type = "l", lwd = 2, col = "blue",
     ylim = y_range,
     xlab = "Number of Elite Users", 
     ylab = "Predicted Average Star Rating",
     main = "Model Predictions: Linear, Quadratic, Log, & Cubic Models")

# Add the quadratic model predictions (red line):
lines(new_data$elite_cnt, new_data$pred_quad, col = "red", lwd = 2)

# Add the log model predictions (green line):
lines(new_data$elite_cnt, new_data$pred_log, col = "green", lwd = 2)

# Add the cubic model predictions (purple line):
lines(new_data$elite_cnt, new_data$pred_cubic, col = "purple", lwd = 2)

# Add a legend:
legend("topright", legend = c("Linear", "Quadratic", "Log", "Cubic"),
       col = c("blue", "red", "green", "purple"), lty = 1, lwd = 2)

# AIC and BIC values

AIC(linear_model, quad_model, log_model, cubic_model)
BIC(linear_model, quad_model, log_model, cubic_model)




#8

# Suppose 'analysis_data' contains your columns of interest.
cor(biz[, c("elite_cnt", "biz_age", "price_level", 
                      "biz.rws.cnt", "repeated_cnt")], 
    use = "complete.obs")

#VIF
install.packages("car")
library(car)
model_new <- lm(biz.stars ~ elite_cnt + price_level + metro + biz_age +
                  biz.rws.cnt + repeated_cnt, data = biz)

vif(model_new)

#9.1
n=1000

coef_elite_cnt <- NULL

for (i in 1:n) {
  sample_d1 = biz[sample(1:nrow(biz), nrow(biz), replace = TRUE), ]
  linear_bootstrap <- lm(biz.stars ~ elite_cnt + price_level + metro + biz_age, data = sample_d1)
  #Saving the coefficients
  
  coef_elite_cnt <-
    c(coef_elite_cnt, linear_bootstrap$coefficients["elite_cnt"])
}

coefs1 <- rbind(coef_elite_cnt)

coefs_df1=t(as.data.frame(coefs1))
coefs_df1=as.data.frame(coefs_df1)
colnames(coefs_df1)

library(dplyr)
alpha=0.05 #95% confidence interval
coefs_df1 %>% 
  dplyr::summarize(mean = mean(coef_elite_cnt),
                   lower = mean(coef_elite_cnt) - qt(1- alpha/2, (n() - 1))*sd(coef_elite_cnt)/sqrt(n()),
                   upper = mean(coef_elite_cnt) + qt(1- alpha/2, (n() - 1))*sd(coef_elite_cnt)/sqrt(n()))


#9.2
linear_model1 = lm(biz.stars ~ elite_cnt + factor(price_level) + factor(metro) + biz_age, data = biz)
summary(linear_model1)

n=1000

coef_intercept <- NULL
coef_elite_cnt <- NULL
coef_price_level_1 <- NULL
coef_price_level_2 <- NULL
coef_price_level_3 <- NULL
coef_price_level_4 <- NULL
coef_Phoenix <- NULL
coef_Pittsburgh <- NULL
coef_Vegas <- NULL
coef_biz_age <- NULL

for (i in 1:n) {
  sample_d1 = biz[sample(1:nrow(biz), nrow(biz), replace = TRUE), ]
  linear_bootstrap <- lm(biz.stars ~ elite_cnt + factor(price_level) + factor(metro) + biz_age, data = sample_d1)
  #Saving the coefficients
  coef_intercept <-
    c(coef_intercept, linear_bootstrap$coefficients[1])
  coef_elite_cnt <-
    c(coef_elite_cnt, linear_bootstrap$coefficients[2])
  coef_price_level_1 <-
    c(coef_price_level_1, linear_bootstrap$coefficients[3])
  coef_price_level_2 <-
    c(coef_price_level_2, linear_bootstrap$coefficients[4])
  coef_price_level_3 <-
    c(coef_price_level_3, linear_bootstrap$coefficients[5])
  coef_price_level_4 <-
    c(coef_price_level_4, linear_bootstrap$coefficients[6])
  coef_Phoenix <-
    c(coef_Phoenix, linear_bootstrap$coefficients[7])
  coef_Pittsburgh <-
    c(coef_Pittsburgh, linear_bootstrap$coefficients[8])
  coef_Vegas <-
    c(coef_Vegas, linear_bootstrap$coefficients[9])
  coef_biz_age <-
    c(coef_biz_age, linear_bootstrap$coefficients[10])
}
coefs2 <- rbind(coef_intercept, coef_elite_cnt, coef_price_level_1, coef_price_level_2,coef_price_level_3, 
                coef_price_level_4, coef_Phoenix, coef_Pittsburgh, coef_Vegas,coef_biz_age)

coefs_df2=t(as.data.frame(coefs2))
coefs_df2=as.data.frame(coefs_df2)
colnames(coefs_df2)

mean_elite2 <- mean(coefs_df2$coef_elite_cnt)

library(dplyr)
alpha=0.05 #95% confidence interval
coefs_df1 %>% 
  dplyr::summarize(mean = mean(coef_elite_cnt),
                   lower = mean(coef_elite_cnt) - qt(1- alpha/2, (n() - 1))*sd(coef_elite_cnt)/sqrt(n()),
                   upper = mean(coef_elite_cnt) + qt(1- alpha/2, (n() - 1))*sd(coef_elite_cnt)/sqrt(n()))






#10
# Load necessary packages
library(dplyr)
library(ggplot2)

# Assume that your data is stored in the data frame "biz"
# and that biz$price_level is a factor indicating the different price levels.

# Step 1: Summarize the Data by Price Level

descriptives <- biz %>%
  group_by(price_level) %>%
  summarize(
    n = n(), 
    mean_star = mean(biz.stars, na.rm = TRUE),
    sd_star = sd(biz.stars, na.rm = TRUE)
  ) %>%
  mutate(
    se_star = sd_star / sqrt(n),                   # Standard Error
    # Construct a 95% CI assuming a t-distribution
    t_val = qt(0.975, df = n - 1),
    ci_lower = mean_star - t_val * se_star,
    ci_upper = mean_star + t_val * se_star
  )

# View the summarized output
print(descriptives)

# Visualize
ggplot(descriptives, aes(x = price_level, y = mean_star)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, color = "blue") +
  labs(title = "Average Star Ratings by Price Level",
       x = "Price Level",
       y = "Average Star Rating") +
  theme_minimal() +
  theme(text = element_text(size = 14))

# Bar chart with error bars for each price level
ggplot(descriptives, aes(x = price_level, y = mean_star, fill = price_level)) +
  geom_bar(stat = "identity", color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  scale_fill_brewer(palette = "Purples") +
  labs(title = "Average Star Ratings by Price Level",
       x = "Price Level",
       y = "Average Star Rating") +
  theme_minimal() +
  theme(text = element_text(size = 14),
        legend.position = "none")

# Make sure to install and load the patchwork package if you haven't already
# install.packages("patchwork")
library(ggplot2)
library(patchwork)

# Plot 1: Scatter plot with error bars
p1 <- ggplot(descriptives, aes(x = price_level, y = mean_star)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, color = "blue") +
  labs(title = "Average Star Ratings by Price Level",
       x = "Price Level",
       y = "Average Star Rating") +
  theme_minimal() +
  theme(text = element_text(size = 14))

# Plot 2: Bar chart with error bars and purple-ish palette
p2 <- ggplot(descriptives, aes(x = price_level, y = mean_star, fill = price_level)) +
  geom_bar(stat = "identity", color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  scale_fill_brewer(palette = "Purples") +
  labs(title = "Average Star Ratings by Price Level",
       x = "Price Level",
       y = "Average Star Rating") +
  theme_minimal() +
  theme(text = element_text(size = 14),
        legend.position = "none")

# Combine the two plots side by side
combined_plot <- p1 + p2 + plot_layout(ncol = 2)
print(combined_plot)


# --- End of HW1.R ---


# --- Start of Homework3-copy.R ---

setwd("/Users/itsxuuu/Desktop/463 Machine Learning/Data")
yelp = read.csv("yelp.biz.sample_W3.csv")
str(yelp)

#1.1
yelp$has_elite = ifelse(yelp$elite_cnt > 0, 1, 0)
glm1.1=glm(has_elite ~ rst.stars + price_level, data=yelp, family=binomial)
summary(glm1.1)

#1.2
library(emmeans) 
pairs(emmeans(glm1.1, ~ price_level, type="response"), reverse = T)

#1.3
glm1.3=glm(has_elite ~ rst.stars + price_level + factor(prime_location) + dist_destination + factor(health_alarm), data = yelp, family = binomial)
summary(glm1.3)

#1.4
anova(glm1.1, glm1.3, test="LRT")

#2.1
library(nnet)
multinom=multinom(price_level ~ factor(has_elite), data=yelp, maxit=1000)
summary(multinom)
install.packages("stargazer")
stargazer::stargazer(multinom, type = "text")

#2.2
emmeans(multinom, ~ has_elite|price_level, mode="prob")
pairs(emmeans(multinom, ~ has_elite|price_level, mode="prob"))

#3.1 3.2
yelp$price_level.f=paste0(yelp$price_level," pricelevel")
yelp$price_level.f=as.factor(yelp$price_level.f)
yelp$has_elite.f=as.factor(yelp$has_elite)
library(MASS)
ordinal <- polr(price_level.f ~ has_elite.f, data = yelp, Hess = TRUE)
summary(ordinal)
ctable <- coef(summary(ordinal))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ctable

#3.3
emmeans(ordinal, ~ has_elite.f|price_level.f, mode = "prob") 
pairs(emmeans(ordinal, ~ has_elite.f|price_level.f, mode = "prob"))

#3.4
library(brant)
brant(ordinal)

# --- End of Homework3-copy.R ---


# --- Start of 463 Week4 HW.R ---

setwd("/Users/itsxuuu/Desktop/463 Machine Learning/Data")
biz=read.csv("yelp.biz.sample_W3.csv")

biz$has_elite = ifelse(biz$elite_cnt > 0, 1, 0)

#1.1
m1<- glm(has_elite ~ rst.stars + price_level,
             data = biz, 
             family = binomial(link = "logit"))
summary(m1)

#1.2
library(emmeans) 
emmeans(m1, pairwise ~ price_level)
pairs(emmeans(m1, ~ price_level, type="response"), reverse = T)
pairs(emmeans(m1, ~ price_level, type="response"))


#1.3
m2<- glm(has_elite ~ rst.stars + price_level + prime_location + dist_destination + health_alarm,
         data = biz, 
         family = binomial(link = "logit"))
summary(m2)

#1.4
logLik(m1)
logLik(m2)
anova(m1, m2, test="LRT")

#2.1
install.packages("nnet")
library(nnet)
m3=multinom(price_level~factor(has_elite), 
                        data=biz, maxit=1000)
summary(m3)
stargazer::stargazer(m3, type = "text")

#2.2
emmeans(m3, ~ has_elite|price_level, mode="prob")
pairs(emmeans(m3, ~ has_elite|price_level, mode="prob"))

#3.1, 3.2
biz$price_level.f=paste0(biz$price_level," pricelevel")
biz$price_level.f=as.factor(biz$price_level.f)
biz$has_elite.f=as.factor(biz$has_elite)
library(MASS)
m4 <- polr(price_level.f ~ has_elite.f, data = biz, Hess = TRUE)
summary(m4)

ctable <- coef(summary(m4))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ctable

#3.3
emmeans(m4, ~ has_elite.f|price_level.f, mode = "prob") 
pairs(emmeans(m4, ~ has_elite.f|price_level.f, mode = "prob"))

#3.4
install.packages("brant")
library(brant)
brant(m4)


# --- End of 463 Week4 HW.R ---


# --- Start of 463 HW4-Week5.R ---

# 463-HW4
setwd('/Users/gracechen/Desktop/Quarter 3⃣️/IMC463 Machine Learning 1/HW/HW4')

yelp=read.csv("yelp.biz.sample.csv")
biz=read.csv("yelp.biz.sample.csv")

### Q1.1

biz$is_open <- factor(biz$is_open, levels=c(0,1))

glm.model1=glm(is_open ~ elite_cnt + price_level, data=biz, family=binomial)

summary(glm.model1)


### Q1.2
# predictions from the model
pred_probs <- predict(glm.model1, type = "response")


install.packages("pROC") #if you have not installed the package
library(pROC)
# ROC curve analysis
roc_obj <- roc(biz$is_open, pred_probs)

plot(roc_obj, main = "ROC Curve", print.auc = T, 
     legacy.axes = TRUE, lwd = 2)


#Cost-Sensitive Thresholding
cost_FP <- 55  
cost_FN <- 20  


library(dplyr)
# Calculate costs for all thresholds
# coords returns the coordinates of the ROC curve at one or several specified point(s).
costs <- coords(roc_obj, "all", 
                ret = c("threshold", "fp", "fn")) %>%
    mutate(total_cost = fp * cost_FP + fn * cost_FN)

# count the fp and fn under each thresholld
# coords(roc_obj, "all", ret = c("threshold", "fp", "fn"))


# Find optimal threshold: minimizing total cost
optimal_threshold <- costs$threshold[which.min(costs$total_cost)]
print(paste("Optimal threshold:", round(optimal_threshold, 3)))


### Q1.3
##** youden index: This threshold represents the point where the model achieves ** 
# Extract sensitivity, specificity, and thresholds
youden <- coords(roc_obj, "all", ret = c("threshold", "sensitivity", "specificity"))
youden


# Calculate Youden's J and find the optimal threshold
youden$youden_j <- youden$sensitivity + youden$specificity - 1
youden

optimal_idx <- which.max(youden$youden_j)
optimal_threshold2 <- youden$threshold[optimal_idx]
print(paste("Optimal Threshold (Youden's Index):", round(optimal_threshold2, 3)))

##** confusion matrix 方法一** 
# Step 1: Convert predicted probabilities into class predictions
predicted_class <- ifelse(pred_probs >= optimal_threshold2, 1, 0)

# Step 2: Create a confusion matrix
conf_matrix <- table(Predicted = predicted_class, Actual = biz$is_open)

# Step 3: Print the confusion matrix
print(conf_matrix)

##** confusion matrix 方法二** 
# count the tp tn fp and fn under optimal_threshold2
coords(roc_obj, optimal_threshold2, ret = c("threshold", "tp", "tn", "fp", "fn"))

## 表格
conf_matrix <- matrix(
    c(2963, 1215, 853, 935),
    nrow = 2,
    byrow = TRUE,
    dimnames = list(
        "Predicted" = c("Positive", "Negative"),
        "Actual" = c("Positive", "Negative")
    )
)

print(conf_matrix)



### Q1.4
# Step 1: Get all threshold-level stats from the ROC object
all_coords <- coords(roc_obj, "all", ret = c("threshold", "tp", "tn", "fp", "fn"))

# Step 2: Filter thresholds between 0.574 and 0.7
filtered_coords <- subset(all_coords, threshold >= 0.574 & threshold <= 0.7)

# Step 3: Calculate accuracy for each threshold
filtered_coords$accuracy <- (filtered_coords$tp + filtered_coords$tn) / 
    (filtered_coords$tp + filtered_coords$tn + filtered_coords$fp + filtered_coords$fn)

# Step 4: Compute mean accuracy
mean_accuracy <- mean(filtered_coords$accuracy)

# Step 5: Print result
cat("Mean Accuracy between thresholds 0.574 and 0.7 is", round(mean_accuracy, 4), "\n")


### Q1.5
glm.model2= glm(is_open ~ elite_cnt + price_level * biz.stars + repeated_cnt, biz, family = 
                  binomial)

summary(glm.model2)

# model 1
pred_probs1 <- predict(glm.model1, type = "response")

library(pROC)
roc_obj1 <- roc(biz$is_open, pred_probs1)
print(roc_obj1)

plot(roc_obj1, main = "ROC Curve", print.auc = T,
     legacy.axes = TRUE, lwd = 2)

# model 2
pred_probs2 <- predict(glm.model2, type = "response")

roc_obj2 <- roc(biz$is_open, pred_probs2)
print(roc_obj2)

plot(roc_obj2, main = "ROC Curve", print.auc = T,
     legacy.axes = TRUE, lwd = 2)



### Q1.6

#** Generate gains table **

biz$pred_prob2 <- pred_probs2 <- predict(glm.model2, type = "response")

str(biz)

install.packages("gains")
library(gains)
gains_table <- gains(actual = biz$is_open, predicted = biz$pred_prob2, 
                     groups = 10)
print(gains_table)


### Q2.1

set.seed(123)
train_indices <- sample(1:nrow(biz), size = 0.7 * nrow(biz))
                        
train_data <- biz[train_indices, ]
test_data <- biz[-train_indices, ]

# Model 1
glm.model1=glm(is_open ~ elite_cnt + price_level, data=biz, family=binomial)
# Model 2
glm.model2= glm(is_open ~ elite_cnt + price_level * biz.stars + repeated_cnt, biz, family = 
                    binomial)


# Predictions
train_pred1 <- predict(glm.model1, train_data, type = "response")
test_pred1 <- predict(glm.model1, test_data, type = "response")

train_pred2 <- predict(glm.model2, train_data, type = "response")
test_pred2 <- predict(glm.model2, test_data, type = "response")

library(pROC)
# Compare AUC
model1_auc <- c(
    auc(roc(train_data$is_open, train_pred1)),
    auc(roc(test_data$is_open, test_pred1)))

names(model1_auc) <- c("Train Set", "Test Set")
print(model1_auc)

model2_auc <- c(
    auc(roc(train_data$is_open, train_pred2)),
    auc(roc(test_data$is_open, test_pred2)))

names(model2_auc) <- c("Train Set", "Test Set")
print(model2_auc)


### Q2.2

###****************** 10-fold cross-validation *************************
glm.model3= glm(is_open ~ poly(elite_cnt, 2, raw=T) + price_level*biz.stars*repeated_cnt + city,
                biz, family=binomial)

summary(glm.model3)

# Set seed for reproducibility
set.seed(123)

# Define number of folds
k <- 10
folds <- cut(seq(1, nrow(data)), breaks = k, labels = FALSE)
table(folds)

biz$is_open_f <- factor(ifelse(biz$is_open==1, "open", "closed"),
                        levels=c("open","closed"))
str(biz)


# Load required library
library(pROC)

###********Model 1**********

# Initialize vector to store AUCs
auc_values1_test <- numeric(k)
auc_values1_train <- numeric(k)

# Perform k-fold CV
for(i in 1:k) {
    # Split into train and test sets
    test_indices <- which(folds == i)
    train_data <- biz[-test_indices, ]
    test_data <- biz[test_indices, ]
    
    model <- glm(is_open_f ~ elite_cnt + price_level,
                 data = train_data, family = binomial)
    
    # Predict probabilities on the test set
    pred_probs1_train <- predict(model, newdata = train_data, type = "response")
    pred_probs1_test <- predict(model, newdata = test_data, type = "response")
    
    # Compute AUC
    roc_obj1_train <- roc(train_data$is_open_f, pred_probs1_train)
    roc_obj1_test <- roc(test_data$is_open_f, pred_probs1_test)
    auc_values1_train[i] <- auc(roc_obj1_train)
    auc_values1_test[i] <- auc(roc_obj1_test)
}

# Combine results into a data frame
cv_results1 <- data.frame(
    Fold = 1:k,
    AUC_Train = auc_values1_train,
    AUC_Test = auc_values1_test
)
cv_results1

# Mean AUC for training data
mean_auc_train1 <- mean(cv_results1$AUC_Train)

# Mean AUC for test data
mean_auc_test1 <- mean(cv_results1$AUC_Test)

# Print the results
mean_auc_train1 #0.6464
mean_auc_test1 #0.6373
# Train AUC ≈ Test AUC	Stable performance.	Trust the model.

# Predictions
pred1 <- predict(glm.model1, biz, type = "response")

model1_auc <- auc(roc(biz$is_open, pred1))
print(model1_auc) # 0.6456
## baseline auc for model 1: Area under the curve: 0.6456


###********Model 2**********

# Initialize vector to store AUCs
auc_values2_test <- numeric(k)
auc_values2_train <- numeric(k)

# Perform k-fold CV
for(i in 1:k) {
    # Split into train and test sets
    test_indices <- which(folds == i)
    train_data <- biz[-test_indices, ]
    test_data <- biz[test_indices, ]
    
    model <- glm(is_open ~ elite_cnt + price_level * biz.stars + repeated_cnt, biz, family = 
                     binomial)
    
    # Predict probabilities on the test set
    pred_probs2_train <- predict(model, newdata = train_data, type = "response")
    pred_probs2_test <- predict(model, newdata = test_data, type = "response")
    
    # Compute AUC
    roc_obj2_train <- roc(train_data$is_open_f, pred_probs2_train)
    roc_obj2_test <- roc(test_data$is_open_f, pred_probs2_test)
    auc_values2_train[i] <- auc(roc_obj2_train)
    auc_values2_test[i] <- auc(roc_obj2_test)
}

# Combine results into a data frame
cv_results2 <- data.frame(
    Fold = 1:k,
    AUC_Train = auc_values2_train,
    AUC_Test = auc_values2_test
)
cv_results2

# Mean AUC for training data
mean_auc_train2 <- mean(cv_results2$AUC_Train)

# Mean AUC for test data
mean_auc_test2 <- mean(cv_results2$AUC_Test)

# Print the results
mean_auc_train2 #0.7035
mean_auc_test2 #0.6962

# Train AUC ≈ Test AUC	Stable performance.	Trust the model.

# Predictions
pred2 <- predict(glm.model2, biz, type = "response")

model2_auc <- auc(roc(biz$is_open, pred2))
print(model2_auc) # 0.703
## baseline auc for model 2: Area under the curve: 0.703




###********Model 3**********

# Initialize vector to store AUCs
auc_values3_test <- numeric(k)
auc_values3_train <- numeric(k)

# Perform k-fold CV
for(i in 1:k) {
    # Split into train and test sets
    test_indices <- which(folds == i)
    train_data <- biz[-test_indices, ]
    test_data <- biz[test_indices, ]
    
    model <- glm(is_open ~ poly(elite_cnt, 2, raw=T) + price_level*biz.stars*repeated_cnt + city,
                 biz, family=binomial)
    
    # Predict probabilities on the test set
    pred_probs3_train <- predict(model, newdata = train_data, type = "response")
    pred_probs3_test <- predict(model, newdata = test_data, type = "response")
    
    # Compute AUC
    roc_obj3_train <- roc(train_data$is_open_f, pred_probs3_train)
    roc_obj3_test <- roc(test_data$is_open_f, pred_probs3_test)
    auc_values3_train[i] <- auc(roc_obj3_train)
    auc_values3_test[i] <- auc(roc_obj3_test)
}

# Combine results into a data frame
cv_results3 <- data.frame(
    Fold = 1:k,
    AUC_Train = auc_values3_train,
    AUC_Test = auc_values3_test
)
cv_results3

# Mean AUC for training data
mean_auc_train3 <- mean(cv_results3$AUC_Train)

# Mean AUC for test data
mean_auc_test3 <- mean(cv_results3$AUC_Test)

# Print the results
mean_auc_train3 #0.7217
mean_auc_test3 #0.7122

# Train AUC ≈ Test AUC	Stable performance.	Trust the model.

# Predictions
pred3 <- predict(glm.model3, biz, type = "response")

model3_auc <- auc(roc(biz$is_open, pred3))
print(model3_auc) # 0.721
## baseline auc for model 3: Area under the curve: 0.721



###****************** Use caret *************************
# install.packages("caret")       # if you haven’t already
# install.packages("pROC")
library(caret)
library(pROC)

set.seed(123)

# 1. Make sure your response is a two‐level factor:
biz$is_open_f <- factor(ifelse(biz$is_open==1, "open", "closed"),
                        levels=c("open","closed"))

# 2. Define a 10‐fold CV that computes class‐probabilities and ROC:
ctrl <- trainControl(
    method            = "cv",
    number            = 10,
    classProbs        = TRUE,
    summaryFunction   = twoClassSummary,
    savePredictions   = TRUE
)

# 3. Train each model, optimizing for AUC:
cv1 <- train(is_open_f ~ elite_cnt + price_level,
             data    = biz,
             method  = "glm",
             family  = "binomial",
             metric  = "ROC",
             trControl = ctrl)

cv2 <- train(is_open_f ~ elite_cnt + price_level * biz.stars + repeated_cnt,
             data    = biz,
             method  = "glm",
             family  = "binomial",
             metric  = "ROC",
             trControl = ctrl)

cv3 <- train(is_open_f ~ poly(elite_cnt, 2, raw=TRUE) +
                 price_level * biz.stars * repeated_cnt +
                 city,
             data    = biz,
             method  = "glm",
             family  = "binomial",
             metric  = "ROC",
             trControl = ctrl)

# 4. Extract the per‐fold AUCs and compute the mean:
mean_auc1 <- mean(cv1$resample$ROC)
mean_auc2 <- mean(cv2$resample$ROC)
mean_auc3 <- mean(cv3$resample$ROC)

# 5. Report:
cat("Model 1 mean 10-fold AUC:", round(mean_auc1,3), "\n")
cat("Model 2 mean 10-fold AUC:", round(mean_auc2,3), "\n")
cat("Model 3 mean 10-fold AUC:", round(mean_auc3,3), "\n")




##**** 三个model一起*****
library(pROC)

set.seed(123)

# create 10 roughly equal-sized folds
n   <- nrow(biz)
folds <- sample(rep(1:10, length.out = n))

# storage for AUCs:  columns = Model1, Model2, Model3
aucs_train <- matrix(NA, nrow = 10, ncol = 3)
aucs_test  <- matrix(NA, nrow = 10, ncol = 3)
colnames(aucs_train) <- colnames(aucs_test) <- c("Model1","Model2","Model3")

for(f in 1:10) {
    # split
    is_train   <- folds != f
    train_data <- biz[is_train, ]
    test_data  <- biz[!is_train, ]
    
    # fit all three models on the training fold
    m1 <- glm(is_open ~ elite_cnt + price_level,
              data = train_data, family = binomial)
    m2 <- glm(is_open ~ elite_cnt + price_level*biz.stars + repeated_cnt,
              data = train_data, family = binomial)
    m3 <- glm(is_open ~ poly(elite_cnt,2,raw=TRUE)
              + price_level*biz.stars*repeated_cnt
              + city,
              data = train_data, family = binomial)
    
    # predict probabilities on both train and test
    preds_train <- lapply(list(m1,m2,m3),
                          function(m) predict(m, train_data, type="response"))
    preds_test  <- lapply(list(m1,m2,m3),
                          function(m) predict(m, test_data,  type="response"))
    
    # compute AUCs
    for(i in 1:3) {
        aucs_train[f,i] <- auc(train_data$is_open, preds_train[[i]])
        aucs_test[f, i] <- auc(test_data$is_open,  preds_test[[i]])
    }
}

# average across folds
mean_train_auc <- colMeans(aucs_train)
mean_test_auc  <- colMeans(aucs_test)

# nicely tabulate
results <- data.frame(
    Model    = c("Model1","Model2","Model3"),
    Train_AUC = mean_train_auc,
    Test_AUC  = mean_test_auc
)
print(results)



## Q2.2.2

# baseline is Model1’s Test AUC
baseline    <- results$Test_AUC[results$Model=="Model1"]

# compute relative improvements
improv2     <- (results$Test_AUC[results$Model=="Model2"] - baseline) / baseline
improv3     <- (results$Test_AUC[results$Model=="Model3"] - baseline) / baseline

# pick best of Model2/3
if (improv2 > improv3) {
    best_model  <- "Model2"; best_improv <- improv2
} else {
    best_model  <- "Model3"; best_improv <- improv3
}

# decision logic
if (best_improv < 0.05) {
    cat("All more complex models improve < 5% → stick with Model1 (simplest).\n")
} else if (best_improv > 0.15) {
    cat(sprintf("%s is substantially better (%.1f%% ↑) → use %s.\n",
                best_model, best_improv*100, best_model))
} else {
    cat(sprintf("%s yields a moderate gain of %.1f%% (5–15%%) → not worth the extra complexity; stick with Model1.\n",
                best_model, best_improv*100))
}





# --- End of 463 HW4-Week5.R ---


# --- Start of Week3 HW.R ---

setwd("/Users/itsxuuu/Desktop/463 Machine Learning/Data")
biz=read.csv("yelp.biz.sample_W3.csv")

#1.1
# checking rental_cost and dist_destination by prime_location
cor(biz$rental_cost, biz$dist_destination) 
cor(biz[biz$prime_location==1,]$rental_cost, biz[biz$prime_location==1,]$dist_destination)
cor(biz[biz$prime_location==0,]$rental_cost, biz[biz$prime_location==0,]$dist_destination)
# without prime_location, there's almost no correlation

cor(biz[biz$prime_location==1,]$rental_cost, biz$prime_location==1)

#1.2
m1 <- lm(rental_cost ~ dist_destination, biz)
summary(m1)

#1.3
m2 <- lm(rental_cost ~ dist_destination+prime_location, biz)
summary(m2)

#1.4
install.packages("car")
library(car)
vif(m2)

#1.5 
anova(lm(rental_cost ~ dist_destination, biz))
anova(lm(rental_cost ~ dist_destination+prime_location, biz))
drop1(lm(rental_cost ~ dist_destination, biz))
drop1(lm(rental_cost ~ dist_destination+prime_location, biz))

#2.1
cor(biz$inspector_visit, biz$dist_destination) 
cor(biz$inspector_visit, biz$health_alarm) 
cor(biz$dist_destination, biz$health_alarm) 

#2.2
m3 <- lm(inspector_visit ~ dist_destination, biz)
summary(m3)

#2.3
m4 <- lm(inspector_visit ~ dist_destination+health_alarm, biz)
summary(m4)

#2.4
vif(m4)

#3.1
m5 <- lm(biz.rws.cnt ~ rst.stars*factor(is_open), biz)
summary(m5)

#3.2
library(emmeans)
emm4 <- emmeans(m5, ~ factor(is_open) | rst.stars, at = list(rst.stars = 4))

#3.3
m6 <- lm(biz.rws.cnt ~ elite_cnt * is_open * rst.stars, biz)
summary(m6)

#3.4
-6.08+3.69*100-9.85*1+8.03*4-1.21*100*1+0.08*100*4+11.82*1*4+0.63*100*1*4
-6.08+3.69*100-9.85*1+8.03*5-1.21*100*1+0.08*100*5+11.82*1*5+0.63*100*1*5

#3.5

em_4.5 <- emmeans(m6, ~ rst.stars | elite_cnt + is_open, 
                  at = list(elite_cnt = 100, is_open = 1, rst.stars = c(4, 5)))
pairs(em_4.5)  








# --- End of Week3 HW.R ---


