
#___________________________ CHARLEY: FINAL PROJECT CODE _________________________________

# My analysis to support H0: There is not a significant difference in world happiness
# levels when comparing data from pre, and during.


# I want to focus on the three variables (making models for each, each year) putting
# them side by side. I will compare errors and interpretations if models do change, I
# will compare models if they do not change too much (from positive linear relationship)
.

library('readr')
library('ggplot2')
library('dplyr')
library('tidyverse')
library('rsample')
library('forcats')
library('randomForest')
library('glmnet')
library('glmnetUtils')


data2019 <- read_csv("/Users/charleyteltschik/Documents/MGSC_310/datasets/2019.csv")
data2020 <- read_csv("/Users/charleyteltschik/Documents/MGSC_310/datasets/2020.csv")


#DATA CLEANING
data2019 <- data2019 %>% rename(`Country` = `Country or region`,
                                `Happiness score` = Score)
data2020 <- data2020 %>% rename(`Country` = `Country name`,
                                `Happiness score` = `Ladder score`)

names(data2019)
names(data2020)


# ______________________________________DATA 2019_______________________________________

# with this data, I'll be demonstrating happiness levels based on Freedom to make 
# decisions (national average of binary answers: 1 = yes, 0 = no) and Social support
# (national average of binary answers: 1 = yes, 0 = no)

# THIS CODE DEMONSTRATES DATA WITH RECORDED HAPPINESS SCORES AS IS
data_happy7 <- data2019 %>% filter(`Happiness score` >= 7.0) %>% select(Country | `Happiness score`)
print(data_happy7)
ggplot(data_happy7, aes(x = Country, y = `Happiness score`, fill = `Happiness score`)) +
  geom_bar(stat = "identity", position = "dodge")

data_happy6 <- data2019 %>% filter(`Happiness score` >= 6.0, `Happiness score` < 7.0) %>% 
  select(Country | `Happiness score`)
print(data_happy6)
ggplot(data_happy6, aes(x = Country, y = `Happiness score`, fill = `Happiness score`)) +
  geom_bar(stat = "identity", position = "dodge") + ylim(0,8)

data_happy5 <- data2019 %>% filter(`Happiness score` >= 5.0, `Happiness score` < 6.0) %>% 
  select(Country | `Happiness score`)
print(data_happy5)
ggplot(data_happy5, aes(x = Country, y = `Happiness score`, fill = `Happiness score`)) +
  geom_bar(stat = "identity", position = "dodge") + ylim(0,8)

data_happy4 <- data2019 %>% filter(`Happiness score` >= 4.0, `Happiness score` < 5.0) %>% 
  select(Country | `Happiness score`)
print(data_happy4)
ggplot(data_happy4, aes(x = Country, y = `Happiness score`, fill = `Happiness score`)) +
  geom_bar(stat = "identity", position = "dodge") + ylim(0,8)

data_happy3 <- data2019 %>% filter(`Happiness score` >= 3.0, `Happiness score` < 4.0) %>% 
  select(Country | `Happiness score`)
print(data_happy3)
ggplot(data_happy3, aes(x = Country, y = `Happiness score`, fill = `Happiness score`)) +
  geom_bar(stat = "identity", position = "dodge") + ylim(0,8)

data_happy2 <- data2019 %>% filter(`Happiness score` >= 2.0, `Happiness score` < 3.0) %>% 
  select(Country | `Happiness score`)
print(data_happy2)
ggplot(data_happy2, aes(x = Country, y = `Happiness score`, fill = `Happiness score`)) +
  geom_bar(stat = "identity", position = "dodge") + ylim(0,8)

data_list <- list(`Scores 7+` = data_happy7, `7 > Scores > 6` = data_happy6, `6 > Scores > 5` = data_happy5,
                  `4 > Scores > 5` = data_happy4, `3 > Scores > 4` = data_happy3, `2 > Scores > 3` = data_happy2)
combined_data <- bind_rows(data_list, .id = "dataset")
ggplot(combined_data, aes(x = Country, y = `Happiness score`)) +
  geom_point() +
  labs(title = "Happiness scores 2019", x = "Country", y = "Happiness Score (out of 10)") +
  facet_wrap(~ dataset) + geom_smooth(method = "lm")



# SPLITTING DATA INTO TRAIN AND TEST SETS

data2019_split <- initial_split(data2019, prop = 0.75)
data2019_train <- training(data2019_split)
data2019_test <- testing(data2019_split)

data2019_train <- data2019_train %>% mutate(
  social_support = `Social support`,
  freed_choices = `Freedom to make life choices`,
  generosity = `Generosity`,
  perc_corrup = `Perceptions of corruption`,
  health_life = `Healthy life expectancy`
)

data2019_test <- data2019_test %>% mutate(
  social_support = `Social support`,
  freed_choices = `Freedom to make life choices`,
  generosity = `Generosity`,
  perc_corrup = `Perceptions of corruption`,
  health_life = `Healthy life expectancy`
)


# LINEAR REGRESSIN MODEL 1
mod1 <- lm(`Happiness score` ~ `Freedom to make life choices`,
                        data = data2019_train)
summary(mod1)

ggplot(mod1, aes(x = `Freedom to make life choices`, y = `Happiness score`)) +
  geom_point()


# LOOKING AT VARIABLE IMPORTANCE
rf_fit3 <- randomForest(
  `Happiness score` ~ social_support + freed_choices + generosity + perc_corrup +
    health_life, data = data2019_train)
print(rf_fit3)

importance(rf_fit3)
varImpPlot(rf_fit3)


# LINEAR REGRESSION MODEL 2
mod2 <- lm(`Happiness score` ~ `Social support`,
           data = data2019_train)
summary(mod2)

ggplot(mod2, aes(x = `Social support`, y = `Happiness score`)) +
  geom_point()


# PREDICTED TRUE PLOTS: FREEDOM TO MAKE LIFE CHOICES AND SOCIAL SUPPORT
mod3 <- lm(`Happiness score` ~ freed_choices + social_support,
           data = data2019_train)
summary(mod1)

preds_train <- predict(mod2, newdata = data2019_train)
preds_test <- predict(mod2, newdata = data2019_test)

results_train <- 
  tibble(
    `preds` = preds_train,
    `true` = paste(data2019_train$freed_choices, data2019_train$social_support),
    `type` = "train"
  )

results_test <- 
  tibble(
    `preds` = preds_test,
    `true` = paste(data2019_test$freed_choices, data2019_test$social_support),
    `type` = "test"
  )

results_df <- 
  bind_rows(results_train, results_test)

ggplot(results_df, aes(x = true, y = preds)) +
  geom_point(aes(color = type)) + 
  geom_abline(color = "red") +
  facet_wrap(~ type) +
  ylim(0,10) +
  theme_minimal(base_size = 16) + 
  theme(legend.position="bottom") +
  labs(title = "Predicted Trues: Freedom & Social Support (2019)")


# CREATING RIDGE REGRESSION & LOOKING AT COEFFICIENTS
ridge_mod <- cv.glmnet(`Happiness score` ~ social_support + freed_choices,
                       data = data2019_train,
                       alpha = 0)
plot(ridge_mod)

coef(ridge_mod, s = ridge_mod$lambda.min)





# ______________________________________DATA 2020_______________________________________

# with this data, I'll be demonstrating happiness levels based on Freedom to make 
# decisions (national average of binary answers: 1 = yes, 0 = no), Social support
# (national average of binary answers: 1 = yes, 0 = no), and healthy life expectancy
# (national average of binary answers: 1 = yes, 0 = no)

# THIS CODE DEMONSTRATES DATA WITH RECORDED HAPPINESS SCORES AS IS
data_happy7 <- data2022 %>% filter(`Happiness score` >= 7.0) %>% select(Country | `Happiness score`)
print(data_happy7)
ggplot(data_happy7, aes(x = Country, y = `Happiness score`, fill = `Happiness score`)) +
  geom_bar(stat = "identity", position = "dodge")

data_happy6 <- data2022 %>% filter(`Happiness score` >= 6.0, `Happiness score` < 7.0) %>% 
  select(Country | `Happiness score`)
print(data_happy6)
ggplot(data_happy6, aes(x = Country, y = `Happiness score`, fill = `Happiness score`)) +
  geom_bar(stat = "identity", position = "dodge") + ylim(0,8)

data_happy5 <- data2022 %>% filter(`Happiness score` >= 5.0, `Happiness score` < 6.0) %>% 
  select(Country | `Happiness score`)
print(data_happy5)
ggplot(data_happy5, aes(x = Country, y = `Happiness score`, fill = `Happiness score`)) +
  geom_bar(stat = "identity", position = "dodge") + ylim(0,8)

data_happy4 <- data2022 %>% filter(`Happiness score` >= 4.0, `Happiness score` < 5.0) %>% 
  select(Country | `Happiness score`)
print(data_happy4)
ggplot(data_happy4, aes(x = Country, y = `Happiness score`, fill = `Happiness score`)) +
  geom_bar(stat = "identity", position = "dodge") + ylim(0,8)

data_happy3 <- data2022 %>% filter(`Happiness score` >= 3.0, `Happiness score` < 4.0) %>% 
  select(Country | `Happiness score`)
print(data_happy3)
ggplot(data_happy3, aes(x = Country, y = `Happiness score`, fill = `Happiness score`)) +
  geom_bar(stat = "identity", position = "dodge") + ylim(0,8)

data_happy2 <- data2022 %>% filter(`Happiness score` >= 2.0, `Happiness score` < 3.0) %>% 
  select(Country | `Happiness score`)
print(data_happy2)
ggplot(data_happy2, aes(x = Country, y = `Happiness score`, fill = `Happiness score`)) +
  geom_bar(stat = "identity", position = "dodge") + ylim(0,8)

data_list <- list(`Scores 7+` = data_happy7, `7 > Scores > 6` = data_happy6, `6 > Scores > 5` = data_happy5,
                  `4 > Scores > 5` = data_happy4, `3 > Scores > 4` = data_happy3, `2 > Scores > 3` = data_happy2)
combined_data <- bind_rows(data_list, .id = "dataset")
ggplot(combined_data, aes(x = Country, y = `Happiness score`)) +
  geom_point() +
  labs(title = "Happiness scores 2019", x = "Country", y = "Happiness Score (out of 10)") +
  facet_wrap(~ dataset) + geom_smooth(method = "lm")


# SPLITTING THE DATA INTO TRAIN & TEST SETS
data2022_split <- initial_split(data2022, prop = 0.75)
data2022_train <- training(data2022_split)
data2022_test <- testing(data2022_split)

data2020_train <- data2020_train %>% mutate(
  social_support = `Social support`,
  freed_choices = `Freedom to make life choices`
  generosity = `Generosity`,
  perc_corrup = `Perceptions of corruption`,
  health_life = `Healthy life expectancy`
)

data2020_test <- data2020_test %>% mutate(
  social_support = `Social support`,
  freed_choices = `Freedom to make life choices`,
  generosity = `Generosity`,
  perc_corrup = `Perceptions of corruption`,
  health_life = `Healthy life expectancy`
)


# LINEAR REGRESSION MODEL 4
mod4 <- lm(`Happiness score` ~ `Freedom to make life choices`,
           data = data2022_train)
summary(mod1)

ggplot(mod1, aes(x = `Freedom to make life choices`, y = `Happiness score`)) +
  geom_point() + geom_smooth(method = "lm") +
  labs(title = "Impact of Freedom to Make Life Choices on Happiness Score in 2022")


# LOOKING AT VARIABLE IMPORTANCE
rf_fit2 <- randomForest(
  `Happiness score` ~ social_support + freed_choices + generosity + perc_corrup +
    health_life, data = data2020_train)
print(rf_fit2)

importance(rf_fit2)
varImpPlot(rf_fit2)


# LINEAR REGRESSION MODEL 5
mod5 <- lm(`Happiness score` ~ `Social support`,
           data = data2020_train)
summary(mod2)

ggplot(mod2, aes(x = `Social support`, y = `Happiness score`)) +
  geom_point()+ geom_smooth(method = "lm") + 
  labs(title = "Impact of Social Support on Happiness Score in 2020")


# PREDICTED TRUE PLOTS
preds_train <- predict(mod, newdata = data2020_train)
preds_test <- predict(mod, newdata = data2020_test)

results_train <- 
  tibble(
    `preds` = preds_train,
    `true` = paste(data2020_train$freed_choices, data2020_train$social_support),
    `type` = "train"
  )

results_test <- 
  tibble(
    `preds` = preds_test,
    `true` = paste(data2020_test$freed_choices, data2020_test$social_support),
    `type` = "test"
  )

results_df <- 
  bind_rows(results_train, results_test)

ggplot(results_df, aes(x = true, y = preds)) +
  geom_point(aes(color = type)) + 
  geom_abline(color = "red") +
  facet_wrap(~ type) +
  theme_minimal(base_size = 16) + 
  theme(legend.position="bottom") +
  labs(title = "Predicted Trues: Freedom & Social Support (2020)")


# RIDGE REGRESSION
ridge_mod1 <- cv.glmnet(`Happiness score` ~ social_support + freed_choices,
                        data = data2020_train,
                        alpha = 0)
plot(ridge_mod1)

coef(ridge_mod1, s = ridge_mod$lambda.min)


