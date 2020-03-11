#---------------------------------------------------------
# Intro to Logistic Regression
# Mental health inpatient length of stay (LoS)
# Thu Mar 05 14:59:03 2020
#---------------------------------------------------------

# Another helpful primer can be found at https://stats.idre.ucla.edu/other/dae/
# See the logistic regression example in R

## Note: If you don't have the packages below (eg. tidymodels) in your library,
## you will need to install them with:
# install.packages("tidyverse")
# install.packages("tidymodels")
# install.packages("plotROC")

library(tidyverse)
library(tidymodels)
library(plotROC)


# 0. Load the data --------------------------------------------------------

mh_los <- read_rds("mh_los.rds")


# 1. Split data into test and training sets to avoid overfitting -----------------------------------

# Set seed to make sure "randomness" is reproducible:
set.seed(7117)

data_split <- initial_split(mh_los, strata = "long_stay", prop = 0.8)

los_train <- training(data_split)
los_test  <- testing(data_split)



# 2. Quick exploration of dataset --------------------------------------------

los_train %>% 
  count(long_stay)

los_train %>% 
  ggplot(aes(sex, long_stay, col = interaction(sex, long_stay)))+
  geom_jitter(height = .2)+
  theme(legend.position = "none")+
  NULL

los_train %>% 
  ggplot(aes(age, long_stay, col = long_stay))+
  geom_jitter(height= .1, alpha = .4)+
  theme(legend.position = "none")+
  xlim(0, 100)

los_train %>% 
  ggplot(aes(diag, long_stay, col = diag))+
  geom_jitter(height = .1)+
  theme(legend.position = "none")


# 3. Model on training set -------------------------------------------------------

# The glm function is used to fit generalized linear models. Signal logistic regression by 
# giving the function a description of the correct error distribution (family = "binomial" )

our_model <-   glm(formula = long_stay ~ age + sex + diag,
                   #  for logistic regression use binomial error distribution:
                   family = "binomial", 
                   data = los_train
)


# 4. View coefficents ------------------------------------------------------------

# Coefficients (log odds) and some stats (all printed to the console window)
summary(our_model)

# Or:

broom::tidy(our_model) 

# Exponentiate coefficients (so now odds ratios):

broom::tidy(our_model) %>% 
  mutate(estimate = exp(estimate))


# 5. Add predictions to TEST set ---------------------------------------------------------

los_predict <- los_test %>%
  # Model will give probability of los > 60 days (a long stay):
  mutate(.prob =  predict(our_model, newdata = . , type = "response")) %>% 
  # set probaility threshold (for long stay) at 0.5:
  mutate(.pred =  ifelse(.prob >= 0.5, 1, 0)) %>%
  mutate(long_stay = as_factor(long_stay)) %>% 
  mutate(.pred = as_factor(.pred))


# 6. Some peformance metrics for classification ---------------------------------------------

# Confusion matrix
los_predict %>% conf_mat(truth = long_stay, estimate = .pred)

# Accuracy
los_predict %>% accuracy(truth = long_stay, estimate = .pred)

# this is actually sensitivity (due to default factor levels):
los_predict %>% yardstick::spec(truth = long_stay, estimate = .pred)

# and this is, in fact, specificity:
los_predict %>% sens(truth = long_stay, estimate = .pred)


# 7. ROC curve and area under ROC metric ----------------------------------------------

los_predict %>% 
  # some recoding needed:
  mutate_at(vars(long_stay, .pred), list(~as.character(.))) %>% 
  mutate_at(vars(long_stay, .pred), list(~as.numeric(.))) %>% 
  #
  ggplot()+
  geom_roc(aes(d = long_stay, m =.prob), labelround = 2)+
  style_roc()

# AUROC
los_predict %>%
  roc_auc(truth = long_stay,  .prob)




