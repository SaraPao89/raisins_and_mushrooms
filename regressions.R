library(dplyr)
library(estimatr)
library(stats)
library(tree)

# load the dataset
# Class = 1 if raisin is of Kecimen type, 0 if it is Besni
raisins = read.csv("https://raw.githubusercontent.com/LeonardoAcquaroli/raisins_and_mushrooms/main/datasets/Raisin_Dataset.csv",sep=";")
# remove the column of the literal class
#raisins = raisins %>% select(-Class_literal) # don't know why it doesn't work
raisins = raisins[-c(8)]
#raisins$Area = raisins$Area/1000 # scale Area by 1000 for better interpretability. Recall Area is the n. of pixel inside the boundaries of the raisin

# 1. ols
ols = lm("Class ~ .",data=raisins)
summary(ols)

# 2. robust ols
ols_robust = lm_robust(Class ~ .,data=raisins,se_type = "HC1")
summary(ols_robust)
## Robust ols with rlm by MASS
#summary(rlm(Class ~ ., raisins))

# 3. Logistic
logistic = glm(Class ~ ., data = raisins, family = binomial(link='logit'))
hist(predict(logistic))
max(predict(logistic))
min(predict(logistic))

# 4. Ridge
# 5. Lasso
# 6. Tree
plot(tree(Class ~ ., data = raisins))
text(tree(Class ~ ., data = raisins))
