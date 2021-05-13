library(tidyverse)
library(readxl)
library(ggthemes)
library(ggrepel)
library(stargazer)
library(kableExtra)

#Import Data
filename <- "COVAX_deidentify"
dat_external <- read_csv(filename)

#Chart Illustrating Volunteer Rate per Corps
dat_ext_character <- dat_external %>% mutate(volunteer = recode(volunteer, `0` = "Non-Volunteer", `1` = "Volunteer"))
unit_corps <- table(dat_ext_character$corps, dat_ext_character$volunteer)
unit_corps_prop <- prop.table(unit_corps, margin = 1)
unit_corps2 <- cbind(unit_corps, unit_corps_prop[,2])
knitr::kable(unit_corps2, "simple",
             col.name = c("No", "Yes", "Volunteer Rate"),
             align = c("ccc"),
             digits = 3,
             caption = "Volunteers per Corps")

#Violin Plot Illustrating the distribution of volunteers by age and further stratified by Corps
dat_ext_character %>%
  ggplot(aes(volunteer, age)) +
  geom_violin(bw = 4,  alpha = .2) +
  geom_boxplot(width = .2) + 
  facet_wrap(~corps) +
  labs(x = "Volunteer Status", y = "Age") +
  theme(legend.position = "none")

#Chart Illustrating Volunteer Rate per Priority Group
unit_group <- table(dat_ext_character$group, dat_ext_character$volunteer)
unit_group_prop <- prop.table(unit_group, margin = 1)
unit_group2 <- cbind(unit_group, unit_group_prop[,2])
knitr::kable(unit_group2, "simple",
             col.name = c("No", "Yes", "Volunteer Rate"),
             align = c("ccc"),
             digits = 3,
             caption = "Volunteers per Priority Group")

#Violin Plot Comparing Rank and Mission Critical Status stratified by Volunteer Decision
dat_ext_character %>%
  ggplot(aes(volunteer, continuousrank), group_by(volunteer)) +
  geom_violin(bw = 4, position = 'identity', aes(color = critical), alpha = .3) +
  labs(x = "Volunteer Status", y = "Rank (1 = Private; 20 = Colonel)", color = "Mission Critical") +
  theme_bw()

library(rpart)
library(rpart.plot)
library(caret)
library(pROC)
library(ROCR)
library(randomForest)

# Divide the data set into a training and testing subsets
sample_rows <- sample(nrow(dat_external), .75*nrow(dat_external))
covax_train <- dat_external[sample_rows,]
covax_test <- dat_external[-sample_rows,]

#Decision Tree
volunteer_tree <- rpart(volunteer ~ critical + age + officer + warrant + nco + aviation + medical + sex,
                        data = covax_train, method = "class", control = rpart.control(cp = 0.006, minsplit = 200, maxdepth = 3))
rpart.plot(volunteer_tree, box.palette = "Grays")

p_hat_tree <- predict(volunteer_tree, newdata = covax_test, type = "class")
CM_tree <- confusionMatrix(p_hat_tree, factor(covax_test$volunteer), positive = "1")
CM_tree
tree_accuracy <- round(CM_tree$overall["Accuracy"], 3)
tree_specificity <- round(CM_tree$byClass["Specificity"], 3)
Tree_Assessment <- cbind(tree_accuracy, tree_specificity)
auc <- performance(prediction(covax_test$volunteer, p_hat_tree), 'auc')
auc_tree <- slot(auc, 'y.values')
auc_tree <- round(auc_tree[[1]], 3)

#Random Forest
volunteer_forest <- randomForest(volunteer ~ critical + age + officer + warrant + nco + aviation + medical + sex, data = covax_train)
var_importance <- round(varImp(volunteer_forest), 3)
knitr::kable(var_importance, col.names = "Variable Importance",
             caption = "Random Forest Variable Importance") %>%
  row_spec(0, bold = TRUE) %>%
  kable_styling(latex_options = "hold_position")

p_hat_forest <- predict(volunteer_forest, newdata = covax_test, type = "response")
y_hat_forest <- ifelse(p_hat_forest > 0.5, 1, 0) %>% factor()

CM_forest <- confusionMatrix(y_hat_forest, factor(covax_test$volunteer), positive = "1")
CM_forest
forest_accuracy <- round(CM_forest$overall["Accuracy"], 3)
forest_specificity <- round(CM_forest$byClass["Specificity"], 3)

ROC_forest <- roc(covax_test$volunteer, p_hat_forest)
auc_forest <- round(auc(ROC_forest), 3)

#Logistic Regression using all Explanatory Variables
log_total_model <- glm(volunteer ~ critical + age + officer + warrant + nco + aviation + medical + sex, data = covax_train, family = "binomial")
p_hat_logit_total <- predict(log_total_model, newdata = covax_test, type = "response")
y_hat_logit_total <- ifelse(p_hat_logit_total > 0.5, 1, 0) %>% factor()

CM_log_total <- confusionMatrix(y_hat_logit_total, factor(covax_test$volunteer), positive = "1")
total_accuracy <- round(CM_log_total$overall["Accuracy"], 3)
total_specificity <- round(CM_log_total$byClass["Specificity"], 3)

ROC_logit_total <- roc(covax_test$volunteer, p_hat_logit_total)
auc_total <- round(auc(ROC_logit_total), 3)

#Accounting for intra-company correlation through Cluster Robust Standard Errors
library(sandwich)
library(lmtest)
log_total_clust <- coeftest(log_total_model, vcov = vcovCL, cluster = ~unit_code)

##Stepwise Logarithmic Regression
null_model <- glm(volunteer ~ 1, data = covax_train, family = "binomial")
step_model <- step(null_model, scope = list(lower = null_model, upper = log_total_model), direction = "both")

p_hat_step <- predict(step_model, newdata = covax_test, type = "response")
y_hat_step <- ifelse(p_hat_step > 0.5, 1, 0) %>% factor()

CM_step <- confusionMatrix(y_hat_step, factor(covax_test$volunteer), positive = "1")
step_accuracy <- round(CM_step$overall["Accuracy"], 3)
step_specificity <- round(CM_step$byClass["Specificity"], 3)

ROC_step <- roc(covax_test$volunteer, p_hat_step)
auc_step <- round(auc(ROC_step), 3)

step_model_clust <- coeftest(step_model, vcov = vcovCL, cluster = ~unit_code)

#Logistic Regression using Random Forest Variables of Importance Greater than 20 (Equivalent to Decision Tree)
log_tree_model <- glm(volunteer ~ critical + age + officer, data = covax_train, family = "binomial")

#Despite identifying the probability of being a volunteer as 0.387, I preserve 0.5 as the inflection point to skew the model toward specificity
p_hat_tree_logit <- predict(log_tree_model, newdata = covax_test, type = "response")
y_hat_tree_logit <- ifelse(p_hat_tree_logit > 0.5, 1, 0) %>% factor()

CM_tree_log <- confusionMatrix(y_hat_tree_logit, factor(covax_test$volunteer), positive = "1")
tree_log_accuracy <- round(CM_tree_log$overall["Accuracy"], 3)
tree_log_specificity <- round(CM_tree_log$byClass["Specificity"], 3)

ROC_tree_logit <- roc(covax_test$volunteer, p_hat_tree_logit)
auc_tree_logit <- round(auc(ROC_tree_logit), 3)

log_tree_clust <- coeftest(log_tree_model, vcov = vcovCL, cluster = ~unit_code)

#Summary Statistics
library(table1)
label(dat_ext_character$officer) <- "Officer"
label(dat_ext_character$warrant) <- "Warrant Officer"
label(dat_ext_character$nco) <- "NCO"
label(dat_ext_character$enlisted) <- "Enlisted"
label(dat_ext_character$age) <- "Age"
label(dat_ext_character$sex) <- "Sex"
label(dat_ext_character$medical) <- "Medical"
label(dat_ext_character$aviation) <- "Aviation"
sumstats <- table1(~ officer + warrant + nco + enlisted + age + sex + medical + aviation | volunteer,
                   data = dat_ext_character,
                   overall = FALSE,
                   caption = "Summary Statistics Regarding Soldier Vaccination Decisions",
                   footnote = "Discrete Variables = Number (Percent of Total); Continuous Variables = Mean (SD)")
t1kable(sumstats) %>%
  kable_styling(latex_options = "hold_position")

#Regression Output
stargazer(log_total_clust, step_model_clust, log_tree_clust,
          title = "Logistic Regression Results",
          dep.var.caption = "Effect on Vaccine Uptake",
          column.labels = c("Total Model", "Stepwise Model", "Tree Model"),
          covariate.labels = c("Mission Critical", "Age", "Officer", "Warrant Officer", "NCO", "Aviation Specialty", "Medical Specialty", "Sex"),
          add.lines = list(c("Clustered SE", "Yes", "Yes", "Yes"),
                           c("Observations (Training Set)", "1,932", "1,932", "1,932")),
          header = FALSE)

##Compare ROC Plots for Myriad Prediction Models
preds_list <- list(p_hat_tree, p_hat_forest, p_hat_logit_total, p_hat_tree_logit, p_hat_step)

# List of actual values (same for all)
m <- length(preds_list)
actuals_list <- rep(list(covax_test$volunteer), m)

# Plot the ROC curves
pred <- prediction(preds_list, actuals_list)
rocs <- performance(pred, "tpr", "fpr")
plot(rocs, col = as.list(1:m), main = "ROC Curves", xlab = "1 - Specificity", ylab = "Sensitivity")
legend(x = "bottomright", 
       legend = c("Decision Tree","Random Forest", "Decision Tree Logit", "Total Logit", "Stepwise Logit"),
       fill = 1:m)

#Chart Clearly Comparing AUCs
Model <- c("Random Forest", "Stepwise Logit", "Total Logit", "Tree Logit", "Decision Tree")
Accuracy <- c(forest_accuracy, step_accuracy, total_accuracy, tree_log_accuracy, tree_accuracy)
Specificity <-c(forest_specificity, step_specificity, total_specificity, tree_log_specificity, tree_specificity)
AUC_Assessment <- rbind(auc_forest, auc_step, auc_total, auc_tree_logit, auc_tree)
knitr::kable(cbind(Model, Accuracy, Specificity, AUC_Assessment), 
             col.names = c("Model", "Accuracy", "Specificity", "Area Under Curve"),
             caption = "Prediction Model Comparison",
             row.names = FALSE) %>%
  row_spec(0, bold = TRUE) %>%
  kable_styling(latex_options = "hold_position")