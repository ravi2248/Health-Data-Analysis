#remove all the objects currently stored in the environment
rm(list = ls())

#loads NHANES package
library(NHANES)

data(NHANES)

#creating object with NHANES data
nhanes_data <- NHANES::NHANES

#can see the few rows of NHANES data
head(nhanes_data)

#each variable summary statistics
summary(nhanes_data)

#creating a vector with required columns for our project
attributes <- c("ID", "Gender", "Age", "Race1", "BMI", "PhysActive", "Diabetes", "UrineVol1", "UrineFlow1", "UrineVol2", "UrineFlow2", "SmokeNow")

#creates new dataframe having only the required attributes
data <- nhanes_data[, names(nhanes_data) %in% attributes]

#shows total missing values of each column
colSums(is.na(data))

#finding mean of Body Mass Index
bmi_mean <- mean(data$BMI, na.rm = TRUE)
bmi_mean

#assign mean of BMI inplace of null values 
data$BMI[is.na(data$BMI)] <- bmi_mean

#removing the UrineVol2 and UrineFlow2 columns because of having more null values
data <- subset(data, select = -c(UrineVol2, UrineFlow2))

#loads mice package
library(mice)

#set seed for reproducibility
set.seed(123)

#creates multiple imputations using Predictive Mean Matching
impute <- mice(data, method = 'pmm', m = 5)

#replaces missing values
data <- complete(impute)

#shows total missing values of each column
colSums(is.na(data))

#removes rows having null values
data <- na.omit(data)

#loads ggplot2 package
library(ggplot2)

#plots scatter plot of UrineVol1 and UrineFlow1
ggplot(data, aes(x = UrineVol1, y = UrineFlow1)) +
  geom_point() +
  labs(title = "Urine Volume VS Urine Flow",
       x = "Urine Volume",
       y = "Urine Flow") +
  theme_minimal()

#function returns creatinine value
cal_creatinine <- function(vol, flow) {
  return(vol * flow / 1000)
}

#creating column with creatinine values
data$Creatinine <- cal_creatinine(data$UrineVol1, data$UrineFlow1)

#creating Obesity values
data <- data %>%
  mutate(Obesity = ifelse(BMI >= 30, "Yes", "No"))

#each variable summary statistics of required attributes data
summary(data)

#plots distribution of Obesity
ggplot(data, aes(x = Obesity)) +
  geom_bar(fill = "yellow", color = "red") +
  labs(title = "Distribution of Obesity",
       x = "Obesity",
       y = "Frequency") +
  theme_minimal()

#shows total missing values of each column
colSums(is.na(data))

#plots distribution of creatinine
ggplot(data, aes(x = Creatinine)) +
  geom_histogram(binwidth = 1, fill = "green", color = "white") +
  labs(title = "Distribution of Creatinine",
       x = "Creatinine",
       y = "Frequency") +
  theme_minimal()

#plots distribution of BMI
ggplot(data, aes(x = BMI)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "white") +
  labs(title = "Distribution of BMI",
       x = "BMI",
       y = "Frequency") +
  theme_minimal()

#plots distribution of BMI by Gender
ggplot(data, aes(x = BMI, fill = Gender)) +
  geom_histogram(binwidth = 1, position = "identity", alpha = 0.5) +
  labs(title = "Distribution of BMI by Gender",
       x = "BMI",
       y = "Frequency",
       fill = "Gender") +
  theme_minimal()

#plots distribuition of BMI by Race
ggplot(data, aes(x = BMI, fill = Race1)) +
  geom_histogram(binwidth = 1, position = "identity", alpha = 0.5) +
  labs(title = "Distribution of BMI by Race",
       x = "BMI",
       y = "Frequency",
       fill = "Race") +
  theme_minimal()

#plots distribution of BMI by PhyActice
ggplot(data, aes(x = BMI, fill = PhysActive)) +
  geom_histogram(binwidth = 1, position = "identity", alpha = 0.5) +
  labs(title = "Distribution BMI by Physical Activity",
       x = "BMI",
       y = "Frequency",
       fill = "PhysActive") +
  theme_minimal()

#plots distribution of BMI and Age
ggplot(data, aes(x = Age, y = BMI)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "BMI VS Age",
       x = "Age",
       y = "BMI") +
  theme_minimal()

#plots distribution of BMI by smoking status
ggplot(data, aes(x = BMI, fill = SmokeNow)) +
  geom_histogram(binwidth = 1, position = "identity", alpha = 0.5) +
  labs(title = "Distribution of BMI by Smoking Status",
       x = "BMI",
       y = "Frequency",
       fill = "SmokeNow") +
  theme_minimal()

#calculates correlation matrix between the variables Age, BMI, UrineVol1, UrineFlow1
corr_mat <- cor(data[, c("Age", "BMI", "UrineVol1", "UrineFlow1")])
corr_mat

#loads reshape2 package
library(reshape2)

#plots correlation matrix of Age, BMI, UrineVol1, UrineFlow1
ggplot(data = melt(corr_mat), aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "green", mid = "yellow", high = "orange", 
                       midpoint = 0, limits = c(-1,1),
                       name="Correlation") +
  labs(title = "Correlation Heatmap",
       x = "Variables",
       y = "Variables") +
  theme_minimal()

#plots distribution of BMI by Diabetes
ggplot(data, aes(x = BMI, fill = Diabetes)) +
  geom_histogram(binwidth = 1, position = "identity", alpha = 0.5) +
  labs(title = "Distribution of BMI by Diabetes",
       x = "BMI",
       y = "Frequency",
       fill = "Diabetes") +
  theme_minimal()

#calculates correlation between BMI and Creatinine
corr_mat <- cor(data$BMI, data$Creatinine)
corr_mat

#plots distribution of BMI and Creatinine
ggplot(data, aes(x = BMI, y = Creatinine)) +
  geom_point() +
  labs(title = "BMI VS Creatinine",
       x = "BMI",
       y = "Creatinine") +
  theme_minimal()

#plots distribution of BMI VS Creatinine by Gender
ggplot(data, aes(x = BMI, y = Creatinine, color = Gender)) +
  geom_point() +
  labs(title = "BMI VS Creatinine Levels by Gender",
       x = "BMI",
       y = "Creatinine",
       color = "Gender") +
  theme_minimal()

#fits linear regression model Creatinine as response variable and BMI, Age, Gender, Race1, PhyActive as predictor variables
model <- lm(Creatinine ~ BMI + Age + Gender + Race1 + PhysActive, data = data)
model

#shows summary of linear regression model
summary(model)

#fits linear regression model Creatine as reponse variable and BMI as predictor variable
strat_analysis <- lm(Creatinine ~ BMI, data = data)
strat_analysis

#provides coefficients, p-values, etc,... from the summary of the model
strat_results <- coef(summary(strat_analysis))
strat_results

#plots the distribution of coefficients of BMI by Gender
ggplot(data = data.frame(strat_results), aes(x = row.names(strat_results), y = Estimate, ymin = Estimate - 1.96 * Std..Error, ymax = Estimate + 1.96 * Std..Error)) +
  geom_pointrange() +
  labs(title = "Coefficients of BMI by Gender",
       x = "Gender",
       y = "Estimate") +
  theme_minimal()

#creates subgroups of gender column
subgroups <- unique(data[["Gender"]])

#list to store regression model results for each subgroup
strat_models <- list()

#fit regression models for each subgroup
for (s in subgroups) {
  subgroup_data <- filter(data, !!as.name("Gender") == s)
  model <- lm(Creatinine ~ BMI, data = subgroup_data)
  strat_models[[s]] <- model
}
strat_models

#iterates over each subgroup 
lapply(names(strat_models), function(subgroup) {
  
  #extract data for the current subgroup
  subgroup_data <- filter(data, !!as.name("Gender") == subgroup)
  
  #checks the variable 'Creatinine' exists in the subgroup
  if ("Creatinine" %in% colnames(subgroup_data)) {
    
    #predicts creatinine using the regression model
    subgroup_data$Predicted_Creatinine <- predict(strat_models[[subgroup]], newdata = subgroup_data)
    #plot a scatter plot of association between creatinine and BMI for each subgroup
    ggplot(data = subgroup_data, aes(x = BMI, y = Creatinine)) +
      geom_point(color = "green") +
      geom_line(aes(y = Predicted_Creatinine), color = "red") +
      labs(title = paste("Association between Creatinine and BMI for", subgroup),
           x = "BMI",
           y = "Creatinine") +
      theme_minimal()
    
  } else {
    NULL
  }
})

#loads dplyr package
library(dplyr)

#group data of gender
grouped_data <- data %>% 
  group_by(Gender)

#calculate summary statistics for Creatinine and BMI within each group
summary_stats <- grouped_data %>% 
  summarize(
    Mean_Creatinine = mean(Creatinine, na.rm = TRUE),
    Mean_BMI = mean(BMI, na.rm = TRUE)
  )

#plots association between Creatinine and BMI for each subgroup
ggplot(summary_stats, aes(x = Mean_BMI, y = Mean_Creatinine, color = Gender)) +
  geom_point() +
  labs(title = "Association between Creatinine and BMI by Gender",
       x = "Mean BMI",
       y = "Mean Creatinine") +
  theme_minimal()

#if obesity value is yes then it will rewrite it as 1 otherwise 0
data$Obesity <- ifelse(data$Obesity == "Yes", 1, 0)

#fits logistic regression model response variable is Obesity and predictor variable is Creatinine
model <- glm(Obesity ~ Creatinine, data = data, family = binomial)
model

#provides summary statistics of the model
summary(model)

#fits logistic regression model response var is Obesity snd predictor variables are Creatinine, Age, Gender, Race1
model <- glm(Obesity ~ Creatinine + Age + Gender + Race1, data = data, family = binomial)
model

#provides summary statistics of the model
summary(model)

#subset data having obesity value 1
obese <- subset(data, Obesity == 1)

#subset data having obesity value 0
non_obese <- subset(data, Obesity == 0)

#t-test for creatinine with obese and without obese
t_test_result <- t.test(obese$Creatinine, non_obese$Creatinine)
t_test_result

#categorizing ages into three groups
data$AgeGroup <- cut(data$Age, breaks = c(0, 30, 60, max(data$Age)),
                            labels = c("Young Adults", "Middle-Aged Adults", "Older Adults"),
                            include.lowest = TRUE)

#storing unique values of AgeGroup
age_groups <- unique(data$AgeGroup)

#stores t-test results
t_test_results <- list()

#iterating each unique value of AgeGroup
for (age_group in age_groups) {
  
  #subset the AgeGroup of different categories
  age_group_data <- subset(data, AgeGroup == age_group)
  #subset the data with obese(1)
  obese_age <- subset(age_group_data, Obesity == 1)
  #subset the data without obese(0)
  non_obese_age <- subset(age_group_data, Obesity == 0)
  #t-test on with obese and without obese
  t_test_result_age <- t.test(obese_age$Creatinine, non_obese_age$Creatinine)
  #intializing the test results
  t_test_results[[age_group]] <- t_test_result_age
}

#can see the results
t_test_results