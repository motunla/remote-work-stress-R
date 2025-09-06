# Remote Work Stress Analysis
# STAT 4030 Group Project
# Contributors: Mayowa Otunla & Rohan Bhatia
# Includes data cleaning, linear and logistic regression, visualization, and PMSE comparison
# Datasets: synthetic_employee_burnout.csv, post_pandemic_remote_work_health_impact_2025.csv

# Load libraries
library(ggplot2)
library(dplyr)

# Data Import
employee_burnout <- read.csv("C:/Users/myska/Downloads/synthetic_employee_burnout.csv")
View(employee_burnout)
str(employee_burnout)

# Checking for missing values
sapply(employee_burnout, anyNA)  

# Factor Conversion
employee_burnout$Gender    <- as.factor(employee_burnout$Gender)
employee_burnout$JobRole   <- as.factor(employee_burnout$JobRole)
employee_burnout$Burnout   <- as.factor(employee_burnout$Burnout)
employee_burnout$Gender    <- relevel(employee_burnout$Gender, ref = "Male")
employee_burnout$JobRole   <- relevel(employee_burnout$JobRole, ref = "Analyst")

# Scatterplots
ggplot(employee_burnout, aes(x = RemoteRatio, y = StressLevel)) +
  geom_point(color = "#4682B4", size = 3, alpha = 0.65) +
  geom_smooth(method = "lm", color = "#B22222", se = FALSE, linewidth = 1.2) +
  labs(title = "Figure 1: Stress Level by Remote Ratio", x = "Remote Ratio", y = "Stress Level") +
  theme_minimal(base_size = 14)

ggplot(employee_burnout, aes(x = Age, y = StressLevel)) +
  geom_point(color = "#4682B4", size = 3, alpha = 0.65) +
  geom_smooth(method = "lm", color = "#B22222", se = FALSE, linewidth = 1.2) +
  labs(title = "Figure 2: Stress Level by Age", x = "Age", y = "Stress Level") +
  theme_minimal(base_size = 14)

# Correlation Calculations
cor_remote <- round(cor(employee_burnout$RemoteRatio, employee_burnout$StressLevel), 3)
cor_age    <- round(cor(employee_burnout$Age, employee_burnout$StressLevel), 3)

# ANOVA Test
synthetic.aov <- aov(StressLevel ~ JobRole, data = employee_burnout)
summary(synthetic.aov)

# Linear Models
model_full <- lm(StressLevel ~ Age + Gender + JobRole + Experience + WorkHoursPerWeek +
                   RemoteRatio + SatisfactionLevel + Burnout, data = employee_burnout)
summary(model_full)
model_reduced <- lm(StressLevel ~ WorkHoursPerWeek + SatisfactionLevel + Burnout,
                    data = employee_burnout)
summary(model_reduced)

# PMSE Comparison
pmse_full    <- mean((employee_burnout$StressLevel - predict(model_full))^2)
pmse_reduced <- mean((employee_burnout$StressLevel - predict(model_reduced))^2)

# Load Second Dataset
remotework_health_impact <- read.csv("C:/Users/myska/Downloads/post_pandemic_remote_work_health_impact_2025 (1).csv")
str(remotework_health_impact)

# Checking for missing values
sapply(remotework_health_impact, anyNA)  

# Factor Conversion
factor_vars <- c("Gender", "Region", "Industry", "Job_Role", "Work_Arrangement",
                 "Hours_Per_Week", "Mental_Health_Status", "Burnout_Level",
                 "Work_Life_Balance_Score", "Physical_Health_Issues",
                 "Social_Isolation_Score", "Salary_Range")
remotework_health_impact[factor_vars] <- lapply(remotework_health_impact[factor_vars], factor)

# Logistic Regression
remotework_health_impact$Burnout_High <- ifelse(remotework_health_impact$Burnout_Level == "High", 1, 0)
burnout_work_model <- glm(Burnout_High ~ Work_Arrangement * Age,
                          data = remotework_health_impact,
                          family = binomial)
summary(burnout_work_model)
burnout_work_full_model <- glm(Burnout_High ~ Work_Arrangement * Age + Gender +
                                 Mental_Health_Status + Job_Role,
                               data = remotework_health_impact,
                               family = binomial)
summary(burnout_work_full_model)

# Burnout Count Tables (Online and Onsite)
real <- remotework_health_impact
remote_burnout <- real %>% filter(Work_Arrangement == "Remote") %>% count(Job_Role, Burnout_Level)
onsite_burnout <- real %>% filter(Work_Arrangement == "Onsite") %>% count(Job_Role, Burnout_Level)

# Burnout Level by Work Arrangement Normalized Stacked Bar Plot
ggplot(real, aes(x = Work_Arrangement, fill = Burnout_Level)) +
  geom_bar(position = "fill") +
  labs(title = "Burnout Level Distribution by Work Arrangement",
       x = "Work Arrangement",
       y = "Proportion",
       fill = "Burnout Level") +
  theme_minimal(base_size = 14)
