# Load necessary libraries
library(ggplot2)  # For visualization
library(car)      # For checking assumptions
library(dplyr)    # For data manipulation
library(car)

# Import data 
# my_data <- read.delim(file.choose())  # For .txt files
# my_data <- read.csv(file.choose())    # For .csv files

# Load the dataset
dat_CRD <- data.frame(
  Sample = 1:14,
  TRT1 = c(30.1, 21.1, 29.3, 24.5, 29.1, 25.4, 31.8, 30.7, 26.6, 22.5, 26.0, 21.7, 34.8, 35.4),
  TRT2 = c(28.2, 22.3, 20.6, 16.7, 22.8, 22.7, 28.1, 28.3, 24.7, 19.2, 23.6, 19.0, 34.3, 31.3),
  TRT3 = c(24.4, 20.2, 23.7, 18.5, 27.7, 26.7, 30.6, 24.0, 26.0, 23.6, 15.5, 16.3, 23.4, 19.8)
)

# Convert the above data to long format
dat_CRD_long <- pivot_longer(dat_CRD, cols = -Sample, names_to = "Treatment", values_to = "Response")

dat_CRD_long$Treatment <- as.factor(dat_CRD_long$Treatment)

# Visualization
## Boxplot
ggplot(dat_CRD_long, aes(x = Treatment, y = Response, fill = Treatment)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Treatment Effect on Maize Yield", x = "Treatment", y = "Response")

# Histogram
ggplot(dat_CRD_long, aes(x = Response)) +
  geom_histogram(aes(y = ..density..), bins = 15, fill = "skyblue", color = "black", alpha = 0.7) +
  geom_density(color = "red", size = 0.5) +
  theme_minimal() +
  labs(title = "Distribution of Maize Yield", x = "Response", y = "Density")


# Running One-Way ANOVA
anova_mod <- aov(Response ~ Treatment, data = dat_CRD_long)
summary(anova_mod)

par(mfrow = c(2,2))
# Test Assumptions
plot(anova_mod)
par(mfrow = c(1,1))

# Extract the residuals
aov_residuals <- residuals(object = anova_mod)

## Normality check
shapiro.test(x = aov_residuals)  # Shapiro-Wilk test for normality

## Homogeneity of variance
leveneTest(aov_residuals ~ dat_CRD_long$Treatment)  # Levene’s test


# Post-hoc tests (Tukey's HSD)
tukey_test <- TukeyHSD(anova_mod)
print(tukey_test)

# Visualizing post-hoc results
plot(TukeyHSD(anova_mod), las = 2)

data_summary <- group_by(dat_CRD_long, Treatment) %>%
  summarise(mean = mean(Response), sd = sd(Response), n = n())
data_summary

####################################################################################################################################################
#Data Analysis 2
####################################################################################################################################################
# Creating a data frame 
dat <- data.frame(
  ID = c("P01", "P02", "P03", "P04", "P05", "P06", 
         "P07", "P08", "P09", "P10", "P11", "P12",
         "P13", "P14", "P15", "P16", "P17", "P18",
         "P19", "P20", "P21", "P22", "P23", "P24"),
  Group = c(rep("Standard",6),rep("Probiotic",6), rep("Enzyme",6),rep("Essential Oil",6)),
  Weight = c(12.8, 13.5, 11.9, 12.3, 14.1, 13.0,
             15.2, 16.4, 14.8, 15.9, 17.2, 16.1,
             14.5, 15.3, 13.9, 14.8, 15.6, 14.2,
             13.7, 14.4, 12.9, 13.5, 14.8, 13.2)
)

# Convert Group to factor to maintain order
dat$Group <- factor(data$Group, levels = c("Standard", "Probiotic", "Enzyme", "Essential Oil"))

# Looking at the first few rows
head(dat)

# Data Summary
summary(dat)

# Data Structure
str(dat)


# Visualization
## Boxplot
ggplot(dat, aes(x = Group, y = Weight, fill = Group)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Boxplot of Feed Supplement on Weight", x = "Group", y = "Weight")

# Histogram
ggplot(dat, aes(x = Weight)) +
  geom_histogram(aes(y = ..density..), bins = 15, fill = "skyblue", color = "black", alpha = 0.7) +
  geom_density(color = "red", size = 0.5) +
  theme_minimal() +
  labs(title = "Distribution of  Weight", x = "Response", y = "Density")+
  facet_wrap(dat$Group)


# Running One-Way ANOVA
anova_mod2 <- aov(Weight ~ Group, data = dat)
summary(anova_mod2)

par(mfrow = c(2,2))
# Test Assumptions
plot(anova_mod2)
par(mfrow = c(1,1))

# Extract the residuals
aov_residuals <- residuals(object = anova_mod2)

# Normality check
shapiro.test(x = aov_residuals)

## Homogeneity of variance
leveneTest(aov_residuals ~ dat$Group) 


# Post-hoc tests (Tukey's HSD)
tukey_test <- TukeyHSD(anova_mod2)
print(tukey_test)

# Visualizing post-hoc results
plot(TukeyHSD(anova_mod2), las = 2)

data_summary2 <- group_by(dat, Group) %>%
  summarise(mean = mean(Weight), sd = sd(Weight), n = n())
data_summary2


######################################################################## Exercise ##################################################################
# Perform the following using R built in Dataset

# Load the dataset
data(ToothGrowth)

# Display the structure of the dataset
str(ToothGrowth)

#Dataset Description
?ToothGrowth

# View the first few rows
head(ToothGrowth)

# Summary statistics
summary(ToothGrowth)

##################################################################################################################################################




















