
library(readr)
library(dplyr)
library(car)
library(lme4)
library(glmmTMB)
library(tidyr)
library(ggplot2)
library(stringr)
library(forcats)
library(tidyr)
library(patchwork)

#Data Import
Fungicide_Data <- read_csv("Fungicide Exp 5 Design.csv")
View(Fungicide_Data)

str(Fungicide_Data)


# Pivot the measurement columns into long format
Fungicide_long <- Fungicide_Data %>%
  pivot_longer(cols = starts_with("Measurement"), 
               names_to = "Measurement_Number", 
               values_to = "Measurement_Value") %>%
  filter(!is.na(Measurement_Value))  # Remove missing values if needed

# Quick check
head(Fungicide_long)


# Summarize if needed
fungicide_summary_long <- Fungicide_long %>%
  group_by(Treatments, Measurement_Number) %>%
  summarise(Avg_Length = mean(Measurement_Value, na.rm = TRUE)) %>%
  ungroup()

# Plot
ggplot(fungicide_summary_long, aes(x = Treatments, y = Avg_Length, fill = Measurement_Number)) +
  geom_bar(stat = "identity", position = "dodge") +  # You can also try position = "stack"
  coord_polar(start = 0) +
  ylim(0, max(fungicide_summary_long$Avg_Length) * 1.5) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    legend.position = "right"
  ) +
  labs(fill = "Measurement", title = "Individual Measurements by Treatment")

fungicide_summary_long2 <- Fungicide_long %>%
  group_by(Species, Measurement_Number) %>%
  summarise(Avg_Length = mean(Measurement_Value, na.rm = TRUE)) %>%
  ungroup()

ggplot(fungicide_summary_long2, aes(x = Species, y = Avg_Length, fill = Measurement_Number)) +
  geom_bar(stat = "identity", position = "dodge") +  # You can also try position = "stack"
  coord_polar(start = 0) +
  ylim(0, max(fungicide_summary_long$Avg_Length) * 1.5) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    legend.position = "right"
  ) +
  labs(fill = "Measurement", title = "Individual Measurements by Species")


Fungicide_Data$Avg_Measurement <- rowMeans(Fungicide_Data[,c("Measurement 1","Measurement 2", "Measurement 3","Measurement 4")], na.rm = TRUE)
summary(Fungicide_Data$Avg_Measurement)

Fungicide_Data[is.na(Fungicide_Data$Avg_Measurement),]
Fungicide_Data <- Fungicide_Data[!is.na(Fungicide_Data$Avg_Measurement), ]


# Remove the four measurement columns
Fungicide_Data <- Fungicide_Data[, !(names(Fungicide_Data) %in% c("Measurement 1", "Measurement 2", "Measurement 3", "Measurement 4","Comments","Dates"))]
str(Fungicide_Data)


# Rename variables
Fungicide_Data <- Fungicide_Data %>% rename(Dose = ...10)

#Converting to a Factor
Fungicide_Data <- Fungicide_Data %>%
  mutate(
    Species = as.factor(Species),
    Isolates = as.factor(Isolates),
    Treatments = as.factor(Treatments),
    Dilution = as.factor(Dilution),
    techRep = as.factor(techRep),
    Exp_Code = as.factor(Exp_Code),
    Dose = as.factor(Dose)
  )

# Check the structure of the dataset
str(Fungicide_Data)
summary(Fungicide_Data)

#######################################Exploratory Data Analysis#########################################################
hist(Fungicide_Data$Avg_Measurement, 
     breaks = 30, 
     main = "Histogram of Average Fungal Measurement",
     xlab = "Average Measurement",
     col = "skyblue")

ggplot(Fungicide_Data, aes(x = Species, y = Avg_Measurement)) +
  geom_boxplot(fill = "skyblue") +
  theme_minimal() +   #white background
  labs(title = "Avg Measurement by Species",
       x = "Species",
       y = "Average Measurement")

ggplot(Fungicide_Data, aes(x = Treatments, y = Avg_Measurement)) +
  geom_boxplot(fill = "lightpink") +
  theme_minimal() +   #white background
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Avg Measurement by Treatment",
       x = "Treatment",
       y = "Average Measurement")


# summarize Avg_Measurement for each Treatment
fungicide_summary <- Fungicide_Data %>%
  group_by(Treatments) %>%
  summarise(Avg_Length = mean(Avg_Measurement, na.rm = TRUE)) %>%
  arrange(desc(Avg_Length))

# Create a circular bar plot
ggplot(fungicide_summary, aes(x = Treatments, y = Avg_Length, fill = Treatments)) +
  geom_bar(stat = "identity") +
  coord_polar(start = 0) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    legend.position = "right"
  ) +
  labs(fill = "Treatment Type", title = "Average Measurement by Treatment")


# summarize Avg_Measurement for each Species
fungicide_summary <- Fungicide_Data %>%
  group_by(Species) %>%
  summarise(Avg_Length = mean(Avg_Measurement, na.rm = TRUE)) %>%
  arrange(desc(Avg_Length))

# Create a circular bar plot
ggplot(fungicide_summary, aes(x = Species, y = Avg_Length, fill = Species)) +
  geom_bar(stat = "identity") +
  coord_polar(start = 0) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    legend.position = "right"
  ) +
  labs(fill = "Species", title = "Average Measurement by Species")

library(ggplot2)

ggplot(Fungicide_Data, aes(x = Species, y = Avg_Measurement, color = Dose)) +
  geom_point(position = position_jitter(width = 0.2), size = 2) +
  facet_wrap(~ Treatments) +
  theme_minimal() +
  labs(title = "Species Crossed with Dose within Treatments",
       x = "Species",
       y = "Average Measurement") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # tilt x labels if Species names are long

ggplot(Fungicide_Data, aes(x = Species, y = Avg_Measurement, fill = Dose)) +
  geom_boxplot() +
  facet_wrap(~ Treatments) +
  theme_minimal() +
  labs(title = "Dose Nested within Species across Treatments",
       x = "Dose",
       y = "Average Measurement") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

library(ggplot2)

ggplot(Fungicide_Data, aes(x = Species, y = Avg_Measurement, fill = Dose)) +
  geom_boxplot() +
  facet_wrap(~ Treatments, scales = "free_x") +  # allow x-axis to adjust for each Treatment
  theme_minimal() +
  labs(title = "Dose Nested within Species across Treatments",
       x = "Species",  # Corrected x label
       y = "Average Measurement") +
  theme(
    strip.background = element_rect(color = "black", fill = "gray90", size = 1.5), # thicker facet strip border
    panel.spacing = unit(1, "lines"),  # more space between panels
    panel.border = element_rect(color = "black", fill = NA, size = 1.2),  # visible black border around each panel
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggplot(Fungicide_Data, aes(x = Species, y = Avg_Measurement, fill = Dose)) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  facet_wrap(~ Treatments) +
  theme_minimal() +
  labs(title = "Average Measurement across Species, Doses, and Treatments",
       x = "Species",
       y = "Average Measurement") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(Fungicide_Data, aes(x = Avg_Measurement, y = Dose, fill = Dose)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA, color = "black") +  # <-- add color = "black"
  geom_jitter(height = 0.2, alpha = 0.5, color = "black") +
  facet_wrap(~Species) +
  labs(x = "Average Measurement", y = "Dose Group", fill = "Dose") +
  theme_bw() +  # still using white background
  theme(legend.position = "none")

ggplot(Fungicide_Data, aes(x = Avg_Measurement, y = Dose, fill = Dose)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA, color = "black") +  # <-- add color = "black"
  geom_jitter(height = 0.2, alpha = 0.5, color = "black") +
  facet_wrap(~Treatments) +
  labs(x = "Average Measurement", y = "Dose Group", fill = "Dose") +
  theme_bw() +  # still using white background
  theme(legend.position = "none")


#Proportion of Zero
library(skimr)
skim(Fungicide_Data$Avg_Measurement)

percentage_zeros <- sum(Fungicide_Data$Avg_Measurement == 0, na.rm = TRUE) / nrow(Fungicide_Data) * 100
print(percentage_zeros)

#Distribution of Species and treatments
table(Fungicide_Data$Species)
table(Fungicide_Data$Treatments)


# Check if any groups have complete separation
table(Fungicide_Data$Species, Fungicide_Data$Avg_Measurement > 0)
table(Fungicide_Data$Treatments, Fungicide_Data$Avg_Measurement > 0)

#Cross Tabulation of Species and Treatments
table(Fungicide_Data$Species, Fungicide_Data$Treatments)

table(Fungicide_Data$Treatments, Fungicide_Data$Dose)


###################################################################################################################################
                                                    #Normal Distribution
###################################################################################################################################
library(lme4)
library(emmeans)
model1 <- lmer(Avg_Measurement ~ Species * Treatments + 
                 (1 | Dose:Treatments) + (1 | Species:Dose:Treatments), 
               data = Fungicide_Data)

# ANOVA Table
summary(model1)

car::Anova(model1, type = 3)

# Pairwise Comparisons (Tukey-adjusted)
emmeans(model1, pairwise ~ Treatments, adjust = "tukey")
emmeans(model1, pairwise ~ Species, adjust = "tukey")
emmeans(model1, pairwise ~ Treatments | Species, adjust = "tukey")
emmeans(model1, pairwise ~ Treatments * Species, adjust = "tukey")

###################################################################################################################################

#####################################################################################################################################################
# Model diagnostics
#####################################################################################################################################################
plot(model1)

par(mfrow = c(2,2))

#Normality of residuals

# QQ plot
qqnorm(residuals(model1))
qqline(residuals(model1))

# Histogram of residuals
hist(residuals(model1))

# Shapiro-Wilk test
shapiro.test(residuals(model1))

#Homogeneity of variance

# Plot residuals vs fitted values
plot(fitted(model1), residuals(model1))
abline(h=0, col="red")

# Check for influential observations
# Cook's distance plot
plot(cooks.distance(model1))

# 3. Independence and Homogeneity
# Plot residuals vs each predictor
plot(residuals(model1) ~ Fungicide_Data$Treatments)
plot(residuals(model1) ~ Fungicide_Data$Species)

par(mfrow = c(1,1))
###################################################################################################################################

#####################################################################################################################################
                                      # Normal Distribution on Filtered Data
#####################################################################################################################################
Fungicide_filtered <- subset(Fungicide_Data, Avg_Measurement > 0)

model2 <- lmer(Avg_Measurement ~ Species * Treatments + 
                 (1 | Dose:Treatments) + (1 | Species:Dose:Treatments), 
               data = Fungicide_filtered)
summary(model2)

car::Anova(model1, type = 3)

# Pairwise Comparisons (Tukey-adjusted)
emmeans(model2, pairwise ~ Treatments, adjust = "tukey")
emmeans(model1, pairwise ~ Species, adjust = "tukey")
emmeans(model2, pairwise ~ Treatments | Species, adjust = "tukey")
emmeans(model2, pairwise ~ Treatments * Species, adjust = "tukey")
####################################################################################################################################

#####################################################################################################################################
                                   # Gamma Distribution on Filtered Data
#####################################################################################################################################

model3 <- glmmTMB(Avg_Measurement ~ Species * Treatments + 
                           (1 | Dose:Treatments) + (1 | Species:Dose:Treatments), 
                         data = Fungicide_filtered, 
                         family = Gamma(link = "log"))

# ANOVA Table
summary(model3)

car::Anova(model3, type = 3)

# Pairwise Comparisons (Tukey-adjusted)
emmeans(model3, pairwise ~ Treatments, adjust = "tukey")
emmeans(model1, pairwise ~ Species, adjust = "tukey")
emmeans(model3, pairwise ~ Treatments | Species, adjust = "tukey")
emmeans(model3, pairwise ~ Treatments * Species, adjust = "tukey")
####################################################################################################################################


######################################################################################################################################################
                                                  #Zero-Inflated Gamma Model
########################################################################################################################################
#Fungicide_Data$Species <- factor(Fungicide_Data$Species, levels = c("Fusarium oxysporum", "Diaporthe longicolla", 
#                                                                    "Fusarium solani", "Rhizoctonia solani"))

Fungicide_Data$Treatments <- factor(Fungicide_Data$Treatments, levels = c("DelaroComplete 3 active ingr (Proth+Trif+Fluop)", "Endura 1 active ingr (Boscalid)", 
                                                                    "Quadris 1 active ingr (Azoxystrobin)", "Topguard 1 active ingr (Flutriafol)", "Topguard EQ 2 active ingr (Flut+Azoxys)"))
zigamma_model <- glmmTMB(Avg_Measurement ~ Species * Treatments + (1 | Dose:Treatments) +(1 | Species:Dose:Treatments),  
                         family = ziGamma(link = "log"), 
                         ziformula = ~ 1, 
                         data = Fungicide_Data)
summary(zigamma_model)



# zigamma_model2 <- glmmTMB(Avg_Measurement ~ Species * Treatments + (1 | Dose:Treatments) + (1 | Species:Dose:Treatments),
#                           family = ziGamma(link = "log"), 
#                           ziformula = ~ Species + Treatments, 
#                           data = Fungicide_Data)
# summary(zigamma_model2)
# 
# 
# zigamma_model3 <- glmmTMB(Avg_Measurement ~ Species * Treatments + (1 | Dose:Treatments) +(1 | Species:Dose:Treatments),  
#                          family = ziGamma(link = "log"), 
#                          ziformula = ~ Species + Treatments + (1 | Dose:Treatments), 
#                          data = Fungicide_Data)
# 
# # Summary of the Model
# summary(zigamma_model3)

# anova(zigamma_model, zigamma_model2,zigamma_model3,zigamma_model4, test = "Chisq")

car::Anova(zigamma_model, type = 3)

library(parameters)
model_parameters(zigamma_model, exponentiate = T)


library(emmeans)

#Simple effects

calc.est1 <- emmeans(zigamma_model, ~Species * Treatments, type="response")

calc.est1

contrast(calc.est1)


# Convert to data frame
emm_df <- as.data.frame(calc.est1)

emm_focus <- emm_df %>%
  filter(Species == "Fusarium oxysporum" & 
           Treatments %in% c("Quadris 1 active ingr (Azoxystrobin)", "Topguard 1 active ingr (Flutriafol)", "Topguard EQ 2 active ingr (Flut+Azoxys)"))

ggplot(emm_focus, aes(x = Treatments, y = response)) +
  geom_point(size = 3, color = "darkblue") +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2, color = "darkblue") +
  geom_line(aes(group = 1), color = "darkblue") +
  theme_minimal(base_size = 13) +
  labs(
    title = "Interaction Plot",
    x = "Treatment",
    y = "Estimated Average Growth (response scale)"
  )


#Pairwise differences
test.info1<- contrast(calc.est1, method="pairwise", type="response")
test.info1


# Extract the summary of pairwise contrasts
test.info1_summary <- summary(test.info1)

# Filter significant results (p-value < 0.05)
significant_results <- test.info1_summary[test.info1_summary$p.value < 0.05, ]

# Significant results
print(significant_results)

conf <- confint(test.info1, adjust="tukey", level=0.95)

# Filter only for significant comparisons
conf_significant <- conf[conf$contrast %in% significant_results$contrast, ]

# Print the filtered confidence intervals
print(conf_significant)

# Extract BLUPs (random effects)
blup_results <- ranef(zigamma_model)

# View the random effects
print(blup_results)

# Extract and prepare BLUPs
blup_df <- as.data.frame(blup_results$cond$`Dose:Treatments`)

blup_df$Group <- rownames(blup_df)
colnames(blup_df) <- c("BLUP", "Group")

# Sort for cleaner plotting
blup_df <- blup_df %>%
  arrange(BLUP) %>%
  mutate(Group = factor(Group, levels = Group),
         Category = ifelse(BLUP <= 0, "≤ 0", "> 0"))

# Forest plot with color
ggplot(blup_df, aes(x = BLUP, y = Group, color = Category)) +
  geom_point(size = 3) +
  scale_color_manual(values = c("≤ 0" = "red", "> 0" = "blue")) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  theme_minimal(base_size = 11) +
  labs(
    title = "Forest Plot of BLUPs",
    x = "BLUP Estimate",
    y = "Group (Dose:Treatment)",
    color = "BLUP Value"
  )

# Extract and prepare BLUPs
blup_df <- as.data.frame(blup_results$cond$`Species:Dose:Treatments`)
blup_df$Group <- rownames(blup_df)
colnames(blup_df) <- c("BLUP", "Group")

# Separate the Group column into Species, Dose, and Treatments
blup_df <- blup_df %>%
  separate(Group, into = c("Species", "Dose", "Treatments"), sep = ":", remove = FALSE) %>%
  arrange(BLUP) %>%
  mutate(Group = factor(Group, levels = Group),
         Label = paste(Dose, Treatments, sep = ":"),
         Category = ifelse(BLUP <= 0, "≤ 0", "> 0"))

library(stringr)
# Remove text in parentheses from Label
blup_df <- blup_df %>%
  mutate(
    Label = str_remove(Label, "\\s*\\([^\\)]+\\)"),
    Label = str_trim(Label)
  )

# Forest plot faceted by Species, using Label without Species
ggplot(blup_df, aes(x = BLUP, y = Label, color = Category)) +
  geom_point(size = 3) +
  scale_color_manual(values = c("≤ 0" = "red", "> 0" = "blue")) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  facet_wrap(~Species, scales = "free_y") +
  theme_minimal(base_size = 11) +
  labs(
    title = "Forest Plot of BLUPs by Species",
    x = "BLUP Estimate",
    y = "Dose:Treatment",
    color = "BLUP Value"
  )

# Prepare data
blup_df <- as.data.frame(blup_results$cond$`Species:Dose:Treatments`)
blup_df$Group <- rownames(blup_df)
colnames(blup_df) <- c("BLUP", "Group")

blup_df <- blup_df %>%
  separate(Group, into = c("Species", "Dose", "Treatments"), sep = ":", remove = FALSE) %>%
  mutate(
    Label = paste(Dose, Treatments, sep = ":"),
    Label = str_remove(Label, "\\s*\\([^\\)]+\\)"),
    Label = str_trim(Label),
    Category = ifelse(BLUP <= 0, "≤ 0", "> 0")
  )

# Function to create individual plots
plot_species_blup <- function(species_name, data) {
  data_species <- data %>%
    filter(Species == species_name) %>%
    mutate(Label = fct_reorder(Label, BLUP, .desc = FALSE))
  
  ggplot(data_species, aes(x = BLUP, y = Label, color = Category)) +
    geom_point(size = 3) +
    scale_color_manual(values = c("≤ 0" = "red", "> 0" = "blue")) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    theme_minimal(base_size = 11) +
    labs(
      title = species_name,
      x = "BLUP Estimate",
      y = "Dose:Treatment",
      color = "BLUP Value"
    )
}

# List of species
species_list <- unique(blup_df$Species)

# Create plots for each species
plots <- lapply(species_list, plot_species_blup, data = blup_df)

# Combine using patchwork
combined_plot <- wrap_plots(plots, ncol = 2) +
  plot_annotation(title = "Forest Plot of BLUPs by Species")

# Show plot
print(combined_plot)


######################################################################################################################################

########################################## Model Fit for Zero-Inflated ################################################################
library(DHARMa)
sim_res <- simulateResiduals(zigamma_model3)
plot(sim_res)  

resid_plot <- plotResiduals(sim_res, form = Fungicide_Data$Avg_Measurement)
testDispersion(sim_res)


# Extract residuals and fitted values
library(lmtest)
bp_test <- bptest(zigamma_model2)
print(bp_test) #Heteroscedasticity  detected

VarCorr(zigamma_model2)
testZeroInflation(sim_res)

#######################################################################################################################################
                                         #Tweedie Model
#######################################################################################################################################
tweedie_model <- glmmTMB(
  Avg_Measurement ~ Species * Treatments + (1 | Dose:Treatments) + (1 | Species:Dose:Treatments),  
  family = tweedie(link = "log"),  
  data = Fungicide_Data
)
#tweedie_model <- update(tweedie_model, dispformula = ~ Species * Treatments)
summary(tweedie_model)

car::Anova(tweedie_model, type = 3)


model_parameters(tweedie_model, exponentiate = T)
####################################################################################################################################
########################################## Model Fit for Tweedie ###################################################################
tweedie_res <- simulateResiduals(tweedie_model)
plot(tweedie_res)


testDispersion(tweedie_res)

testZeroInflation(tweedie_res)
#######################################################################################################################################
#LogNormal Model
#######################################################################################################################################
ziln_model <- glmmTMB(Avg_Measurement ~ Species * Treatments + (1 | Dose:Treatments) + (1 | Species:Dose:Treatments),  
                      family = lognormal(link = "log"), 
                      ziformula = ~ Species + Treatments + (1 | Dose:Treatments), 
                      data = Fungicide_Data)

#ziln_model <- update(ziln_model, dispformula = ~ Species * Treatments)

# Summary of the Model
summary(ziln_model)

car::Anova(ziln_model, type = 3)
anova(ziln_model, update(ziln_model, dispformula = ~1))


########################################## Model Fit for LogNormal ###################################################################
ziln_res <- simulateResiduals(ziln_model)
plot(ziln_res)


testDispersion(ziln_model)

testZeroInflation(ziln_model)


##########################################################
AIC(zigamma_model, ziln_model, tweedie_model)















library(lme4)

vg_Measurement ~ Species * Treatments + 
  (1 | Dose:Treatments)+
  (1 | Species:Dose:Treatments) 

library(lme4)

binary_model <- glmer(I(Avg_Measurement > 0) ~ Species * Treatments + 
                      
                        (1 | Dose:Treatments) +  
                        (1 | Species:Dose:Treatments),
                      family = binomial,
                      data = Fungicide_Data)

summary(binary_model)


# Check if any groups have complete separation
table(Fungicide_Data$Species, Fungicide_Data$Avg_Measurement > 0)
table(Fungicide_Data$Treatments, Fungicide_Data$Avg_Measurement > 0)
summary(glm(Avg_Measurement > 0 ~ Treatments * logDose + I(logDose^2), data = Fungicide_Data, family = binomial))

#Endura 1 active ingr (Boscalid) and Quadris 1 active ingr (Azoxystrobin) have a complete sepration


# Then model the non-zero measurements
continuous_model <- lmer(Avg_Measurement ~ Species * Treatments + 
                           (1 | Dose:Treatments) +  
                           (1 | Species:Dose:Treatments),
                         data = subset(Fungicide_Data, Avg_Measurement > 0))
summary(continuous_model)

par(mfrow = c(2,2))
qqnorm(residuals(continuous_model))
qqline(residuals(continuous_model))
hist(residuals(continuous_model))
shapiro.test(residuals(continuous_model))

# Plot residuals vs fitted values
plot(fitted(continuous_model), residuals(continuous_model))
abline(h=0, col="red")

plot(cooks.distance(continuous_model))

plot(residuals(continuous_model) ~ Fungicide_Data$Treatments)
plot(residuals(continuous_model) ~ Fungicide_Data$Species)

par(mfrow = c(1,1))


gamma_model <- glmer(Avg_Measurement ~ Species * Treatments + 
                       (1 | Dose:Treatments) +  
                       (1 | Species:Dose:Treatments),
                     data = subset(Fungicide_Data, Avg_Measurement > 0),
                     family = Gamma(link = "log"))

summary(gamma_model)























#log-transformed dose (added small constant to handle zeros)
Fungicide_Data$logDose <- log10(Fungicide_Data$Dose + 0.01)

library(lme4)
library(emmeans)

mixed_model <- lmer(Avg_Measurement ~ Species|Treatments + Dose/Treatments, data = Fungicide_Data)

# Summary of the model
summary(mixed_model)

# LSMeans comparisons (equivalent to SAS lsmeans)
emmeans(lmm_model, pairwise ~ Treatments, adjust = "tukey")  # Treatments LSMeans
emmeans(lmm_model, pairwise ~ Treatments * Species, adjust = "tukey")  # Treatments x Species
emmeans(lmm_model, pairwise ~ Dose | Treatments * Species, adjust = "tukey")  # Slice Dose(Treatments) by Species



lme_model1 <- lmer(Avg_Measurement ~ Treatments + logDose/Treatments + 
                     (1 | Species) +                    
                     (1 | Species:Treatments)+
                     (1 | Species:logDose/Treatments),       
                   data = Fungicide_Data, REML = FALSE)

lme_model1 <- lmer(Avg_Measurement ~ Treatments + logDose:Treatments + 
                     (1 | Species) +                    
                     (1 | Species:Treatments) + 
                     (1 | Species:logDose:Treatments),       
                   data = Fungicide_Data, REML = FALSE)

summary(lme_model1)



lme_model2 <- lmer(Avg_Measurement ~  Treatments + logDose/Treatments + I(logDose^2)/Treatments + 
                     (1 |Species) +              
                     (1 | Species:Treatments)+
                     (1 |Species:logDose/Treatments), 
                   data = Fungicide_Data)
summary(lme_model2)



# ANOVA for fixed effects
anova(lme_model1)
anova(lme_model2)

car::Anova(lme_model,type ="III")
car::Anova(lme_model2,type ="III")


# Random effects structure
print(VarCorr(lme_model), comp = "Variance")

#Likelihood Ratio Test
anova(lme_model1, lme_model2)

# Extract random effects
ranef(lme_model2)

# Extract fixed effects
fixef(lme_model2)
#####################################################################################################################################################
# Model diagnostics
#####################################################################################################################################################
par(mfrow = c(2,2))

#Normality of residuals

# QQ plot
qqnorm(residuals(lme_model1))
qqline(residuals(lme_model1))

qqnorm(residuals(lme_model2))
qqline(residuals(lme_model2))

# Histogram of residuals
hist(residuals(lme_model1))

hist(residuals(lme_model2))

# Shapiro-Wilk test
shapiro.test(residuals(lme_model1))

shapiro.test(residuals(lme_model2))

#Homogeneity of variance

# Plot residuals vs fitted values
plot(fitted(lme_model1), residuals(lme_model1))
abline(h=0, col="red")

plot(fitted(lme_model2), residuals(lme_model2))
abline(h=0, col="red")

# 3. Independence and Homogeneity
# Plot residuals vs each predictor
plot(residuals(mod2) ~ Fungicide_Data$Treatments)
plot(residuals(mod2) ~ Fungicide_Data$Species)

# Check for influential observations
# Cook's distance plot
plot(cooks.distance(lme_model1))
######################################################################################################################################################

#nrow(Fungicide_Data)
#length(residuals(mod2))
#missing_rows <- !rownames(Fungicide_Data) %in% names(residuals(mod2))
#Fungicide_Data[missing_rows, ]

Fungicide_Data$logAvg_Measurement <- log(Fungicide_Data$Avg_Measurement + 1)

#####################################################################################################################################################
#Trying some other model
#####################################################################################################################################################





library(tweedie)
library(statmod)  
tweedie_glm <- glm(Avg_Measurement ~ Treatments + Species + Treatments:logDose,
                   family = tweedie(link.power = 0, var.power = 1.5),  
                   data = Fungicide_Data)

summary(tweedie_glm)

tweedie_glm <- glm(Avg_Measurement ~ Treatments + Species + Treatments:Species +
                     logDose * Treatments + Species:logDose:Treatments, 
                   family = tweedie(link.power = 0, var.power = 1.5),  
                   data = Fungicide_Data)

Fungicide_Data$Avg_Measurement_adj <- Fungicide_Data$Avg_Measurement + 1

lme_model <- glmmTMB(Avg_Measurement_adj ~ Treatments * logDose + I(logDose^2) + 
                       (1|Species) + (1|Treatments:Dose), 
                     data = Fungicide_Data, 
                     family = gaussian(link = "log"),
                     ziformula = ~ Treatments + logDose)  


lme_model_gamma <- glmmTMB(Avg_Measurement_adj ~ Treatments * logDose + I(logDose^2) + 
                             (1|Species) + (1|Treatments:Dose), 
                           data = Fungicide_Data, 
                           family = Gamma(link = "log"),
                           ziformula = ~ Treatments + logDose)

lme_model_tweedie <- glmmTMB(Avg_Measurement_adj ~ Treatments * logDose + I(logDose^2) + 
                               (1|Species) + (1|Treatments:Dose), 
                             data = Fungicide_Data, 
                             family = tweedie(link = "log"),
                             ziformula = ~ Treatments + logDose)
########################################################################################################################################

# Load required packages
library(glmmTMB)
library(parameters)  # for model_parameters function

# Create a zero-inflated Gamma model for your fungicide data
zigamma_fungicide <- glmmTMB(Avg_Measurement ~ Treatments + logDose/Treatments + 
                               (1 | Species) + (1 | Species:Treatments) + 
                               (1 | Species:logDose/Treatments),
                             family = ziGamma(link = "log"),
                             ziformula = ~ Treatments + logDose/Treatments,
                             data = Fungicide_Data)

# Examine model parameters with exponentiated coefficients
model_parameters(zigamma_fungicide, exponentiate = TRUE)











