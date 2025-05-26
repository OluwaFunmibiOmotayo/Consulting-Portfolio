#-----------------------------------------------------------------------------
# FUNGICIDE DATA ANALYSIS
# Comparison of three different modeling approaches: 
# 1. Linear Mixed Model (Normal distribution)
# 2. Generalized Linear Mixed Model with Gamma distribution
# 3. Log-normal Mixed Model
#-----------------------------------------------------------------------------

# Load required packages
library(lme4)       # For mixed models
library(emmeans)    # For least squares means and pairwise comparisons
library(DHARMa)     # For residual diagnostics
library(ggplot2)    # For enhanced plotting
library(car)        # For additional diagnostic tools

#-----------------------------------------------------------------------------
# 1. LINEAR MIXED MODEL (Normal Distribution)
#-----------------------------------------------------------------------------

# Fit linear mixed model
lmm_model <- lmer(Avg_Measurement ~ Treatments + Dose:Treatments + 
                    (1|Species) + (1|Species:Dose:Treatments), 
                  data = Fungicide_Data, REML = FALSE)

# Model summary
summary(lmm_model)

# Anova to test fixed effects
anova(lmm_model)






# Least squares means and pairwise comparisons
# For Treatments
lmm_emm_treatments <- emmeans(lmm_model, ~ Treatments)
lmm_pairs_treatments <- pairs(lmm_emm_treatments, adjust = "tukey")
print(lmm_pairs_treatments)

# For Dose within Treatments
lmm_emm_dose <- emmeans(lmm_model, ~ Dose | Treatments)
lmm_pairs_dose <- pairs(lmm_emm_dose, adjust = "tukey")
print(lmm_pairs_dose)

# Diagnostic plots
par(mfrow = c(2, 2))

# Residuals vs Fitted
plot(lmm_model, main = "Residuals vs Fitted")

# QQ Plot of Residuals
qqnorm(residuals(lmm_model))
qqline(residuals(lmm_model), col = "red")

# Histogram of Residuals
hist(residuals(lmm_model), breaks = 30, 
     main = "Histogram of Residuals", 
     xlab = "Residuals")

# Scale-Location plot
plot(fitted(lmm_model), sqrt(abs(residuals(lmm_model))),
     main = "Scale-Location Plot",
     xlab = "Fitted values", 
     ylab = "√|Standardized residuals|")
abline(h = 0, lty = 2)

par(mfrow = c(1, 1))

# Formal test for normality
shapiro_test_lmm <- shapiro.test(residuals(lmm_model))
print(shapiro_test_lmm)

# Enhanced diagnostics using DHARMa
lmm_simres <- simulateResiduals(fittedModel = lmm_model)
plot(lmm_simres)

#-----------------------------------------------------------------------------
# 2. GENERALIZED LINEAR MIXED MODEL (Gamma Distribution)
#-----------------------------------------------------------------------------

# Add small offset to avoid zeros if necessary
# Only run this if you have zero or very small values
# Fungicide_Data$Adj_Measurement <- Fungicide_Data$Avg_Measurement + 0.0001

Fungicide_Data$Avg_Measurement_new <- Fungicide_Data$Avg_Measurement + 0.0001  # Small offset to avoid zero issues

# Fit GLMM with Gamma distribution and log link
gamma_model <- glmer(Avg_Measurement_new ~ Treatments + Dose:Treatments + 
                       (1|Species) + (1|Species:Dose:Treatments), 
                     family = Gamma(link = "log"), 
                     data = Fungicide_Data)

# Model summary
summary(gamma_model)

glmm_gamma <- glmer(Avg_Measurement ~ Treatments + Dose:Treatments + 
                      (1 | Species) + (1 | Species:Dose:Treatments), 
                    family = Gamma(link = "log"), 
                    data = Fungicide_Data)
summary(glmm_gamma)

# Least squares means and pairwise comparisons with back-transformation
# For Treatments
gamma_emm_treatments <- emmeans(gamma_model, ~ Treatments, type = "response")
gamma_pairs_treatments <- pairs(gamma_emm_treatments, adjust = "tukey")
print(gamma_pairs_treatments)

# For Dose within Treatments
gamma_emm_dose <- emmeans(gamma_model, ~ Dose | Treatments, type = "response")
gamma_pairs_dose <- pairs(gamma_emm_dose, adjust = "tukey")
print(gamma_pairs_dose)

# Diagnostic plots
par(mfrow = c(2, 2))

# Pearson Residuals vs Fitted
plot(fitted(gamma_model), residuals(gamma_model, type = "pearson"),
     main = "Pearson Residuals vs Fitted",
     xlab = "Fitted values", 
     ylab = "Pearson residuals")
abline(h = 0, lty = 2)

# QQ Plot of Pearson Residuals
qqnorm(residuals(gamma_model, type = "pearson"))
qqline(residuals(gamma_model, type = "pearson"), col = "red")

# Histogram of Pearson Residuals
hist(residuals(gamma_model, type = "pearson"), breaks = 30,
     main = "Histogram of Pearson Residuals", 
     xlab = "Pearson Residuals")

# Scale-Location plot
plot(fitted(gamma_model), sqrt(abs(residuals(gamma_model, type = "pearson"))),
     main = "Scale-Location Plot",
     xlab = "Fitted values", 
     ylab = "√|Pearson residuals|")
abline(h = 0, lty = 2)

par(mfrow = c(1, 1))

# Enhanced diagnostics using DHARMa
gamma_simres <- simulateResiduals(fittedModel = gamma_model)
plot(gamma_simres)

# Test for overdispersion
testDispersion(gamma_simres)

# Test for zero-inflation
testZeroInflation(gamma_simres)

#-----------------------------------------------------------------------------
# 3. LOG-NORMAL MIXED MODEL
#-----------------------------------------------------------------------------

# Fit log-normal mixed model by log-transforming the response
lognormal_model <- lmer(log(Avg_Measurement) ~ Treatments + Dose:Treatments + 
                          (1|Species) + (1|Species:Dose:Treatments),  
                        data = Fungicide_Data, REML = TRUE)

# Model summary
summary(lognormal_model)

# Least squares means and pairwise comparisons with back-transformation
# For Treatments (back-transformed to original scale)
lognormal_emm_treatments <- emmeans(lognormal_model, ~ Treatments)
lognormal_pairs_treatments <- pairs(lognormal_emm_treatments, adjust = "tukey")
# Back-transform to original scale
lognormal_emm_treatments_orig <- exp(lognormal_emm_treatments)
print(lognormal_emm_treatments_orig)
print(lognormal_pairs_treatments)

# For Dose within Treatments
lognormal_emm_dose <- emmeans(lognormal_model, ~ Dose | Treatments)
lognormal_pairs_dose <- pairs(lognormal_emm_dose, adjust = "tukey")
# Back-transform to original scale
lognormal_emm_dose_orig <- exp(lognormal_emm_dose)
print(lognormal_emm_dose_orig)
print(lognormal_pairs_dose)

# Diagnostic plots
par(mfrow = c(2, 2))

# Residuals vs Fitted
plot(lognormal_model, main = "Residuals vs Fitted (Log Scale)")

# QQ Plot of Residuals
qqnorm(residuals(lognormal_model))
qqline(residuals(lognormal_model), col = "red")

# Histogram of Residuals
hist(residuals(lognormal_model), breaks = 30,
     main = "Histogram of Residuals (Log Scale)", 
     xlab = "Residuals")

# Scale-Location plot
plot(fitted(lognormal_model), sqrt(abs(residuals(lognormal_model))),
     main = "Scale-Location Plot (Log Scale)",
     xlab = "Fitted values", 
     ylab = "√|Standardized residuals|")
abline(h = 0, lty = 2)

par(mfrow = c(1, 1))

# Formal test for normality of log-residuals
shapiro_test_log <- shapiro.test(residuals(lognormal_model))
print(shapiro_test_log)

# Enhanced diagnostics using DHARMa
lognormal_simres <- simulateResiduals(fittedModel = lognormal_model)
plot(lognormal_simres)

#-----------------------------------------------------------------------------
# MODEL COMPARISON
#-----------------------------------------------------------------------------

# AIC comparison of the three models
AIC_lmm <- AIC(lmm_model)
AIC_gamma <- AIC(gamma_model)
AIC_lognormal <- AIC(lognormal_model)

model_comparison <- data.frame(
  Model = c("Linear Mixed Model", "Gamma GLMM", "Log-normal Mixed Model"),
  AIC = c(AIC_lmm, AIC_gamma, AIC_lognormal)
)

print(model_comparison)

# Model with lowest AIC is preferred
cat("The best-fitting model based on AIC is:", 
    model_comparison$Model[which.min(model_comparison$AIC)], "\n")

# Optional: create publication-ready plots of results from the best model
# Example for treatments from the best model (assuming linear mixed model)
ggplot(as.data.frame(lmm_emm_treatments), aes(x = Treatments, y = emmean)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2) +
  labs(title = "Estimated Marginal Means by Treatment",
       x = "Treatment",
       y = "Estimated Response") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))