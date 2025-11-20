##########################
### Linear Models in R ###
##########################

### Libraries ###

# install libraries (do it just once)
if (!require(nhanesA)) install.packages("nhanesA")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(ggpubr)) install.packages("ggpubr")

# or just...
# install.packages("nhanesA")

# load libraries (do it any time you start working)
library(nhanesA)
library(ggplot2)
library(ggpubr)

### Set paths ###

path_to_figures = "/Users/aniol/Documents/presentations/hy/Linear_Models_in_R/figures"

### Download public dataset - NHANES ###

# https://wwwn.cdc.gov/nchs/nhanes/

# Download demographics (for age)
demo <- nhanes("DEMO_J")   # 2017–2018 cycle

# Download body measures (for height & weight)
bmx <- nhanes("BMX_J")

# Merge by participant ID (SEQN)
df <- merge(demo, bmx, by = "SEQN")

# Select relevant columns: ID, age, height, and weight
df <- df[, c("SEQN", "RIDAGEYR", "RIAGENDR", "BMXHT", "BMXWT", "INDFMPIR")]
# RIAGENDR Gender 1 = Male, 2 = Female
# INDFMPIR Poverty Income Ratio (PIR) Continuous (income relative to federal poverty line)
# DMDEDUC2 Education level (for adults 20+) 1–5 (less than 9th grade → college graduate)

# Rename columns for clarity
names(df) <- c("ID", "Age", "Gender", "Height_cm", "Weight_kg", "SES")

### inspect dataset ###

# check object type or class
class(df)

# inspect objects of class "data.frame"
dim(df)      # dimensions
head(df)     # first rows
tail(df)     # last rows
str(df)      # structure overview
summary(df)  # summary statistics of each variable



## ========================= ##
## Linear regression models  ##
## ========================= ##

## Prepare data

# subset data for children
df_children = subset(df, Age < 18)

# remove NA values
df_clean <- na.omit(df_children[, c("Age", "Height_cm")])

# Add small random noise to Age for visualization purposes
# NOTE: This step is NOT necessary for fitting the linear model. 
# lm() works perfectly fine with discrete ages. 
# We only add this tiny jitter so that points with the same age (in whole years)
# do not overlap in the plot, making the visualization easier to interpret.
set.seed(123)  # for reproducibility
df_clean$Age <- df_clean$Age + runif(nrow(df_clean), -0.5, 0.5)







## --- model 1 - height as function of age, without intercept ---

# Fit linear model
model1 <- lm(Height_cm ~ 0 + Age, data = df_clean)

# Summary of fitted model
summary(model1)

# Extract coefficients
coef_age <- coef(model1)
intercept <- 0
slope <- coef_age["Age"]      # slope

# create regression line based on lm() outputs - option 2 without predict()
x_vals <- seq(0, 20, length.out = 200)
y_vals <- intercept + slope * x_vals
line_df <- data.frame(Age = x_vals, Height = y_vals)

# create regression line based on lm() outputs - option 1 with predict()
line_df <- data.frame(Age = seq(0, 20, length.out = 200))
line_df$Height <- predict(model1, newdata = line_df)

# Plot
plot <- ggplot(df_clean, aes(x = Age, y = Height_cm)) + 
  geom_point(alpha = 0.5) + 
  geom_line(data = line_df, aes(x = Age, y = Height), color = "red", size = 1) +
  labs(x = "Age (years)", y = "Height (cm)",
       title = "Height ~ Age") +
  theme_minimal() +
  ylim(0, 250) +
  xlim(0, 20)

print(plot)

# Save figure
figure_filename <- paste(path_to_figures, "/lm_model1.png", sep = "")
ggsave(figure_filename, plot = plot, width = 8, height = 6, dpi = 300)

## model 1 - plot residuals of 5 random observations

# Compute predicted values for all observed ages
df_clean$Predicted <- intercept + slope * df_clean$Age

# Select 5 random observations for residuals
set.seed(123)  # reproducibility
resid_indices <- sample(nrow(df_clean), 5)
resid_df <- df_clean[resid_indices, ]

# Plot
plot <- ggplot(df_clean, aes(x = Age)) + 
  geom_point(aes(y = Height_cm), color = "black", alpha = 0.5) +  # observed data
  geom_line(data = line_df, aes(x = Age, y = Height), color = "red", size = 1) +  # lm line
  geom_segment(data = resid_df,
               aes(x = Age, xend = Age,
                   y = Height_cm, yend = Predicted),
               color = "green", size = 1) +  # residuals for 5 points
  labs(x = "Age (years)", y = "Height (cm)",
       title = "Height ~ Age") +
  theme_minimal() +
  ylim(0, 250) +
  xlim(0, 20)

print(plot)

# save figure
figure_filename <- paste(path_to_figures, "/lm_model1_5residuals.png", sep = "")
ggsave(figure_filename, plot = plot, width = 8, height = 6, dpi = 300)


















## --- model 2 - height as function of age, without intercept ---

# Fit linear model
model2 <- lm(Height_cm ~ Age, data = df_clean)

# Summary of fitted model
summary(model2)

# Extract coefficients
coef_age <- coef(model2)
intercept <- coef_age[1]      # intercept
slope <- coef_age["Age"]      # slope

# create regression line based on lm() outputs - option 1 with predict()
line_df <- data.frame(Age = seq(0, 20, length.out = 200))
line_df$Height <- predict(model2, newdata = line_df)

# create regression line based on lm() outputs - option 2 without predict()
x_vals <- seq(0, 20, length.out = 200)
y_vals <- intercept + slope * x_vals
line_df <- data.frame(Age = x_vals, Height = y_vals)

# Plot
plot <- ggplot(df_clean, aes(x = Age, y = Height_cm)) + 
  geom_point(alpha = 0.5) + 
  geom_line(data = line_df, aes(x = Age, y = Height), color = "red", size = 1) +
  labs(x = "Age (years)", y = "Height (cm)",
       title = "Height ~ Age + Intercept") +
  theme_minimal() +
  ylim(0, 250) +
  xlim(0, 20)

print(plot)

# save figure
figure_filename <- paste(path_to_figures, "/lm_model2.png", sep = "")
ggsave(figure_filename, plot = plot, width = 8, height = 6, dpi = 300)













## --- model 3 - height as function of age and SES, without intercept ---

# Subset for children and clean data
df_children <- subset(df, Age < 18)
df_clean <- na.omit(df_children[, c("Age", "Height_cm", "SES")])

# Add small random noise for visualization (optional)
set.seed(123)
df_clean$Age <- df_clean$Age + runif(nrow(df_clean), -0.5, 0.5)

# Fit model WITH intercept
model3 <- lm(Height_cm ~ Age + SES, data = df_clean)
summary(model3)


## Plots

# Plot 1: Height vs Age

# create regression line based on lm() outputs (held SES constant) 
mean_SES <- mean(df_clean$SES)
age_seq <- seq(min(df_clean$Age), max(df_clean$Age), length.out = 100)
pred_age <- data.frame(Age = age_seq, SES = mean_SES)
pred_age$Height_pred <- predict(model3, newdata = pred_age)

# plot
p1 = ggplot(df_clean, aes(x = Age, y = Height_cm)) +
  geom_point(color = "gray50", alpha = 0.7) +
  geom_line(data = pred_age, aes(x = Age, y = Height_pred), color = "red", size = 1.2) +
  labs(
    title = "Height ~ Age (SES held constant)",
    x = "Age (years)",
    y = "Height (cm)"
  ) +
  theme_minimal(base_size = 14)

# Plot 2: Height vs SES

# create regression line based on lm() outputs (held Age constant) 
mean_Age <- mean(df_clean$Age)
ses_seq <- seq(min(df_clean$SES), max(df_clean$SES), length.out = 100)
pred_ses <- data.frame(Age = mean_Age, SES = ses_seq)
pred_ses$Height_pred <- predict(model3, newdata = pred_ses)

# plot
p2 = ggplot(df_clean, aes(x = SES, y = Height_cm)) +
  geom_point(color = "gray50", alpha = 0.7) +
  geom_line(data = pred_ses, aes(x = SES, y = Height_pred), color = "red", size = 1.2) +
  labs(
    title = "Height ~ SES (Age held constant)",
    x = "SES",
    y = "Height (cm)"
  ) +
  theme_minimal(base_size = 14)


## Combine the two plots side-by-side

combined_plot = ggarrange(p1, p2,
                          ncol = 2, nrow = 1,
                          labels = c("A", "B"),        # optional subplot labels
                          common.legend = FALSE,
                          align = "hv")

# save figure
figure_filename <- paste(path_to_figures, "/lm_model3_height_plots.png", sep = "")
ggsave(figure_filename, combined_plot,
       width = 10, height = 5, dpi = 300)













## --- Model 4: Height as a function of Age and Weight_kg ---

# Subset for children and clean data
df_children <- subset(df, Age < 18)
df_clean <- na.omit(df_children[, c("Age", "Height_cm", "Weight_kg")])

# Add small random noise for visualization (optional)
set.seed(123)
df_clean$Age <- df_clean$Age + runif(nrow(df_clean), -0.5, 0.5)

# Fit model
model4 <- lm(Height_cm ~ Age + Weight_kg, data = df_clean)
summary(model4)

## Plots

# Plot 1: Height vs Age

# create regression line based on lm() outputs (held Weight_kg constant) 
age_seq <- seq(min(df_clean$Age), max(df_clean$Age), length.out = 100)
mean_Weight_kg <- mean(df_clean$Weight_kg)
pred_age <- data.frame(Age = age_seq, Weight_kg = mean_Weight_kg)
pred_age$Height_pred <- predict(model4, newdata = pred_age)

# plot
p1 <- ggplot(df_clean, aes(x = Age, y = Height_cm)) +
  geom_point(color = "gray50", alpha = 0.7) +
  geom_line(data = pred_age, aes(x = Age, y = Height_pred), color = "red", size = 1.2) +
  labs(
    title = "Height ~ Age (Weight held constant)",
    x = "Age (years)",
    y = "Height (cm)"
  ) +
  theme_minimal(base_size = 14)

# Plot 2: Height vs Weight_kg

# create regression line based on lm() outputs (held Age constant) 
weight_seq <- seq(min(df_clean$Weight_kg), max(df_clean$Weight_kg), length.out = 100)
mean_Age <- mean(df_clean$Age)
pred_weight <- data.frame(Age = mean_Age, Weight_kg = weight_seq)
pred_weight$Height_pred <- predict(model4, newdata = pred_weight)

# plot
p2 <- ggplot(df_clean, aes(x = Weight_kg, y = Height_cm)) +
  geom_point(color = "gray50", alpha = 0.7) +
  geom_line(data = pred_weight, aes(x = Weight_kg, y = Height_pred), color = "red", size = 1.2) +
  labs(
    title = "Height ~ Weight (Age held constant)",
    x = "Weight (kg)",
    y = "Height (cm)"
  ) +
  theme_minimal(base_size = 14)


# Combine the two plots side-by-side

combined_plot <- ggarrange(
  p1, p2,
  ncol = 2, nrow = 1,
  labels = c("A", "B"),
  common.legend = FALSE,
  align = "hv"
)

# save figure

figure_filename <- paste0(path_to_figures, "/lm_model4_height_plots.png")
ggsave(figure_filename, combined_plot,
       width = 10, height = 5, dpi = 300)












## --- Model 5: Height as a function of Age, Weight_kg, and Weight_kg^2 ---

# Fit the models

# Linear model
model_linear <- lm(Height_cm ~ Age + Weight_kg, data = df_clean)
summary(model_linear)

# Quadratic model (add Weight^2)
model_quad <- lm(Height_cm ~ Age + Weight_kg + I(Weight_kg^2), data = df_clean)
summary(model_quad)

## Plots

mean_Age <- mean(df_clean$Age)
weight_seq <- seq(min(df_clean$Weight_kg), max(df_clean$Weight_kg), length.out = 200)

# Linear predictions
pred_linear <- data.frame(Age = mean_Age, Weight_kg = weight_seq)
pred_linear$Height_pred <- predict(model_linear, newdata = pred_linear)

# Quadratic predictions
pred_quad <- data.frame(Age = mean_Age, Weight_kg = weight_seq)
pred_quad$Height_pred <- predict(model_quad, newdata = pred_quad)

# Plot 1: Height vs Age
p1 <- ggplot(df_clean, aes(x = Age, y = Height_cm)) +
  geom_point(color = "gray50", alpha = 0.7) +
  geom_line(data = pred_age, aes(x = Age, y = Height_pred), color = "red", size = 1.2) +
  labs(
    title = "Height ~ Age (Weight held constant)",
    x = "Age (years)",
    y = "Height (cm)"
  ) +
  theme_minimal(base_size = 14)

# Plot 2: Height vs Weight_kg
p2 <- ggplot(df_clean, aes(x = Weight_kg, y = Height_cm)) +
  geom_point(alpha = 0.7, color = "gray50") +
  geom_line(data = pred_linear, aes(y = Height_pred),
            color = "red", linetype = "solid", size = 1) +
  geom_line(data = pred_quad, aes(y = Height_pred),
            color = "blue", size = 1.2) +
  labs(
    title = "Height ~ Weight (Age held constant)",
    x = "Weight (kg)",
    y = "Height (cm)"
  ) +
  theme_minimal(base_size = 14)


# Combine the two plots side-by-side

combined_plot <- ggarrange(
  p1, p2,
  ncol = 2, nrow = 1,
  labels = c("A", "B"),
  common.legend = FALSE,
  align = "hv"
)

# save figure

figure_filename <- paste0(path_to_figures, "/lm_model5_height_quadratic.png")
ggsave(figure_filename, combined_plot,
       width = 10, height = 5, dpi = 300)















## --- Model 6: Height as a function of Age, Weight_kg, and their interaction ---

# Fit model
model6 <- lm(Height_cm ~ Age * Weight_kg, data = df_clean)
summary(model6)

## Generate predictions for plotting the interaction

# We'll visualize the interaction using Age on x-axis and lines for different Weight levels
weight_levels <- quantile(df_clean$Weight_kg, probs = c(0.25, 0.5, 0.75))  # 25th, 50th, 75th percentile
age_seq <- seq(min(df_clean$Age), max(df_clean$Age), length.out = 100)

# Create a prediction grid
pred_grid <- expand.grid(
  Age = age_seq,
  Weight_kg = weight_levels
)
pred_grid$Height_pred <- predict(model6, newdata = pred_grid)

## Plot interaction: Height vs Age for different Weight levels

p_interaction <- ggplot(df_clean, aes(x = Age, y = Height_cm)) +
  geom_point(color = "gray50", alpha = 0.7) +
  geom_line(data = pred_grid, aes(x = Age, y = Height_pred, color = factor(Weight_kg)), size = 1.2) +
  scale_color_manual(
    name = "Weight (kg)",
    values = c("blue", "green", "red"),
    labels = paste0("Q", c(25,50,75))
  ) +
  labs(
    title = "Height ~ Age * Weight Interaction",
    x = "Age (years)",
    y = "Height (cm)"
  ) +
  theme_minimal(base_size = 14)

p_interaction

# save figure
figure_filename <- paste0(path_to_figures, "/lm_model6_height_interaction.png")
ggsave(figure_filename, p_interaction,
       width = 7, height = 5, dpi = 300)






## Alternative interaction plot: Height vs Weight for different Age levels

# Choose representative Age levels (25th, 50th, 75th percentiles)
age_levels <- quantile(df_clean$Age, probs = c(0.25, 0.5, 0.75))

# Sequence of Weight values for prediction
weight_seq <- seq(min(df_clean$Weight_kg), max(df_clean$Weight_kg), length.out = 100)

# Create prediction grid
pred_grid_alt <- expand.grid(
  Age = age_levels,
  Weight_kg = weight_seq
)
pred_grid_alt$Height_pred <- predict(model6, newdata = pred_grid_alt)

# Plot
p_interaction_alt <- ggplot(df_clean, aes(x = Weight_kg, y = Height_cm)) +
  geom_point(color = "gray50", alpha = 0.7) +
  geom_line(data = pred_grid_alt, aes(x = Weight_kg, y = Height_pred, color = factor(Age)), size = 1.2) +
  scale_color_manual(
    name = "Age (years)",
    values = c("blue", "green", "red"),
    labels = paste0("Q", c(25,50,75))
  ) +
  labs(
    title = "Height ~ Weight * Age Interaction",
    x = "Weight (kg)",
    y = "Height (cm)"
  ) +
  theme_minimal(base_size = 14)

p_interaction_alt

# save figure
figure_filename_alt <- paste0(path_to_figures, "/lm_model6_height_interaction_alt.png")
ggsave(figure_filename_alt, p_interaction_alt,
       width = 7, height = 5, dpi = 300)


