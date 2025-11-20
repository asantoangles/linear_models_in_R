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

# subset rows by the value of a variable
df_adults = subset(df, Age > 18)

# extract the values of one column, as a vector
age_adults = df_adults$Age

# check class (type of data)
class(age_adults)

# inspect dimensions of a vector
dim(age_adults) # dim() does not work on vectors
length(age_adults)









## ======================== ##
## Descriptive Statistics.  ##
## ======================== ##

### Centrality ###

# mean
mean(df$Age)

# median
median(df$Age)

### Dispersion ###

# variance
var(df$Age)

# std
sd(df$Age)





### histograms ###

### frequency distribution of height

# subset dataset with only adults
df_adults = subset(df, Age > 18)

# Histogram 
plot = ggplot(df_adults, aes(x = Height_cm)) +
  geom_histogram(binwidth = 1.5, fill = "lightgray", color = "lightgray") +
  labs(title = "Frequency Distribution of Height",
       x = "Height (cm)",
       y = "Count") +
  theme_minimal()

print(plot)

# save figure
figure_filename = paste(path_to_figures, "/Histogram_Height_Adults.png", sep = "")
ggsave(figure_filename, plot = plot, width = 8, height = 6, dpi = 300)





### probability distribution of height (with density line)

# subset dataset with only adults
df_adults = subset(df, Age > 18)

# Histogram with density line
plot <- ggplot(df_adults, aes(x = Height_cm)) +
  geom_histogram(aes(y = ..density..), binwidth = 1.5, 
                 fill = "lightgray", color = "lightgray") +   # Histogram scaled to density
  geom_density(color = "black", size = 1.2) +                  # Add smooth density line
  labs(title = "Probability Distribution of Height",
       x = "Height (cm)",
       y = "Density") +
  theme_minimal()

print(plot)

# save figure
figure_filename = paste(path_to_figures, "/Histogram_Height_Adults_Density.png", sep = "")
ggsave(figure_filename, plot = plot, width = 8, height = 6, dpi = 300)







### frequency distribution of height + central tendency

# Compute mean and median
height_mean <- mean(df_adults$Height_cm, na.rm = TRUE)
height_median <- median(df_adults$Height_cm, na.rm = TRUE)

# Compute mode using KDE
d <- density(df_adults$Height_cm, na.rm = TRUE)
height_mode <- d$x[which.max(d$y)]

# Histogram with mean, median, and mode lines
plot <- ggplot(df_adults, aes(x = Height_cm)) +
  geom_histogram(binwidth = 1.5, fill = "lightgray", color = "lightgray") +
  geom_vline(aes(xintercept = height_mean), color = "blue", linetype = "dashed", size = 1.2) +
  geom_vline(aes(xintercept = height_median), color = "red", linetype = "dotted", size = 1.2) +
  geom_vline(aes(xintercept = height_mode), color = "darkgreen", linetype = "solid", size = 1.2) +
  labs(title = "Frequency Distribution of Height",
       x = "Height (cm)",
       y = "Count") +
  theme_minimal() +
  annotate("text", x = height_mean + 5, y = max(table(df_adults$Height_cm)) * 0.9,
           label = "Mean", color = "blue", size = 5, fontface = "bold") +
  annotate("text", x = height_median - 8, y = max(table(df_adults$Height_cm)) * 1.5,
           label = "Median", color = "red", size = 5, fontface = "bold") +
  annotate("text", x = height_mode - 7, y = max(table(df_adults$Height_cm)) * 3,
           label = "Mode", color = "darkgreen", size = 5, fontface = "bold")

print(plot)

# Save figure
figure_filename <- paste(path_to_figures, "/Histogram_Height_Adults_Central_Tendency.png", sep = "")
ggsave(figure_filename, plot = plot, width = 8, height = 6, dpi = 300)


### frequency distribution of height + dispersion (SD line)

# Compute statistics
height_mean <- mean(df_adults$Height_cm, na.rm = TRUE)
height_var  <- var(df_adults$Height_cm, na.rm = TRUE)
height_sd   <- sd(df_adults$Height_cm, na.rm = TRUE)

# Histogram
plot <- ggplot(df_adults, aes(x = Height_cm)) +
  geom_histogram(binwidth = 1.5, fill = "lightgray", color = "lightgray") +
  labs(title = "Frequency Distribution of Height",
       x = "Height (cm)",
       y = "Count") +
  theme_minimal() +
  # Add horizontal line for 1 SD centered at mean
  geom_segment(aes(x = height_mean - height_sd, 
                   xend = height_mean + height_sd,
                   y = max(table(df_adults$Height_cm)) * 3,
                   yend = max(table(df_adults$Height_cm)) * 3),
               color = "blue", size = 1.5) +
  # Add vertical line at mean for reference
  geom_vline(xintercept = height_mean, color = "red", linetype = "dashed", size = 1.2) +
  annotate("text", x = height_mean, y = max(table(df_adults$Height_cm)) * 0.55,
           label = "Mean", color = "red", fontface = "bold") + 
  annotate("text", x = height_mean + 4.2, y = max(table(df_adults$Height_cm)) * 3.3,
           label = "+SD", color = "blue", fontface = "bold") + 
  annotate("text", x = height_mean - 4.2, y = max(table(df_adults$Height_cm)) * 3.3,
           label = "-SD", color = "blue", fontface = "bold")
print(plot)

# Save figure
figure_filename <- paste(path_to_figures, "/Histogram_Height_Adults_Dispersion.png", sep = "")
ggsave(figure_filename, plot = plot, width = 8, height = 6, dpi = 300)








### frequency distribution of height + dispersion (SD zones)

## for normal distribution, ~68% of observations fall within ±1 SD of the mean

# Compute statistics
height_mean <- mean(df_adults$Height_cm, na.rm = TRUE)
height_sd   <- sd(df_adults$Height_cm, na.rm = TRUE)

# Flag observations within ±1 SD
df_adults$within_sd <- ifelse(df_adults$Height_cm >= (height_mean - height_sd) &
                                df_adults$Height_cm <= (height_mean + height_sd),
                              "Inside SD", "Outside SD")

# Histogram with both fill and border mapped to within_sd
plot <- ggplot(df_adults, aes(x = Height_cm, fill = within_sd, color = within_sd)) +
  geom_histogram(binwidth = 1.5) +
  scale_fill_manual(values = c("Inside SD" = "lightblue", "Outside SD" = "lightgray")) +
  scale_color_manual(values = c("Inside SD" = "lightblue", "Outside SD" = "lightgray")) +
  guides(fill = "none", color = "none") +  # remove legends
  labs(title = "Frequency Distribution of Height (Adults)",
       x = "Height (cm)",
       y = "Count") +
  theme_minimal() +
  # Vertical lines for mean and ±1 SD
  geom_vline(xintercept = height_mean, color = "red", linetype = "dashed", size = 1.2) +
  geom_vline(xintercept = height_mean - height_sd, color = "blue", linetype = "dotted", size = 1.2) +
  geom_vline(xintercept = height_mean + height_sd, color = "blue", linetype = "dotted", size = 1.2) +
  # Labels
  annotate("text", x = height_mean, y = max(table(df_adults$Height_cm)) * 0.55,
           label = "Mean", color = "red", fontface = "bold") +
  annotate("text", x = height_mean + height_sd, y = max(table(df_adults$Height_cm)) * 0.5,
           label = "+1 SD", color = "blue", fontface = "bold") +
  annotate("text", x = height_mean - height_sd, y = max(table(df_adults$Height_cm)) * 0.5,
           label = "-1 SD", color = "blue", fontface = "bold")

print(plot)

# Save figure
figure_filename <- paste(path_to_figures, "/Histogram_Height_Adults_Dispersion_1SD.png", sep = "")
ggsave(figure_filename, plot = plot, width = 8, height = 6, dpi = 300)





## for normal distribution, ~95% within ±2 SD

# Compute statistics
height_mean <- mean(df_adults$Height_cm, na.rm = TRUE)
height_sd   <- sd(df_adults$Height_cm, na.rm = TRUE)

# Flag observations within ±2 SD
df_adults$within_sd <- ifelse(df_adults$Height_cm >= (height_mean - 2*height_sd) &
                                df_adults$Height_cm <= (height_mean + 2*height_sd),
                              "Inside SD", "Outside SD")

# Histogram with both fill and border mapped to within_sd
plot <- ggplot(df_adults, aes(x = Height_cm, fill = within_sd, color = within_sd)) +
  geom_histogram(binwidth = 1.5) +
  scale_fill_manual(values = c("Inside SD" = "lightblue", "Outside SD" = "lightgray")) +
  scale_color_manual(values = c("Inside SD" = "lightblue", "Outside SD" = "lightgray")) +
  guides(fill = "none", color = "none") +  # remove legends
  labs(title = "Frequency Distribution of Height (Adults)",
       x = "Height (cm)",
       y = "Count") +
  theme_minimal() +
  # Vertical lines for mean and ±1 SD
  geom_vline(xintercept = height_mean, color = "red", linetype = "dashed", size = 1.2) +
  geom_vline(xintercept = height_mean - 2*height_sd, color = "blue", linetype = "dotted", size = 1.2) +
  geom_vline(xintercept = height_mean + 2*height_sd, color = "blue", linetype = "dotted", size = 1.2) +
  # Labels
  annotate("text", x = height_mean, y = max(table(df_adults$Height_cm)) * 0.55,
           label = "Mean", color = "red", fontface = "bold") +
  annotate("text", x = height_mean + height_sd, y = max(table(df_adults$Height_cm)) * 0.5,
           label = "+2 SD", color = "blue", fontface = "bold") +
  annotate("text", x = height_mean - height_sd, y = max(table(df_adults$Height_cm)) * 0.5,
           label = "-2 SD", color = "blue", fontface = "bold")
print(plot)

# Save figure
figure_filename <- paste(path_to_figures, "/Histogram_Height_Adults_Dispersion_2SD.png", sep = "")
ggsave(figure_filename, plot = plot, width = 8, height = 6, dpi = 300)







## for normal distribution, ~99.7% within ±3 SD

# Compute statistics
height_mean <- mean(df_adults$Height_cm, na.rm = TRUE)
height_sd   <- sd(df_adults$Height_cm, na.rm = TRUE)

# Flag observations within ±2 SD
df_adults$within_sd <- ifelse(df_adults$Height_cm >= (height_mean - 3*height_sd) &
                                df_adults$Height_cm <= (height_mean + 3*height_sd),
                              "Inside SD", "Outside SD")

# Histogram with both fill and border mapped to within_sd
plot <- ggplot(df_adults, aes(x = Height_cm, fill = within_sd, color = within_sd)) +
  geom_histogram(binwidth = 1.5) +
  scale_fill_manual(values = c("Inside SD" = "lightblue", "Outside SD" = "lightgray")) +
  scale_color_manual(values = c("Inside SD" = "lightblue", "Outside SD" = "lightgray")) +
  guides(fill = "none", color = "none") +  # remove legends
  labs(title = "Frequency Distribution of Height (Adults)",
       x = "Height (cm)",
       y = "Count") +
  theme_minimal() +
  # Vertical lines for mean and ±1 SD
  geom_vline(xintercept = height_mean, color = "red", linetype = "dashed", size = 1.2) +
  geom_vline(xintercept = height_mean - 3*height_sd, color = "blue", linetype = "dotted", size = 1.2) +
  geom_vline(xintercept = height_mean + 3*height_sd, color = "blue", linetype = "dotted", size = 1.2) +
  # Labels
  annotate("text", x = height_mean, y = max(table(df_adults$Height_cm)) * 0.55,
           label = "Mean", color = "red", fontface = "bold") +
  annotate("text", x = height_mean + height_sd, y = max(table(df_adults$Height_cm)) * 0.5,
           label = "+3 SD", color = "blue", fontface = "bold") +
  annotate("text", x = height_mean - height_sd, y = max(table(df_adults$Height_cm)) * 0.5,
           label = "-3 SD", color = "blue", fontface = "bold")
print(plot)

# Save figure
figure_filename <- paste(path_to_figures, "/Histogram_Height_Adults_Dispersion_3SD.png", sep = "")
ggsave(figure_filename, plot = plot, width = 8, height = 6, dpi = 300)



















### frequency distribution of Height_cm (adults + children)

# Compute mean and median
height_mean <- mean(df$Height_cm, na.rm = TRUE)
height_median <- median(df$Height_cm, na.rm = TRUE)

# Compute mode using KDE
d <- density(df$Height_cm, na.rm = TRUE)
height_mode <- d$x[which.max(d$y)]

# Histogram with mean, median, and mode lines
plot <- ggplot(df, aes(x = Height_cm)) +
  geom_histogram(binwidth = 2, fill = "lightgray", color = "lightgray") +
  geom_vline(aes(xintercept = height_mean), color = "blue", linetype = "dashed", size = 1.2) +
  geom_vline(aes(xintercept = height_median), color = "red", linetype = "dotted", size = 1.2) +
  geom_vline(aes(xintercept = height_mode), color = "darkgreen", linetype = "solid", size = 1.2) +
  labs(title = "Frequency Distribution of Height",
       x = "Height (cm)",
       y = "Count") +
  theme_minimal() +
  annotate("text", x = height_mean + 5, y = max(table(df_adults$Height_cm)) * 0.9,
           label = "Mean", color = "blue", size = 5, fontface = "bold") +
  annotate("text", x = height_median - 8, y = max(table(df_adults$Height_cm)) * 1.5,
           label = "Median", color = "red", size = 5, fontface = "bold") +
  annotate("text", x = height_mode - 7, y = max(table(df_adults$Height_cm)) * 3,
           label = "Mode", color = "darkgreen", size = 5, fontface = "bold")

print(plot)

# Save figure
figure_filename <- paste(path_to_figures, "/Histogram_Height_Central_Tendency.png", sep = "")
ggsave(figure_filename, plot = plot, width = 8, height = 6, dpi = 300)



### long-tailed distribution

# Download lab data: triglycerides 2017–2018
data_trigl <- nhanes("TRIGLY_J")  # check the actual NHANES table code

# Compute mean and median
height_mean <- mean(data_trigl$LBXTR, na.rm = TRUE)
height_median <- median(data_trigl$LBXTR, na.rm = TRUE)

# Compute mode using KDE
d <- density(data_trigl$LBXTR, na.rm = TRUE)
height_mode <- d$x[which.max(d$y)]

# Histogram with mean, median, and mode lines
plot <- ggplot(data_trigl, aes(x = LBXTR)) +
  geom_histogram(binwidth = 2, fill = "lightgray", color = "lightgray") +
  geom_vline(aes(xintercept = height_mean), color = "blue", linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = height_median), color = "red", linetype = "dotted", size = 1) +
  geom_vline(aes(xintercept = height_mode), color = "darkgreen", linetype = "solid", size = 1) +
  labs(title = "Frequency Distribution of Cholesterol",
       x = "Height (cm)",
       y = "Count") +
  theme_minimal() +
  annotate("text", x = height_mean + 5, y = max(table(df_adults$Height_cm)) * 0.9,
           label = "Mean", color = "blue", size = 5, fontface = "bold") +
  annotate("text", x = height_median - 8, y = max(table(df_adults$Height_cm)) * 1.5,
           label = "Median", color = "red", size = 5, fontface = "bold") +
  annotate("text", x = height_mode - 7, y = max(table(df_adults$Height_cm)) * 3,
           label = "Mode", color = "darkgreen", size = 5, fontface = "bold") + 
  xlim(c(0,500))

print(plot)

# Save figure
figure_filename <- paste(path_to_figures, "/Histogram_Colesterol_Longtail_Central_Tendency.png", sep = "")
ggsave(figure_filename, plot = plot, width = 8, height = 6, dpi = 300)


















## ======================== ##
## Sample vs Population.    ##
## ======================== ##

# Treat the NHANES dataset as the population. Compute its true population variance.
# Then, draw 500 random samples of 30 observations each. For each sample, compute 
# the variance using both denominators (n and n−1, i.e., with and without Bessel's 
# correction), and determine which estimate is closer to the true population variance.

## population mean vs sample mean

# Choose variable to analyze
x <- df_adults$Height_cm
x <- na.omit(x)

# Parameters
n_samples <- 500   # number of samples
n_obs <- 50        # observations per sample

# True population variance
pop_mean <- mean(x)

# Initialize data frame to store results
results <- data.frame(
  SampleID = integer(n_samples),
  PopulationMean = numeric(n_samples),
  SampleMean = numeric(n_samples),
  SEM = numeric(n_samples),
  stringsAsFactors = FALSE
)

# Loop over samples
for (i in 1:n_samples) {
  
  # Draw random sample
  samp <- sample(x, n_obs, replace = FALSE)
  
  # Compute mean and SEM
  SampleMean <- mean(samp)
  SampleStd <- sum((samp - mean(samp))^2) / (n_obs - 1)  # divide by n-1
  SEM = SampleStd / sqrt(n_obs)
  
  # Store in results
  results$SampleID[i] <- i
  results$PopulationMean[i] <- pop_mean
  results$SampleMean[i] <- SampleMean
  results$SEM[i] <- SEM
}

# Print sample-level table
print(head(results, 10))  # show first 10 rows for brevity

# Pick consistent binwidth
bw <- 1.5

# Overlaid histograms
plot <- ggplot() +
  
  # Population histogram
  geom_histogram(
    data = df_adults,
    aes(x = Height_cm),
    binwidth = bw,
    fill = "lightgray",
    color = "lightgray"
  ) +
  
  
  # Sample-mean histogram
  geom_histogram(
    data = results,
    aes(x = SampleMean),
    binwidth = bw,
    fill = "blue",
    color = "blue",
    alpha = 0.4
  ) +
  
  labs(
    title = "Population Height Distribution with Overlayed Sample-Mean Distribution",
    x = "Height (cm)",
    y = "Count"
  ) +
  theme_minimal()

print(plot)

# Save figure
figure_filename <- paste(path_to_figures, "/Histogram_Population_Sample_Mean.png", sep = "")
ggsave(figure_filename, plot = plot, width = 8, height = 6, dpi = 300)


## population mean vs sample variance

# Parameters
n_samples <- 500   # number of samples
n_obs <- 30        # observations per sample

# True population variance
true_pop_var <- mean((x - mean(x))^2)

# Initialize data frame to store results
results <- data.frame(
  SampleID = integer(n_samples),
  TrueVar = numeric(n_samples),
  SampleVar = numeric(n_samples),
  PopVar = numeric(n_samples),
  Closest = character(n_samples),
  stringsAsFactors = FALSE
)

# Loop over samples
for (i in 1:n_samples) {
  
  # Draw random sample
  samp <- sample(x, n_obs, replace = FALSE)
  
  # Compute variances
  pop_var <- mean((samp - mean(samp))^2)                  # divide by n
  sample_var <- sum((samp - mean(samp))^2) / (n_obs - 1)  # divide by n-1
  
  # Determine which is closer to true variance
  closest <- ifelse(abs(sample_var - true_pop_var) < abs(pop_var - true_pop_var),
                    "SampleVar", "PopVar")
  
  # Store in results
  results$SampleID[i] <- i
  results$TrueVar[i] <- true_pop_var
  results$SampleVar[i] <- sample_var
  results$PopVar[i] <- pop_var
  results$Closest[i] <- closest
}

# Convert to factor with explicit order
results$Closest <- factor(results$Closest, levels = c("SampleVar", "PopVar"))

# Summarize counts for barplot
summary_counts <- table(results$Closest)

# Barplot
par(mfrow = c(1, 1))  # one plot per figure
barplot(summary_counts,
        main = "Variance",
        col = c("skyblue", "salmon"),
        ylab = "Times Closer to True Variance",
        names.arg = c("SampleVar (n-1)", "PopVar (n)"))

# Print sample-level table
print(head(results, 10))  # show first 10 rows for brevity









## ======================== ##
## Normal Distribution.     ##
## ======================== ##

height_mean <- mean(df_adults$Height_cm, na.rm = TRUE)
height_sd <- sd(df_adults$Height_cm, na.rm = TRUE)

# Simulate normal data using rnorm()
n = 1000 # change here the number of simulated values
height_sim <- rnorm(n, mean = height_mean, sd = height_sd)

# Combine into one data frame with a source label
df_compare <- rbind(
  data.frame(Height_cm = df_adults$Height_cm, Source = "Empirical"),
  data.frame(Height_cm = height_sim,          Source = "Simulated")
)


# Plot overlaid probability histograms
plot <- ggplot() +
  # Empirical histogram
  geom_histogram(data = subset(df_compare, Source == "Empirical"),
                 aes(x = Height_cm, y = ..density.., fill = Source),
                 binwidth = 1.5,
                 color = NA,
                 alpha = 0.5) +
  # Simulated histogram
  geom_histogram(data = subset(df_compare, Source == "Simulated"),
                 aes(x = Height_cm, y = ..density.., fill = Source),
                 binwidth = 1.5,
                 color = NA,
                 alpha = 0.5) +
  labs(title = "Empirical vs Simulated Normal Distribution of Height",
       x = "Height (cm)",
       y = "Probability") +
  scale_fill_manual(values = c("Empirical" = "gray60", "Simulated" = "skyblue")) +
  guides(fill = "none") +   # remove legend
  theme_minimal(base_size = 14)

# Display plot
print(plot)

# Save figure
figure_filename <- paste(path_to_figures, "/Histogram_Height_Adults_Simulated_Normal_n", n, ".png", sep = "")
ggsave(figure_filename, plot = plot, width = 8, height = 6, dpi = 300)










## ======================== ##
## Central Limit Theorem    ##
## ======================== ##

### 1) Sample from a Binomial distribution ###

# Parameters
n_tosses <- 1     # number of coin tosses per sample
p_heads <- 0.5     # probability of heads
n_samples <- 1000  # number of samples

# Draw n_samples from Binomial(n_tosses, p_heads)
binom_samples <- rbinom(n_samples, size = n_tosses, prob = p_heads)

# Convert binomial samples to a data frame and factorize
df_binom <- data.frame(Heads = factor(binom_samples, levels = c(0, 1)))

# Plot histogram with two bars
plot <- ggplot(df_binom, aes(x = Heads)) +
  geom_bar(width = 0.4, fill = "skyblue", color = "white") +
  labs(title = "Histogram of Coin Toss Outcomes",
       x = "Outcome",
       y = "Frequency") +
  theme_minimal(base_size = 14)

# Print plot
print(plot)

# Save figure
figure_filename <- paste(path_to_figures, "/Central_Limit_plot1.png", sep = "")
ggsave(figure_filename, plot = plot, width = 8, height = 6, dpi = 300)


### 2) Sampling distribution of the mean ###

# Parameters
n_tosses <- 10     # number of coin tosses per sample
p_heads <- 0.5     # probability of heads
n_samples <- 1000  # number of samples

# Draw n_samples from Binomial(n_tosses, p_heads)
binom_samples <- rbinom(n_samples, size = n_tosses, prob = p_heads)

# Convert binomial samples to proportions
df_binom <- data.frame(Heads = binom_samples / 10)

# Plot histogram with appropriate binwidth
plot = ggplot(df_binom, aes(x = Heads)) +
  geom_histogram(binwidth = 0.05, fill = "skyblue", color = "white", boundary = 0) +
  scale_x_continuous(breaks = seq(0, 1, 0.1)) +
  labs(title = "Histogram of the Sampling Distribution of the Mean",
       x = "Sample Mean (Proportion of Heads)",
       y = "Frequency") +
  theme(
    panel.background = element_blank(),   # remove panel background
    plot.background = element_blank(),    # remove plot background
    panel.grid = element_blank(),         # remove gridlines
    axis.ticks = element_blank()          # remove tick marks
  )

print(plot)

# Save figure
figure_filename <- paste(path_to_figures, "/Central_Limit_plot2.png", sep = "")
ggsave(figure_filename, plot = plot, width = 8, height = 6, dpi = 300)












