# This is a more practical-focused session, looking at how the modelling
# process works in applied reality

# We will carry on using the datasets from last time that you are by now familiar with

# First, load the libraries we need:
# the dataset
library(nhanesA)
# and the plotting functionality
library(ggplot2)

setwd("~/Your/Directory/Path/") # Change this to the appropriate directory on your own computer

################################
# An additional note about loading data
################################
# This is a special case, where we are using a well-established publicly-accessible
# dataset that exists in an *R package*
# When you work with your own data, you will most likely have it as an Excel file or similar
# To read in a data table like that, you need a different function, e.g.
# my_data<-read.csv2("file_path/MyFileName.csv")   # if you have saved it as a csv (most stable)
# or to read directly from an Excel file:
# install.packages("readxl")
# library(readxl)
# my_data<-read_excel("file_path/MyFileName.csv") 
################################

# Assemble the data as per last time
demo <- nhanes("DEMO_J")  # demog data
bmx <- nhanes("BMX_J") # physio data
df <- merge(demo, bmx, by = "SEQN") # merge the two by participant ID

df <- df[, c("SEQN", "RIDAGEYR", "RIAGENDR", "BMXHT", "BMXWT", "INDFMPIR")] # Select relevant columns - makes it less clunky to work with
names(df) <- c("ID", "Age", "Gender", "Height_cm", "Weight_kg", "SES") # Rename columns for clarity
# And this time we will be only looking at ADULTS only:
df <- subset(df, Age>=18)


# # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # 

# Now onto the lecture

# # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # 


#######################################################
# Section 1 - playing with R syntax and how R "thinks"
#######################################################


## Make some demo objects to play with

my_model <- lm(Weight_kg ~ Height_cm + Gender, data=df, na.action=na.omit) # A demo model
my_ids <- c(paste0("subj", c(1:5)), "subj_A", "subj_B") # A demo vector


## Different functions on different object types
# Type these out yourself from the slide! These lines are here for reference only

# my_model # A brief overview
# my_ids # The full vector
# str(my_model) # Too much info!!
# str(my_ids) # Very informative
# summary(my_model) # Very informative
# summary(my_ids) # Maybe useful, we pretty much knew this
# coef(my_model) # Handy
# coef(my_ids) # Doesn't run - trying to do an operation on something that doesn't exist


## Assigning model to an object vs. just running it
# Code along from the slide! These lines are here for reference only

# Simply run the model (like pressing "OK" in SPSS)
lm(Weight_kg ~ Height_cm + Gender, data=df, na.action=na.omit)
# Assign the model to an object instead
my_model <- lm(Weight_kg ~ Height_cm + Gender, data=df, na.action=na.omit) # Notice no printed output!
# Get that output by calling the model object
my_model


## Using the model object - some examples (more later)

summary(my_model)
plot(predict(my_model), resid(my_model)) # A scatterplot (plot()) of the model residuals (resid()) against the predicted values from the model (predict())
# Compare to another model - first build another model
another_model <- lm(Weight_kg ~ Height_cm, data=df, na.action=na.omit) # Excluding the gender term
anova(my_model, another_model) # An F-test test to compare them (more later)


## The order of arguments to a function is important!

# Check out what ends up on the x vs. y axis in these code snippets
plot(df$Weight_kg, df$Height_cm)
plot(df$Weight_kg ~ df$Height_cm)
plot(x=df$Height_cm, y=df$Weight_kg)
plot(y=df$Height_cm, x=df$Weight_kg)


## Need clarification about how the function works? Use ? with the function name:

?plot

# What plot() does depends on the object type
plot(my_model) # Now we are applying a plot method that works for object type lm, not object type vector as with our variables above


#######################################################
## Section 2 - Data wrangling
#######################################################


## Different ways of processing data

# Using subset
# Type these out yourself from the slide! These lines are here for reference only

# df_male <- subset(df, Gender==”Male”)
# df_kids <- subset(df, Age<18)
## N.B. == means a logical operation: is this value equal to the other value
## A single =, as in our plotting function, specifies an argument

# Checking out the results
# Type these out yourself from the slide! These lines are here for reference only

# str(df_male) # As expected
# head(df_male) # As expected	
# table(df_male$Gender) # As expected
# str(df_kids) # Where has the data gone?!
# head(df_kids) # Why are there no rows?
# table(df_kids$Age) # There seem to be zero cases?
# Where have we made a mistake? 
# Clue: check out line 36

## Alternatively, use indexing - more precise and flexible
# Type these out yourself from the slide! These lines are here for reference only

# df[1:3, c(1,3,4)]
# df[0,]	
# df[,0]	
# df[1:3, c("ID", "Gender", "Height_cm")]
# df[which(df$Age>75), ]
# Additional to the presentation: logical operators (and &, or |, not !)
summary(df[which(df$Age>75 & df$Gender=="Female"), ])
summary(df[which(df$Age>75 | df$Gender=="Female"), ])
summary(df[which(df$Age>75 & df$Gender!="Female"), ])


## Create a new variable
# Making a logical (binary) variable to indicate whether a person is obese (1) or not (0)
# In one line:
df$Obese<-ifelse(df$Weight_kg/((df$Height_cm/100)^2)>=25, 1, 0) 


# Bonus explanation: splitting the thinking up (good way to learn!)
df$BMI<-df$Weight_kg/((df$Height_cm/100)^2) # Make a BMI variable
df$Obese_log<-numeric(nrow(df)) # Create an empty numeric vector of the same length (number of rows, nrow(), as the dataframe)
df$Obese_log[which(df$BMI>=25)]<-1 # For any row that has a BMI over 25, make the new variable 1
head(df) # Just to check that the numbers are being filled in (NAs are still left from the empty vector, because we have not given these rows a value yet)
# Note that in the above, the index has no comma in, just a single criterion.
# This is because we are now indexing a vector, which has only 1 dimension,
# whereas before we were indexing a dataframe, 
# where we needed to specify two dimensions (row and column)
df$Obese_log[which(df$BMI<25)]<-0 # Fill in 0s for BMIs under 25


#######################################################
## Section 3 - Linear modelling in practice
#######################################################


## Step 1: Prepare a new dataset data & fit the model 
rm(my_model) # rm() means remove - deletes the specified objects from the environment so that nothing gets confused (e.g. part-filling old dataframes with shorter data with the same name)

measures <- read.table("https://www.mv.helsinki.fi/home/mjxpirin/medstat_course/material/Davis_height_weight.txt", as.is = TRUE, header = TRUE)

my_model <- lm(repwt ~ weight + sex, data=measures, na.action=na.exclude)


## Step 2: Check assumptions
resid(my_model)
hist(resid(my_model))
plot(my_model)
plot(measures$repwt, predict(my_model))
# This all looks very nice, we can trust the output to be meaningful
# (the model is capturing the data/process/system well enough)


## Step 3: Interpret results
summary(my_model)
# See presentation for explanation!


## Step 4: Plotting the model

# Base R graphics
plot(measures$weight, measures$repwt, pch = 19, col = "orange", main = "Base R graphics")
abline(my_model, col = "red", lwd = 1.5) # Warning message! Translation: only plotting according to fitted intercept and age coefficient (i.e. sex is not included here at all)

# Using ggplot

ggplot(measures, aes(y=repwt, x=weight, group=sex, colour=sex)) +
	geom_point(shape=4) +
	geom_line(aes(y=predict(my_model))) +
	theme_classic() +
	ggtitle("Using ggplot2")


## Aside: Related tests

# Compare the t-test outcome...
t.test(measures$repwt[which(measures$sex=="F")], measures$repwt[which(measures$sex=="M")])
# ... with the linear model
summary(lm(repwt ~ sex, data=measures, na.action=na.exclude))


## Your turn

# Potentially interesting models:
mod_h <- lm(weight ~ height, data = measures)
mod_s <- lm(weight ~ sex, data = measures)
mod_hs <- lm(weight ~ height + sex, data = measures)
mod_int <- lm(weight ~ height * sex, data = measures)

# Check assumptions for all models (just one here as an example)
hist(resid(mod_int))
plot(mod_int)
plot(measures$weight, predict(mod_int))

# One outlier seems to be pulling things off course (the only one with weight > c. 115 kg), let's try without it
mod_h_nooutlier <- lm(weight ~ height, data = measures[which(measures$weight<115),])
mod_s_nooutlier <- lm(weight ~ sex, data = measures[which(measures$weight<115),])
mod_hs_nooutlier <- lm(weight ~ height + sex, data = measures[which(measures$weight<115),])
mod_int_nooutlier <- lm(weight ~ height * sex, data = measures[which(measures$weight<115),])

# Check assumptions again - different dataset means a different model fit
hist(resid(mod_int_nooutlier))
plot(mod_int_nooutlier)
plot(measures$weight[which(measures$weight<115)], predict(mod_int_nooutlier)) # Note the same subset of raw data as what the model was fitted on (and is therefore predicting from)

# Not perfect, but looks pretty good
# We can go on to interpret this model
summary(mod_int_nooutlier)
# The interaction term is just about significant
# Interpretation: the slope for height for sex M is 0.3497 steeper than the slope for height for the reference category (by default the first alphabetically, so here F), which in turn is the one given in the output as the height term
# Not easy to get head around - and this is as easy as interactions come!

# Plot it to visualise the effect and sanity check the model
ggplot(measures[which(measures$weight<115),], aes(y= weight, x=height, group=sex, colour=sex)) +
	geom_point() +
	geom_line(aes(y=predict(mod_int_nooutlier))) +
	theme_minimal() +
	scale_colour_manual(values=c("purple", "orange"))

# BUT an interaction makes the model substantially more complex
# Is the extra complexity worth it for model fit, compared to simpler models?
# Following the principle of parsimony ("Occam's razor") - if a simpler explanation is as good as a more complicated one, the simpler should be preferred)


## Model comparison

anova(mod_int_nooutlier, mod_hs_nooutlier) # Note the p-value! Based on sums of squares, just like the model fit
library(lmtest)
lrtest(mod_int_nooutlier, mod_hs_nooutlier) # What about the p-value now? Based on model likelihood, NOT squares
AIC(mod_int_nooutlier, mod_hs_nooutlier)
AIC(mod_h_nooutlier, mod_s_nooutlier, mod_hs_nooutlier, mod_int_nooutlier) # Check out all the candidate models to get a feeling for
# It is clear that including height makes the model much better than just sex; sex also makes a big improvement
# But whether or not sex should be allowed to interact with height, we don't get a clear answer on.


#######################################################
## Section 4 - Common mistakes
#######################################################


## Regarding small sample sizes
# These details are not in the presentation, but for interest, 
# here is an example of looping and sampling etc. to illustrate the power of R 
# Basic idea:
# 1. simulate a population with a known regression relationship
# 2. take samples of different sizes from this population
# 3. fit a model on that sample
# 4. examine patterns in how well these models have captured the true population parameters

## Here we go! First, simulate a population in which we know the true regression coefficients
# This is based on a simpler univariable model to avoid issues of correlation between several predictors
sim_model <- lm(repwt ~ weight, data=measures, na.action=na.exclude)

set.seed(1234) # To make the random sampling replicable

# Simulate the independent variable and error (sampling from appropriate distributions)
# Make the population large, for sampling from later
weight_sim <- rnorm(10000,65.30,13.34) # Parameters same as measures dataset
error_sim <- rnorm(10000,0,2.24) # These parameters are taken from the distribution of the residuals in the model above

# Create the dependent variable based on known ("true") coefficients:

repwt_sim <- coef(sim_model)[[1]]+(coef(sim_model)[[2]]*weight_sim) + error_sim

# Put the simulated population together in a single dataframe to model later
sim_dat<-data.frame("Weight"=weight_sim, "ReportedWeight"= repwt_sim)

# Now let's see how this behaves in practice
# to get a feel for the importance of sample size in applying regression models
# (which applies similarly to any statsitical test/model)

# We'll try a few different sample sizes
# and loop around them to run the same model several times on different datasets

# First, create a dataset to receive the outcome of each simulation
# This is neatest if we decide in advance how many runs we want to do and dimension the dataframe accordingly
samplesizes <- c(5,10,50,100, 1000) # These are the sample sizes we'll play with
# Now build the dataframe
sim_results <- data.frame("SampleSize"=rep(samplesizes, each=10), # Each element of samplesizes is repeated 10 times,
					"Rep"=rep(c(1:10), times=length(samplesizes)), # To keep track of which repetition we are on - repeat the sequence 1-10 the number of times that the vector samplesizes is long
					# Now variables to receive the model outputs (creating these requires specifying how long the vector is)
					"Beta"=numeric(length(samplesizes*10)),
					"LoCI"=numeric(length(samplesizes*10)),
					"UpCI"=numeric(length(samplesizes*10))) 
					

for (n in samplesizes) { # We will run all of the following code for each of these 5 sample sizes - from now on n means whichever sample size we're processing
	
	for (i in c(1:10)) { # And for each sample size, we'll run the sampling & modelling 10 times - from now on, i means whichever of these 10 repeats we are processing
	
		test_set <- sample(10000, size=n) # Randomly sample n values from our population of 10000 to index by in the next step
	
		test_model<-lm(ReportedWeight ~ Weight, data=sim_dat[test_set,]) # Define the model and run it on the relevant sample
	
		# Now take the regression coefficient and CIs from this sampled model 
		# and put them inteo the correct row of the dataframe for future use
		sim_results $Beta[which(sim_results$SampleSize==n & sim_results $Rep==i)] <- coef(test_model)[[2]]
		sim_results$LoCI[which(sim_results$SampleSize==n & sim_results $Rep==i)] <- confint(test_model, "Weight")[[1]]
		sim_results$UpCI[which(sim_results$SampleSize==n & sim_results $Rep==i)] <- confint(test_model, "Weight")[[2]]
		
		# And for good measure, plot the sample with fitted and true relationships
		# We want to save it, so open the device first and specify its name
		pdf(paste0("Sample size ", n, ", Repetition ", i, ".pdf"))
		# Now the plot
		plot(sim_dat$Weight[test_set], sim_dat$ReportedWeight[test_set], main=paste0("Sample size ", n, ", Repetition", i))
		abline(sim_model, lty=2) # The true relationship, dotted line
		abline(test_model, lty=1) # The sample model, solid line
		dev.off() # To close the graphics device and save the file
		
		} # Close the loop for i
	} # Close the loop for n
	

# Plot the outcomes to visualise how sample size impacts effect size estimation
ggplot(sim_results, aes(x=Rep, y=Beta)) + 
	geom_point(size=2) + geom_errorbar(aes(ymax=UpCI, ymin=LoCI)) +
	geom_hline(aes(yintercept=coef(sim_model)[[2]]), linetype=2, size=0.7, color="turquoise3") +
	facet_wrap(~SampleSize) +
	theme_bw()
	

# And the sampling distributions
ggplot(sim_results, aes(Beta, group=SampleSize)) + 
	geom_vline(aes(xintercept=coef(sim_model)[[2]]), linetype=2, size=0.7, color="turquoise3") +
	geom_density(adjust=1.2) +
	facet_wrap(~SampleSize, scales="free_y")


################################	
## Secion 5: expanding the framework
################################


## Looking at a count dataset: a simulated dataset from the Statistical Methods and Data Analytics unit at UCLA
# Their description: "The number of awards earned by students at one high school. Predictors of the number of awards earned include the type of program in which the student was enrolled (e.g., vocational, general or academic) and the score on their final exam in math."

# Load the data from source
pois_data <- read.csv("https://stats.idre.ucla.edu/stat/data/poisson_sim.csv")
# Relabel a factor for ease of use
pois_data$prog <- factor(pois_data$prog, levels=1:3, labels=c("General", "Academic",  "Vocational"))
# Make id a factor
 pois_data$id <- factor(pois_data$id)


## Look at what the distribution is like
ggplot(pois_data, aes(num_awards, fill = prog)) +
  geom_histogram(position="dodge") # dodge puts the different "prog" categories next to each other instead of on top (default)
# Clearly behaving in a different way to typical continuous (normal) data!


## If we miss that and run a standard lm
pois_lm_model <- lm(num_awards ~ prog + math, data= pois_data)
summary(pois_lm_model)

hist(resid(pois_lm_model))
plot(pois_lm_model)
# Not good! Can't trust those coefficients


## These data are not well-behaved for a standard linear model
# A typical first attempt for count data is instead a Poisson GLM
pois_model <- glm(num_awards ~ prog + math, family="poisson", data= pois_data)
summary(pois_model)
exp(coef(pois_model))

# We won't go into assessing model fit for these because it is more complicated/subtle
# because of the link function


## But let's at least examine the fits graphically
# First predict from the models
# The (wrong) linear fit
pois_data$lm_pred <- predict(pois_lm_model) 
# And predict from the better poisson fit
pois_data $pois_pred <- predict(pois_model, type="response") # type="response" makes it predict on the scale of the raw data, not the linear scale (which includes the link function, i.e. log for a poisson model)

# Plot the raw data and the two different fits
# The wrong lm() fit
ggplot(pois_data, aes(x = math, y = lm_pred, colour = prog)) +
  geom_point(aes(y = num_awards), alpha=0.5, size=0.7, position=position_jitter(h=.2)) + # jitter() adds a bit of random noise, in this case in the height, so the points don't all sit exactly on the integers
  geom_line()
# And the better poisson fit	
ggplot(pois_data, aes(x = math, y = pois_pred, colour = prog)) +
  geom_point(aes(y = num_awards), alpha=0.5, size=0.7, position=position_jitter(h=.2)) +
  geom_line()	
  
  
###############
## The end! ##
###############
  