###################################################
## Associated paper: Nelson, A.S., M. Gelambi, E. Morales-M., and S.R. Whitehead. 2023. Fruit secondary 
## metabolites mediate the quantity and quality of a seed dispersal mutualism. Ecology.
## File: Effects of natural variation in amide content on seed dispersal quantity (Appendix S1)
## Corresponding Author: Annika S. Nelson (University of California, Irvine; annikasn@uci.edu)
###################################################

## README ----
# This script is divided into the following sections:
# 1. Preliminaries
#    1.1 Load required packages
#    1.2 Set working directory
# 2. Create standard curve for piperine
#    2.1 Import and clean standard curve data
#    2.2 Make the standard curve
# 3. Correct sample peak areas based on the internal standard
#    3.1 Import and clean sample data
#    3.2 Get the median internal standard peak area
#    3.3 Correct sample peak areas
# 4. Use the standard curve parameters to convert peak areas to concentrations
# 5. Calculate the percent dry mass of each compound
#    5.1 Merge sample data with metadata
#    5.2 Calculate the percent sample dry mass
# 6. Test whether ant dispersal depends on amide concentration
#    6.1 Test whether ant dispersal depends on amide concentration of fruit pulp
#    6.2 Test whether ant dispersal depends on amide concentration of seeds
# 7. Data visualization
#    7.1 Make Appendix S1: Figure S5

###################################################
## 1. Preliminaries ----
###################################################
## 1.1 Load required packages ----
library(dplyr)
library(lmerTest)
library(ggplot2)

## 1.2 Set working directory ----
setwd() # Choose your working directory based on where the data files are saved

###################################################
## 2. Create standard curve for piperine ----
###################################################
## 2.1 Import and clean standard curve data ----

# Import piperine standard curve data
piperine_sd_curve <- read.csv("AppendixS1_Piperine_SD_Curve.csv", header = T, na.strings = "")

# Summarize the piperine standard curve data, so that the data frame just contains the mean peak area for each concentration of each compound
piperine_sd_curve <- piperine_sd_curve %>% group_by(concentration) %>%
  summarize(piperine = mean(piperine.peak.area)) 

## 2.2 Make the standard curve ----

# Plot the standard curve, setting the intercept to 0
ggplot(piperine_sd_curve, aes(x = concentration, y = piperine)) + 
  geom_point() + 
  stat_smooth(method = "lm", formula = y~0+x) + 
  theme_classic() + 
  xlab("Concentration (mg/mL)") + ylab("Peak area")

# Obtain the R2
summary(lm(piperine ~ 0 + concentration, data = piperine_sd_curve)) # 0.96 R2

# Set the intercept to 0
piperine_intercept <- 0

# Get the slope
piperine_slope <- summary(lm(piperine ~ 0 + concentration, data = piperine_sd_curve))$coefficients[1,1]

###################################################
## 3. Correct sample peak areas based on the internal standard ----
###################################################
## 3.1 Import and clean sample data ----

# Import the sample data
amide_data <- read.csv("AppendixS1_Sample_Chemistry.csv", header = F, na.strings = "")[-c(2),]
colnames(amide_data) <- as.character(amide_data[1,]) # set the first row as the column names
amide_data <- data.frame(amide_data[-1,]) # remove the first row that we just changed to a header

# Define all peak areas as numeric
amide_data[2:32] <- sapply(sapply(amide_data[2:32], as.character), as.numeric)

## 3.2 Get the median internal standard peak area ----

# Visualize the spread in the internal standard (piperine) peak areas
hist(amide_data$Piperine)

# Calculate the median peak area
medITSD <- median(amide_data$Piperine)

## 3.2 Correct sample peak areas ----

# Create a function to correct the peak areas based on the internal standard
ITSD_correction <- function(x) {
  amide_data[x] = amide_data[x]*(medITSD/amide_data[12])
}

# Create an empty data frame with the same dimensions as the original data frame
chemdata_corrected <- data.frame(matrix(ncol = 32, nrow = 41))

# Fill in the first column of the new data frame with the sample names
chemdata_corrected[,1] <- amide_data[,1]

# Fill in all corrected peak areas in the remaining empty columns
for(i in c(2:32)) {
  chemdata_corrected[,i] = ITSD_correction(i)
}

# Add column names to the data frame
columns <- colnames(amide_data[,1:32])
colnames(chemdata_corrected) <- columns

###################################################
## 4. Use the standard curve parameters to convert peak areas to concentrations ----
###################################################

# The formula for the piperine standard curve is: peak area = intercept + slope*concentration
# Solving for concentration, we get: concentration = (peak area - intercept)/slope

# Create a function to solve for concentration using this formula
concentration <- function(x) {
  chemdata_corrected[x] = (chemdata_corrected[x] - piperine_intercept)/piperine_slope
}

# Replace all peak areas in the data frame with concentrations
for(i in c(2:32)) {
  chemdata_corrected[,i] = concentration(i)
}

###################################################
## 5. Calculate the percent dry mass of each compound ----
###################################################
## 5.1 Merge sample data with metadata ----

# Import sample metadata
amide_metadata <- read.csv("AppendixS1_Sample_Metadata.csv", header = T, na.strings = "")

# Join the data with the metadata
full_data <- full_join(amide_metadata, chemdata_corrected, by = "sample")

# Define the proportion of seeds removed as a numeric variable
full_data$prop.seeds.removed <- as.numeric(as.character(full_data$prop.seeds.removed))

## 5.2 Calculate the percent sample dry mass ----

# The formula for calculating the percent dry mass is: % dry mass = (sample concentration mg/mL * 0.1 mL solvent volume) * (1/sample dry mass (mg)) * 100

# Create a function for calculating percent dry mass
perc_mass_function <- function(x) {
  full_data[x] = ((full_data[x]*0.1)*(1/full_data[10])*100)
}

# Calculate the percent dry mass of each compound
for(i in c(11:41)) {
  full_data[,i] = perc_mass_function(i)
}

# Calculate total amide concentrations in each sample
full_data$tot_amides <- rowSums(full_data[,c(11:20,22:41)])

###################################################
## 6. Test whether ant dispersal depends on amide concentration ----
###################################################
## 6.1 Test whether ant dispersal depends on amide concentration of fruit pulp ----

# Linear model testing whether the proportion of seeds handled by ants depends on the total amide content of fruit pulp
model <- lm(prop.seeds.removed ~ tot_amides, data = subset(full_data, tissue == "Fruit pulp"))

# Test for normality of model residuals
shapiro.test(resid(model))
hist(resid(model))
qqnorm(resid(model))
qqline(resid(model))

# F test for statistical significance
anova(model)

## 6.2 Test whether ant dispersal depends on amide concentration of seeds ----

# Test whether the proportion of seeds handled by ants in the nest depends on amide content of seeds
model <- lm(prop.seeds.removed ~ tot_amides, data = subset(full_data, tissue == "Seed"))

# Test for normality of model residuals
shapiro.test(resid(model))
hist(resid(model))
qqnorm(resid(model))
qqline(resid(model))

# F test for statistical significance
anova(model)

###################################################
## 7. Data visualization ----
###################################################
## 7.1 Make Appendix S1: Figure S5 ----

figS5a <- ggplot(subset(full_data, tissue == "Fruit pulp"), aes(x = tot_amides, y = prop.seeds.removed)) + 
  geom_point() + 
  stat_smooth(method = "lm", col = "black") + 
  theme_classic() + 
  xlab("Fruit pulp amide percent mass") + 
  ylab("Proportion of seeds removed") +
  theme(legend.position = "None")

figS5b <- ggplot(subset(full_data, tissue == "Seed"), aes(x = tot_amides, y = prop.seeds.removed)) + 
  geom_point(alpha = 1) + 
  stat_smooth(method = "lm", col = "black", linetype = "dashed") + 
  theme_classic() + 
  xlab("Seed amide percent mass") + 
  ylab("Proportion of seeds removed") +
  theme(legend.position = "None")

# Combine the panels into one figure
figS5 <- ggpubr::ggarrange(figS5a, figS5b, ncol = 2, nrow = 1, 
                           align = "hv", labels = c("(a)","(b)"), label.x = -0.03)

# Visually inspect the figure
figS5
dev.off()

# Save the figure as a pdf
ggsave("figS5.pdf", figS5, width = 6, height = 3)
