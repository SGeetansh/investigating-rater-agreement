library(irr)
library(irrCAC)
library(dplyr)

################################################################################
############# Basic code to understand both statistics #########################
################################################################################

# Assume you have a matrix of binary ratings with missing values
binary_ratings <- matrix(
c(1, 0, 1, 1,   # Object 1, Rater 1, 2, 3, 4
  0, 1, NA, 0,    # Object 2, Rater 1, 2, 3, NA
  1, 1, 1, 1),    # Object 3, Rater 1, 2, 3, 4
  nrow = 3, ncol = 4, byrow = TRUE)
# Impute missing values with the mean
binary_ratings[is.na(binary_ratings)] <- mean(binary_ratings, na.rm = TRUE)

# Calculate Gwet's AC1
ac1_result <- irrCAC::gwet.ac1.raw(binary_ratings)
## Coeff value: 0.41284
### This value represents the level of agreement among raters, with higher values 
### indicating better agreement.
### Keep in mind that the interpretation of Gwet's AC1 depends on the context of your study, 
### and there are no strict thresholds for what constitutes "good" or "poor" agreement—it often depends 
### on the field and the specific nature of the data being analyzed.

# Calculate Krippendorff's Alpha 
alpha_result <- irr::kripp.alpha(binary_ratings)
## Coeff value: -0.314
### A negative value for Krippendorff's alpha suggests that there is less agreement 
### among raters than would be expected by chance. In other words, the observed agreement is 
### lower than what one would anticipate if the ratings were purely random.


################################################################################
####################### Generating a random matrix #############################
################################################################################

# Can change number of subjects/items, number of raters and percentage missing
generate_random_binary_matrix <- function(num_subjects, num_raters, missing_percentage) {
  # Generate a random binary matrix
  binary_ratings <- matrix(sample(c(0, 1), num_subjects * num_raters, replace = TRUE),
                           nrow = num_subjects, ncol = num_raters)
  colnames(binary_ratings) <- paste0("Rater", 1:num_raters)
  rownames(binary_ratings) <- paste0("Subject", 1:num_subjects)
  
  # Set a specified percentage of values to NA
  num_missing <- round((missing_percentage / 100) * length(binary_ratings))
  binary_ratings[sample(length(binary_ratings), num_missing)] <- NA
  
  return(binary_ratings)
}

# CASE 1: Generate a random binary matrix with 100 subjects, 100 raters, and 20% missing data
mat1 <- generate_random_binary_matrix(100, 100, 20)
print(mat1)

# Implementing on Case 1
## Calculate alpha
alpha_mat1 <- irr::kripp.alpha(mat1)
# alpha = -0.00292

# Update the matrix to mean impute the NA values
mat1[is.na(mat1)] <- mean(mat1, na.rm = TRUE)
# Calculate Gwet's AC1
ac1_mat1 <- irrCAC::gwet.ac1.raw(mat1)
# gwets ac1 = 0.06016

# CASE 2: Generate a random binary matrix with 100 subjects, 100 raters, and 80% missing data
mat2 <- generate_random_binary_matrix(100, 100, 80)
print(mat2)

# Implementing on Case 2
## Calculate alpha
alpha_mat2 <- irr::kripp.alpha(mat2)
# alpha = 0.00169

# Update the matrix to mean impute the NA values
mat2[is.na(mat2)] <- mean(mat2, na.rm = TRUE)
# Calculate Gwet's AC1
ac1_mat2 <- irrCAC::gwet.ac1.raw(mat2)
# gwets ac1 = 0.59102

################################################################################
###################### Comparison of Cases 1 and 2 #############################
################################################################################
simple_comparison <- data.frame(
  Case = c("1", "2"),
  'Missing Data' = c(20, 80),
  Alpha = c(alpha_mat1$value, alpha_mat2$value),
  AC1 = c(ac1_mat1$est$coeff.val,ac1_mat2$est$coeff.val) 
)

################################################################################
########################## Introducing Bias ####################################
################################################################################

#Altering the last function to include bias
generate_random_binary_matrix <- function(num_subjects, num_raters, missing_percentage, bias_probabilities) {
  # Generate a random binary matrix
  binary_ratings <- matrix(sample(c(0, 1), num_subjects * num_raters, replace = TRUE),
                           nrow = num_subjects, ncol = num_raters)
  colnames(binary_ratings) <- paste0("Rater", 1:num_raters)
  rownames(binary_ratings) <- paste0("Subject", 1:num_subjects)
  
  # Introduce bias for each rater based on bias_probabilities
  for (i in 1:num_raters) {
    bias_mask <- sample(c(0, 1), num_subjects, replace = TRUE, prob = c(bias_probabilities[i], 1 - bias_probabilities[i]))
    binary_ratings[, i] <- ifelse(bias_mask, binary_ratings[, i], 1 - binary_ratings[, i])
  }
  
  # Set a specified percentage of values to NA
  num_missing <- round((missing_percentage / 100) * length(binary_ratings))
  binary_ratings[sample(length(binary_ratings), num_missing)] <- NA
  
  return(binary_ratings)
}

# Simple case to check: Generate a random binary matrix with bias for Rater2, 4 subjects, 3 raters, and 20% missing data
bias_probabilities <- c(0.1, 0.3, 0.2)  # Adjust bias probabilities for each rater
bias_trial <- generate_random_binary_matrix(4, 3, 20, bias_probabilities)
print(bias_trial)

irr::kripp.alpha(bias_trial)
# alpha= -0.2 
bias_trial[is.na(bias_trial)] <- mean(bias_trial, na.rm = TRUE)
irrCAC::gwet.ac1.raw(bias_trial)
# ac1= 0.27273

# opposite results for both

# Now update bias_probabilities to be a randonly generated vector which randomly assigns bias
generate_random_binary_matrix <- function(num_subjects, num_raters, missing_percentage, bias_probabilities_range) {
  # Generate a random binary matrix
  binary_ratings <- matrix(sample(c(0, 1), num_subjects * num_raters, replace = TRUE),
                           nrow = num_subjects, ncol = num_raters)
  colnames(binary_ratings) <- paste0("Rater", 1:num_raters)
  rownames(binary_ratings) <- paste0("Subject", 1:num_subjects)
  
  # Generate random bias probabilities for each rater
  bias_probabilities <- runif(num_raters, min = bias_probabilities_range[1], max = bias_probabilities_range[2])
  
  # Introduce bias for each rater based on randomly generated bias_probabilities
  for (i in 1:num_raters) {
    bias_mask <- sample(c(0, 1), num_subjects, replace = TRUE, prob = c(bias_probabilities[i], 1 - bias_probabilities[i]))
    binary_ratings[, i] <- ifelse(bias_mask, binary_ratings[, i], 1 - binary_ratings[, i])
  }
  
  # Set a specified percentage of values to NA
  num_missing <- round((missing_percentage / 100) * length(binary_ratings))
  binary_ratings[sample(length(binary_ratings), num_missing)] <- NA
  
  return(binary_ratings)
}

# The bias_probabilities_range parameter defines the minimum and maximum values for 
# the uniform distribution from which these random bias probabilities are drawn.
bias_probabilities_range <- c(0.1, 0.9)  # Adjust the range of bias probabilities

# Case 3: Generate a random binary matrix with randomly generated bias for 100 subjects, 100 raters, and 20% missing data
bias_mat1 <- generate_random_binary_matrix(100, 100, 20, bias_probabilities_range)

alpha_bias_mat1 <- irr::kripp.alpha(bias_mat1)
# alpha= 0.00082 
bias_mat1[is.na(bias_mat1)] <- mean(bias_mat1, na.rm = TRUE)
ac1_bias_mat1 <- irrCAC::gwet.ac1.raw(bias_mat1)
# ac1= 0.05852

# Case 4: Generate a random binary matrix with randomly generated bias for 100 subjects, 100 raters and 80% missing data
bias_mat2 <- generate_random_binary_matrix(100, 100, 80, bias_probabilities_range)
alpha_bias_mat2 <- irr::kripp.alpha(bias_mat2)
# alpha= -0.00395
bias_mat2[is.na(bias_mat2)] <- mean(bias_mat2, na.rm = TRUE)
ac1_bias_mat2 <- irrCAC::gwet.ac1.raw(bias_mat2)
# ac1= 0.59018
################################################################################
###################### Comparison of Cases 3 and 4 #############################
################################################################################
bias_comparison <- data.frame(
  Case = c("3", "4"),
  'Missing Data' = c(20, 80),
  Alpha = c(alpha_bias_mat1$value, alpha_bias_mat2$value),
  AC1 = c(ac1_bias_mat1$est$coeff.val,ac1_bias_mat2$est$coeff.val) 
)

# Combine data frames with dplyr's bind_rows
combined_df1<- bind_rows(simple_comparison, bias_comparison)

################################################################################
############################ Computing ICC #####################################
################################################################################
ICC <- data.frame(
  ICC = c(icc(mat1)$value  ,icc(mat2)$value ,icc(bias_mat1)$value ,icc(bias_mat2)$value))

combined_df2<- bind_cols(combined_df1, ICC)

################################################################################
# How can I skew the data?
# Skew means one or some answers have a higher probability of being eitgher 1 or 0
# Change original function for generating the data matrix to have a variable that allows me to 
# decide how many subjects have a skew and randomly assigns the direction of skew
################################################################################