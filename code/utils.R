library(irr)
library(irrCAC)
library(dplyr)

impute_missing_values_matrix <- function(input_matrix) {
    num_cols <- ncol(input_matrix)

    for (i in 1:num_cols) {
        column <- input_matrix[, i]

        # Count the number of ones and zeroes in the column
        ones_count <- sum(column == 1, na.rm = TRUE)
        zeroes_count <- sum(column == 0, na.rm = TRUE)

        # Calculate the probabilities for one and zero based on counts
        total <- ones_count + zeroes_count
        prob_one <- ones_count / total
        prob_zero <- zeroes_count / total

        # Replace missing values with randomly selected values based on probabilities
        missing_indices <- is.na(column)
        num_missing <- sum(missing_indices)

        if (num_missing > 0) {
            random_values <- sample(c(1, 0), num_missing, replace = TRUE, prob = c(prob_one, prob_zero))
            column[missing_indices] <- random_values
            input_matrix[, i] <- column
        }
    }

    return(input_matrix)
}

generate_random_binary_matrix <- function(num_subjects, num_raters, missing_percentage) {
    # Generate a random binary matrix
    binary_ratings <- matrix(sample(c(0, 1), num_subjects * num_raters, replace = TRUE),
                             nrow = num_subjects, ncol = num_raters,
                             dimnames = list(paste0("Subject", 1:num_subjects),
                                             paste0("Rater", 1:num_raters)))

    # Set a specified percentage of values to NA
    num_missing <- round((missing_percentage / 100) * length(binary_ratings))
    binary_ratings[sample(length(binary_ratings), num_missing)] <- NA

    return(binary_ratings)
}

generate_random_binary_matrix_with_bias <- function(num_subjects, num_raters, missing_percentage, bias_probabilities) {
    # Generate a random binary matrix
    binary_ratings <- matrix(sample(c(0, 1), num_subjects * num_raters, replace = TRUE),
                             nrow = num_subjects, ncol = num_raters,
                             dimnames = list(paste0("Subject", 1:num_subjects),
                                             paste0("Rater", 1:num_raters)))

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

generate_random_binary_matrix_with_random_bias <- function(num_subjects, num_raters, missing_percentage, bias_probabilities_range) {
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
