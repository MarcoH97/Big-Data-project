# Load packages
library(combinat)
library(foreach)
library(doParallel)
library(readxl)
library(progress)
library(dplyr)
library(parallel)



# Get the directory path of the current code
PATH <- dirname(rstudioapi::getSourceEditorContext()$path)
# Set the working directory to where the code is 
setwd(PATH)

# Import the excel file
data <- read_excel("C:/Users/marco/Documents/HSG/Big Data/BDBD_data.xlsx")

# Remove rows with NA
data <- na.omit(data)

# Set the first column as row index
rownames(data) <- data[,1]

# Remove the first column from the data frame
data <- data[, -1]


# Define a function to generate weighted columns
generate_weighted_cols <- function(data, max_comb_size) {
  # Get the number of columns in the dataframe
  num_cols <- ncol(data)
  
  # Iterate over i for i-combinations
  for (i in 2:min(num_cols, max_comb_size)) {
    # Generate all i-combinations of column indices
    combos <- combinat::combn(1:num_cols, i, simplify = FALSE)
    
    # Iterate over each combination
    for (combo in combos) {
      # Calculate the new column as the row-wise mean of the selected columns
      new_col <- rowMeans(data[, combo])
      
      # Create the new column name
      new_col_name <- paste(names(data)[combo], collapse = " ")
      
      # Add the new column to the dataframe
      data[[new_col_name]] <- new_col
    }
  }
  return(data)
}

# Use the function on your dataframe with max combination size 
start_time <- Sys.time()
data <- generate_weighted_cols(data, 6)
end_time <- Sys.time()
execution_time <- end_time - start_time
print(execution_time)

# Check the size of the object
print(object.size(data), units = "MB")


#### CPU optimisation ####

# Define a function to generate weighted columns for a single combination
generate_single_combination <- function(combo, data) {
  # Calculate the new column as the row-wise mean of the selected columns
  new_col <- rowMeans(data[, combo])
  
  # Create the new column name
  new_col_name <- paste(names(data)[combo], collapse = " ")
  
  # Return a data frame with the new column
  return(data.frame(new_col_name = new_col))
}

# Define a function to generate weighted columns
generate_weighted_cols <- function(data, max_comb_size) {
  # Get the number of columns in the dataframe
  num_cols <- ncol(data)
  
  # Get the number of cores
  num_cores <- detectCores()
  
  # Create a cluster with the number of cores
  cl <- makeCluster(num_cores)
  
  # Register the cluster
  registerDoParallel(cl)
  
  # Iterate over i for i-combinations
  for (i in 2:min(num_cols, max_comb_size)) {
    # Generate all i-combinations of column indices
    combos <- combinat::combn(1:num_cols, i, simplify = FALSE)
    
    # Calculate the new columns in parallel
    new_cols <- foreach(combo = combos, .packages = "combinat") %dopar% {
      generate_single_combination(combo, data)
    }
    
    # Combine the new columns into a single data frame
    new_cols_df <- do.call(cbind, new_cols)
    
    # Add the new columns to the dataframe
    data <- cbind(data, new_cols_df)
  }
  
  # Stop the cluster
  stopCluster(cl)
  
  return(data)
}

# Use the function on your dataframe with max combination size 
start_time <- Sys.time()
data <- generate_weighted_cols(data, 2)
end_time <- Sys.time()
execution_time <- end_time - start_time
print(execution_time)