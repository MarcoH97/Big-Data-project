# Load packages
library(combinat)
library(foreach)
library(doParallel)
library(readxl)
library(progress)
library(dplyr)
library(parallel)
library(future.apply)
library(progressr)



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
plan(multisession)  # use available cores for parallel processing
handlers(global = TRUE)
# options(future.globals.maxSize = 1024 * 1024 * 1024) # Crashes R, don't uncomment!

generate_weighted_cols_parallel <- function(data, max_comb_size) {
  
  # Get the number of columns in the dataframe
  num_cols <- ncol(data)
  
  # Iterate over i for i-combinations
  for (i in 2:min(num_cols, max_comb_size)) {
    
    # Generate all i-combinations of column indices
    combos <- combinat::combn(1:num_cols, i, simplify = FALSE)
    
    # Create progress bar
    p <- progressor(along = combos)
    
    # Calculate the new column and column name in parallel
    new_cols <- future_lapply(combos, function(combo) {
      # Calculate the new column as the row-wise mean of the selected columns
      new_col <- rowMeans(data[, combo])
      
      # Create the new column name
      new_col_name <- paste(names(data)[combo], collapse = " ")
      
      p()  # update progress bar
      
      return(list(name = new_col_name, column = new_col))
    })
    
    # Add new columns to the data frame
    for (new_col in new_cols) {
      data[[new_col$name]] <- new_col$column
    }
  }
  
  return(data)
}



start_time <- Sys.time()
data <- generate_weighted_cols_parallel(data, 2)
end_time <- Sys.time()
execution_time <- end_time - start_time
print(execution_time)

# Check the size of the object
print(object.size(data), units = "MB")
