#test test
# Load packages
library(combinat)
library(foreach)
library(doParallel)
library(readxl)
library(progress)


# Get the directory path of the current code
PATH <- dirname(rstudioapi::getSourceEditorContext()$path)
# Set the working directory to where the code is 
setwd(PATH)

# Import the excel file
data <- read_excel("C:/Users/marco/Documents/HSG/Big Data/BDBD_data.xlsx")

# Remove rows with NA
data <- na.omit(data)

# Set the first column as row index
row.names(data) <- data[,1]

# Remove the first column from the data frame
data <- data[, -1]

# Get column names
cols <- colnames(data)

#register a parallel backend using all available cores
registerDoParallel(makeCluster(detectCores()))

# create a progress bar
pb <- progress_bar$new(total = length(cols) - 1)

# For each number from 2 to the number of columns in the data
for (n in 2:length(cols)) {
  
  # update the progress bar
  pb$tick()
  
  # Generate all combinations of columns of size 'n'
  combinations <- combinat::combn(cols, n, simplify = FALSE)
  
  # For each combination of columns
  foreach(i = 1:length(combinations), .combine = rbind) %dopar% {
    comb <- combinations[[i]]
    
    # calculate the new column applying appropriate weights
    new_col <- rowSums(data[, comb] / n)
    
    # create a name for the new column
    new_col_name <- paste(comb, collapse = "-")
    
    # add the new column to the data
    data[, new_col_name] <- new_col
  }
}

# stop the parallel cluster
stopImplicitCluster()