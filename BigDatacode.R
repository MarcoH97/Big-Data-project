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
library(RSQLite)
library(DBI)
library(ggplot2)
library(reshape2)
library(scales)


# Get the directory path of the current code
PATH <- dirname(rstudioapi::getSourceEditorContext()$path)
# Set the working directory to where the code is 
setwd(PATH)

# Import the excel file
data <- read_excel("C:/Users/marco/Documents/HSG/Big Data/BDBD_data.xlsx")

# Remove rows with NA
data <- na.omit(data)

#data$Dates <- as.Date(data$Dates)

# Set the first column as row index
#rownames(data) <- data[,1]

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
data <- generate_weighted_cols(data, 5)
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
data <- generate_weighted_cols_parallel(data, 3)
end_time <- Sys.time()
execution_time <- end_time - start_time
print(execution_time)

# Check the size of the object
print(object.size(data), units = "MB")

#### SQLite ####
sanitize_column_name <- function(name) {
  # Remove or replace special characters
  name <- gsub("`", "", name)
  name <- gsub(" ", "_", name)
  
  # If the name starts with a digit, prepend "X"
  if (grepl("^[0-9]", name)) {
    name <- paste("X", name, sep = "")
  }
  
  return(name)
}

generate_weighted_cols_sqlite <- function(data, max_comb_size) {
  # Create a temporary SQLite database
  con <- dbConnect(RSQLite::SQLite(), ":memory:")
  
  # Change the column names if they are numeric
  names(data) <- ifelse(grepl("^[0-9]", names(data)), paste("X", names(data), sep = ""), names(data))
  
  # Copy the data frame to the database
  dbWriteTable(con, "data", data)
  
  # Get the column names
  cols <- colnames(data)
  
  # Iterate over i for i-combinations
  for (i in 2:min(length(cols), max_comb_size)) {
    # Generate all i-combinations of column names
    combos <- combinat::combn(cols, i, simplify = FALSE)
    
    # Iterate over each combination
    for (combo in combos) {
      # Create the new column name ensuring it doesn't start with a digit
      new_col_name <- paste("X", paste(combo, collapse = "_"), sep = "")
      
      # Calculate the new column as the mean of the selected columns
      sql <- paste0('ALTER TABLE data ADD COLUMN "', new_col_name, '" REAL;')
      dbExecute(con, sql)
      sql <- paste0('UPDATE data SET "', new_col_name, '" = (', paste('COALESCE("', combo, '", 0)', sep = "", collapse = " + "), ")/", length(combo), ";")
      dbExecute(con, sql)
    }
  }
  
  # Read the updated data back into R
  data <- dbReadTable(con, "data")
  
  # Disconnect from the database
  dbDisconnect(con)
  
  return(data)
}

# Add a prefix to numeric column names
names(data) <- ifelse(grepl("^[0-9]", names(data)), paste("X", names(data), sep = ""), names(data))

start_time <- Sys.time()
data <- generate_weighted_cols_sqlite(data, 2)
end_time <- Sys.time()
execution_time <- end_time - start_time
print(execution_time)

# Check the size of the object
print(object.size(data), units = "MB")


#### Algo ####
# Define function
calculate_returns <- function(data, years, threshold) {
  
  # Calculate number of rows (days) per year
  days_per_year <- 252  # typically there are 252 trading days in a year
  
  # Convert years to trading days
  period <- years * days_per_year
  
  # Initialize output data frame
  output <- data.frame(Column = character(),
                       Worst_Period_End_Value = numeric(),
                       stringsAsFactors = FALSE)
  
  # Iterate over each column
  for (col in names(data)) {
    
    if (col == "Date") next  # skip Date column
    
    # Initialize worst period end value as infinity
    worst_period_end_value <- Inf
    
    # Initialize flag indicating whether column dropped below threshold
    below_threshold <- FALSE
    
    # Iterate over each day
    for (i in 1:(nrow(data) - period + 1)) {
      
      # Calculate product of returns for the period
      period_return <- prod(1 + data[i:(i + period - 1), col]) * 100
      
      # Check if period return dropped below threshold
      if (period_return < threshold) {
        below_threshold <- TRUE
        break
      }
      
      # Update worst period end value
      worst_period_end_value <- min(worst_period_end_value, period_return)
      
    }
    
    # If column never dropped below threshold, add to output
    if (!below_threshold) {
      output <- rbind(output, data.frame(Column = col,
                                         Worst_Period_End_Value = worst_period_end_value,
                                         stringsAsFactors = FALSE))
    }
    
  }
  
  # Return output
  return(output)
  
}

# Use function
# Replace "10" and "100" with your desired years and threshold
start_time <- Sys.time()
result <- calculate_returns(data, 10, 85)
end_time <- Sys.time()
execution_time <- end_time - start_time
print(execution_time)


# Print result
print(result)


#### Algo parallel processing ####

calculate_returns_single_column <- function(col_name, data, period, threshold) {
  # Skip Date column
  if (col_name == "Date") return(NULL)
  
  # Initialize worst period end value as infinity
  worst_period_end_value <- Inf
  
  # Initialize flag indicating whether column dropped below threshold
  below_threshold <- FALSE
  
  # Iterate over each day
  for (i in 1:(nrow(data) - period + 1)) {
    # Calculate product of returns for the period
    period_return <- prod(1 + data[i:(i + period - 1), col_name]) * 100
    
    # Check if period return dropped below threshold
    if (period_return < threshold) {
      below_threshold <- TRUE
      break
    }
    
    # Update worst period end value
    worst_period_end_value <- min(worst_period_end_value, period_return)
  }
  
  # If column never dropped below threshold, return result
  if (!below_threshold) {
    return(data.frame(Column = col_name,
                      Worst_Period_End_Value = worst_period_end_value,
                      stringsAsFactors = FALSE))
  } else {
    return(NULL)
  }
}

calculate_returns_parallel <- function(data, years, threshold) {
  # Calculate number of rows (days) per year
  days_per_year <- 252  # typically there are 252 trading days in a year
  
  # Convert years to trading days
  period <- years * days_per_year
  
  # Create cluster with number of cores  
  cl <- makeCluster(detectCores() - 1)
  
  # Export necessary objects to the workers
  clusterExport(cl, c("data", "period", "threshold"))
  
  # Apply the function to each column in parallel
  output <- parLapply(cl, names(data), calculate_returns_single_column)
  
  # Stop the cluster
  stopCluster(cl)
  
  # Combine results and remove NULL results (columns that dropped below threshold)
  output <- do.call("rbind", output)
  
  # Return output
  return(output)
}

# Use function
# Replace "10" and "85" with your desired years and threshold
start_time <- Sys.time()
result <- calculate_returns_parallel(data, 10, 85)
end_time <- Sys.time()
execution_time <- end_time - start_time
print(execution_time)


# Print result
print(result)






#### Algo test ####
# Define function
calculate_returns <- function(data, years, threshold) {
  
  # Calculate number of rows (days) per year
  days_per_year <- 252
  
  # Convert years to trading days
  period <- years * days_per_year
  
  # Initialize output data frame
  output <- data.frame(Column = character(),
                       Worst_Period_Start = numeric(),
                       Worst_Period_End_Value = numeric(),
                       stringsAsFactors = FALSE)
  
  # Iterate over each column
  for (col in names(data)) {
    
    # Initialize worst period end value as infinity
    worst_period_end_value <- Inf
    worst_period_start <- 0
    
    # Initialize flag indicating whether column dropped below threshold
    below_threshold <- FALSE
    
    # Iterate over each day
    for (i in 1:(nrow(data) - period + 1)) {
      
      # Calculate product of returns for the period
      period_return <- prod(1 + data[i:(i + period - 1), col]) * 100
      
      # Check if period return dropped below threshold
      if (period_return < threshold) {
        below_threshold <- TRUE
        break1
      }
      
      # Update worst period end value and its start
      if (period_return < worst_period_end_value) {
        worst_period_end_value <- period_return
        worst_period_start <- i
      }
    }
    
    # If column never dropped below threshold, add to output
    if (!below_threshold) {
      output <- rbind(output, data.frame(Column = col,
                                         Worst_Period_Start = worst_period_start,
                                         Worst_Period_End_Value = worst_period_end_value,
                                         stringsAsFactors = FALSE))
    }
  }
  
  # Return output
  return(output)
}


# Use function
result <- calculate_returns(data, 10, 85)

# Plot each surviving column's worst period
for (i in 1:nrow(result)) {
  
  # Extract column name
  col <- result$Column[i]
  
  # Extract start date of worst period
  start <- result$Worst_Period_Start[i]
  
  # Calculate end date of worst period
  end <- start + 10 * 252 - 1  # 10 years of trading days
  
  # Subset data for worst period
  subset_data <- data.frame(Index = start:end, Value = data[start:end, col])
  
  # Plot time series
  p <- ggplot(data = subset_data, aes(x = Index, y = Value)) +
    geom_line() +
    labs(title = paste("Worst period for", col), x = "Trading Days", y = col) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
  
  # Print plot
  print(p)
  
}


#### Data visualization ####
# Plot the correlation matrix of all the returns
correlation_matrix <- cor(data)
print(correlation_matrix)

# Melt the correlation matrix to long format for ggplot2
melted_cormat <- melt(correlation_matrix)

# Plot the correlation matrix
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 10, hjust = 1), 
        axis.title = element_blank()) +
  coord_fixed(ratio = 1/1.5)







# PLot the mean-variance graph for the variables
means <- colMeans(data)
daily_sd <- apply(data, 2, sd)

# Annualizing means
annual_means <- (1 + means)^252 - 1
# Annualize the standard deviation
annual_sd <- daily_sd * sqrt(252)

plot_df <- data.frame(means=annual_means, sds=annual_sd)

# Add a new column for color
plot_df$group <- NA
# Assign group labels based on column number
plot_df$group[1:26] <- 'Original strategies'
plot_df$group[27:351] <- '2 combinations'
plot_df$group[352:2951] <- '3 combinations'
plot_df$group[2952:17901] <- '4 combinations'
plot_df$group[17902:83681] <- '5 combinations'


# Plot with color aesthetic mapped to group
ggplot(plot_df, aes(x=annual_means, y = annual_sd, color=group))+
  geom_point()+
  labs(x='Mean', y="Standard deviation", title="Annualised mean and standard deviation of all the investment strategies")+
  scale_x_continuous(labels = scales::percent)+
  scale_y_continuous(labels = scales::percent)+
  scale_color_manual(values=c('Original strategies' = '#000000', '2 combinations' = '#FF0000', 
                              '3 combinations' = '#00FF00', '4 combinations' = '#0000FF', 
                              '5 combinations' = '#FFA500'))+
  theme_bw()+
  theme(plot.title = element_text(size= 14, hjust = 0.5, face ="bold"), 
        axis.text.x = element_text(angle = 90, hjust = 1, face = "bold"),
        axis.text.y = element_text(angle = 90, hjust = 1, face = "bold"))



# 26 strategies only
ggplot(plot_df, aes(x=means_percent, y = var_percent))+
  geom_point()+
  labs(x='Mean', y="Variance", title="Mean and variance of all the investment strategies")+
  scale_x_continuous(labels = percent)+
  scale_y_continuous(labels = percent)+
  theme_bw()+
  theme(plot.title = element_text(size= 14, hjust = 0.5, face ="bold"), 
        axis.text.x = element_text(angle = 90, hjust = 1, face = "bold"),
        axis.text.y = element_text(angle = 90, hjust = 1, face = "bold"))


# Plot the runtime differences
# Create the data
combinations <- c(2, 3, 4, 5)
normal_runtime <- c(0.001279648, 0.018118033, 0.231115833, 8.696434)
#c(0.07677889, 1.087082, 13.86695, 521.78604)
parallel_runtime <- c(0.019332633, 0.061772267, 0.342696167, 1.232462)
#c(1.159958, 3.706336, 20.56177, 73.94772)

runtime_df <- data.frame(Combinations = combinations, Normal_Runtime = normal_runtime, Parallel_Runtime = parallel_runtime)

# Plotting the data
# Define the desired legend title and labels
legend_title <- "Type of generation"
legend_labels <- c("Normal Runtime", "Parallel Runtime")

ggplot(runtime_df, aes(x = Combinations)) +
  geom_line(aes(y = Normal_Runtime, color = "Normal R"), size = 1) +
  geom_line(aes(y = Parallel_Runtime, color = "Parallel Processing"), size = 1) +
  labs(title = "Runtime Comparison for data generation",
       x = "Combinations",
       y = "Runtime (minuites)",
       color = legend_title) +
  theme_bw() +
  theme(plot.title =  element_text(size= 14, hjust = 0.5, face ="bold"), 
        axis.text.x = element_text(angle = 90, hjust = 1, face = "bold"),
        axis.text.y = element_text(angle = 90, hjust = 1, face = "bold"),
        legend.position = "right")


# Create the data
combinations2 <- c(2, 3, 4, 5, 6, 7, 8, 9, 10)
columns2 <- c(351, 2951, 17901, 83681, 313911, 971711, 2533986, 5658536, 10970271)
file_size2 <- c(14, 115, 694, 3245, 12171, 37675, 98248, 219394, 425342)

# Create the dataframe
size_df <- data.frame(Combinations = combinations2, Columns = columns2, File_Size_MB = file_size2)
