# Assisted by ChatGPT (https://chat.openai.com/) while writing the code below.
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


##############################################################################

# Get the directory path of the current code file
PATH <- dirname(rstudioapi::getSourceEditorContext()$path)

# Set the working directory to that of the current code file 
setwd(PATH)

# Load functions file
source("BDA_BDBD_functions - Luca.R")

##############################################################################

# DATA COLLECTION: leveraging data from multiple sources
# Loading the raw data into R from different sources, each with different data formats.

# Load raw data for prices of selected indices 
data_indices_full <- read_excel("Bloomberg_Terminal-spreadsheet_builder.xlsx", sheet = 1, col_names = FALSE)
index_prices_local_currency <- data_indices_full[7:nrow(data_indices_full), ]
colnames(index_prices_local_currency) <- data_indices_full[4, ]
names(index_prices_local_currency)[1] <- "Dates"
index_prices_local_currency$Dates <- as.Date(as.numeric(index_prices_local_currency$Dates), origin = "1899-12-30")

# Load raw data for prices of selected indices 
data_FX_full <- read_excel("Bloomberg_Terminal-spreadsheet_builder.xlsx", sheet = 2, col_names = FALSE)
CHF_FX <- data_FX_full[7:nrow(data_indices_full), ]
colnames(CHF_FX) <- data_FX_full[4, ]
names(CHF_FX)[1] <- "Dates"
CHF_FX$Dates <- as.Date(as.numeric(CHF_FX$Dates), origin = "1899-12-30")

# Load raw data for Swiss inflation (CPI in %)
data_inflation_full <- read_excel("API_FP.CPI.TOTL.ZG_DS2_en_excel_v2_5454868.xls", sheet = 1, col_names = FALSE)
data_inflation <- data.frame(t(data_inflation_full[4:nrow(data_inflation_full), ]))
colnames(data_inflation) <- data_inflation[1, ]
data_inflation <- data_inflation[-(1:4), ]
names(data_inflation)[1] <- "Dates"
swiss_inflation <- data_inflation[, c('Dates', 'Switzerland')]

# Load and merge raw data for CHF money market rates and CHF spot interest rates on Swiss Confederation bond issues
data_ST_rf_CHF <- read_excel("snb-chart-data-zimomach-en-all-20230502_1430.xlsx", skip = 15, col_names = TRUE)
data_LT_rf_CHF <- read_excel("snb-chart-data-rendeidglfzch-en-all-20230502_1430.xlsx", skip = 15, col_names = TRUE)
names(data_ST_rf_CHF)[1] <- "Dates"
names(data_LT_rf_CHF)[1] <- "Dates"
data_ST_rf_CHF <- data_ST_rf_CHF[, c('Dates', 'SARON close of trading')]
CHF_rf_rates <- merge(data_ST_rf_CHF, data_LT_rf_CHF, by = "Dates", all = TRUE)

# Remove from our  R environment the variables that we no longer need
rm(data_indices_full, data_FX_full, data_inflation_full, data_inflation, data_ST_rf_CHF, data_LT_rf_CHF)

# Aligning dates across different data frames 


##############################################################################

# DATA CLEANING AND DATA INTEGRATION

class(index_prices_local_currency$Dates)
class(CHF_FX$Dates)
class(swiss_inflation$Dates)
class(CHF_rf_rates$Dates)

# Convert Dates to a Date object
index_prices_local_currency$Dates <- as.Date(index_prices_local_currency$Dates)
CHF_FX$Dates <- as.Date(CHF_FX$Dates)
CHF_rf_rates$Dates <- as.Date(CHF_rf_rates$Dates)
swiss_inflation$Dates <- as.Date(paste(swiss_inflation$Dates, "-01-01", sep = ""), format = "%Y-%m-%d")

# Sort the data frames by the 'Dates' column in descending order (from most recent to older)
index_prices_local_currency <- index_prices_local_currency[order(index_prices_local_currency$Dates, decreasing = TRUE),]
CHF_FX <- CHF_FX[order(CHF_FX$Dates, decreasing = TRUE),]
swiss_inflation <- swiss_inflation[order(swiss_inflation$Dates, decreasing = TRUE),]
CHF_rf_rates <- CHF_rf_rates[order(CHF_rf_rates$Dates, decreasing = TRUE),]

# Change the column name for Swiss inflation (CPI in %) and convert the data to actual percentages
if (names(swiss_inflation)[2] == "Switzerland") {
  names(swiss_inflation)[2] <- "Swiss inflation (CPI)"
  swiss_inflation$"Swiss inflation (CPI)" <- as.numeric(swiss_inflation$"Swiss inflation (CPI)") / 100
}

# Convert "#N/A N/A" to NA (in character or factor columns only)
is_char_or_factor <- sapply(index_prices_local_currency, function(col) is.character(col) | is.factor(col))
index_prices_local_currency[is_char_or_factor] <- lapply(index_prices_local_currency[is_char_or_factor], function(col) {
  ifelse(col == "#N/A N/A", NA, col)
})
is_char_or_factor <- sapply(CHF_FX, function(col) is.character(col) | is.factor(col))
CHF_FX[is_char_or_factor] <- lapply(CHF_FX[is_char_or_factor], function(col) {
  ifelse(col == "#N/A N/A", NA, col)
})


# Determine indices that do not contain sufficiently long dated price data and are not essential to the investment universe
index_prices_local_currency_NA_dates <- determine_start_dates(index_prices_local_currency)
print(index_prices_local_currency_NA_dates)

# Remove indices (columns) that do not contain sufficiently long dated price data and are not essential to the investment universe
# --> removing mid cap equity indices (insufficiently long dated price data for Switzerland, remove the corresponding index for other geographies, and total stock market index already covers large- and mid-cap equity)
# --> removing large cap equity indices (total stock market index already covers large- and mid-cap equity)
# --> removing real estate index (insufficiently long dated price data)
# --> removing 2 duplicates of I08240CH
index_prices_local_currency <- index_prices_local_currency[, !(colnames(index_prices_local_currency) %in% c("MXCHMC Index", "MXEUMC Index", "MXUSMC Index", "MXEFMC Index", "MXWOMC Index", 
                                                                                                            "MXUSLC Index", "MXEULC Index", "MXEFLC Index", "MXCHLC Index", "MXWOLC Index", 
                                                                                                            "TENHGU Index", 
                                                                                                            "I08240 Index", "I08240EU Index"))]

# Determine currency pairs that do not contain sufficiently long dated price data and are not essential to the investment universe
CHF_FX_NA_dates <- determine_start_dates(CHF_FX)
print(CHF_FX_NA_dates)

# Remove currency pairs (columns) that do not contain sufficiently long dated price data and are not essential to the investment universe
# NOT APPLICABLE

# Provide better names to remaining columns of dataframes index_prices_local_currency and CHF_FX
index_prices_local_currency <- rename_columns(index_prices_local_currency)
CHF_FX <- rename_columns(CHF_FX)

# Transform non-NA values from character to numeric
index_prices_local_currency <- index_prices_local_currency %>%
  mutate_at(
    vars(-Dates),
    ~as.numeric(na_if(., ""))  # Convert non-empty strings to numeric
  )
CHF_FX <- CHF_FX %>%
  mutate_at(
    vars(-Dates),
    ~as.numeric(na_if(., ""))  # Convert non-empty strings to numeric
  )


# For each geography, combine the three intermediate-term treasuries (3-5Y, 5-7Y, 7-10Y) into one investable security that is weighted 1/3 in each
index_prices_local_currency <- index_prices_local_currency %>%
  mutate(`US IT treasuries (3-5Y, 5-7Y, 7-10Y)` = (`US IT treasuries 3-5Y` + `US IT treasuries 5-7Y` + `US IT treasuries 7-10Y`) / 3,
         `Europe IT treasuries (3-5Y, 5-7Y, 7-10Y)` = (`Europe IT treasuries 3-5Y` + `Europe IT treasuries 5-7Y` + `Europe IT treasuries 7-10Y`) / 3,
         `EM IT treasuries (3-5Y, 5-7Y, 7-10Y)` = (`EM IT treasuries 3-5Y` + `EM IT treasuries 5-7Y` + `EM IT treasuries 7-10Y`) / 3,
         `Switzerland IT treasuries (3-5Y, 5-7Y, 7-10Y)` = (`Switzerland IT treasuries 3-5Y` + `Switzerland IT treasuries 5-7Y` + `Switzerland IT treasuries 7-10Y`) / 3,
         `World IT treasuries (3-5Y, 5-7Y, 7-10Y)` = (`World IT treasuries 3-5Y` + `World IT treasuries 5-7Y` + `World IT treasuries 7-10Y`) / 3)

# Remove the original intermediate-term treasuries columns
index_prices_local_currency <- index_prices_local_currency %>%
  select(-ends_with("3-5Y"), -ends_with("5-7Y"), -ends_with("7-10Y"))

# Rearrange the indices (columns) to a more logical order
index_prices_local_currency <- rearrange_columns(index_prices_local_currency)

# Remove longer dated observations until each column contains values for each remaining date (row)
# i.e. Filter the data frames to include only rows starting from the latest start date
index_prices_local_currency <- filter(index_prices_local_currency, 
                                      Dates > max(determine_start_dates(index_prices_local_currency)))
CHF_FX <- filter(CHF_FX,
                 Dates >= max(determine_start_dates(index_prices_local_currency)))
CHF_rf_rates <- filter(CHF_rf_rates,
                       Dates >= max(determine_start_dates(index_prices_local_currency)))

# Generate dataframe containing index prices in CHF (calculated from index_prices_local_currency and CHF_FX)
# Initialize dataframe that will contain index prices in CHF
index_prices_CHF <- index_prices_local_currency
# Select the columns representing USD, EUR, and CHF denominated indexes
usd_indexes <- c("US", "US small cap", "Europe small cap", "EM", "EM small cap", "Switzerland small cap", "World", "World small cap", "US ST treasuries 1-3Y", "US IT treasuries (3-5Y, 5-7Y, 7-10Y)", "US LT treasuries 10Y+", "EM ST treasuries 1-3Y", "EM IT treasuries (3-5Y, 5-7Y, 7-10Y)", "EM LT treasuries 10Y+", "World ST treasuries 1-3Y", "World IT treasuries (3-5Y, 5-7Y, 7-10Y)", "World LT treasuries 10Y+", "Gold bullion")
eur_indexes <- c("Europe", "Europe ST treasuries 1-3Y", "Europe IT treasuries (3-5Y, 5-7Y, 7-10Y)", "Europe LT treasuries 10Y+")
chf_indexes <- c("Switzerland", "Switzerland ST treasuries 1-3Y", "Switzerland IT treasuries (3-5Y, 5-7Y, 7-10Y)", "Switzerland LT treasuries 10Y+")
# Multiply USD denominated columns by CHF/USD exchange rate
index_prices_CHF[, usd_indexes] <- index_prices_CHF[, usd_indexes] * CHF_FX[["CHF per USD"]]
# Multiply EUR denominated columns by CHF/EUR exchange rate
index_prices_CHF[, eur_indexes] <- index_prices_CHF[, eur_indexes] * CHF_FX[["CHF per EUR"]]

# Inspect dataframe index_prices_CHF
print(index_prices_CHF)
colnames(index_prices_CHF)
head(index_prices_CHF, 10)
tail(index_prices_CHF, 10)

# Generate dataframe containing daily price returns in CHF (calculated from index_prices_CHF)
# Initialize dataframe that will contain daily price returns in CHF
index_daily_returns_CHF <- data.frame(index_prices_CHF$Dates)
names(index_daily_returns_CHF)[1] <- "Dates"
# Calculate daily price returns for each index in CHF
num_rows <- nrow(index_prices_CHF)
for (col in colnames(index_prices_CHF)[-1]) {
  prices <- index_prices_CHF[[col]]
  returns <- (prices[1:(num_rows - 1)] / prices[2:num_rows]) - 1
  index_daily_returns_CHF[[col]] <- c(returns, NA)
}
index_daily_returns_CHF <- na.omit(index_daily_returns_CHF) # This removes the final row, which only contains returns of value NA


##############################################################################

# DATA PREPARATION
# Generating new columns from our dataframe "index_daily_returns_CHF" containing daily price returns in CHF
# ...

index_daily_returns_CHF <- index_daily_returns_CHF[, -1]

# Define a function to generate weighted columns
generate_weighted_cols <- function(index_daily_returns_CHF, max_comb_size) {
  # Get the number of columns in the dataframe
  num_cols <- ncol(index_daily_returns_CHF)
  
  # Iterate over i for i-combinations
  for (i in 2:min(num_cols, max_comb_size)) {
    # Generate all i-combinations of column indices
    combos <- combinat::combn(1:num_cols, i, simplify = FALSE)
    
    # Iterate over each combination
    for (combo in combos) {
      # Calculate the new column as the row-wise mean of the selected columns
      new_col <- rowMeans(index_daily_returns_CHF[, combo])
      
      # Create the new column name
      new_col_name <- paste(names(index_daily_returns_CHF)[combo], collapse = " ")
      
      # Add the new column to the dataframe
      index_daily_returns_CHF[[new_col_name]] <- new_col
    }
  }
  return(index_daily_returns_CHF)
}

# Use the function on your dataframe with max combination size 
start_time <- Sys.time()
index_daily_returns_CHF <- generate_weighted_cols(index_daily_returns_CHF, 3)
end_time <- Sys.time()
execution_time <- end_time - start_time
print(execution_time)



















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
