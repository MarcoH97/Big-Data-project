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
library(zoo)
library(gridExtra)
library(xts)


####
####
####
####
####

# Get the directory path of the current code file
PATH <- dirname(rstudioapi::getSourceEditorContext()$path)

# Set the working directory to that of the current code file 
setwd(PATH)

# Load functions file
source("BDA_BDBD_functions - Luca.R")


####
####
####
####
####

#### DATA COLLECTION: leveraging data from multiple sources ####
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

# Inspect the size of the downloaded data
print(paste("data_indices_full (downloaded):", round(object.size(data_indices_full) / 1048576, 2), "MB"))
print(paste("data_FX_full (downloaded):", round(object.size(data_FX_full) / 1048576, 2), "MB"))
print(paste("data_inflation_full (downloaded):", round(object.size(data_inflation_full) / 1048576, 2), "MB"))
print(paste("data_ST_rf_CHF (downloaded):", round(object.size(data_ST_rf_CHF) / 1048576, 2), "MB"))
print(paste("data_ST_rf_CHF (downloaded):", round(object.size(data_ST_rf_CHF) / 1048576, 2), "MB"))

# Remove from our R environment the variables that we no longer need
rm(data_indices_full, data_FX_full, data_inflation_full, data_inflation, data_ST_rf_CHF, data_LT_rf_CHF)

# Inspect the size of the raw data that we continue from
print(paste("index_prices_local_currency:", round(object.size(index_prices_local_currency) / 1048576, 2), "MB"))
print(paste("CHF_FX:", round(object.size(CHF_FX) / 1048576, 2), "MB"))
print(paste("CHF_rf_rates:", round(object.size(CHF_rf_rates) / 1048576, 2), "MB"))
print(paste("swiss_inflation:", round(object.size(swiss_inflation) / 1048576, 2), "MB"))


####
####
####
####
####

#### DATA CLEANING AND DATA INTEGRATION ####

# Inspect classes of the Dates columns of the different data frames 
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

# To prepare for upcoming analysis: sort data frames from oldest observations (at the top) to most recent observations (at the bottom)
index_prices_local_currency <- index_prices_local_currency %>% 
  arrange(Dates)
CHF_FX <- CHF_FX %>% 
  arrange(Dates)
swiss_inflation <- swiss_inflation %>% 
  arrange(Dates)
CHF_rf_rates <- CHF_rf_rates %>% 
  arrange(Dates)

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
  returns <- (prices[2:num_rows] / prices[1:(num_rows - 1)]) - 1
  index_daily_returns_CHF[[col]] <- c(NA, returns)
}

index_daily_returns_CHF <- na.omit(index_daily_returns_CHF) # This removes the first row, which only contains returns of value NA

# Inspect the size of the cleaned data that we continue from
print(paste("index_daily_returns_CHF:", round(object.size(index_daily_returns_CHF) / 1048576, 2), "MB"))
print(paste("CHF_FX:", round(object.size(CHF_FX) / 1048576, 2), "MB"))
print(paste("CHF_rf_rates:", round(object.size(CHF_rf_rates) / 1048576, 2), "MB"))
print(paste("swiss_inflation:", round(object.size(swiss_inflation) / 1048576, 2), "MB"))


####
####
####
####
####

#### DATA PREPARATION ####

# Feature engineering a large set of investment strategies (as separate columns). Strategies differ in: 
# (a) their strategic asset allocation, i.e. different (equally-weighted) combinations of the 26 index return series.
# (b) their periodic rebalancing technique: e.g., daily/monthly/quarterly/yearly)
# Notice that, as we increase the number of combinations that we implement, the number of additional columns increases exponentially.

# Generate new data frames with additional investment strategies of equally-weighted indices, from our daily index returns in CHF
strategies_max_2_comb_daily_rebal <- generate_weighted_cols_daily_rebal(index_daily_returns_CHF, 2)
# [1] "Execution time:  0.358608961105347 seconds"
strategies_max_3_comb_daily_rebal <- generate_weighted_cols_daily_rebal(index_daily_returns_CHF, 3)
# [1] "Execution time:  3.06886792182922 seconds"
strategies_max_4_comb_daily_rebal <- generate_weighted_cols_daily_rebal(index_daily_returns_CHF, 4)
# [1] "Execution time:  149.042740821838 seconds"
# strategies_max_5_comb_daily_rebal <- generate_weighted_cols_daily_rebal(index_daily_returns_CHF, 5)
# # [1] "Execution time:  ??????? seconds" (TOO LONG)


### Transform the dataframe from daily return series to xts aggregated time series that represent returns for yearly/quarterly/monthly/weekly rebalancing

# Assuming 'Dates' column is in the format of "yyyy-mm-dd"
index_daily_returns_CHF$Dates <- as.Date(index_daily_returns_CHF$Dates)

xts_index_daily_returns_CHF <- xts(index_daily_returns_CHF[-1], order.by=index_daily_returns_CHF$Dates)

# Define the function that applies the formula for compound returns 
compound_return <- function(x) {
  prod(1 + x) - 1
}

# Define the function that aggregates returns
compound_return_multi_col  <- function(x) {
  apply(x, 2, compound_return)
}

### Generate new xts objects with different investment strategies of equally-weighted indices, using our weekly/monthly/quarterly/yearly index returns in CHF
# Create aggregated time series that represent returns for yearly/quarterly/monthly/weekly rebalancing 
xts_index_yearly_returns_CHF <- apply.yearly(xts_index_daily_returns_CHF, compound_return_multi_col)
xts_index_quarterly_returns_CHF <- apply.quarterly(xts_index_daily_returns_CHF, compound_return_multi_col)
xts_index_monthly_returns_CHF <- apply.monthly(xts_index_daily_returns_CHF, compound_return_multi_col)
xts_index_weekly_returns_CHF <- apply.weekly(xts_index_daily_returns_CHF, compound_return_multi_col)


## Max 2 combinations: yearly/quarterly/monthly/weekly/daily rebalancing
xts_strategies_max_2_comb_yearly_rebal <- xts_generate_weighted_cols(xts_return_series = xts_index_yearly_returns_CHF,
                                                                     max_comb_size = 2)
# [1] "Execution time:  1.55694222450256 seconds"

xts_strategies_max_2_comb_quarterly_rebal <- xts_generate_weighted_cols(xts_return_series = xts_index_quarterly_returns_CHF,
                                                                        max_comb_size = 2)
# [1] "Execution time:  1.8990490436554 seconds"

xts_strategies_max_2_comb_monthly_rebal <- xts_generate_weighted_cols(xts_return_series = xts_index_monthly_returns_CHF,
                                                                      max_comb_size = 2)
# [1] "Execution time:  1.95903587341309 seconds"

xts_strategies_max_2_comb_weekly_rebal <- xts_generate_weighted_cols(xts_return_series = xts_index_weekly_returns_CHF,
                                                                     max_comb_size = 2)
# [1] "Execution time:  3.07245182991028 seconds"

xts_strategies_max_2_comb_daily_rebal <- xts_generate_weighted_cols(xts_return_series = xts_index_daily_returns_CHF,
                                                                    max_comb_size = 2)
# [1] "Execution time:  6.8100950717926 seconds"


## Max 3 combinations: yearly/quarterly/monthly/weekly/daily rebalancing
xts_strategies_max_3_comb_yearly_rebal <- xts_generate_weighted_cols(xts_return_series = xts_index_yearly_returns_CHF,
                                                                     max_comb_size = 3)
# [1] "Execution time:  161.960761070251 seconds"

xts_strategies_max_3_comb_quarterly_rebal <- xts_generate_weighted_cols(xts_return_series = xts_index_quarterly_returns_CHF,
                                                                        max_comb_size = 3)
# [1] "Execution time:  166.492907047272 seconds"

xts_strategies_max_3_comb_monthly_rebal <- xts_generate_weighted_cols(xts_return_series = xts_index_monthly_returns_CHF,
                                                                     max_comb_size = 3)
# [1] "Execution time:  178.4279088974 seconds"

xts_strategies_max_3_comb_weekly_rebal <- xts_generate_weighted_cols(xts_return_series = xts_index_weekly_returns_CHF,
                                                                     max_comb_size = 3)
# [1] "Execution time:  261.833109855652 seconds"

xts_strategies_max_3_comb_daily_rebal <- xts_generate_weighted_cols(xts_return_series = xts_index_daily_returns_CHF,
                                                                     max_comb_size = 3)
# [1] "Execution time:  757.605960130692 seconds"


## Max 4 combinations: yearly/quarterly/monthly/weekly/daily rebalancing
# xts_strategies_max_4_comb_yearly_rebal <- xts_generate_weighted_cols(xts_return_series = xts_index_yearly_returns_CHF,
#                                                                      max_comb_size = 4)
# # [1] "Execution time:  ??????? seconds" (TOO LONG)
# 
# xts_strategies_max_4_comb_quarterly_rebal <- xts_generate_weighted_cols(xts_return_series = xts_index_quarterly_returns_CHF,
#                                                                         max_comb_size = 4)
# # [1] "Execution time:  ??????? seconds" (TOO LONG)
# 
# xts_strategies_max_4_comb_yearly_rebal <- xts_generate_weighted_cols(xts_return_series = xts_index_yearly_returns_CHF,
#                                                                      max_comb_size = 4)
# # [1] "Execution time:  ??????? seconds" (TOO LONG)
# 
# xts_strategies_max_4_comb_yearly_rebal <- xts_generate_weighted_cols(xts_return_series = xts_index_yearly_returns_CHF,
#                                                                      max_comb_size = 4)
# # [1] "Execution time:  ??????? seconds" (TOO LONG)
# 
# xts_strategies_max_4_comb_yearly_rebal <- xts_generate_weighted_cols(xts_return_series = xts_index_yearly_returns_CHF,
#                                                                      max_comb_size = 4)
# # [1] "Execution time:  ??????? seconds" (TOO LONG)



## Max 5 combinations: yearly/quarterly/monthly/weekly/daily rebalancing
# xts_strategies_max_5_comb_yearly_rebal <- xts_generate_weighted_cols(xts_return_series = xts_index_yearly_returns_CHF,
#                                                                      max_comb_size = 5)
# # [1] "Execution time:  ??????? seconds" (TOO LONG)
# 
# xts_strategies_max_5_comb_quarterly_rebal <- xts_generate_weighted_cols(xts_return_series = xts_index_quarterly_returns_CHF,
#                                                                         max_comb_size = 5)
# # [1] "Execution time:  ??????? seconds" (TOO LONG)
# 
# xts_strategies_max_5_comb_yearly_rebal <- xts_generate_weighted_cols(xts_return_series = xts_index_yearly_returns_CHF,
#                                                                      max_comb_size = 5)
# # [1] "Execution time:  ??????? seconds" (TOO LONG)
# 
# xts_strategies_max_5_comb_yearly_rebal <- xts_generate_weighted_cols(xts_return_series = xts_index_yearly_returns_CHF,
#                                                                      max_comb_size = 5)
# # [1] "Execution time:  ??????? seconds" (TOO LONG)
# 
# xts_strategies_max_5_comb_yearly_rebal <- xts_generate_weighted_cols(xts_return_series = xts_index_yearly_returns_CHF,
#                                                                      max_comb_size = 5)
# # [1] "Execution time:  ??????? seconds" (TOO LONG)



# ------------------------------------------------------------------------------------------------ #


# strategies_max_5_comb_daily_rebal <- generate_weighted_cols_daily_rebal(index_daily_returns_CHF, 5)
# strategies_max_6_comb_daily_rebal <- generate_weighted_cols_daily_rebal(index_daily_returns_CHF, 6)

# ------------------------------------------------------------------------------------------------ #

# Inspect the size of the generated dataframes that contain the different investment strategies
print(paste("strategies_max_2_comb_daily_rebal (generated):", round(object.size(strategies_max_2_comb_daily_rebal) / 1048576, 2), "MB"))
# [1] "strategies_max_2_comb_daily_rebal (generated): 13.64 MB"
print(paste("strategies_max_3_comb_daily_rebal (generated):", round(object.size(strategies_max_3_comb_daily_rebal) / 1048576, 2), "MB"))
# [1] "strategies_max_3_comb_daily_rebal (generated): 114.36 MB"
print(paste("strategies_max_4_comb_daily_rebal (generated):", round(object.size(strategies_max_4_comb_daily_rebal) / 1048576, 2), "MB"))
# [1] "strategies_max_4_comb_daily_rebal (generated): 693.83 MB"

# Inspect the size of the generated xts objects that contain the different investment strategies
print(paste("xts_strategies_max_2_comb_yearly_rebal (generated):", round(object.size(xts_strategies_max_2_comb_yearly_rebal) / 1048576, 2), "MB"))
# [1] "xts_strategies_max_2_comb_yearly_rebal (generated): 0.09 MB"
print(paste("xts_strategies_max_2_comb_quarterly_rebal (generated):", round(object.size(xts_strategies_max_2_comb_quarterly_rebal) / 1048576, 2), "MB"))
# [1] "xts_strategies_max_2_comb_quarterly_rebal (generated): 0.25 MB"
print(paste("xts_strategies_max_2_comb_monthly_rebal (generated):", round(object.size(xts_strategies_max_2_comb_monthly_rebal) / 1048576, 2), "MB"))
# [1] "xts_strategies_max_2_comb_monthly_rebal (generated): 0.66 MB"
print(paste("xts_strategies_max_2_comb_weekly_rebal (generated):", round(object.size(xts_strategies_max_2_comb_weekly_rebal) / 1048576, 2), "MB"))
# [1] "xts_strategies_max_2_comb_weekly_rebal (generated): 2.75 MB"
print(paste("xts_strategies_max_2_comb_daily_rebal (generated):", round(object.size(xts_strategies_max_2_comb_daily_rebal) / 1048576, 2), "MB"))
# [1] "xts_strategies_max_2_comb_daily_rebal (generated): 13.6 MB"

print(paste("xts_strategies_max_3_comb_yearly_rebal (generated):", round(object.size(xts_strategies_max_3_comb_yearly_rebal) / 1048576, 2), "MB"))
# [1] "xts_strategies_max_3_comb_yearly_rebal (generated): 0.88 MB"
print(paste("xts_strategies_max_3_comb_quarterly_rebal (generated):", round(object.size(xts_strategies_max_3_comb_quarterly_rebal) / 1048576, 2), "MB"))
# [1] "xts_strategies_max_3_comb_quarterly_rebal (generated): 2.18 MB"
print(paste("xts_strategies_max_3_comb_monthly_rebal (generated):", round(object.size(xts_strategies_max_3_comb_monthly_rebal) / 1048576, 2), "MB"))
# [1] "xts_strategies_max_3_comb_monthly_rebal (generated): 5.67 MB"
print(paste("xts_strategies_max_3_comb_weekly_rebal (generated):", round(object.size(xts_strategies_max_3_comb_weekly_rebal) / 1048576, 2), "MB"))
# [1] "xts_strategies_max_3_comb_weekly_rebal (generated): 23.2 MB"
print(paste("xts_strategies_max_3_comb_daily_rebal (generated):", round(object.size(xts_strategies_max_3_comb_daily_rebal) / 1048576, 2), "MB"))
# [1] "xts_strategies_max_3_comb_daily_rebal (generated): 114.18 MB"


####
####
####
####
####

#### DATA ANALYSIS AND VISUALIZATION ####

# (PART 1) A small initial analysis: visualizing correlations between daily returns of the different indices. 

# Plot the correlation matrix between returns of the initial 26 indices
plot_correlation_matrix(index_daily_returns_CHF)


# (PART 2) A larger, still simple analysis: calculating and visualizing mean returns and standard deviations for each of the candidate investment strategies. 

# Plot the mean-variance graph for daily returns of each investment strategy in the data frame (first column is "Dates")
# First for only the 26 initial variables, than for all combinations up to 2, than for all higher number of combinations.
plot_mean_variance_graph(index_daily_returns_CHF)
plot_mean_variance_graph(strategies_max_2_comb_daily_rebal)
plot_mean_variance_graph(strategies_max_3_comb_daily_rebal)
plot_mean_variance_graph(strategies_max_4_comb_daily_rebal)

# Notice how the linear regressions nicely show how, on average, diversification is granting you more bang (return) for your buck (risk)


# ------------------------------------------------------------------------------------------------------#

### SAVE CURRENT WORKSPACE / LOAD CURRENT WORKSPACE ###

# Save all objects in the workspace to a file named 'my_workspace.RData'
# save.image('all_data_max_4_comb.RData')

# Get the directory path of the current code file
PATH <- dirname(rstudioapi::getSourceEditorContext()$path)
# Set the working directory to that of the current code file 
setwd(PATH)
# Load the objects from 'my_workspace.RData' into the workspace
# It's a good practice to start a new session or clear the workspace before loading the saved objects.
load('all_data_max_4_comb.RData')
# Load functions file
source("BDA_BDBD_functions - Luca.R")


# ------------------------------------------------------------------------------------------------------ #


# (PART 3) The most challenging analysis:
# (PART 3A) Based on historical data, determine the optimal investment strategy that corresponds exactly to a given user-specified set of investment parameters 


# ---------------------------------------------------------------------------- #

# Call the function determine_optimal_strategy_v1 with the specified input parameters
# Remark that it returns 7 objects: list(plot_lowest_cum_returns, optimal_strategy, plot_optimal_strategy, df_above_threshold, df_refused, plot_list_different_periods_within_strategies, total_function_time)

# Call the function determine_optimal_strategy_v1 with specified input parameters
your_candidate_strategies_results_v1 <- determine_optimal_strategy_v1(df_return_series = index_daily_returns_CHF, 
                                                                      time_horizon_years = 10, 
                                                                      minimum_allowable_percentage = 0.85)
# [1] "Execution time for FUNCTION determine_optimal_strategy_v1 and DATASET index_daily_returns_CHF : 8.52 seconds"

# Extract the results from the function output into individual variables
your_plot_lowest_returns_for_each_strategy_v1 <- your_candidate_strategies_results_v1[[1]]
your_optimal_strategy_v1 <- your_candidate_strategies_results_v1[[2]]
your_plot_optimal_strategy_v1 <- your_candidate_strategies_results_v1[[3]]
your_strategies_above_threshold_v1 <- your_candidate_strategies_results_v1[[4]]
your_strategies_below_threshold_v1 <- your_candidate_strategies_results_v1[[5]]
your_plots_for_each_strategy_v1 <- your_candidate_strategies_results_v1[[6]]
your_total_function_time_v1 <- your_candidate_strategies_results_v1[[7]]

# your_plot_lowest_returns_for_each_strategy_v1
# Plot the intermediate evolution of the lowest cumulative return series for each investment strategy (different colors for non-refused "group 1 strategies", and refused "group 2 strategies")
print(paste("Plot of the intermediate evolution of the lowest cumulative return series for each investment strategy: (see plot)"))
print(your_plot_lowest_returns_for_each_strategy_v1)

# your_optimal_strategy_v1
# Print the optimal strategy (the one strategy that delivers the best "lowest cumulative return" out of all strategies that always stayed above the threshold)
print(paste("The optimal strategy (delivers the best 'lowest cumulative return' out of all strategies that always stayed above the threshold):"))
print(your_optimal_strategy_v1)

# plot_optimal_strategy_v1
# Plot the optimal strategy's worst-case, best-case and other cumulative return series 
print(paste("the optimal strategy's worst-case, best-case and other cumulative return series: (see plot)"))
print(your_plot_optimal_strategy_v1)

# your_strategies_above_threshold_v1
# Print strategies that stayed above the threshold
print(paste("Strategies that stayed above the threshold:", nrow(your_strategies_above_threshold_v1)))
print(your_strategies_above_threshold_v1)

# your_strategies_below_threshold_v1
# Print strategies that are refused for having decreased below the threshold
print(paste("Refused strategies (decreased below the threshold):", nrow(your_strategies_below_threshold_v1)))
print(your_strategies_below_threshold_v1)

# your_plots_for_each_strategy_v1
# (1) To access a specific plot from the plot list your_plots_for_each_strategy, you would index it using the strategy name as follows:
# In the place of "Strategy Name", use the exact name of the strategy you're interested in, like "US", "Europe", "World", etc.
specific_plot_v1 = your_plots_for_each_strategy_v1[["US"]]
print(specific_plot_v1)

# (2) If you would like to display multiple plots together, you can use the gridExtra package. 
# You need to specify the plots you want to display as follows:
grid.arrange(
  your_plots_for_each_strategy_v1[[your_optimal_strategy_v1]],
  your_plots_for_each_strategy_v1[["Europe"]],
  ncol = 1  # Or any other number of columns you want
)
# Notice that, for a given investment strategy, the algorithm analyzes all relevant time periods. 
# We display only a subset of this, for illustration purposes.

# your_total_function_time
# Print the total execution time of the function 
print(paste("Execution time for TOTAL FUNCTION: ", round(your_total_function_time_v1, 2), "seconds"))


# ---------------------------------------------------------------------------- #

# Let's again call the function determine_optimal_strategy_v1, but compare outcomes and processing times 
# between different strategies, for various time horizons and minimum allowable percentages...
# Remark that the strong increase in computational time at a certain point could be a consequence of overusing the processing power, 
# rather than from the algorithmic logic itself.

# Define the range of years and minimum allowable percentage to compare different strategies over different time horizons and thresholds
# your_time_horizon_years <- seq(2, 12, 2)
# your_minimum_allowable_percentage <- seq(0.50, 0.90, 0.00)
your_time_horizon_years <- seq(4, 12, 4) # shorter runtime than e.g., seq(2, 12, 2)
your_minimum_allowable_percentage <- seq(0.60, 0.80, 0.20) # shorter runtime than e.g., seq(0.50, 0.90, 0.10)

# Call the function compare_results with specified input parameters
# Remark that it returns 2 objects: list(df_comparison, results_compared)
your_results_v1 <- compare_results_v1(df_return_series = index_daily_returns_CHF, 
                                      time_horizon_years = your_time_horizon_years, 
                                      minimum_allowable_percentage = your_minimum_allowable_percentage,
                                      optimization_function = determine_optimal_strategy_v1)
df_comparison_v1 <- your_results_v1[[1]]
results_compared_v1 <- your_results_v1[[2]]

# Compare optimal strategies and total function times for different combinations of years and minimum values
print(df_comparison_v1)
#   Years MinVal                      OptimalStrategy TotalFunctionTime
# 1     4    0.6       Switzerland ST treasuries 1-3Y          5.587855
# 2     4    0.8       Switzerland ST treasuries 1-3Y          5.584006
# 3     8    0.6 EM IT treasuries (3-5Y, 5-7Y, 7-10Y)          6.210600
# 4     8    0.8       Switzerland ST treasuries 1-3Y          5.845380
# 5    12    0.6                EM LT treasuries 10Y+          7.221342
# 6    12    0.8       Switzerland LT treasuries 10Y+          7.020189
# Notice that optimizing for longer time horizons seems to cause longer computing times.
# Notice that optimizing for different minimum portfolio values seems to be uncorrelated with computing times.

# Access the results for a specific combination of years and minimum_value 
# (e.g.,: 8 years; 0.50 minimum_value)
years <- 8
minimum_value <- 0.60
result_v1 <- results_compared_v1[[paste0("years_", years, "_min_", minimum_value)]]

# Extract and display the values from the result_v1

# your_plot_lowest_returns_for_each_strategy
# Plot the intermediate evolution of the lowest cumulative return series for each investment strategy (different colors for non-refused "group 1 strategies", and refused "group 2 strategies")
print(result_v1$PlotLowestReturns)

# your_optimal_strategy
# Print the optimal strategy (the one strategy that delivers the best "lowest cumulative return" out of all strategies that always stayed above the threshold)
print(paste("The optimal strategy (delivers the best 'lowest cumulative return' out of all strategies that always stayed above the threshold):"))
print(result_v1$OptimalStrategy)

# plot_optimal_strategy
# Plot the optimal strategy's worst-case, best-case and other cumulative return series 
print(result_v1$PlotOptimalStrategy)

# your_strategies_above_threshold_v1
# Print strategies that stayed above the threshold
print(paste("Strategies that stayed above the threshold:", nrow(result_v1$StrategiesAboveThreshold)))
print(result_v1$StrategiesAboveThreshold)

# your_strategies_below_threshold_v1
# Print strategies that are refused for having decreased below the threshold
print(paste("Refused strategies (decreased below the threshold):", nrow(result_v1$StrategiesBelowThreshold)))
print(result_v1$StrategiesBelowThreshold)

# your_plots_for_each_strategy
# (1) To access a specific plot from the plot list your_plots_for_each_strategy, you would index it using the strategy name as follows:
# In the place of "Strategy Name", use the exact name of the strategy you're interested in, like "US", "Europe", "World", etc.
specific_plot = result_v1$PlotsForEachStrategy[["US"]]
print(specific_plot)

# (2) If you would like to display multiple plots together, you can use the gridExtra package. 
# You need to specify the plots you want to display as follows:
grid.arrange(
  result_v1$PlotsForEachStrategy[[result_v1$OptimalStrategy]],
  result_v1$PlotsForEachStrategy[["Europe"]],
  ncol = 1  # Or any other number of columns you want
)
# Notice that, for a given investment strategy, the algorithm analyzes all relevant time periods. 
# We display only a subset of this, for illustration purposes.

# your_total_function_time
# Print the total execution time of the function 
print(paste("Execution time for TOTAL FUNCTION: ", round(result_v1$TotalFunctionTime, 2), "seconds"))


# ---------------------------------------------------------------------------- #


# Function "determine_optimal_strategy_v2" aims to improve upon the performance of "determine_optimal_strategy_v1". 
# Here are some potential improvements that we seek to implement sequentially:
# 1. Profiling 
# 2. Vectorized operations (and family functions?) instead of loops
# 3. Reduce number of operations 
# 4. Memory pre-allocation 
# 5. Parallel computation 
# 6. Memory efficiency (Use more efficient data structures) 
# 7. Use optimized libraries 
# 8. Memoization/Caching
# 9. Machine learning techniques (possibly heuristics)
# 10. External computation

# 2. AGAIN Vectorized operations (and family functions?) instead of loops



# 1. Profiling:

# Install profvis if not already installed
if (!"profvis" %in% installed.packages()) {
  install.packages("profvis")
}
# Load the profvis library
library(profvis)

# Now we can run our function inside the profvis function to generate a profile
profvis({
  determine_optimal_strategy_v1_timers(df_return_series = index_daily_returns_CHF, 
                                       time_horizon_years = 12, 
                                       minimum_allowable_percentage = 0.80)
})
# [1] "Execution time for FUNCTION determine_optimal_strategy_v1_timers and DATASET index_daily_returns_CHF : 8.85 seconds"

# Run our function inside the profvis function to generate a profile, this time on a larger dataset
profvis({
  determine_optimal_strategy_v1(df_return_series = strategies_max_2_comb_daily_rebal, 
                                time_horizon_years = 5, 
                                minimum_allowable_percentage = 0.75)
})
# [1] "Execution time for FUNCTION determine_optimal_strategy_v1 and DATASET strategies_max_2_comb_daily_rebal : 85.37 seconds"


# 2. Vectorized operations (and family functions?) instead of loops

# Define the range of years and minimum allowable percentage to compare different strategies over different time horizons and thresholds
your_time_horizon_years <- seq(4, 12, 4)
your_minimum_allowable_percentage <- seq(0.60, 0.80, 0.20)

# Call the function compare_results with specified input parameters, for function determine_optimal_strategy_v1_timers
your_results_v1_timers <- compare_results_v1(df_return_series = index_daily_returns_CHF,
                                             time_horizon_years = your_time_horizon_years,
                                             minimum_allowable_percentage = your_minimum_allowable_percentage,
                                             optimization_function = determine_optimal_strategy_v1_timers)
df_comparison_v1_timers <- your_results_v1_timers[[1]]
results_compared_v1_timers <- your_results_v1_timers[[2]]

# Compare optimal strategies and total function times for function determine_optimal_strategy_v1_timers
print(df_comparison_v1_timers)
# RUN 1 (memory free)...
# Years MinVal                      OptimalStrategy TotalFunctionTime
# 1     4    0.6       Switzerland ST treasuries 1-3Y          5.329378
# 2     4    0.8       Switzerland ST treasuries 1-3Y          4.892113
# 3     8    0.6 EM IT treasuries (3-5Y, 5-7Y, 7-10Y)          6.269587
# 4     8    0.8       Switzerland ST treasuries 1-3Y          5.713225
# 5    12    0.6                EM LT treasuries 10Y+          9.074858
# 6    12    0.8       Switzerland LT treasuries 10Y+          9.461457
# RUN 3 (memory less free)...
# Years MinVal                      OptimalStrategy TotalFunctionTime
# 1     4    0.6       Switzerland ST treasuries 1-3Y          6.664254
# 2     4    0.8       Switzerland ST treasuries 1-3Y          7.099568
# 3     8    0.6 EM IT treasuries (3-5Y, 5-7Y, 7-10Y)          7.086873
# 4     8    0.8       Switzerland ST treasuries 1-3Y          7.505601
# 5    12    0.6                EM LT treasuries 10Y+         14.713905
# 6    12    0.8       Switzerland LT treasuries 10Y+         13.476833


# Call the function compare_results with specified input parameters, for function determine_optimal_strategy_vectorization_timers
your_results_vectorization_timers <- compare_results_v1(df_return_series = index_daily_returns_CHF, 
                                                        time_horizon_years = your_time_horizon_years, 
                                                        minimum_allowable_percentage = your_minimum_allowable_percentage,
                                                        optimization_function = determine_optimal_strategy_vectorization_timers)
df_comparison_vectorization_timers <- your_results_vectorization_timers[[1]]
results_compared_vectorization_timers <- your_results_vectorization_timers[[2]]

# Compare optimal strategies and total function times for function determine_optimal_strategy_vectorization_timers
print(df_comparison_vectorization_timers)
# RUN 2 (memory less free)...
# Years MinVal                      OptimalStrategy TotalFunctionTime
# 1     4    0.6       Switzerland ST treasuries 1-3Y         10.876775
# 2     4    0.8       Switzerland ST treasuries 1-3Y          9.719702
# 3     8    0.6 EM IT treasuries (3-5Y, 5-7Y, 7-10Y)          9.054941
# 4     8    0.8       Switzerland ST treasuries 1-3Y          9.180789
# 5    12    0.6                EM LT treasuries 10Y+         10.770448
# 6    12    0.8       Switzerland LT treasuries 10Y+         12.855706
# RUN 4 (memory less free)...
# Years MinVal                      OptimalStrategy TotalFunctionTime
# 1     4    0.6       Switzerland ST treasuries 1-3Y         12.113778
# 2     4    0.8       Switzerland ST treasuries 1-3Y         10.926505
# 3     8    0.6 EM IT treasuries (3-5Y, 5-7Y, 7-10Y)          9.380281
# 4     8    0.8       Switzerland ST treasuries 1-3Y          9.713642
# 5    12    0.6                EM LT treasuries 10Y+         14.149580
# 6    12    0.8       Switzerland LT treasuries 10Y+         16.598828

# Let's compare the computation time for a larger dataset (strategies_max_2_comb_daily_rebal instead of index_daily_returns_CHF)
# Define the time horizon and minimum allowable percentage 
your_time_horizon_years <- seq(10, 10, 0) 
your_minimum_allowable_percentage <- seq(0.75, 0.75, 0) 

# Call the function compare_results with specified input parameters, for function determine_optimal_strategy_v1_timers
your_results_v1_timers <- compare_results_v1(df_return_series = strategies_max_2_comb_daily_rebal, 
                                             time_horizon_years = your_time_horizon_years, 
                                             minimum_allowable_percentage = your_minimum_allowable_percentage,
                                             optimization_function = determine_optimal_strategy_v1_timers)
df_comparison_v1_timers <- your_results_v1_timers[[1]]
results_compared_v1_timers <- your_results_v1_timers[[2]]

# Compare optimal strategies and total function times for function determine_optimal_strategy_v1_timers
print(df_comparison_v1_timers)
# RUN 1 (memory free)...
# Years MinVal            OptimalStrategy TotalFunctionTime
# 1    10   0.75 Switzerland | Gold bullion          80.59188


# Call the function compare_results with specified input parameters, for function determine_optimal_strategy_vectorization_timers
your_results_vectorization_timers <- compare_results_v1(df_return_series = strategies_max_2_comb_daily_rebal, 
                                                        time_horizon_years = your_time_horizon_years,
                                                        minimum_allowable_percentage = your_minimum_allowable_percentage,
                                                        optimization_function = determine_optimal_strategy_vectorization_timers)
df_comparison_vectorization_timers <- your_results_vectorization_timers[[1]]
results_compared_vectorization_timers <- your_results_vectorization_timers[[2]]

# Compare optimal strategies and total function times for function determine_optimal_strategy_vectorization_timers
print(df_comparison_vectorization_timers)
# Years MinVal            OptimalStrategy TotalFunctionTime
# 1    10   0.75 Switzerland | Gold bullion          158.5932


#' CONCLUSION for vectorization and family functions:
#' Our trials with different vectorization algorithms and family functions led to even longer running times than for our initial (v1) function
#' The increase in computation time is due to the nature of the operations performed and the data structure manipulations involved:
#' (i) Use of rolling window calculations: 
#' In your function, you're applying the zoo::rollapply function to each of your series. 
#' This involves moving a window of time_horizon_days through the data and recalculating the product of the returns and finding the minimum in each window. 
#' This operation is time consuming and complexity increases with increasing time_horizon_days.
#' (ii) Looping through columns: 
#' While lapply is generally faster than a traditional for loop in R, this speed benefit may not be evident 
#' when the function applied to each element of the list (in this case, each column of your dataframe) involves intensive computation, 
#' such as the rolling window operation you're performing.
#' (iii) Data frame manipulation: 
#' The repeated binding and data frame transformations (i.e., converting the list results into a data frame using do.call(rbind, ...)) 
#' may also contribute to the increased computation time.

#' To make the function faster, you might consider parallelizing the computations, optimizing the logic, 
#' or using more efficient data structures or libraries designed for fast computations, such as data.table in R, if it is applicable to your use case.


# 3. Reduce number of operations 

#' Implementations to more find (or estimate) the optimal strategy from a set of possible strategies in a more computationally efficient manner:
#' (a) in comparing the different time periods for a certain investment strategy, we might not have to compare between every possible starting date,
#' but we could compare different starting dates that are e.g., 1 week, 1 month, 1 quarter, 0.5 year, 1 year apart...
#' this would require respectively 5 / 21 / 63 / 126 / 252 times less computations.
#' Depending on the volatility of the underlying data and the chosen strategy, this might be an acceptable compromise.
#' (b) skip (i.e. break out of the loop for) an investment strategy if it can no longer be the optimal strategy, 
#' when it has at least one relevant time period during which it drops below the minimum acceptable cumulative return
#' (c) generate desired plots separately from the process of finding the optimal investment strategy
#' (d) transform the dataset from daily return series to weekly/monthly/quarterly/bi-yearly/yearly return series, which would imply weekly/monthly/quarterly/bi-yearly/yearly rebalancing and less computations
#' 

### Testing our simplified function to determine the optimal investment strategy based on historical data...

# df_return_series <- index_daily_returns_CHF
# df_return_series <- strategies_max_2_comb_daily_rebal
# df_return_series <- strategies_max_3_comb_daily_rebal
# df_return_series <- strategies_max_4_comb_daily_rebal
time_horizon_years <- 12
minimum_allowable_percentage <- 0.75
granularity = "yearly"

results_simplified_optimal_strategy <- determine_optimal_strategy_simplified(df_return_series = index_daily_returns_CHF, 
                                                                             time_horizon_years, 
                                                                             minimum_allowable_percentage, 
                                                                             granularity)
#' [1] "Execution time for FUNCTION determine_optimal_strategy_simplified, DATASET index_daily_returns_CHF , GRANULARITY yearly : 
#' 0.04 seconds"
#' [1] "Execution time for FUNCTION determine_optimal_strategy_simplified, DATASET strategies_max_2_comb_daily_rebal , GRANULARITY yearly : 
#' 0.47 seconds"
#' [1] "Execution time for FUNCTION determine_optimal_strategy_simplified, DATASET strategies_max_3_comb_daily_rebal , GRANULARITY yearly : 
#' 7.04 seconds"
#' [1] "Execution time for FUNCTION determine_optimal_strategy_simplified, DATASET strategies_max_4_comb_daily_rebal , GRANULARITY yearly : 
#' 189.6 seconds"


print(results_simplified_optimal_strategy[[1]])
print(paste("Execution time for TOTAL FUNCTION: ", round(results_simplified_optimal_strategy[[4]], 2), "seconds"))
head(results_simplified_optimal_strategy[[2]], 3)
tail(results_simplified_optimal_strategy[[2]], 3)
head(results_simplified_optimal_strategy[[3]], 3)
tail(results_simplified_optimal_strategy[[3]], 3)


### Testing our first advanced function (A) to determine the optimal investment strategy based on historical data...

# df_return_series <- index_daily_returns_CHF
# df_return_series <- strategies_max_2_comb_daily_rebal
# df_return_series <- strategies_max_3_comb_daily_rebal
# df_return_series <- strategies_max_4_comb_daily_rebal
time_horizon_years <- 12
minimum_allowable_percentage <- 0.75
cutoff_percentile = 0.50

results_advanced_optimal_strategy_A <- determine_optimal_strategy_advanced_A(df_return_series = index_daily_returns_CHF, 
                                                                             time_horizon_years,
                                                                             minimum_allowable_percentage,
                                                                             cutoff_percentile = 0.50)
#' time_horizon_years <- 12
#' minimum_allowable_percentage <- 0.75
#' X_percentile = 0.50
#' [1] "Execution time for FUNCTION determine_optimal_strategy_advanced_A and DATASET index_daily_returns_CHF : 
#' 0.14 seconds"
#' [1] "Execution time for FUNCTION determine_optimal_strategy_advanced_A and DATASET strategies_max_2_comb_daily_rebal : 
#' 2.16 seconds"
#' [1] "Execution time for FUNCTION determine_optimal_strategy_advanced_A and DATASET strategies_max_3_comb_daily_rebal : 
#' 45.28 seconds"

#' time_horizon_years <- 12
#' minimum_allowable_percentage <- 0.75
#' cutoff_percentile = 0.25
#' [1] "Execution time for FUNCTION determine_optimal_strategy_advanced_A and DATASET strategies_max_3_comb_daily_rebal : 
#' 14.08 seconds"
#' [1] "Execution time for FUNCTION determine_optimal_strategy_advanced_A and DATASET strategies_max_4_comb_daily_rebal : 
#' 278.19 seconds" --> 216 seconds yearly; 24.3 seconds bi-yearly; 9.36 seconds quarterly; 3.99 seconds monthly; 7.42 seconds weekly; 17.11 seconds daily

print(results_advanced_optimal_strategy_A[[1]])
print(paste("Execution time for TOTAL FUNCTION: ", round(results_advanced_optimal_strategy_A[[4]], 2), "seconds"))
head(results_advanced_optimal_strategy_A[[2]], 3)
tail(results_advanced_optimal_strategy_A[[2]], 3)
head(results_advanced_optimal_strategy_A[[3]], 3)
tail(results_advanced_optimal_strategy_A[[3]], 3)


### Testing our second advanced function (B) to determine the optimal investment strategy based on historical data...

# df_return_series <- index_daily_returns_CHF
# df_return_series <- strategies_max_2_comb_daily_rebal
# df_return_series <- strategies_max_3_comb_daily_rebal
# df_return_series <- strategies_max_4_comb_daily_rebal
time_horizon_years <- 12
minimum_allowable_percentage <- 0.75
cutoff_percentile = 0.50

results_advanced_optimal_strategy_B <- determine_optimal_strategy_advanced_B(df_return_series = index_daily_returns_CHF, 
                                                                             time_horizon_years, 
                                                                             minimum_allowable_percentage, 
                                                                             cutoff_percentile = 0.50)
#' time_horizon_years <- 12
#' minimum_allowable_percentage <- 0.75
#' cutoff_percentile = 0.50
#' [1] "Execution time for FUNCTION determine_optimal_strategy_advanced_B and DATASET index_daily_returns_CHF : 
#' 0.23 seconds"
#' [1] "Execution time for FUNCTION determine_optimal_strategy_advanced_B and DATASET strategies_max_2_comb_daily_rebal : 
#' 3.83 seconds"
#' [1] "Execution time for FUNCTION determine_optimal_strategy_advanced_B and DATASET strategies_max_2_comb_daily_rebal : 
#' 83.58 seconds"

#' #' time_horizon_years <- 12
#' minimum_allowable_percentage <- 0.75
#' cutoff_percentile = 0.25
#' 
#' [1] "Execution time for FUNCTION determine_optimal_strategy_advanced_B and DATASET strategies_max_3_comb_daily_rebal : 
#' 10.73 seconds"


print(results_advanced_optimal_strategy_B[[1]])
print(paste("Execution time for TOTAL FUNCTION: ", round(results_advanced_optimal_strategy_B[[4]], 2), "seconds"))
head(results_advanced_optimal_strategy_B[[2]], 3)
tail(results_advanced_optimal_strategy_B[[2]], 3)
head(results_advanced_optimal_strategy_B[[3]], 3)
tail(results_advanced_optimal_strategy_B[[3]], 3)


#### CURRENT WORK ####

### Now for xts objects, also testing our simplified function to determine the optimal investment strategy based on historical data...

xts_results_simplified_optimal_strategy_max_2_comb_yearly_rebal <- xts_determine_optimal_strategy_simplified(xts_return_series = xts_strategies_max_2_comb_yearly_rebal, 
                                                                                     time_horizon_years = 12, 
                                                                                     minimum_allowable_percentage = 0.75, 
                                                                                     granularity = "yearly")
# [1] "Execution time for FUNCTION xts_determine_optimal_strategy_simplified, DATASET df_return_series , GRANULARITY yearly : 2.07 seconds"
xts_results_simplified_optimal_strategy_max_2_comb_quarterly_rebal <- xts_determine_optimal_strategy_simplified(xts_return_series = xts_strategies_max_2_comb_quarterly_rebal, 
                                                                                     time_horizon_years = 12, 
                                                                                     minimum_allowable_percentage = 0.75, 
                                                                                     granularity = "quarterly")

xts_results_simplified_optimal_strategy_max_2_comb_monthly_rebal <- xts_determine_optimal_strategy_simplified(xts_return_series = xts_strategies_max_2_comb_monthly_rebal, 
                                                                                     time_horizon_years = 12, 
                                                                                     minimum_allowable_percentage = 0.75, 
                                                                                     granularity = "monthly")
# [1] "Execution time for FUNCTION xts_determine_optimal_strategy_simplified, DATASET df_return_series , GRANULARITY quarterly : 7 seconds"
xts_results_simplified_optimal_strategy_max_2_comb_weekly_rebal <- xts_determine_optimal_strategy_simplified(xts_return_series = xts_strategies_max_2_comb_weekly_rebal, 
                                                                                     time_horizon_years = 12, 
                                                                                     minimum_allowable_percentage = 0.75, 
                                                                                     granularity = "weekly")
# [1] "Execution time for FUNCTION xts_determine_optimal_strategy_simplified, DATASET df_return_series , GRANULARITY weekly : 60.61 seconds"
xts_results_simplified_optimal_strategy_max_2_comb_daily_rebal <- xts_determine_optimal_strategy_simplified(xts_return_series = xts_strategies_max_2_comb_daily_rebal, 
                                                                                     time_horizon_years = 12, 
                                                                                     minimum_allowable_percentage = 0.75, 
                                                                                     granularity = "daily")
# EXECUTION TIME ???
xts_results_simplified_optimal_strategy_max_3_comb_yearly_rebal <- xts_determine_optimal_strategy_simplified(xts_return_series = xts_strategies_max_3_comb_yearly_rebal, 
                                                                                                             time_horizon_years = 12, 
                                                                                                             minimum_allowable_percentage = 0.75, 
                                                                                                             granularity = "yearly")
# [1] "Execution time for FUNCTION xts_determine_optimal_strategy_simplified, DATASET df_return_series , GRANULARITY yearly : 19.92 seconds"
xts_results_simplified_optimal_strategy_max_3_comb_quarterly_rebal <- xts_determine_optimal_strategy_simplified(xts_return_series = xts_strategies_max_3_comb_quarterly_rebal, 
                                                                                                                time_horizon_years = 12, 
                                                                                                                minimum_allowable_percentage = 0.75, 
                                                                                                                granularity = "quarterly")
# EXECUTION TIME ???
xts_results_simplified_optimal_strategy_max_3_comb_monthly_rebal <- xts_determine_optimal_strategy_simplified(xts_return_series = xts_strategies_max_3_comb_monthly_rebal, 
                                                                                                              time_horizon_years = 12, 
                                                                                                              minimum_allowable_percentage = 0.75, 
                                                                                                              granularity = "monthly")
# EXECUTION TIME ???
xts_results_simplified_optimal_strategy_max_3_comb_weekly_rebal <- xts_determine_optimal_strategy_simplified(xts_return_series = xts_strategies_max_3_comb_weekly_rebal, 
                                                                                                             time_horizon_years = 12, 
                                                                                                             minimum_allowable_percentage = 0.75, 
                                                                                                             granularity = "weekly")
# EXECUTION TIME ???
xts_results_simplified_optimal_strategy_max_3_comb_daily_rebal <- xts_determine_optimal_strategy_simplified(xts_return_series = xts_strategies_max_3_comb_daily_rebal, 
                                                                                                            time_horizon_years = 12, 
                                                                                                            minimum_allowable_percentage = 0.75, 
                                                                                                            granularity = "daily")
# EXECUTION TIME ???


# It is clear that these functions applied to our xts objects are running less efficiently than the functions applied to our dataframes


### Other idea (~ 4.	Memory pre-allocation):
# In advance, compute a dataset that contains all cumulative products for different possible time horizons (1, 2, ..., 20 years).
# Depending on the user-specified time horizon and minimum allowable value, within the relevant subset of the data (i.e. the part that corresponds to the user-specified time horizon) we simply search for the one optimal strategy (the one column) that contains
# (i) no cumulative products below the threshold
# (ii) the highest minimum cumulative product, between all investment strategies.

#' This is a good idea. However, keep in mind that this approach will require a significant amount of memory, 
#' as you will be storing a large number of cumulative product series. 
#' You will need to manage your memory resources wisely to ensure your system does not run out of memory.

#' This approach makes sense and should significantly reduce the amount of computation needed to find the optimal strategy. 
#' However, it will still be computationally intensive because you will need to search through a potentially large number of cumulative product series. 
#' You could potentially speed this up using binary search or other optimized search algorithms.

#' We could also more efficiently plot graphs if we start from such a dataset of cumulative products.
#' This is also a good idea, especially if you are planning to plot multiple graphs for different time horizons. 
#' You can precompute the data needed for your plots and simply plot them when needed. 
#' This will reduce the computation needed when generating your plots and can lead to a more responsive user interface.


# 4. Memory pre-allocation 
# ...





# 5. Parallel computation 
# ...






# 6. Memory efficiency (Use more efficient data structures) 
# ...






# 7. Use optimized libraries 
# ...






# 8. Memoization/Caching
# ...






# 9. Machine learning techniques (possibly heuristics)
# ...






# 10. External computation
# ...









# ---------------------------------------------------------------------------- #

# Compare the performance of function determine_optimal_strategy_v1 with that of determine_optimal_strategy_v2 

# # Define the range of years and minimum allowable percentage
# your_time_horizon_years <- seq(4, 12, 8) 
# your_minimum_allowable_percentage <- seq(0.50, 0.90, 0.40) 
# 
# # Call the function compare_results with specified input parameters, running determine_optimal_strategy_v1
# # Remark that it returns 2 objects: list(df_comparison, results_compared)
# your_results_v1 <- compare_results(df_return_series = index_daily_returns_CHF, 
#                                    time_horizon_years = your_time_horizon_years, 
#                                    minimum_allowable_percentage = your_minimum_allowable_percentage,
#                                    optimization_function = determine_optimal_strategy_v1)
# df_comparison_v1 <- your_results_v1[[1]]
# results_compared_v1 <- your_results_v1[[2]]
# 
# # Call the function compare_results with specified input parameters, running determine_optimal_strategy_v2
# # Remark that it returns 2 objects: list(df_comparison, results_compared)
# 
# your_results_v2 <- compare_results(df_return_series = index_daily_returns_CHF, 
#                                    time_horizon_years = your_time_horizon_years, 
#                                    minimum_allowable_percentage = your_minimum_allowable_percentage,
#                                    optimization_function = determine_optimal_strategy_v2)
# df_comparison_v2 <- your_results_v1[[1]]
# results_compared_v2 <- your_results_v1[[2]]
# 
# # Compare optimal strategies and total function times between determine_optimal_strategy_v1 and determine_optimal_strategy_v2
# print(df_comparison_v1)
# print(df_comparison_v2)








# ------------------------------------------------------------------------------------------------ #


### Testing our simplified function to determine the optimal investment strategy based on historical data...

# df_return_series <- strategies_max_4_comb_daily_rebal
# time_horizon_years <- 12
# minimum_allowable_percentage <- 0.75
# granularity = "yearly"
# 
# results_simplified_optimal_strategy <- determine_optimal_strategy_simplified(df_return_series, time_horizon_years, 
#                                                                              minimum_allowable_percentage, granularity)
# print(results_simplified_optimal_strategy[[1]])
# print(paste("Execution time for TOTAL FUNCTION: ", round(results_simplified_optimal_strategy[[4]], 2), "seconds"))


### Testing our first advanced function (A) to determine the optimal investment strategy based on historical data...

# df_return_series <- strategies_max_3_comb_daily_rebal
# time_horizon_years <- 12
# minimum_allowable_percentage <- 0.75
# 
# results_advanced_optimal_strategy_A <- determine_optimal_strategy_advanced_A(df_return_series, time_horizon_years, 
#                                                                              minimum_allowable_percentage, X_percentile = 0.50)
# print(results_advanced_optimal_strategy_A[[1]])
# print(paste("Execution time for TOTAL FUNCTION: ", round(results_advanced_optimal_strategy_A[[4]], 2), "seconds"))

### Testing our first advanced function (A) on a larger dataset, but taking smaller X_percentiles

# df_return_series <- strategies_max_4_comb_daily_rebal
# time_horizon_years <- 12
# minimum_allowable_percentage <- 0.75
# 
# results_advanced_optimal_strategy_A <- determine_optimal_strategy_advanced_A(df_return_series, time_horizon_years, 
#                                                                              minimum_allowable_percentage, X_percentile = 0.40)
# print(results_advanced_optimal_strategy_A[[1]])
# print(paste("Execution time for TOTAL FUNCTION: ", round(results_advanced_optimal_strategy_A[[4]], 2), "seconds"))

### Testing our second advanced function (B) to determine the optimal investment strategy based on historical data...

# df_return_series <- strategies_max_3_comb_daily_rebal
# time_horizon_years <- 12
# minimum_allowable_percentage <- 0.75
# 
# results_advanced_optimal_strategy_B <- determine_optimal_strategy_advanced_B(df_return_series, time_horizon_years,
#                                                                              minimum_allowable_percentage, X_percentile = 0.50)
# print(results_advanced_optimal_strategy_B[[1]])
# print(paste("Execution time for TOTAL FUNCTION: ", round(results_advanced_optimal_strategy_B[[4]], 2), "seconds"))



### Testing our first advanced function (B) on a larger dataset, but taking smaller X_percentiles

# df_return_series <- strategies_max_4_comb_daily_rebal
# time_horizon_years <- 12
# minimum_allowable_percentage <- 0.75
# 
# results_advanced_optimal_strategy_B <- determine_optimal_strategy_advanced_B(df_return_series, time_horizon_years, 
#                                                                              minimum_allowable_percentage, X_percentile = 0.40)
# print(results_advanced_optimal_strategy_B[[1]])
# print(paste("Execution time for TOTAL FUNCTION: ", round(results_advanced_optimal_strategy_B[[4]], 2), "seconds"))


























































####
####
####
####
####

# MARCO'S WORK FOR THE PRESENTATION THAT IS STILL UNUSED ABOVE............






### CPU optimisation ###
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

### SQLite ###
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

