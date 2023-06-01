# Import packages
library(dplyr)


# Define a function that determines the start of N/A data for each column of a data frame
determine_start_dates <- function(dataframe) {
  # Initialize a list to hold the results
  no_data_start_dates <- list()
  
  # Loop over each column, skipping the first one (Dates)
  for (col_name in colnames(dataframe)[-1]) {
    # Get the column data (excluding the NAs at the end)
    col_data <- dataframe[[col_name]]
    
    # If there are any remaining NAs, get the date of the first one
    if (any(is.na(col_data))) {
      no_data_start_date <- dataframe$Dates[min(which(is.na(col_data)))]
    } else {  # If there are no NAs, the column has data for all dates
      no_data_start_date <- min(dataframe$Dates)
    }
    
    # Add the result to the list
    no_data_start_dates[[col_name]] <- no_data_start_date
  }
  
  # Convert the list to a data frame using bind_rows(), and transpose the data frame  
  no_data_start_dates <- t(bind_rows(no_data_start_dates))
  
  # Add applicable column name to the data frame
  colnames(no_data_start_dates) <- "Start of N/A data"
  
  # Sort by date
  no_data_start_dates <- no_data_start_dates[order(no_data_start_dates, decreasing = TRUE), ]
  
  return(no_data_start_dates)
}


# Define a function that renames selected columns for dataframe index_prices_local_currency or dataframe CHF_FX
rename_columns <- function(dataframe) {
  # Create a named vector of the new and old column names
  if (identical(colnames(dataframe), colnames(index_prices_local_currency))) {
    new_colnames <- c("US" = "MXUS Index", 
                      "US small cap" = "MXUSSC Index", 
                      "Europe" = "MXEU Index", 
                      "Europe small cap" = "MXEUSC Index", 
                      "EM" = "MXEF Index", 
                      "EM small cap" = "MXEFSC Index", 
                      "Switzerland" = "MXCH Index", 
                      "Switzerland small cap" = "MXCHSC Index", 
                      "World" = "MXWO Index", 
                      "World small cap" = "MXWOSC Index", 
                      "US ST treasuries 1-3Y" = "LU13TRUU Index",
                      "US IT treasuries 3-5Y" = "LU35TRUU Index",
                      "US IT treasuries 5-7Y" = "I05749US Index",
                      "US IT treasuries 7-10Y" = "I05750US Index",
                      "US LT treasuries 10Y+" = "I05751US Index",
                      "Europe ST treasuries 1-3Y" = "I02504EU Index",
                      "Europe IT treasuries 3-5Y" = "I02505EU Index",
                      "Europe IT treasuries 5-7Y" = "I02506EU Index",
                      "Europe IT treasuries 7-10Y" = "I02507EU Index",
                      "Europe LT treasuries 10Y+" = "I02508EU Index",
                      "EM ST treasuries 1-3Y" = "I12885US Index",
                      "EM IT treasuries 3-5Y" = "I12886US Index",
                      "EM IT treasuries 5-7Y" = "I12887US Index",
                      "EM IT treasuries 7-10Y" = "I12888US Index",
                      "EM LT treasuries 10Y+" = "I12890US Index",
                      "Switzerland ST treasuries 1-3Y" = "I08236CH Index",
                      "Switzerland IT treasuries 3-5Y" = "I08237CH Index",
                      "Switzerland IT treasuries 5-7Y" = "I08238CH Index",
                      "Switzerland IT treasuries 7-10Y" = "I08239CH Index",
                      "Switzerland LT treasuries 10Y+" = "I08240CH Index",
                      "World ST treasuries 1-3Y" = "I03450US Index",
                      "World IT treasuries 3-5Y" = "I03451US Index",
                      "World IT treasuries 5-7Y" = "I03452US Index",
                      "World IT treasuries 7-10Y" = "I03453US Index",
                      "World LT treasuries 10Y+" = "I03454US Index",
                      "Gold bullion" = "XAU Curncy")
  }
  else if (identical(colnames(dataframe), colnames(CHF_FX))) {
    new_colnames <- c("USD per CHF" = "CHFUSD Curncy",
                      "CHF per USD" = "USDCHF Curncy",
                      "EUR per CHF" = "CHFEUR Curncy",
                      "CHF per EUR" = "EURCHF Curncy")
  }
  else {
    new_colnames = NULL
  }
  
  # Rename the columns using dplyr's rename function
  dataframe_renamed <- rename(dataframe, !! new_colnames)
  
  return(dataframe_renamed)
}

# Define a function that rearranges the columns for dataframe index_prices_local_currency
rearrange_columns <- function(dataframe) {
  dataframe_rearranged <- dataframe %>%
    select(
      Dates, 
      US, 
      `US small cap`, 
      Europe, 
      `Europe small cap`, 
      EM, 
      `EM small cap`, 
      Switzerland, 
      `Switzerland small cap`, 
      World, 
      `World small cap`, 
      `US ST treasuries 1-3Y`,
      `US IT treasuries (3-5Y, 5-7Y, 7-10Y)`,
      `US LT treasuries 10Y+`,
      `Europe ST treasuries 1-3Y`,
      `Europe IT treasuries (3-5Y, 5-7Y, 7-10Y)`,
      `Europe LT treasuries 10Y+`,
      `EM ST treasuries 1-3Y`,
      `EM IT treasuries (3-5Y, 5-7Y, 7-10Y)`,
      `EM LT treasuries 10Y+`,
      `Switzerland ST treasuries 1-3Y`,
      `Switzerland IT treasuries (3-5Y, 5-7Y, 7-10Y)`,
      `Switzerland LT treasuries 10Y+`,
      `World ST treasuries 1-3Y`,
      `World IT treasuries (3-5Y, 5-7Y, 7-10Y)`,
      `World LT treasuries 10Y+`,
      `Gold bullion`
    )
  
  return(dataframe_rearranged)
}


