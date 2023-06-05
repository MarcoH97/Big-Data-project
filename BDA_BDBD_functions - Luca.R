# Import packages
library(dplyr)
library(zoo)
library(ggplot2)

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

# Define a function that generates columns of equally-weighted indices, with daily rebalancing
generate_weighted_cols_daily_rebal <- function(index_returns, max_comb_size) {
  # Start timer to later display how long the function took to run
  start_time <- Sys.time()
  
  # Create a dataframe that contains the initial investment strategies (investing 100% in an index), without the Dates column
  investment_strategies <- index_returns[, -1]
  
  # Get the number of columns (indices) in the dataframe
  num_cols <- ncol(investment_strategies)
  
  # Iterate over i for i-combinations
  for (i in 2:min(num_cols, max_comb_size)) {
    # Generate all i-combinations of column indices
    combos <- combinat::combn(1:num_cols, i, simplify = FALSE)
    
    # Iterate over each combination
    for (combo in combos) {
      # Calculate the new column as the row-wise mean of the selected columns
      new_col <- rowMeans(investment_strategies[, combo])
      
      # Create the new column name
      new_col_name <- paste(names(investment_strategies)[combo], collapse = " & ")
      
      # Add the new column to the dataframe
      index_returns[[new_col_name]] <- new_col
    }
  }
  
  # Display how long the function took to run
  end_time <- Sys.time()
  execution_time <- as.numeric(end_time - start_time, units = "secs")
  print(paste("Execution time: ", execution_time, "seconds"))
  
  return(index_returns)
}

# Define a function that plots the correlation matrix between returns of different investment strategies (first column is "Dates")
plot_correlation_matrix <- function(df_return_series) {
  # Determine the correlations between returns of the 26 selected indices
  correlation_matrix <- cor(df_return_series[, -1])
  print(correlation_matrix)
  
  # Melt the correlation matrix to long format for ggplot2
  melted_cormat <- melt(correlation_matrix)
  
  # Plot the correlation matrix between returns of the 26 selected indices
  p <- ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
    geom_tile() +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                         midpoint = 0, limit = c(-1,1), space = "Lab", 
                         name="Pearson\nCorrelation") +
    theme_minimal() + 
    theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                     size = 10, hjust = 1), 
          axis.title = element_blank()) +
    coord_fixed(ratio = 1/1.5)
  
  print(p)
  
  return()
}

# Define a function that plots the mean-variance graph for daily returns of all investment strategies in the data frame (first column is "Dates")
plot_mean_variance_graph <- function(df_return_series) {
  # Start timer to later display how long the function took to run
  start_time <- Sys.time()
  
  # Determine the daily mean return and daily standard deviation 
  mean_returns_daily <- colMeans(df_return_series[, -1])
  std_daily <- apply(df_return_series[, -1], 2, sd)
  
  # Annualize means and standard deviations
  mean_returns_annual <- (1 + mean_returns_daily)^252 - 1
  std_annual <- std_daily * sqrt(252)
  
  # Create a dataframe that contains annualized mean returns and standard deviations of all these investment strategies
  meanvar_plot_df <- data.frame(means=mean_returns_annual, sds=std_annual)
  
  # Add a new column for color
  meanvar_plot_df$group <- NA
  
  # Assign group labels based on column number
  if(nrow(meanvar_plot_df) > 83682) {
    meanvar_plot_df$group[1:26] <- 'Original strategies'
    meanvar_plot_df$group[27:351] <- '2 combinations'
    meanvar_plot_df$group[352:2951] <- '3 combinations'
    meanvar_plot_df$group[2952:17901] <- '4 combinations'
    meanvar_plot_df$group[17902:83681] <- '5 combinations'
    meanvar_plot_df$group[83682:313912] <- '6 combinations'
    
  }
  else if(nrow(meanvar_plot_df) > 17902) {
    meanvar_plot_df$group[1:26] <- 'Original strategies'
    meanvar_plot_df$group[27:351] <- '2 combinations'
    meanvar_plot_df$group[352:2951] <- '3 combinations'
    meanvar_plot_df$group[2952:17901] <- '4 combinations'
    meanvar_plot_df$group[17902:83681] <- '5 combinations'
  }
  else if(nrow(meanvar_plot_df) > 2952) {
    meanvar_plot_df$group[1:26] <- 'Original strategies'
    meanvar_plot_df$group[27:351] <- '2 combinations'
    meanvar_plot_df$group[352:2951] <- '3 combinations'
    meanvar_plot_df$group[2952:17901] <- '4 combinations'
  }
  else if(nrow(meanvar_plot_df) > 352) {
    meanvar_plot_df$group[1:26] <- 'Original strategies'
    meanvar_plot_df$group[27:351] <- '2 combinations'
    meanvar_plot_df$group[352:2951] <- '3 combinations'
  }
  else if(nrow(meanvar_plot_df) > 27) {
    meanvar_plot_df$group[1:26] <- 'Original strategies'
    meanvar_plot_df$group[27:351] <- '2 combinations'
    
  }
  else if(nrow(meanvar_plot_df) <= 26) {
    meanvar_plot_df$group[1:26] <- 'Original strategies'
  }
  
  # Plot with color aesthetic mapped to group
  p <- ggplot(meanvar_plot_df, aes(x=mean_returns_annual, y = std_annual, color=group))+
    geom_point(alpha=0.10)+
    geom_smooth(method = "lm", se = FALSE, linetype = "solid") +
    labs(x='Mean', y="Standard deviation", title="Annualized mean and standard deviation for each candidate investment strategy")+
    scale_x_continuous(labels = scales::percent)+
    scale_y_continuous(labels = scales::percent)+
    scale_color_manual(values=c('Original strategies' = '#000000', '2 combinations' = '#00008B', 
                                '3 combinations' = '#008000', '4 combinations' = '#FF0000', 
                                '5 combinations' = '#4B0082', '6 combinations' = '#FF6600'))+
    theme_bw()+
    theme(plot.title = element_text(size= 14, hjust = 0.5, face ="bold"), 
          axis.text.x = element_text(angle = 90, hjust = 1, face = "bold"),
          axis.text.y = element_text(angle = 90, hjust = 1, face = "bold"))
  
  print(p)
  
  # Display how long the function took to run
  end_time <- Sys.time()
  execution_time <- as.numeric(end_time - start_time, units = "secs")
  print(paste("Execution time: ", execution_time, "seconds"))
  
  return()
}


#' A comprehensive analysis function to determine the optimal investment strategy based on historical data.
#' 
#' This function takes into consideration all possible start times within a given time horizon and a minimum acceptable cumulative return.
#' The function implements two main computations:
#' 1. For each strategy, it evaluates all possible periods within the specified time horizon, and keeps track of the lowest and highest cumulative return.
#' 2. It discards strategies that at any point drop below the minimum acceptable cumulative return, and it identifies the optimal strategy as the one with the highest minimum cumulative return.
#' The function also generates several plots to visualize the the different historic return series for each strategy and the evolution of the lowest cumulative return.
#' 
#' @param df_return_series A data frame containing the return series of different strategies.
#' @param time_horizon_years The time horizon in years.
#' @param minimum_allowable_percentage The minimum allowable cumulative return percentage below which a strategy should be excluded.
#' 
#' @return A list containing the following elements:
#' - plot_lowest_cum_returns: a ggplot object illustrating the evolution of the lowest cumulative return series for each investment strategy.
#' - optimal_strategy: the optimal investment strategy identified.
#' - plot_optimal_strategy: a ggplot object that visualizes worst-case, best-case, and other return series for the optimal strategy.
#' - df_above_threshold: a data frame containing strategies that have not been excluded, their lowest and highest cumulative returns.
#' - df_refused: a data frame containing excluded strategies, their lowest and highest cumulative returns.
#' - plot_list_different_periods_within_strategies: a list of ggplot objects, each representing cumulative returns for different starting dates for a particular strategy.
#' 
determine_optimal_strategy_v1 <- function(df_return_series, time_horizon_years, minimum_allowable_percentage) {
  # Start timer for performance tracking
  total_function_start <- Sys.time()
  
  # Initialize constants and variables
  days_per_year <- 252  
  time_horizon_days <- time_horizon_years * days_per_year
  
  # Initialize data frames to store results
  df_above_threshold <- data.frame(Strategy = character(), LowestCumulativeReturn = numeric())
  df_refused <- data.frame(Strategy = character(), LowestCumulativeReturn = numeric())
  
  # Initialize lists to store the lowest and highest return series for each strategy
  lowest_series <- list()
  highest_series <- list()
  
  # Initialize a list to store return series of the optimal strategy
  optimal_strategy_series <- list()
  
  # Initialize a list to store ggplot objects for each strategy.
  # Each ggplot object represents cumulative returns for different starting dates for a particular strategy.
  plot_list_different_periods_within_strategies <- list()
  
  # Main loop over each strategy
  for (col in names(df_return_series)[-1]) {
    
    # Initialization of variables for current strategy
    lowest_cumulative_return <- Inf
    highest_cumulative_return <- -Inf 
    lowest_cumulative_series <- NULL
    highest_cumulative_series <- NULL 
    exclude <- FALSE
    df_series_to_plot <- data.frame()
    
    # Loop over all possible starting times for the specified time horizon
    for (i in 1:(nrow(df_return_series) - time_horizon_days + 1)) {
      # Compute cumulative returns for the current time period
      cum_returns_time_period <- cumprod(1 + df_return_series[i:(i + time_horizon_days - 1), col])
      
      # Compute series to plot for every year (for visualization purposes)
      if(i %% 252 == 0) {
        df <- data.frame(Year = 1:length(cum_returns_time_period)/252, 
                         CumReturns = cum_returns_time_period,
                         Series_ID = paste("Series", i)) # Identifier for each series
        
        # Combine this df with the df for all series
        df_series_to_plot <- rbind(df_series_to_plot, df)
      }
      
      # Check if the minimum cumulative return is below the threshold and set the flag to exclude the strategy if so
      if (any(cum_returns_time_period < minimum_allowable_percentage)) {
        exclude <- TRUE
        ################# break # We deleted this break from the code, because we are still interested in all the graphs
      }
      
      # Keep track of the lowest cumulative return for the current strategy
      if (cum_returns_time_period[length(cum_returns_time_period)] < lowest_cumulative_return) {
        lowest_cumulative_return <- cum_returns_time_period[length(cum_returns_time_period)]
        lowest_cumulative_series <- cum_returns_time_period
      }
      
      # Keep track of the highest cumulative return for the current strategy
      if (cum_returns_time_period[length(cum_returns_time_period)] > highest_cumulative_return) {
        highest_cumulative_return <- cum_returns_time_period[length(cum_returns_time_period)]
        highest_cumulative_series <- cum_returns_time_period
      }
    }
    
    # Generate plot displaying all series of cumulative returns for the current investment strategy. 
    # Each series represents a different start time within the time horizon.
    plot_different_periods_within_strategy <- 
      ggplot(df_series_to_plot, aes(x=Year, y=CumReturns, color=Series_ID)) +
      geom_line() +
      geom_hline(yintercept = minimum_allowable_percentage, linetype = "dashed", color = "red") +
      labs(x = "Years invested", y = "Cumulative Return", title = paste("Cumulative returns for different starting dates (subset of actual analysis), for", col)) +
      theme(legend.position="none") +
      scale_y_continuous(limits = c(0, NA))
    
    # The resulting plot is added to the list `plot_list_different_periods_within_strategies` for later use.
    plot_list_different_periods_within_strategies[[col]] <- plot_different_periods_within_strategy
    
    # If the strategy is marked for exclusion, add to df_refused, else add to df_above_threshold
    if (exclude == TRUE) {
      df_refused <- rbind(df_refused, data.frame(Strategy = col, LowestCumulativeReturn = lowest_cumulative_return, HighestCumulativeReturn = highest_cumulative_return))
      lowest_series[[col]] <- lowest_cumulative_series
      highest_series[[col]] <- highest_cumulative_series 
    } else {
      df_above_threshold <- rbind(df_above_threshold, data.frame(Strategy = col, LowestCumulativeReturn = lowest_cumulative_return, HighestCumulativeReturn = highest_cumulative_return))
      lowest_series[[col]] <- lowest_cumulative_series
      highest_series[[col]] <- highest_cumulative_series
    }
  }
  
  # Select the optimal strategy as the one with the highest minimum cumulative return that didn't fall below the threshold
  optimal_strategy <- df_above_threshold[which.max(df_above_threshold$LowestCumulativeReturn), "Strategy"]
  
  # Prepare data for plotting lowest cumulative returns for each strategy, categorizing them as above_threshold or excluded
  plot_lowest_cum_returns_data <- do.call(rbind, lapply(names(lowest_series), function(name) {
    group <- ifelse(name %in% df_above_threshold$Strategy, "above_threshold", "excluded")
    data.frame(Year = 1:length(lowest_series[[name]])/252, Return = lowest_series[[name]], Strategy = name, Group = group, stringsAsFactors = FALSE)
  }))
  
  # Assign colors for the plot: 'darkgreen' for strategies above threshold and 'gray90' for excluded strategies
  color_mapping <- setNames(ifelse(unique(plot_lowest_cum_returns_data$Strategy) %in% df_above_threshold$Strategy, "darkgreen", "gray90"), unique(plot_lowest_cum_returns_data$Strategy))
  
  # Rearrange the order of the 'Group' factor to ensure 'above_threshold' group is plotted last (and therefore on top)
  plot_lowest_cum_returns_data$Group <- factor(plot_lowest_cum_returns_data$Group, levels = c("excluded", "above_threshold"))
  
  # Generate a plot illustrating the evolution of the lowest cumulative return series for each investment strategy 
  # Strategies are color-coded based on group - 'above_threshold' and 'excluded'
  plot_lowest_cum_returns <- 
    ggplot(plot_lowest_cum_returns_data, aes(x = Year, y = Return, color = Strategy, group = Strategy)) +
    geom_line(data = subset(plot_lowest_cum_returns_data, Group == "excluded")) +
    geom_line(data = subset(plot_lowest_cum_returns_data, Group == "above_threshold")) +
    scale_color_manual(values = color_mapping) +
    geom_hline(yintercept = minimum_allowable_percentage, linetype = "dashed", color = "red") +
    labs(title = "Lowest cumulative return series, for each candidate investment strategy", x = "Years invested", y = "Cumulative return") +
    theme_minimal() +
    theme(legend.position="none") +
    scale_y_continuous(limits = c(0, NA))
  
  # Extract plot data for the optimal strategy
  plot_optimal_strategy_data <- ggplot_build(plot_list_different_periods_within_strategies[[optimal_strategy]])$data[[1]]
  
  # Generate a plot that visualizes worst-case, best-case, and other return series for the optimal strategy
  plot_optimal_strategy <- 
    ggplot() +
    geom_line(data = plot_optimal_strategy_data, aes(x = x, y = y, color = colour), alpha = 0.2) +
    geom_line(data = data.frame(Year = 1:length(lowest_series[[optimal_strategy]])/252, Return = lowest_series[[optimal_strategy]]), aes(x = Year, y = Return), color = "red") +
    geom_line(data = data.frame(Year = 1:length(highest_series[[optimal_strategy]])/252, Return = highest_series[[optimal_strategy]]), aes(x = Year, y = Return), color = "green") +
    geom_hline(yintercept = minimum_allowable_percentage, linetype = "dashed", color = "red") +
    labs(title = paste("Worst-case, best-case and other historic return series for your optimal strategy:", optimal_strategy), x = "Years invested", y = "Cumulative return") +
    theme_minimal() +
    theme(legend.position="none") +
    scale_y_continuous(limits = c(0, NA))
  
  # Compute and print the total execution time of the function
  total_function_end <- Sys.time()
  total_function_time <- as.numeric(total_function_end - total_function_start, units = "secs")
  print(paste("Execution time for TOTAL FUNCTION: ", total_function_time, "seconds"))
  
  # Return the plots, optimal strategy, and data frames of results
  return(list(plot_lowest_cum_returns, optimal_strategy, plot_optimal_strategy, df_above_threshold, df_refused, plot_list_different_periods_within_strategies, total_function_time))
}










determine_optimal_strategy_v2 <- function() {
  
  
  
  
  
  return()
}


