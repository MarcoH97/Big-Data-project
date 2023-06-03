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
generate_weighted_cols <- function(index_returns, max_comb_size) {
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
      new_col_name <- paste(names(investment_strategies)[combo], collapse = " | ")
      
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


# Define a function that determines for a specified time horizon and a specified minimum acceptable percentage value (minimum threshold):
# [1] for each investment strategy, the lowest cumulative return out of all historic time periods that corresponds to the specified time horizon; 
# [2] which investment strategies should be refused because their value dropped below the minimum threshold at any point over any historic time period that corresponds to the specified time horizon (group 2 strategies)
# [3] a plot that displays (i) the intermediate evolution of the lowest cumulative return series for each investment strategy (different colors for non-refused "group 1 strategies", and refused "group 2 strategies"), for each separate time period's first day until its final day, and (ii) a horizontal dashed line that shows the minimum threshold
determine_optimal_strategy <- function(df_return_series, time_horizon_years, minimum_allowable_percentage) {
  # Start timer to later display how long the function took to run
  start_time <- Sys.time()
  
  # Initialize number of rows (days) per year
  # For example, the average number of trading days per year from 1990 to 2022 has been exactly 252.00.
  days_per_year <- 252  
  
  # Convert time horizon "in years" to time horizon in "trading days"
  time_horizon_days <- time_horizon_years * days_per_year
  
  # Initialize results
  df_above_threshold <- data.frame(Strategy = character(), LowestCumulativeReturn = numeric())
  df_refused <- data.frame(Strategy = character(), LowestCumulativeReturn = numeric())
  lowest_series <- list()
  
  # Initialize a list to store plots that display return series for different time periods, given separate investment strategies
  plot_list_different_periods_within_strategies <- list()
  
  # Loop over each candidate investment strategy (columns in the data frame):
  # [1] to determine its lowest cumulative return out of all historic time periods that corresponds to the specified time horizon
  # [2] to determine which investment strategies should be refused because their value dropped below the minimum threshold at any point over any historic time period that corresponds to the specified time horizon
  # [3] to store data about the intermediate evolution of the lowest cumulative return series
  for (col in names(df_return_series)[-1]) {
    lowest_cumulative_return <- Inf
    lowest_cumulative_series <- NULL
    exclude <- FALSE
    
    # Initialize a df for storing all series data
    df_series_to_plot <- data.frame()
    
    # In the current candidate investment strategy, loop over each starting point of the period:
    # [1] to determine the cumulative return of the subsequent time period that corresponds to the specified time horizon, and to compare this cumulative return to the previously determined lowest cumulative return, so we always keep track of the lowest cumulative return series 
    # [2] to determine whether the value drops below the minimum threshold at any point during the time period that corresponds to the specified time horizon, so that we would refuse the current investment strategy altogether.
    # [3] to update the data about the intermediate evolution of the lowest cumulative return series, if the time period corresponding to the current starting point indeed delivers the lowest cumulative return.
    for (i in 1:(nrow(df_return_series) - time_horizon_days + 1)) {
      # Calculate the cumulative returns between current day i and any subsequent day until the end of the time period length
      cum_returns_time_period <- cumprod(1 + df_return_series[i:(i + time_horizon_days - 1), col])
      
      # ------------- COMPUTATIONALLY HEAVY -------------
      # For a given investment strategy, the algorithm analyzes all time periods. We will display a subset of this for illustration purposes.
      # Every curve implies a different start date. If we plot i %% 252 curves, it plots 1 curve per year; If we plot i %% 21, it plots 1 curve per month; If we plot i %% 5, it plots 1 curve per week.
      if(i %% 252 == 0) {
        df <- data.frame(Year = 1:length(cum_returns_time_period)/252, 
                         CumReturns = cum_returns_time_period,
                         Series_ID = paste("Series", i)) # Identifier for each series
        
        # Combine this df with the df for all series
        df_series_to_plot <- rbind(df_series_to_plot, df)
      }

      # If the cumulative return at any point is below the minimum threshold, mark to exclude and break
      if (any(cum_returns_time_period < minimum_allowable_percentage)) {
        exclude <- TRUE
        ################# break # We deleted this break from the code, because we are still interested in all the graphs
      }
      
      # If the cumulative return at the end of the period is the lowest so far, store it and the return series
      if (cum_returns_time_period[length(cum_returns_time_period)] < lowest_cumulative_return) {
        lowest_cumulative_return <- cum_returns_time_period[length(cum_returns_time_period)]
        lowest_cumulative_series <- cum_returns_time_period
      }
    }
    
    # ------------- COMPUTATIONALLY HEAVY -------------
    # After the loop, plot all series for the current investment strategy on one graph
    plot_different_periods_within_strategy <- 
      ggplot(df_series_to_plot, aes(x=Year, y=CumReturns, color=Series_ID)) +
      geom_line() +
      geom_hline(yintercept = minimum_allowable_percentage, linetype = "dashed", color = "red") +
      labs(x = "Years invested", y = "Cumulative Return", title = paste("Cumulative returns for different starting dates (subset of actual analysis), for", col)) +
      theme(legend.position="none")
    
    # ------------- COMPUTATIONALLY HEAVY -------------
    # Append the plot to the list
    plot_list_different_periods_within_strategies[[col]] <- plot_different_periods_within_strategy
    
    # (1) To access a specific plot from the plot list plot_list_different_periods_within_strategies, you would index it using the strategy name as follows:
    # specific_plot = plot_list_different_periods_within_strategies[["Strategy Name"]]
    # print(specific_plot)
    # In the place of "Strategy Name", use the exact name of the strategy you're interested in, 
    # like "US", "Europe", "World", etc.
    # (2) If you would like to display multiple plots together, you can use the gridExtra package. You need to specify the plots you want to display as follows:
    # library(gridExtra)
    # grid.arrange(
    #   plot_list_different_periods_within_strategies[["US"]],
    #   plot_list_different_periods_within_strategies[["Europe"]],
    #   plot_list_different_periods_within_strategies[["Gold bullion"]],
    #   ncol = 1  # Or any other number of columns you want
    # )
    
    # If the strategy is marked for exclusion, add to df_refused, else add to df_above_threshold
    if (exclude == TRUE) {
      df_refused <- rbind(df_refused, data.frame(Strategy = col, LowestCumulativeReturn = lowest_cumulative_return))
      lowest_series[[col]] <- lowest_cumulative_series
    } else {
      df_above_threshold <- rbind(df_above_threshold, data.frame(Strategy = col, LowestCumulativeReturn = lowest_cumulative_return))
      lowest_series[[col]] <- lowest_cumulative_series
    }
  }
  
  
  
  # Generate a plot that displays the intermediate evolution of the lowest cumulative return series for each investment strategy (different colors for non-refused "group 1 strategies", and refused "group 2 strategies")
  # Preprocess data for plotting
  plot_lowest_cum_returns_data <- do.call(rbind, lapply(names(lowest_series), function(name) {
    group <- ifelse(name %in% df_above_threshold$Strategy, "above_threshold", "excluded")
    data.frame(Year = 1:length(lowest_series[[name]])/252, Return = lowest_series[[name]], Strategy = name, Group = group, stringsAsFactors = FALSE)
  }))
  
  # Define color mapping with alpha transparency for the excluded group
  color_mapping <- setNames(ifelse(unique(plot_lowest_cum_returns_data$Strategy) %in% df_above_threshold$Strategy, "darkgreen", "gray90"), unique(plot_lowest_cum_returns_data$Strategy))
  
  # Change the levels of the 'Group' factor so the above_threshold group plots last (on top)
  plot_lowest_cum_returns_data$Group <- factor(plot_lowest_cum_returns_data$Group, levels = c("excluded", "above_threshold"))
  
  # Generate the plot
  plot_lowest_cum_returns <- 
    ggplot(plot_lowest_cum_returns_data, aes(x = Year, y = Return, color = Strategy, group = Strategy)) +
    geom_line(data = subset(plot_lowest_cum_returns_data, Group == "excluded")) +
    geom_line(data = subset(plot_lowest_cum_returns_data, Group == "above_threshold")) +
    scale_color_manual(values = color_mapping) +
    geom_hline(yintercept = minimum_allowable_percentage, linetype = "dashed", color = "red") +
    labs(title = "Lowest cumulative return series, for each candidate investment strategy", x = "Years invested", y = "Cumulative return") +
    theme_minimal() +
    theme(legend.position="none")

  # Display how long the function took to run
  end_time <- Sys.time()
  execution_time <- as.numeric(end_time - start_time, units = "secs")
  print(paste("Execution time: ", execution_time, "seconds"))
  
  # Return, for a specified time horizon and a specified minimum acceptable percentage value (minimum threshold):
  # [1] for each investment strategy, the lowest cumulative return out of all historic time periods that corresponds to the specified time horizon; 
  # [2] which investment strategies should be refused because their value dropped below the minimum threshold at any point over any historic time period that corresponds to the specified time horizon (group 2 strategies)
  # [3] a plot that displays (i) the intermediate evolution of the lowest cumulative return series for each investment strategy (different colors for non-refused "group 1 strategies", and refused "group 2 strategies"), for each separate time period's first day until its final day, and (ii) a horizontal dashed line that shows the minimum threshold
  return(list(df_above_threshold, df_refused, plot_list_different_periods_within_strategies, plot_lowest_cum_returns))
}

