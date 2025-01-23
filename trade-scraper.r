# Function to install and load required packages
install_and_load <- function(packages) {
  for (package in packages) {
    if (!require(package, character.only = TRUE)) {
      install.packages(package, dependencies = TRUE)
      library(package, character.only = TRUE)
    }
  }
}

# List of required packages
required_packages <- c("ffscrapr", "dplyr", "tidyr", "gsubfn")

# Install and load required packages
install_and_load(required_packages)

# Function to get trades for a given Sleeper user and year
get_sleeper_trades <- function(user_id, year) {
  # Create a connection to the Sleeper API
  conn <- sleeper_connect(season = year, sport = "nfl", user_name = user_id)

  # Get the user's leagues
  leagues <- ff_userleagues(conn, user_id)

  # Create an empty dataframe to store the trades
  trades_df <- data.frame()

  # Loop through each league and get trades
  for (league in leagues$league_id) {

    # Print the league ID and current iteration out of total
    cat("Getting trades for league", league,
        "(", which(leagues$league_id == league), "of", nrow(leagues), ")\n")

    # Connect to the league to get the transaction data
    league_conn <- sleeper_connect(season = year,
                                   league_id = league, user_name = user_id)

    # Initialize the transactions dataframe
    transactions <- NULL

    # Get the transactions for the league
    tryCatch({
      transactions <- ff_transactions(league_conn, transaction_type = "trade")
    }, error = function(e) {
      cat("Error getting transactions for league", league, ":", e$message, "\n")
      return(NULL)
    })

    # Skip the rest if there was an error getting transactions or none found
    if (is.null(transactions) || (nrow(transactions) == 0)) {
      next
    }

    # Filter the transactions to only include trades
    trades <- transactions[transactions$type == "trade", ]

    # Remove columns that are not needed
    trades <- trades[, !(names(trades) %in% c("waiver_priority", "comment", "bbid_amount"))] # nolint: line_length_linter.

    # Skip the rest if there are no trades
    if (nrow(trades) == 0) {
      next
    }

    # Get franchise_id for given user_id using ff_franchises
    franchises <- ff_franchises(league_conn)

    # Replace NA fields with 0 to account for missing teams
    franchises[is.na(franchises)] <- "0"

    # Replace NA with 0 in trades
    trades[is.na(trades)] <- "0"

    # Get the user's franchise_id
    my_franchise_id <- as.integer(franchises[franchises$user_id == league_conn["user_id"], "franchise_id"]) # nolint: line_length_linter.

    # Before comparing, convert to an integer
    trades$franchise_id <- as.integer(trades$franchise_id)
    trades$trade_partner <- as.integer(trades$trade_partner)

    # If trade_partner is NA replace with 0
    trades[is.na(trades$trade_partner), "trade_partner"] <- 0

    # Filter the trades to only include trades involving the user
    mytrades <- trades[trades$franchise_id == my_franchise_id, ] # nolint: line_length_linter.

    # Skip the rest if there are no trades for the user
    if (nrow(mytrades) == 0) {
      next
    }

    # Add a column for the league_id
    mytrades$league_id <- league

    # Substitute the "franchise x" for the username
    mytrades$player_id <- gsubfn("franchise_([0-9]+)", function(x) {
      intx <- as.integer(gsub("franchise_", "", x))
      user_name <- franchises[franchises$franchise_id == intx, "user_name"]
      return(user_name)
    }, mytrades$player_id, perl = TRUE, backref = 0)

    # Rename player_name to assets
    names(mytrades)[names(mytrades) == "player_name"] <- "assets"

    # If the player_id contains "pick",
    # copy the value to assets and replace "_" with " "
    mytrades$assets[grepl("pick", mytrades$player_id)] <- gsub("_", " ", mytrades$player_id[grepl("pick", mytrades$player_id)]) # nolint: line_length_linter.

    # If the player_id contains bid, copy the value to assets,
    # find the integer in the string, convert the integer to its absolute value
    # then append " fab" to the end of the value
    mytrades$assets[grepl("bid", mytrades$player_id)] <- paste0(abs(as.integer(gsub("\\D", "", mytrades$player_id[grepl("bid", mytrades$player_id)]))), " fab") # nolint: line_length_linter.

    # Per transaction, replace trade_partner and franchise_id with user_name
    for (i in seq_len(nrow(mytrades))) {
      if (mytrades$franchise_id[i] == my_franchise_id) {
        mytrades$franchise_id[i] <- user_id
      } else {
        mytrades$franchise_id[i] <- franchises[franchises$franchise_id == mytrades$franchise_id[i], "user_name"] # nolint: line_length_linter.
      }

      if (mytrades$trade_partner[i] == my_franchise_id) {
        mytrades$trade_partner[i] <- user_id
      } else {
        mytrades$trade_partner[i] <- franchises[franchises$franchise_id == mytrades$trade_partner[i], "user_name"] # nolint: line_length_linter.
      }
    }

    # For each trade, combine the assets if the
    # timestamp, franchise_id and type_desc are the same
    mytrades <- mytrades %>%
      group_by(.data$league_id, .data$timestamp, .data$franchise_id, .data$type_desc, .data$trade_partner) %>% # nolint: line_length_linter.
      summarise(assets = paste(.data$assets, collapse = ", "),
                .groups = "drop") %>%
      ungroup()

    # Rename franchise_id to user_name
    names(mytrades)[names(mytrades) == "franchise_id"] <- "user_name"

    # Remove columns that are not needed
    mytrades <- mytrades[, !(names(mytrades) %in% c("player_id", "franchise_name", "type"))] # nolint: line_length_linter.

    # Remove rows that only contain NA values
    mytrades <- mytrades[rowSums(is.na(mytrades)) != ncol(mytrades), ]

    # Add columns to the dataframe to match the trades if empty
    if (nrow(trades_df) == 0) {
      trades_df <- mytrades
    } else {
      trades_df <- rbind(trades_df, mytrades)
    }
  }

  return(trades_df)
}

# Function to get trades involving a specific player for a given user and year
get_sleeper_player_trades <- function(user_id, year, player_name) {
  # Get all trades for the user and year
  trades <- get_sleeper_trades(user_id, year)

  # Filter the trades to only include trades involving the player
  p_trades <- trades[grepl(player_name, trades$assets, ignore.case = TRUE), ]

  # Include the trades with same league id & timestamp from trades in the result
  p_trades <- trades[trades$league_id %in% p_trades$league_id &
                       trades$timestamp %in% p_trades$timestamp, ]

  return(p_trades)
}
