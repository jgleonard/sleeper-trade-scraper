library(shiny)
library(ffscrapr)
library(dplyr)
library(gsubfn)
library(tidyr)

# Source the trade-scraper functions
source("trade-scraper.r")

# Define the User Interface (UI)
ui <- fluidPage(
  titlePanel("Sleeper Trades Scraper"),  # Title of the app
  sidebarLayout(
    sidebarPanel(
      textInput("user_id", "User ID:", value = ""),  # Input for user ID
      numericInput("year", "Year:", value = 2025, min = 2020, max = 2100),  # Input for year
      textInput("player_name", "Player Name (optional):", value = ""),  # Optional input for player name
      actionButton("get_trades", "Get Trades")  # Button to trigger trade fetching
    ),
    mainPanel(
      tableOutput("trades_table")  # Output table to display trades
    )
  )
)

# Define server logic
server <- function(input, output) {
  # Reactive expression to fetch trades when the button is clicked
  trades_data <- eventReactive(input$get_trades, {
    req(input$user_id, input$year)  # Ensure user_id and year are provided
    cat("Fetching trades for user:", input$user_id, "and year:", input$year, "\n")
    
    # Fetch trades based on whether player name is provided
    if (input$player_name != "") {
      cat("Fetching trades for player:", input$player_name, "\n")
      trades <- get_sleeper_player_trades(input$user_id, input$year, input$player_name)
    } else {
      trades <- get_sleeper_trades(input$user_id, input$year)
    }
    
    cat("Fetched trades:\n")
    print(trades)
    if (!is.data.frame(trades)) {
      cat("Error: trades is not a dataframe\n")
      return(data.frame(Error = "No trades found or invalid data format"))
    }
    return(trades)
  })

  # Render the trades table
  output$trades_table <- renderTable({
    trades <- trades_data()
    cat("Rendering trades table:\n")
    print(trades)
    str(trades)  # Inspect the structure of the data frame
    
    # Convert timestamp to user-readable format
    if ("timestamp" %in% colnames(trades)) {
      trades$timestamp <- as.POSIXct(trades$timestamp, origin = "1970-01-01", tz = "UTC")
      trades$timestamp <- format(trades$timestamp, "%Y-%m-%d %H:%M:%S")
    }
    
    # Unnest list columns if present
    if (any(sapply(trades, is.list))) {
      cat("Unnesting list columns in trades\n")
      trades <- trades %>% unnest(cols = c(assets, trade_partner), keep_empty = TRUE)
    }
    trades
  })
}

# Run the application
shinyApp(ui = ui, server = server)