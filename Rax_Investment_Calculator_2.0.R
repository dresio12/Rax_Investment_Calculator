library(shiny)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        background-color: black;
        color: white;
      }
      .shiny-input-container {
        color: black;
      }
      h2, h3 { margin-bottom: 20px; }
    .shiny-input-container { margin-bottom: 20px; }
    .shiny-text-output { margin-top: 15px; }
    .well { padding: 20px; margin-bottom: 20px; }
    .table { margin-top: 15px; padding: 5px; }
    "))
  ),
  
  titlePanel("Card Investment Calculator"),
  
  tags$p(
    "Use this tool to estimate the minimum investment and maximum returns on 
  your card upgrades.",  
    style = "font-size: 15px; color: #cccccc; margin-bottom: 10px;"
  ),
  
  tags$p(
    "The calculations assume you are able to trade all of your unwanted cards in 
  a pack for cards of the pass you are upgrading. Using the Celtics as an 
  example, it assumes you are able to trade all of the non-Celtics cards in a 
  pack (it could be 1, 6, or anything in between) for Celtics cards of 
  equal value. Setting an average rating value of 3.5 means buying 1 pack will
  *eventually* get you 21 rating of Celtics cards for your pass. The same 
  logic applies to player passes as well.",  
    style = "font-size: 15px; color: #cccccc; margin-bottom: 10px;"
    
  ),
  
  tags$p(
    "The estimations are not perfect because I can't tell you the average card 
    ratings for each pack of each sport. For a conservative estimate, use 3.5 
    for General, Starter, and Yesterday packs regardless of sport. For Game 
    packs, use 5 for NBA and 5.5 for NHL. Game pack calculations are based on a 
    cost of 800 per pack.",  
    style = "font-size: 15px; color: #cccccc; margin-bottom: 10px;"
    
  ),
  
  tags$p(
    "Starting from Scratch: Use this calculator to determine the total cost and 
    maximum return on investment when upgrading an individual player or team 
    card. Enter the card type, card cost, target rarity, average card 
    value, and pack type to see how much rax you can expect to earn at minimum,
    along with other useful details. Additionally, choosing the combine profits
    option will show you the combined rax of investing in a player-team card duo.
    For example, setting an Epic target for Tarik Skubal will get you a Rare
    Tigers card, and your combined approximate rax earnings after adding in the 
    800 for the Tigers would be 5200 rax. You can also start with a team card 
    and see what a single card max upgrade for a player card would be.",
    style = "font-size: 15px; color: #cccccc; margin-bottom: 10px;"
  ),
  
  tags$p(
    "Player & Team Upgrade Combo: This calculator helps determine the optimal 
    upgrade path when a player card contributes to a team card’s progress.
    Enter your existing player card ratings, current team card rating, and 
    target team rarity to see the most efficient way to maximize combined 
    earnings. If any player cards are close to the next rarity, you can choose 
    to upgrade them fully.",
    style = "font-size: 15px; color: #cccccc; margin-bottom: 10px;"
  ),
  
  
  fluidRow(
    # Sidebar Column (Column 3)
    column(3,
           # Conditional Panel for Tab 1
           conditionalPanel(
             condition = "input.calc_tabs == 'tab1'",  # Show only when 'tab1' is active
             wellPanel(
               # Card Type selection
               selectInput("card_type", "Card Type:", choices = c("Player", "Team")),
               
               # Card Cost (dynamic based on Card Type)
               numericInput("card_cost", "Initial Card Cost:", value = 200, min = 0),
               
               # Target Card Rarity selection
               selectInput("card_rarity", "Target Card Rarity:", 
                           choices = c("Common", "Uncommon", "Rare", "Epic", 
                                       "Legendary", "Mystic", "Iconic")),
               
               # Average Play Rating
               numericInput("avg_rating", "Average Play Rating:", value = 3.5, min = 0, step = 0.1),
               
               # Pack Type selection
               selectInput("pack_type", "Pack Type:", 
                           choices = c("General", "Yesterday", "Starter", "Game")),
               
               #Combine Player and Team Earnings
               radioButtons("combine_rax", "Combine Player and Team Rax Profits",
                            choices = c("No", "Yes")),
               
               conditionalPanel(
                 condition = "input.combine_rax == 'Yes'",
                 numericInput("other_card_rax", "Other Card Cost:", value = 0, min = 0)
               )
             )
           ),
           
           # Conditional Panel for Tab 2
           conditionalPanel(
             condition = "input.calc_tabs == 'tab2'",  # Show only when 'tab2' is active
             wellPanel(
               # Input for Number of Player Cards
               numericInput("num_player_cards", "Number of Player Cards:", 
                            value = 1, min = 1),
               
               # Dynamically generate rating input boxes for player cards
               uiOutput("rating_values_section"),  
               
               # Current Team Card Rarity
               selectInput("current_team_card_rarity", "Current Team Card Rarity:",
                           choices = c("Common", "Uncommon", "Rare", "Epic", 
                                       "Legendary", "Mystic", "Iconic")),
               
               # Target Team Card Rarity
               selectInput("team_card_rarity", "Target Team Card Rarity:",
                           choices = c("Common", "Uncommon", "Rare", "Epic", 
                                       "Legendary", "Mystic", "Iconic")),
               
               # Upgrade to next rarity option (Yes/No)
               radioButtons("upgrade_card", "Upgrade Team Card to Next Rarity?",
                            choices = c("Yes", "No")),
               
               # Include an option for the profit calculation method (if different scenarios exist)
               selectInput("profit_calc_method", "Profit Calculation Method:",
                           choices = c("Standard", "Optimized", "Custom")),
               
               )
           ),
           
           # Investment Summary displayed below all the inputs, but outside the gray box
           tags$div(
             style = "margin-top: 20px;",  # Add space between the gray box and output
             h3("Investment Summary"),
             div(style = "font-size: 20px; color: white;", textOutput("expected_profit")),
             div(style = "font-size: 20px; color: white;", textOutput("total_investment")),
             div(style = "font-size: 20px; color: white;", textOutput("total_play_ratings")),
             div(style = "font-size: 20px; color: white;", textOutput("total_packs_needed")),
             div(style = "font-size: 20px; color: white;", textOutput("total_play_rating_value")),
             div(style = "font-size: 20px; color: white;", textOutput("total_rating_value_needed")),
            )
    ),
    
    # Main Panel Column (Column 7)
    column(8.5, style = "margin-top: -23px; padding-left: 100px;",
           # Tabset Panel (which controls the different tabs)
           mainPanel(
             tabsetPanel(id = "calc_tabs",
                         tabPanel("Starting from Scratch", value = "tab1"),
                         tabPanel("Player & Team Upgrade Combo", value = "tab2")
             ),
            ),
           
           # Conditional Panel for Tab 1 content
           conditionalPanel(
             condition = "input.calc_tabs == 'tab1'",  # Show only when 'tab1' is active
             
             # Display the Summary Table
             fluidRow(
               column(9,
             div(style = "text-align: center; width: 90%;",
                 h3(textOutput("table_title"))
                )
              ),
             tableOutput("summary_table"),
             # Display card rarity image
             div(style = "text-align: center;",
                 imageOutput("card_image", width = "90%", height = "auto")
                 )
              )
            ),
           
           # Conditional Panel for Tab 2 content
           conditionalPanel(
             condition = "input.calc_tabs == 'tab2'",  # Show only when 'tab2' is active
             # Tab 2 content (display inputs, calculations, etc.)
           )
        )
      )
    )

server <- function(input, output, session) {
  # Reactive expressions and calculations
  calculations <- reactive({
    # Only execute calculations if Tab 1 is selected
    if (input$calc_tabs == "tab1") {  # Replace 'tab1' with the actual ID of Tab 1
      # Get inputs
      card_cost <- input$card_cost
      avg_rating <- input$avg_rating
      target_rarity <- input$card_rarity
      pack_type <- input$pack_type
      
      # Get target play ratings based on rarity
      target_play_ratings <- play_ratings()[[target_rarity]]
      
      # Initialize variables
      total_cards <- pack_details[[pack_type]]$cards
      total_cost <- pack_details[[pack_type]]$cost
      
      # Calculate the number of packs needed to reach the target play rating
      packs_needed <- ceiling(target_play_ratings / (avg_rating * total_cards))
      
      # Calculate total play rating value
      total_play_rating_value <- packs_needed * total_cards * avg_rating
      
      # Calculate total investment (including initial card cost)
      total_investment <- card_cost + (packs_needed * total_cost)
      
      # Combined profit of player and team if option chosen
      expected_profit <- if (input$combine_rax == "No") {
        # No combination, just the max Rax for the selected card rarity
        target_rax <- max_otd_rax()[which(c("Common", "Uncommon", "Rare", "Epic", "Legendary", "Mystic", "Iconic") == input$card_rarity)]
        expected_profit <- target_rax - total_investment
      } else {
        # Get the max Rax of the target rarity for the selected card type
        target_rax <- max_otd_rax()[which(c("Common", "Uncommon", "Rare", "Epic", "Legendary", "Mystic", "Iconic") == input$card_rarity)]
        
        # Special case for Common Player card, no opposite Rax calculation
        if (input$card_type == "Player" && input$card_rarity == "Common") {
          expected_profit <- target_rax - total_investment - ifelse(input$combine_rax == "Yes", input$other_card_rax, 0)
        } else {
          # Determine the opposite card type's Rax (other_rax)
          if (input$card_type == "Player") {
            # Player card selected, so get the expected Team card Rax
            other_rax <- c(1500, 2500, 4000, 6000, 12000, 24000, Inf)
            # Rarity thresholds for Team
            rarity_thresholds <- c(30, 60, 150, 300, 600, 3000, 6000)  # Example thresholds for Team rarity
          } else {
            # Team card selected, so get the expected Player card Rax
            other_rax <- c(500, 1000, 2000, 4000, 8000, 16000, Inf)
            # Rarity thresholds for Player
            rarity_thresholds <- c(10, 30, 80, 180, 380, 1380, 3380)  # Example thresholds for Player rarity
          }
          
          # Find the first rarity threshold where total_play_rating_value >= the required threshold
          closest_match <- max(which(rarity_thresholds <= total_play_rating_value))
          
          # Get the corresponding rarity name
          rarity_name <- c("Common", "Uncommon", "Rare", "Epic", "Legendary", "Mystic", "Iconic")[closest_match]
          
          # Now select the corresponding Rax for the opposite card type
          if (input$card_type == "Player") {
            other_rax_value <- other_rax[which(c("Common", "Uncommon", "Rare", "Epic", "Legendary", "Mystic", "Iconic") == rarity_name)]
          } else {
            other_rax_value <- other_rax[which(c("Common", "Uncommon", "Rare", "Epic", "Legendary", "Mystic", "Iconic") == rarity_name)]
          }
          
          # Set the selected other_rax value
          other_rax <- other_rax_value
          
          # Calculate expected profit
          expected_profit <- target_rax + other_rax - total_investment - ifelse(input$combine_rax == "Yes", input$other_card_rax, 0)
        }
      }
      
      list(
        total_investment = total_investment,
        total_ratings = total_cards * packs_needed,
        total_play_rating_value = total_play_rating_value,
        total_rating_value_needed = target_play_ratings, 
        total_packs = packs_needed,
        expected_profit = expected_profit
      )
    }
  })
  
  player_play_ratings <- reactive({
    c(Common = 10, Uncommon = 30, Rare = 80, Epic = 180,
      Legendary = 380, Mystic = 1380, Iconic = 3380)
  })
  
  team_play_ratings <- reactive({
    c(Common = 30, Uncommon = 60, Rare = 150, Epic = 300,
      Legendary = 600, Mystic = 3000, Iconic = 6000)
  })
  
    
  # Update Card Cost dynamically based on Card Type
  observeEvent(input$card_type, {
    updateNumericInput(session, "card_cost",
                       value = ifelse(input$card_type == "Team", 800, 200))
  })
  
  # Restrict Average Play Rating to one decimal
  observeEvent(input$avg_rating, {
    updateNumericInput(session, "avg_rating",
                       value = round(input$avg_rating, 1))
  })
  
  # Define play rating requirements for each rarity
  play_ratings <- reactive({
    if (input$card_type == "Player") {
      list(
        Common = 10, Uncommon = 30, Rare = 80, Epic = 180,
        Legendary = 380, Mystic = 1380, Iconic = 3380
      )
    } else { # Team
      list(
        Common = 30, Uncommon = 60, Rare = 150, Epic = 300,
        Legendary = 600, Mystic = 3000, Iconic = 6000
      )
    }
  })
  
  # Define maximum OTD Rax for each rarity based on card type
  max_otd_rax <- reactive({
    if (input$card_type == "Player") {
      c(500, 1000, 2000, 4000, 8000, 16000, Inf)
    } else { # Team
      c(1500, 2500, 4000, 6000, 12000, 24000, Inf)
    }
  })
  
  # Define the Rax Investment for each pack type (including initial card cost)
  pack_details <- list(
    General = list(cards = 6, cost = 200),
    Yesterday = list(cards = 5, cost = 250),
    Starter = list(cards = 3, cost = 100),
    Game = list(cards = 6, cost = 800)
  )
  
  # Image path based on rarity
  card_image_path <- reactive({
    rarity <- input$card_rarity
    paths <- list(
      Common = "C:/Users/dresi/Pictures/Screenshots/Screenshot 2024-12-29 132641.png",
      Uncommon = "C:/Users/dresi/Pictures/Screenshots/Screenshot 2024-12-29 133805.png",
      Rare = "C:/Users/dresi/Pictures/Screenshots/Screenshot 2024-12-29 132944.png",
      Epic = "C:/Users/dresi/Pictures/Screenshots/Screenshot 2024-12-29 133002.png",
      Legendary = "C:/Users/dresi/Pictures/Screenshots/Screenshot 2024-12-29 133018.png",
      Mystic = "C:/Users/dresi/Pictures/Screenshots/Screenshot 2024-12-29 133034.png",
      Iconic = "C:/Users/dresi/Pictures/Screenshots/Screenshot 2024-12-29 133048.png"
    )
    paths[[rarity]]
  })
  
  # Summary Table (Only show on Tab 1)
  output$table_title <- renderText({
    if (input$calc_tabs == "tab1") {  # Ensure it only renders on Tab 1
      if (input$card_type == "Player") {
        return("Player OTD Rax Margins")
      } else {
        return("Team OTD Rax Margins")
      }
    }
  })
  
  # Generate the summary table (Only show on Tab 1)
  output$summary_table <- renderUI({
    if (input$calc_tabs == "tab1") {  # Ensure it only renders on Tab 1
      # Determine max OTD Rax based on card type
      max_rax <- max_otd_rax()
      
      # Define rarity colors
      rarity_colors <- c(
        "Common" = "blue", "Uncommon" = "green", "Rare" = "orange",
        "Epic" = "red", "Legendary" = "purple", "Mystic" = "gold",
        "Iconic" = "pink"
      )
      
      # Create the table data
      table_data <- data.frame(
        Card_Rarity = c("Common", "Uncommon", "Rare", "Epic", "Legendary", "Mystic", "Iconic"),
        Maximum_OTD_Rax = max_rax,
        Rax_Investment = sapply(c("Common", "Uncommon", "Rare", "Epic", "Legendary", "Mystic", "Iconic"), function(rarity) {
          # For each rarity, calculate Rax Investment based on input selections
          target_play_ratings <- play_ratings()[[rarity]]
          total_cards <- pack_details[[input$pack_type]]$cards
          total_cost <- pack_details[[input$pack_type]]$cost
          
          # Calculate the number of packs needed for each rarity
          packs_needed <- ceiling(target_play_ratings / (input$avg_rating * total_cards))
          
          # Calculate Rax Investment (including initial card cost)
          rax_investment <- input$card_cost + (packs_needed * total_cost)
          return(rax_investment)
        }),
        Maximum_OTD_Profit = sapply(c("Common", "Uncommon", "Rare", "Epic", "Legendary", "Mystic", "Iconic"), function(rarity) {
          target_play_ratings <- play_ratings()[[rarity]]
          total_cards <- pack_details[[input$pack_type]]$cards
          total_cost <- pack_details[[input$pack_type]]$cost
          
          # Calculate the number of packs needed for the rarity
          packs_needed <- ceiling(target_play_ratings / (input$avg_rating * total_cards))
          
          # Calculate the total Rax Investment
          total_investment <- input$card_cost + (packs_needed * total_cost)
          
          # Calculate Minimum Profit
          max_rax[which(c("Common", "Uncommon", "Rare", "Epic", "Legendary", "Mystic", "Iconic") == rarity)] - total_investment
        })
      )
      
      # Generate the HTML table with colored text
      table_html <- tags$table(
        style = "width: 75%; text-align: center; border-collapse: collapse;",
        tags$thead(
          tags$tr(
            tags$th("Card Rarity", style = "text-align: center; border-bottom: 2px solid white; padding: 5px;"),
            tags$th("Maximum OTD Rax", style = "text-align: center; border-bottom: 2px solid white; padding: 5px;"),
            tags$th("Rax Investment", style = "text-align: center; border-bottom: 2px solid white; padding: 5px;"),
            tags$th("Maximum OTD Profit", style = "text-align: center; border-bottom: 2px solid white; padding: 5px;")
          )
        ),
        tags$tbody(
          lapply(1:nrow(table_data), function(i) {
            tags$tr(
              tags$td(
                table_data$Card_Rarity[i],
                style = paste("color:", rarity_colors[table_data$Card_Rarity[i]], "; padding: 5px;")
              ),
              tags$td(
                formatC(table_data$Maximum_OTD_Rax[i], format = "f", big.mark = ",", digits = 0), # 0 decimals
                style = "padding: 5px;"
              ),
              tags$td(
                formatC(table_data$Rax_Investment[i], format = "f", big.mark = ",", digits = 0), # 0 decimals
                style = "padding: 5px;"
              ),
              tags$td(
                formatC(table_data$Maximum_OTD_Profit[i], format = "f", big.mark = ",", digits = 0), # 0 decimals
                style = "padding: 5px;"
              )
            )
          })
        )
      )
      
      # Return the styled HTML table
      table_html
    }
  })
  
  # Display outputs (Only show on Tab 1)
  output$total_investment <- renderText({
    if (input$calc_tabs == "tab1") {  # Ensure it only renders on Tab 1
      paste("Total Rax Investment:", calculations()$total_investment, "rax")
    }
  })
  
  output$total_play_ratings <- renderText({
    if (input$calc_tabs == "tab1") {  # Ensure it only renders on Tab 1
      paste("Total Play Rating Cards:", calculations()$total_ratings)
    }
  })
  
  output$total_play_rating_value <- renderText({
    if (input$calc_tabs == "tab1") {  # Ensure it only renders on Tab 1
      paste("Total Play Rating Value:", calculations()$total_play_rating_value)
    }
  })
  
  output$total_rating_value_needed <- renderText({
    if (input$calc_tabs == "tab1") {  # Ensure it only renders on Tab 1
      paste("Total Play Rating Value Needed:", calculations()$total_rating_value_needed)
    }
  })
  
  output$total_packs_needed <- renderText({
    if (input$calc_tabs == "tab1") {  # Ensure it only renders on Tab 1
      paste("Total Packs Needed:", calculations()$total_packs)
    }
  })
  
  output$expected_profit <- renderText({
    if (input$calc_tabs == "tab1") {  # Ensure it only renders on Tab 1
      paste("Expected Profit:", calculations()$expected_profit, "rax")
    }
  })
  
  # Render the image
  output$card_image <- renderImage({
    list(src = card_image_path(),
         contentType = "image/png",
         alt = "Card Rarity Image")
  }, deleteFile = FALSE)
  
  #Calculator 2
  # Dynamically generate the rating value input boxes based on the number of player cards
  output$rating_values_section <- renderUI({
    num_cards <- input$num_player_cards
    lapply(1:num_cards, function(i) {
      numericInput(paste0("rating_value_", i), 
                   paste("Card", i, "Rating Value:"), 
                   value = 0, min = 0, step = 0.1)
    })
  })
 
}

shinyApp(ui, server)
