library(shiny)

constants <- list(
  rarity_levels = c("General", "Common", "Uncommon", "Rare", "Epic", "Legendary", "Mystic", "Iconic"),
  pack_details = list(
    General = list(cards = 6, cost = 200),
    Yesterday = list(cards = 5, cost = 250),
    Starter = list(cards = 3, cost = 100),
    Game = list(cards = 6, cost = 800)
  )
)

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
    "The overall goal of the calculators are to give you an estimation of the 
    minimum investment and maximum profits you can expect when upgrading your 
    player and team cards. Important note to keep in mind: Expected Profits are
    RELATIVE to maximum On This Day (OTD) rax earning caps. Certainly, players
    and teams exceed those caps during the season, and this calculator is not
    projecting individual players and teams. So, please understand Expected 
    Profit numbers as the maximum profit to expect if the player or team does 
    not reach the OTD cap for that rarity in a given season.",  
    style = "font-size: 15px; color: #cccccc; margin-bottom: 10px;"
  ),
  
  tags$p(
    "The calculations assume you are able to trade all of your unwanted cards in 
  a pack for cards of the pass you are upgrading. Using the Celtics and general packs as an 
  example, it assumes you are able to trade all of the non-Celtics cards in a 
  pack (it could be 1, 6, or anything in between) for Celtics cards of 
  equal value. Setting an average rating value of 3.5 means buying 1 pack will
  *eventually* get you 21 rating of Celtics cards for your pass. The same 
  logic applies to player passes as well. There is a indirect way to calculate the costs and returns if you have 
    expectations of having to overpay, or being able to underpay, though. For 
    overpay, simply reduce the average card rating by the amount you expect to
    overpay per card. For underpaying, do the opposite. For example, if you 
    think on average you will be overpaying by 0.3 every trade, then reduce the
    average card rating by 0.3.",  
    style = "font-size: 15px; color: #cccccc; margin-bottom: 10px;"
  ),
  
  tags$p(
    "The estimations are not perfect because I can't tell you the average card 
    ratings for each pack of each sport. For a conservative estimate, use 3.5 
    for General, Starter, and Yesterday packs regardless of sport. For UFC, I 
    have no clue because I don't buy any so use your best judgment. For Game 
    packs, use 5 for NBA and 5.5 for NHL. These are simply suggestions, you are
    free to change the average value to whatever you like. Lastly, investment 
    and profit expectations for Game packs are based on a cost of 800 per pack.",  
    style = "font-size: 15px; color: #cccccc; margin-bottom: 10px;"
    
  ),
  
  tags$p(
    "Starting from Scratch: Use this calculator to determine the total cost and 
    expected return on investment when upgrading an individual player or team 
    card. Enter the card type, card cost, target rarity, average card 
    value, and pack type to see how much rax you can expect to earn,
    along with other useful details. Additionally, choosing the Combine Profits
    option will show you the combined rax of investing in a player-team card duo.
    For example, setting an Epic target for Tarik Skubal will get you a Rare
    Tigers card, and your combined approximate rax earnings after subtracting the 
    800 for the Tigers would be 5200 rax. You can also start with a team card 
    and see what a single card max upgrade for a player card would be.",
    style = "font-size: 15px; color: #cccccc; margin-bottom: 10px;"
  ),
  
  tags$p(
    "Non-Zero Start & Optimal Upgrade Path: This calculator has multiple features.
    First, choosing Player as the card type shows you the cost it will take to 
    go from your current rarity to the target rarity. Simply choose your current 
    rarity and rating value (meaning the left number in the X/Y to Rarity), 
    and the calculator will tell you how much it will cost to reach the desired 
    rarity.If you provide the initial card cost, number of play cards and an approximate (or exact)
    average of those cards, it can estimate how much you've spent on the card 
    already and produce an Expected Return value. IMPORTANT: If you have prestiged
    cards, add +2 to the number of play cards for each prestiged card. If you have
    2 prestiged cards, for example, you should add +4 to whatever your card
    number is. Additionally, you should use the original value of the prestiged 
    cards in the process of estimating your average. Otherwise, your expected 
    profit estimation will be impacted.",
    style = "font-size: 15px; color: #cccccc; margin-bottom: 10px;"
  ),
  
  tags$p(
    "Setting the Card Type to Team and Player Card Upgrade Path to No allows you
    to see the same things as for the Player Card. Choosing Yes lets you put in
    all the ",
    style = "font-size: 15px; color: #cccccc; margin-bottom: 10px;"
  ),
  
  fluidRow(
    column(3,
           conditionalPanel(
             condition = "input.calc_tabs == 'tab1'",
             wellPanel(
               selectInput("card_type", "Card Type:", choices = c("Player", "Team")),
               numericInput("card_cost", "Initial Card Cost:", value = 150, min = 150),
               selectInput("card_rarity", "Target Card Rarity:", 
                           choices = constants$rarity_levels),
               selectInput("pack_type", "Pack Type:", 
                           choices = names(constants$pack_details)),
               numericInput("avg_rating", "Average Play Rating:", value = 3.5, min = 0, step = 0.1),
               radioButtons("combine_rax", "Combine Player and Team Rax Profits",
                            choices = c("No", "Yes")),
               conditionalPanel(
                 condition = "input.combine_rax == 'Yes'",
                 numericInput("other_card_rax", "Other Card Cost:", value = 0, min = 150)
               )
             )
           ),
           
           # Conditional Panel for Tab 2
           conditionalPanel(
             condition = "input.calc_tabs == 'tab2'",  # Show only when 'tab2' is active
             wellPanel(
               # Card Type selection
               selectInput("card_type2", "Card Type:", choices = c("Player", "Team")),
               
               conditionalPanel(
                 condition = "input.card_type2 == 'Player'",
                 # Current Card Rarity selection
                 selectInput("player_card_rarity", "Current Card Rarity:", 
                             choices = constants$rarity_levels),
                 
                 # Card Rating Above Current Rarity 
                 numericInput("player_card_rating", "Current Card Rating Over Rarity:",
                              value = 0, min = 0, step = 0.1),
                 
                 # Card Cost (dynamic based on Card Type)
                 numericInput("card_cost2", "Initial Card Cost:", value = 150, min = 150),
                 
                 # Number of Play Cards 
                 numericInput("play_card_num", "Number of Play Cards:",
                              value = 0, min = 0, step = 1),
                 
                 # Pack Type selection
                 selectInput("pack_type_bought", "Most Common Bought Pack:", 
                             choices = names(constants$pack_details)),
                 
                 # Target Card Rarity selection
                 selectInput("ptarget_card_rarity", "Target Card Rarity:", 
                             choices = constants$rarity_levels),
                 
                 # Pack Type selection
                 selectInput("pack_type2", "Pack Type:", 
                             choices = names(constants$pack_details)),
                 
                 # Average Play Rating
                 numericInput("avg_rating2", "Average Play Rating:", value = 3.5, min = 0, step = 0.1)
               ),
               
               conditionalPanel(
                 condition = "input.card_type2 == 'Team'",
                 
                 #Include Player Card Upgrade Path
                 radioButtons("player_path", "Include Player Card Upgrade Path",
                              choices = c("No", "Yes")), 
                 
                 conditionalPanel(
                   condition = "input.player_path == 'No'",
                   
                   # Current Card Rarity selection
                   selectInput("team_card_rarity", "Current Card Rarity:", 
                               choices = constants$rarity_levels),
                   
                   # Card Rating Above Current Rarity 
                   numericInput("team_card_rating", "Current Card Rating Over Rarity:",
                                value = 0, min = 0, step = 0.1),
                   
                   # Card Cost (dynamic based on Card Type)
                   numericInput("card_cost3", "Initial Card Cost:", value = 150, min = 150),
                   
                   # Number of Play Cards 
                   numericInput("play_card_num2", "Number of Play Cards:",
                                value = 0, min = 0, step = 1),
                   
                   # Pack Type selection
                   selectInput("pack_type_bought2", "Most Common Bought Pack:", 
                               choices = names(constants$pack_details)),
                   
                   # Target Card Rarity selection
                   selectInput("ttarget_card_rarity", "Target Card Rarity:", 
                               choices = constants$rarity_levels),
                   
                   # Pack Type selection
                   selectInput("pack_type3", "Pack Type:", 
                               choices = names(constants$pack_details)),
                   
                   # Average Play Rating
                   numericInput("avg_rating3", "Average Play Rating:", value = 3.5, min = 0, step = 0.1)
                 ),
                 
                 conditionalPanel(
                   condition = "input.player_path == 'Yes'",
                   
                   # Current Card Rarity selection
                   selectInput("team_card_rarity2", "Current Card Rarity:", 
                               choices = constants$rarity_levels),
                   
                   # Card Rating Above Current Rarity 
                   numericInput("team_card_rating2", "Current Card Rating Over Rarity:",
                                value = 0, min = 0, step = 0.1),
                   
                   # Pack Type selection
                   selectInput("pack_type_bought3", "Most Common Bought Pack:", 
                               choices = names(constants$pack_details)),
                   
                   # Target Card Rarity selection
                   selectInput("ttarget_card_rarity2", "Target Card Rarity:", 
                               choices = constants$rarity_levels),
                   
                   # Input for Number of Player Cards
                   numericInput("num_player_cards", "Number of Player Cards:", 
                                value = 1, min = 1),
                   
                   # Dynamically generate rating input boxes for player cards
                   uiOutput("rating_values_section"),  
                   
                   # Pack Type selection
                   selectInput("pack_type4", "Pack Type:", 
                               choices = names(constants$pack_details)),
                   
                   # Average Play Rating
                   numericInput("avg_rating4", "Average Play Rating:", value = 3.5, min = 0, step = 0.1),
                   
                   #Upgrade to next rarity option (Yes/No)
                   radioButtons("upgrade_card", "Upgrade Unoptimized to Next Rarity?",
                                choices = c("Yes", "No"))
                 ),
               )
             )
           ),
           
           # Investment Summary displayed below all the inputs, but outside the gray box
           tags$div(
             style = "margin-top: 20px;",
             h3("Investment Summary"),
             div(style = "font-size: 20px; color: white;", textOutput("expected_profit")),
             conditionalPanel(
               condition = "input.calc_tabs == 'tab2'",
               div(style = "font-size: 20px; color: white;", textOutput("upgrade_investment"))),
             div(style = "font-size: 20px; color: white;", textOutput("total_investment")),
             div(style = "font-size: 20px; color: white;", textOutput("total_play_ratings")),
             div(style = "font-size: 20px; color: white;", textOutput("total_packs_needed")),
             conditionalPanel(
               condition = "input.calc_tabs == 'tab1'",
             div(style = "font-size: 20px; color: white;", textOutput("total_play_rating_value"))),
             conditionalPanel(
               condition = "input.calc_tabs == 'tab2'",
               div(style = "font-size: 20px; color: white;", textOutput("new_rating"))),
             div(style = "font-size: 20px; color: white;", textOutput("total_rating_value_needed"))
           )
    ),
    
    # Main Panel Column (Column 7)
    column(8.5, style = "margin-top: -23px; padding-left: 100px;",
           # Tabset Panel (which controls the different tabs)
           mainPanel(
             tabsetPanel(id = "calc_tabs",
                         tabPanel("Starting from Scratch", value = "tab1"),
                         tabPanel("Non-Zero Start & Optimal Upgrade Path", value = "tab2")
             )
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
    if (input$calc_tabs == "tab1") {
      
      pack_info <- constants$pack_details[[input$pack_type]]
      target_play_ratings <- play_ratings()[[input$card_rarity]]
      
      packs_needed <- ceiling(target_play_ratings / (input$avg_rating * pack_info$cards))
      total_play_rating_value <- packs_needed * pack_info$cards * input$avg_rating
      total_investment <- input$card_cost + (packs_needed * pack_info$cost)
      
      # Combined profit of player and team if option chosen
      expected_profit <- if (input$combine_rax == "No") {
        # No combination, just the max Rax for the selected card rarity
        target_rax <- max_otd_rax()[which(c("General","Common", "Uncommon", "Rare", "Epic", "Legendary", "Mystic", "Iconic") == input$card_rarity)]
        expected_profit <- target_rax - total_investment
      } else {
        # Get the max Rax of the target rarity for the selected card type
        target_rax <- max_otd_rax()[which(c("General","Common", "Uncommon", "Rare", "Epic", "Legendary", "Mystic", "Iconic") == input$card_rarity)]
        
        # Special case for Common Player card, no opposite Rax calculation
        if (input$card_type == "Player" && (input$card_rarity == "Common" | input$card_rarity == "General")) {
          expected_profit <- target_rax - total_investment - ifelse(input$combine_rax == "Yes", input$other_card_rax, 0)
        } else {
          # Determine the opposite card type's Rax (other_rax)
          if (input$card_type == "Player") {
            # Player card selected, so get the expected Team card Rax
            other_rax <- c(0, 1500, 2500, 4000, 6000, 12000, 24000, Inf)
            # Rarity thresholds for Team
            rarity_thresholds <- c(0, 30, 60, 150, 300, 600, 3000, 6000)  # Example thresholds for Team rarity
          } else {
            # Team card selected, so get the expected Player card Rax
            other_rax <- c(0, 500, 1000, 2000, 4000, 8000, 16000, Inf)
            # Rarity thresholds for Player
            rarity_thresholds <- c(0, 10, 30, 80, 180, 380, 1380, 3380)  # Example thresholds for Player rarity
          }
          
          # Find the first rarity threshold where total_play_rating_value >= the required threshold
          closest_match <- max(which(rarity_thresholds <= total_play_rating_value))
          
          # Get the corresponding rarity name
          rarity_name <- c("General","Common", "Uncommon", "Rare", "Epic", "Legendary", "Mystic", "Iconic")[closest_match]
          
          # Now select the corresponding Rax for the opposite card type
          if (input$card_type == "Player") {
            other_rax_value <- other_rax[which(c("General","Common", "Uncommon", "Rare", "Epic", "Legendary", "Mystic", "Iconic") == rarity_name)]
          } else {
            other_rax_value <- other_rax[which(c("General","Common", "Uncommon", "Rare", "Epic", "Legendary", "Mystic", "Iconic") == rarity_name)]
          }
          
          # Set the selected other_rax value
          other_rax <- other_rax_value
          
          # Calculate expected profit
          expected_profit <- target_rax + other_rax - total_investment - ifelse(input$combine_rax == "Yes", input$other_card_rax, 0)
        }
      }
      
      list(
        total_investment = total_investment,
        total_ratings = pack_info$cards * packs_needed,
        total_play_rating_value = total_play_rating_value,
        total_rating_value_needed = target_play_ratings,
        total_packs = packs_needed,
        expected_profit = expected_profit
      )
    }
  })
  
  tab2_calculations <- reactive({
    if (input$calc_tabs == "tab2") {
      
      pack_info <- constants$pack_details[[input$pack_type2]]
      current_play_ratings <- play_ratings()[[input$player_card_rarity]] + input$player_card_rating
      target_play_ratings <- play_ratings()[[input$ptarget_card_rarity]]
      previous_packs_needed <- ceiling(input$play_card_num / pack_info$cards)
      previous_cost <- previous_packs_needed * pack_info$cost
      target_rax <- max_otd_rax()[which(constants$rarity_levels == input$ptarget_card_rarity)]
      
      packs_needed <- ceiling(target_play_ratings / (input$avg_rating2 * pack_info$cards))
      total_play_rating_value <- packs_needed * pack_info$cards * input$avg_rating2
      total_investment <- input$card_cost2 + (packs_needed * pack_info$cost) +
        previous_cost
      expected_profit <- target_rax - total_investment
      upgrade_packs_needed <- (ceiling(target_play_ratings / (input$avg_rating2 * pack_info$cards))) - previous_packs_needed
      upgrade_investment <- upgrade_packs_needed * pack_info$cost
      
      rarities <- constants$rarity_levels  
      target_index <- match(input$ptarget_card_rarity, rarities)
      next_rarity <- if (target_index < length(rarities)) rarities[target_index + 1] else NA
      
      rating_mod <- if (!is.na(next_rarity)) {
        play_ratings()[[next_rarity]] - play_ratings()[[input$ptarget_card_rarity]]
      } else {
        "Iconic"  # Default to 0 if there's no next rarity
      }
      
      list(
        total_investment = total_investment,
        upgrade_investment = upgrade_investment,
        total_ratings = pack_info$cards * packs_needed,
        new_rating = total_play_rating_value - play_ratings()[[input$ptarget_card_rarity]],
        total_rating_value_needed = target_play_ratings - current_play_ratings,
        total_packs = upgrade_packs_needed,
        expected_profit = expected_profit,
        rating_mod = rating_mod
      )
    }
  })
  
  player_play_ratings <- reactive({
    c(General = 0, Common = 10, Uncommon = 30, Rare = 80, Epic = 180,
      Legendary = 380, Mystic = 1380, Iconic = 3380)
  })
  
  team_play_ratings <- reactive({
    c(General = 0, Common = 30, Uncommon = 60, Rare = 150, Epic = 300,
      Legendary = 600, Mystic = 3000, Iconic = 6000)
  })
  
  
  # Update Card Cost dynamically based on Card Type
  observeEvent(input$card_type, {
    updateNumericInput(session, "card_cost",
                       value = ifelse(input$card_type == "Team", 800, 200))
  })
  
  # Update Other Card Cost dynamically based on Card Type
  observeEvent(input$card_type, {
    updateNumericInput(session, "other_card_rax",
                       value = ifelse(input$card_type == "Player", 800, 200))
  })
  
  # Restrict Average Play Rating to one decimal
  observeEvent(input$avg_rating, {
    updateNumericInput(session, "avg_rating",
                       value = round(input$avg_rating, 1))
  })
  
  # Define play rating requirements for each rarity
  play_ratings <- reactive({
    if (input$card_type == "Player") {
      player_play_ratings()
    } else {
      team_play_ratings()
    }
  })
  
  # Define maximum OTD Rax for each rarity based on card type
  max_otd_rax <- reactive({
    if (input$card_type == "Player") {
      c(0, 500, 1000, 2000, 4000, 8000, 16000, Inf)
    } else { # Team
      c(0, 1500, 2500, 4000, 6000, 12000, 24000, Inf)
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
      General = "C:/Users/dresi/Pictures/Screenshots/Screenshot 2025-02-13 002710.png",
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
        "General" = "grey", "Common" = "blue", "Uncommon" = "green", "Rare" = "orange",
        "Epic" = "red", "Legendary" = "purple", "Mystic" = "gold",
        "Iconic" = "pink"
      )
      
      # Create the table data
      table_data <- data.frame(
        Card_Rarity = c("General", "Common", "Uncommon", "Rare", "Epic", "Legendary", "Mystic", "Iconic"),
        Maximum_OTD_Rax = max_rax,
        Rax_Investment = sapply(c("General", "Common", "Uncommon", "Rare", "Epic", "Legendary", "Mystic", "Iconic"), function(rarity) {
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
        Maximum_OTD_Profit = sapply(c("General", "Common", "Uncommon", "Rare", "Epic", "Legendary", "Mystic", "Iconic"), function(rarity) {
          target_play_ratings <- play_ratings()[[rarity]]
          total_cards <- pack_details[[input$pack_type]]$cards
          total_cost <- pack_details[[input$pack_type]]$cost
          
          # Calculate the number of packs needed for the rarity
          packs_needed <- ceiling(target_play_ratings / (input$avg_rating * total_cards))
          
          # Calculate the total Rax Investment
          total_investment <- input$card_cost + (packs_needed * total_cost)
          
          # Calculate Minimum Profit
          max_rax[which(c("General", "Common", "Uncommon", "Rare", "Epic", "Legendary", "Mystic", "Iconic") == rarity)] - total_investment
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
  
  # Dynamic reactive expressions to switch between tab calculations
  investment_summary <- reactive({
    if (input$calc_tabs == "tab1") {
      calculations()
    } else if (input$calc_tabs == "tab2") {
      tab2_calculations()
    } else {
      NULL  # Handle edge cases
    }
  })
  
  # Use the reactive investment_summary() for outputs
  output$expected_profit <- renderText({
    req(investment_summary())  # Ensure it's not NULL
    paste("Expected Profit:", investment_summary()$expected_profit, "rax")
  })
  
  output$total_investment <- renderText({
    req(investment_summary())
    paste("Total Rax Investment:", investment_summary()$total_investment, "rax")
  })
  
  output$total_play_ratings <- renderText({
    req(investment_summary())
    paste("Total Play Rating Cards:", investment_summary()$total_ratings)
  })
  
  output$total_packs_needed <- renderText({
    req(investment_summary())
    paste("Total Packs Required:", investment_summary()$total_packs)
  })
  
  output$total_play_rating_value <- renderText({
    req(investment_summary())
    paste("Total Play Rating Value:", investment_summary()$total_play_rating_value)
  })
  
  output$total_rating_value_needed <- renderText({
    req(investment_summary())
    paste("Total Play Rating Value Needed:", investment_summary()$total_rating_value_needed)
  })
  
  output$upgrade_investment <- renderText({
    req(investment_summary())
    paste("Upgrade Investment:", investment_summary()$upgrade_investment)
  })
  
  output$new_rating <- renderText({
    req(investment_summary())
    paste("New Rating:", investment_summary()$new_rating, "/", investment_summary()$rating_mod)
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
