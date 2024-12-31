library(shiny)
library(rsconnect)

rsconnect::setAccountInfo(name='###',
                          token='###',
                          secret='###')

library(shiny)

ui <- fluidPage(
  # Add custom CSS for the black background
  tags$head(
    tags$style(HTML("
      body {
        background-color: black;
        color: white;
      }
      .shiny-input-container {
        color: black;
      }
    "))
  ),
  
  titlePanel("Card Investment Calculator"),
  
  sidebarLayout(
    sidebarPanel(
      # Card Type selection
      selectInput("card_type", "Card Type:",
                  choices = c("Player", "Team")),
      
      # Card Cost (dynamic based on Card Type)
      numericInput("card_cost", "Initial Card Cost:", value = 200, min = 0),
      
      # Target Card Rarity selection
      selectInput("card_rarity", "Target Card Rarity:",
                  choices = c("Common", "Uncommon", "Rare", "Epic", 
                              "Legendary", "Mystic", "Iconic")),
      
      # Average Play Rating
      numericInput("avg_rating", "Average Play Rating:", value = 3.5, min = 0, step = 0.1),
      
      # Pack Type selection (simplified to General, Yesterday, or Starter)
      selectInput("pack_type", "Pack Type:",
                  choices = c("General", "Yesterday", "Starter"))
    ),
    
    mainPanel(
      # Display the Summary Table
      h3(textOutput("table_title")),
      tableOutput("summary_table"),
      
      # Display outputs
      textOutput("total_investment"),
      textOutput("total_play_ratings"),
      textOutput("total_play_rating_value"),
      textOutput("total_rating_value_needed"), # New output
      textOutput("total_packs"),
      
      # Display card rarity image
      imageOutput("card_image", width = "100%", height = "auto")
    )
  )
)

server <- function(input, output, session) {
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
    Starter = list(cards = 3, cost = 100)
  )
  
  # Reactive calculations
  calculations <- reactive({
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
    
    list(
      total_investment = total_investment,
      total_ratings = total_cards * packs_needed,
      total_play_rating_value = total_play_rating_value,
      total_rating_value_needed = target_play_ratings, # New calculation
      total_packs = packs_needed
    )
  })
  
  # Image path based on rarity
  card_image_path <- reactive({
    rarity <- input$card_rarity
    paths <- list(
      Common = "Screenshot 2024-12-29 132641.png",
      Uncommon = "Screenshot 2024-12-29 133805.png",
      Rare = "Screenshot 2024-12-29 132944.png",
      Epic = "Screenshot 2024-12-29 133002.png",
      Legendary = "Screenshot 2024-12-29 133018.png",
      Mystic = "Screenshot 2024-12-29 133034.png",
      Iconic = "Screenshot 2024-12-29 133048.png"
    )
    paths[[rarity]]
  })
  
  # Summary Table
  output$table_title <- renderText({
    if (input$card_type == "Player") {
      return("Player OTD Rax Break-even")
    } else {
      return("Team OTD Rax Break-even")
    }
  })
  
  # Generate the summary table
  output$summary_table <- renderUI({
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
      style = "width: 100%; text-align: left; border-collapse: collapse;",
      tags$thead(
        tags$tr(
          tags$th("Card Rarity", style = "border-bottom: 2px solid white; padding: 5px;"),
          tags$th("Maximum OTD Rax", style = "border-bottom: 2px solid white; padding: 5px;"),
          tags$th("Rax Investment", style = "border-bottom: 2px solid white; padding: 5px;"),
          tags$th("Maximum OTD Profit", style = "border-bottom: 2px solid white; padding: 5px;")
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
              formatC(table_data$Maximum_OTD_Rax[i], format = "f", big.mark = ",",  digits = 0),
              style = "padding: 5px;"
            ),
            tags$td(
              formatC(table_data$Rax_Investment[i], format = "f", big.mark = ",",  digits = 0),
              style = "padding: 5px;"
            ),
            tags$td(
              formatC(table_data$Maximum_OTD_Profit[i], format = "f", big.mark = ",",  digits = 0),
              style = "padding: 5px;"
            )
          )
        })
      )
    )
    
    # Return the styled HTML table
    table_html
  })
  
  
  # Display outputs
  output$total_investment <- renderText({
    paste("Total Rax Investment:", calculations()$total_investment, "rax")
  })
  
  output$total_play_ratings <- renderText({
    paste("Total Play Rating Cards:", calculations()$total_ratings)
  })
  
  output$total_play_rating_value <- renderText({
    paste("Total Play Rating Value:", calculations()$total_play_rating_value)
  })
  
  output$total_rating_value_needed <- renderText({
    paste("Total Rating Value Needed:", calculations()$total_rating_value_needed)
  })
  
  output$total_packs <- renderText({
    paste("Number of Packs Purchased:", calculations()$total_packs)
  })
  
  # Render the image
  output$card_image <- renderImage({
    list(src = card_image_path(),
         contentType = "image/png",
         alt = "Card Rarity Image")
  }, deleteFile = FALSE)
}

shinyApp(ui, server)
