library(shiny)
library(DT)
library(dplyr)
library(rsconnect)

rsconnect::setAccountInfo(name='derkrez',
                          token='',
                          secret='')

final_hitters <- read.csv("final_hitter_projections.csv", check.names = FALSE)
final_pitchers <- read.csv("final_pitcher_projections.csv", check.names = FALSE)

ui <- fluidPage(
  tags$head(
    tags$style(HTML(".blue-header { background-color: #0073e6; color: white; font-weight: bold; }")),
    tags$style(HTML(".hover-highlight tbody tr:hover { background-color: #d9ebff; }")),
    tags$style(HTML(".hover-highlight-compare tbody tr:hover { background-color: #cfd8dc; }")),
    tags$style(HTML("
      /* Adds faint separation lines for table cells */
      table.dataTable th, table.dataTable td {
        border: 1px solid #e0e0e0 !important;  /* Light gray borders for all cells */
      }
      table.dataTable thead th {
        background-color: #0073e6; /* Medium blue background for headers */
        color: white; /* White text for better contrast */
      }
      table.dataTable tbody tr {
        border-bottom: 1px solid #f0f0f0; /* Faint lines separating rows */
      }
      table.dataTable tbody tr:last-child {
        border-bottom: none; /* Remove border from the last row */
      }
    "))
  ),
  
  fluidRow(
    column(12,
           div(style="display: flex; justify-content: space-between; align-items: center;",
               radioButtons("player_type", label = NULL, choices = c("Batters", "Pitchers"), selected = "Batters", inline = TRUE),
               uiOutput("team_select_ui")
           )
    )
  ),
  
  fluidRow(
    column(12,
           checkboxInput("compare", "Projection Comparison", FALSE),
           conditionalPanel(
             condition = "input.compare == true",
             div(style="display: inline-flex;", 
                 selectizeInput("player_search", "Search Player:", choices = NULL, selected = NULL, 
                                options = list(create = TRUE, maxItems = 1, placeholder = 'Type to search for a player'))
             )
           ),
           div(style="display: flex; justify-content: space-between; align-items: center;",
               numericInput("page_size", "Rows per page:", 30, min = 10, max = 1000, step = 10)
           ),
           
           # Conditional tabsets based on player_type
           conditionalPanel(
             condition = "input.player_type == 'Batters'",
             tabsetPanel(id = "batter_tabs",
                         tabPanel("Overview", 
                                  conditionalPanel(
                                    condition = "input.compare == false",
                                    DTOutput("overview_table")
                                  ),
                                  conditionalPanel(
                                    condition = "input.compare == true",
                                    DTOutput("compare_overview_table")
                                  )
                         ),
                         tabPanel("Standard",
                                  conditionalPanel(
                                    condition = "input.compare == false",
                                    DTOutput("standard_table")
                                  ),
                                  conditionalPanel(
                                    condition = "input.compare == true",
                                    DTOutput("compare_standard_table")
                                  )
                         ),
                         tabPanel("Advanced",
                                  conditionalPanel(
                                    condition = "input.compare == false",
                                    DTOutput("advanced_table")
                                  ),
                                  conditionalPanel(
                                    condition = "input.compare == true",
                                    DTOutput("compare_advanced_table")
                                  )
                         )
             )
           ),
           
           conditionalPanel(
             condition = "input.player_type == 'Pitchers'",
             tabsetPanel(id = "pitcher_tabs",
                         tabPanel("Overview", 
                                  conditionalPanel(
                                    condition = "input.compare == false",
                                    DTOutput("pitcher_overview_table")
                                  ),
                                  conditionalPanel(
                                    condition = "input.compare == true",
                                    DTOutput("compare_pitcher_overview_table")
                                  )
                         ),
                         tabPanel("Standard",
                                  conditionalPanel(
                                    condition = "input.compare == false",
                                    DTOutput("pitcher_standard_table")
                                  ),
                                  conditionalPanel(
                                    condition = "input.compare == true",
                                    DTOutput("compare_pitcher_standard_table")
                                  )
                         ),
                         tabPanel("Advanced",
                                  conditionalPanel(
                                    condition = "input.compare == false",
                                    DTOutput("pitcher_advanced_table")
                                  ),
                                  conditionalPanel(
                                    condition = "input.compare == true",
                                    DTOutput("compare_pitcher_advanced_table")
                                  )
                         )
             )
           )
    )
  ),
  tags$div(
    style = "text-align: center; font-size: 12px; margin-top: 20px;",
    "The data I used to generate these projections are courtesy of Fangraphs, Major League Baseball, Sports Info Solutions, and Retrosheet."
  )
)

server <- function(input, output, session) {
  
  # Create dynamic team selection based on player type
  output$team_select_ui <- renderUI({
    if(input$player_type == "Batters") {
      selectInput("team_select", "Team:", choices = c("All Teams", sort(unique(na.omit(final_hitters$Team))), "Free Agents/MiLB"))
    } else {
      selectInput("team_select", "Team:", choices = c("All Teams", sort(unique(na.omit(final_pitchers$Team))), "Free Agents/MiLB"))
    }
  })
  
  # Filter data based on player type and team selection
  filtered_data <- reactive({
    if(input$player_type == "Batters") {
      df <- final_hitters %>% filter(Source == "REZ")
      if (input$team_select == "Free Agents/MiLB") {
        df <- df %>% filter(is.na(Team))
      } else if (input$team_select != "All Teams") {
        df <- df %>% filter(Team == input$team_select)
      }
    } else {
      df <- final_pitchers %>% filter(Source == "REZ")
      if (input$team_select == "Free Agents/MiLB") {
        df <- df %>% filter(is.na(Team))
      } else if (input$team_select != "All Teams") {
        df <- df %>% filter(Team == input$team_select)
      }
    }
    df
  })
  
  # Update player search options based on filtered data
  observe({
    if (input$compare) {
      if(input$player_type == "Batters") {
        updateSelectizeInput(session, "player_search", 
                             choices = unique(final_hitters$Name), 
                             server = TRUE)
      } else {
        updateSelectizeInput(session, "player_search", 
                             choices = unique(final_pitchers$Name), 
                             server = TRUE)
      }
    }
  })
  
  # BATTER TABLES
  
  # Regular batter table rendering functions
  output$overview_table <- renderDT({
    req(input$player_type == "Batters")
    datatable(
      filtered_data() %>% select(Name, Team, G, PA, HR, R, RBI, SB, `BB%`, `K%`, ISO, BABIP, AVG, OBP, SLG, wOBA, `wRC+`, BsR = BaseRunning, WAR),
      options = list(pageLength = input$page_size, lengthMenu = c(30, 50, 100, 500, 1000, nrow(filtered_data()))),
      class = "hover-highlight",
      rownames = FALSE
    ) |>
      formatRound(c('AVG', 'OBP', 'SLG', 'wOBA', 'ISO', 'BABIP'), digits = 3) |>
      formatRound(c('BB%', 'K%', 'BsR', 'WAR' ), digits = 1)
  })
  
  output$standard_table <- renderDT({
    req(input$player_type == "Batters")
    datatable(
      filtered_data() %>% select(Name, Team, G, AB, PA, H, `1B`, `2B`, `3B`, HR, R, RBI, BB, IBB, SO, HBP, SF, SH, SB, CS, AVG) |>
        arrange(desc(AVG)),
      options = list(pageLength = input$page_size, lengthMenu = c(30, 50, 100, 500, 1000, nrow(filtered_data()))),
      class = "hover-highlight",
      rownames = FALSE
    ) |>
      formatRound(c('AVG'), digits = 3)
  })
  
  output$advanced_table <- renderDT({
    req(input$player_type == "Batters")
    datatable(
      filtered_data() %>% select(Name, Team, G, PA, `BB%`, `K%`, `BB/K`, AVG, OBP, SLG, OPS, wOBA, ISO, BABIP, BsR = BaseRunning, `wRC+`) |>
        arrange(desc(`wRC+`)),
      options = list(pageLength = input$page_size, lengthMenu = c(30, 50, 100, 500, 1000, nrow(filtered_data()))),
      class = "hover-highlight",
      rownames = FALSE
    ) |>
      formatRound(c('AVG', 'OBP', 'SLG', 'OPS', 'wOBA', 'ISO', 'BABIP', 'BB/K'), digits = 3) |>
      formatRound(c('BB%', 'K%', 'BsR' ), digits = 1)
  })
  
  # PITCHER TABLES
  
  # Regular pitcher table rendering functions with correct columns
  output$pitcher_overview_table <- renderDT({
    req(input$player_type == "Pitchers")
    datatable(
      filtered_data() %>% select(Name, Team, W, L, SV, G, GS, IP, `K/9`, `BB/9`, `HR/9`, BABIP, `LOB%`, ERA, FIP, WAR) |>
        arrange(desc(WAR)),
      options = list(pageLength = input$page_size, lengthMenu = c(30, 50, 100, 500, 1000, nrow(filtered_data()))),
      class = "hover-highlight",
      rownames = FALSE
    ) |>
      formatRound(c('K/9', 'BB/9', 'HR/9', 'ERA', 'FIP'), digits = 2) |>
      formatRound(c('LOB%', 'WAR' ), digits = 1) |>
      formatRound(c('BABIP'), digits = 3)
  })
  
  output$pitcher_standard_table <- renderDT({
    req(input$player_type == "Pitchers")
    datatable(
      filtered_data() %>% select(Name, Team, W, L, ERA, G, GS, SV, HLD, IP, TBF, H, R, ER, HR, BB, IBB, HBP, SO) |>
        arrange(ERA),
      options = list(pageLength = input$page_size, lengthMenu = c(30, 50, 100, 500, 1000, nrow(filtered_data()))),
      class = "hover-highlight",
      rownames = FALSE
    ) |>
      formatRound(c('ERA'), digits = 2) 
  })
  
  output$pitcher_advanced_table <- renderDT({
    req(input$player_type == "Pitchers")
    datatable(
      filtered_data() %>% select(Name, Team, `K/9`, `BB/9`, `K/BB`, `HR/9`, `K%`, `BB%`, AVG, WHIP, BABIP, `LOB%`, FIP) |>
        arrange(FIP),
      options = list(pageLength = input$page_size, lengthMenu = c(30, 50, 100, 500, 1000, nrow(filtered_data()))),
      class = "hover-highlight",
      rownames = FALSE
    ) |>
      formatRound(c('K/9', 'BB/9', 'HR/9', 'FIP', 'K/BB', 'WHIP'), digits = 2) |>
      formatRound(c('K%', 'BB%', 'LOB%' ), digits = 1) |>
      formatRound(c('BABIP', 'AVG'), digits = 3)
  })
  
  # BATTER COMPARISON TABLES
  
  output$compare_overview_table <- renderDT({
    req(input$player_search)
    req(input$player_type == "Batters")
    filtered <- final_hitters %>% filter(Name == input$player_search)
    datatable(filtered %>% select(Source, Name, Team, G, PA, HR, R, RBI, SB, `BB%`, `K%`, ISO, BABIP, AVG, OBP, SLG, wOBA, `wRC+`, BsR = BaseRunning, WAR),
              options = list(pageLength = 10, lengthMenu = c(10)),
              class = "hover-highlight-compare", rownames = FALSE) %>%
      formatRound(c('AVG', 'OBP', 'SLG', 'wOBA', 'ISO', 'BABIP'), digits = 3) |>
      formatRound(c('BB%', 'K%', 'BsR', 'WAR' ), digits = 1)
  })
  
  output$compare_standard_table <- renderDT({
    req(input$player_search)
    req(input$player_type == "Batters")
    filtered <- final_hitters %>% filter(Name == input$player_search)
    datatable(filtered %>% select(Source, Name, Team, G, AB, PA, H, `1B`, `2B`, `3B`, HR, R, RBI, BB, IBB, SO, HBP, SF, SH, SB, CS, AVG),
              options = list(pageLength = 10, lengthMenu = c(10)),
              class = "hover-highlight-compare", rownames = FALSE) %>%
      formatRound(c('AVG'), digits = 3)
  })
  
  output$compare_advanced_table <- renderDT({
    req(input$player_search)
    req(input$player_type == "Batters")
    filtered <- final_hitters %>% filter(Name == input$player_search)
    datatable(filtered %>% select(Source, Name, Team, G, PA, `BB%`, `K%`, `BB/K`, AVG, OBP, SLG, OPS, wOBA, ISO, BABIP, BsR = BaseRunning, `wRC+`),
              options = list(pageLength = 10, lengthMenu = c(10)),
              class = "hover-highlight-compare", rownames = FALSE) %>%
      formatRound(c('AVG', 'OBP', 'SLG', 'OPS', 'wOBA', 'ISO', 'BABIP', 'BB/K'), digits = 3) |>
      formatRound(c('BB%', 'K%', 'BsR' ), digits = 1)
  })
  
  # PITCHER COMPARISON TABLES with correct columns
  
  output$compare_pitcher_overview_table <- renderDT({
    req(input$player_search)
    req(input$player_type == "Pitchers")
    filtered <- final_pitchers %>% filter(Name == input$player_search)
    datatable(filtered %>% select(Source, Name, Team, W, L, SV, G, GS, IP, `K/9`, `BB/9`, `HR/9`, BABIP, `LOB%`, ERA, FIP, WAR),
              options = list(pageLength = 10, lengthMenu = c(10)),
              class = "hover-highlight-compare", rownames = FALSE) |>
      formatRound(c('K/9', 'BB/9', 'HR/9', 'ERA', 'FIP'), digits = 2) |>
      formatRound(c('LOB%', 'WAR' ), digits = 1) |>
      formatRound(c('BABIP'), digits = 3)
  })
  
  output$compare_pitcher_standard_table <- renderDT({
    req(input$player_search)
    req(input$player_type == "Pitchers")
    filtered <- final_pitchers %>% filter(Name == input$player_search)
    datatable(filtered %>% select(Source, Name, Team, W, L, ERA, G, GS, SV, HLD, IP, TBF, H, R, ER, HR, BB, IBB, HBP, SO),
              options = list(pageLength = 10, lengthMenu = c(10)),
              class = "hover-highlight-compare", rownames = FALSE) |>
      formatRound(c('ERA'), digits = 2) 
  })
  
  output$compare_pitcher_advanced_table <- renderDT({
    req(input$player_search)
    req(input$player_type == "Pitchers")
    filtered <- final_pitchers %>% filter(Name == input$player_search)
    datatable(filtered %>% select(Source, Name, Team, `K/9`, `BB/9`, `K/BB`, `HR/9`, `K%`, `BB%`, AVG, WHIP, BABIP, `LOB%`, FIP),
              options = list(pageLength = 10, lengthMenu = c(10)),
              class = "hover-highlight-compare", rownames = FALSE) |>
      formatRound(c('K/9', 'BB/9', 'HR/9', 'FIP', 'K/BB', 'WHIP'), digits = 2) |>
      formatRound(c('K%', 'BB%', 'LOB%' ), digits = 1) |>
      formatRound(c('BABIP', 'AVG'), digits = 3)
  })
}

shinyApp(ui, server)
