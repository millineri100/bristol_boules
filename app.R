## app.R ##
library(shiny)
library(shinydashboard)
library(magrittr)
library(dplyr)
library(tidyr)
library(readxl)
library(DT)

data <-
    readxl::read_excel("www/data/Bristol Open Boules Rankings.xlsx") %>% 
    dplyr::rename(name = "Name") %>% 
    tidyr::pivot_longer(-name, names_to = "year", values_to = "rank")

winners <-
    readxl::read_excel("www/data/winners.xlsx")

# Find year range to include
year_range <-
    data %>% 
    dplyr::select(year) %>% 
    dplyr::distinct() %>% 
    dplyr::arrange() %>% 
    tail(3) %>% 
    dplyr::pull()

# Filter by last three years and pivot wider
player_form <-
    data %>% 
    dplyr::filter(year %in% year_range) %>% 
    tidyr::pivot_wider(name, names_from = "year", values_from = "rank")

# Set column names to create fix year column names
colnames(player_form) <- c("name", "year_one", "year_two", "year_three")

# Create a form column for last three years
player_form <-
    player_form %>% 
    dplyr::mutate(form = paste(year_one, year_two, year_three, sep = " / "))

data <-
    dplyr::left_join(data,
                     player_form %>% 
                         dplyr::select(name, form),
                     by = "name")

ui <-
    dashboardPage(
    dashboardHeader(title = "Bristol Open"),
    ## Sidebar content
    dashboardSidebar(
        sidebarMenu(
            HTML(paste0(
                "<br>",
                "<img style = 'display: block; margin-left: auto; margin-right: auto;' src='images/boules.jpg' width = '186'></a>",
                "<br>",
                "<p style = 'text-align: center;'><small>Powered by GlennCorp Inc.</small></p>",
                "<br>"
            )),
            menuItem("Home", tabName = "home", icon = icon("home")),
            menuItem("Rankings", tabName = "rankings", icon = icon("trophy")),
            menuItem("Player Stats", tabName = "stats", icon = icon("chart-line"),
                     badgeLabel = "Coming Soon", badgeColor = "green")
        )
    ),
    ## Body content
    dashboardBody(
        
        
        tags$head(tags$style(HTML('
        /* logo */
        .skin-blue .main-header .logo {
                              background-color: #28a197;
                              }

        /* logo when hovered */
        .skin-blue .main-header .logo:hover {
                              background-color: #28a197;
                              }

        /* navbar (rest of the header) */
        .skin-blue .main-header .navbar {
                              background-color: #28a197;
                              }        

        /* main sidebar */
        .skin-blue .main-sidebar {
                              background-color: #333d47;
                              }

        /* active selected tab in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                              background-color: #b1b4b6;
                              color: #000000;
                              }

        /* other links in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                              background-color: #626a6e;
                              color: #FFFFFF;
                              }

        /* other links in the sidebarmenu when hovered */
         .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                              background-color: #f3f2f1;
                              color: #000000;
                              }
        /* toggle button when hovered  */                    
         .skin-blue .main-header .navbar .sidebar-toggle:hover{
                              background-color: #F6D55C;
                              }
                              '))),
        
        
        
        h1("Bristol Open Boules Championship"),
        tabItems(
            # Home tab content
            tabItem(tabName = "home",
                    box(title = h2("Welcome to the home of Bristol boules"),
                        status = "primary",
                        solidHeader = F,
                        collapsible = F,
                        width = 12,
                        fluidRow(column(width = 8, h3(textOutput("welcome"))),
                                 column(width = 2, align = "center",
                                        img(src="images/dry_weather_boules.jpg", width = 200)))),
                    br(),
                    h3("Champions Leaderboard"),
                    fluidRow(tableOutput("winners_table"))
                    # ,
                    # fluidRow(
                    #     box(plotOutput("plot1", height = 250)),
                    #     
                    #     box(
                    #         title = "Controls",
                    #         sliderInput("slider", "Number of observations:", 1, 100, 50)
                    #     )
                    # )
            ),
            
            # Rankings tab content
            tabItem(tabName = "rankings",
                    h2("World Rankings"),
                    selectInput("rankings_year", 
                                label = "Select year",
                                choices = data %>% 
                                    dplyr::select(year) %>% 
                                    dplyr::distinct() %>% 
                                    dplyr::arrange(desc(year)) %>% 
                                    dplyr::pull(),
                                selected = data %>% 
                                    dplyr::select(year) %>% 
                                    dplyr::distinct() %>% 
                                    dplyr::arrange() %>% 
                                    tail(1) %>% 
                                    dplyr::pull()),
                    dataTableOutput("rankings_table")
            ),
            
            #  Stats tab content
            tabItem(tabName = "stats",
                    h2("Player Statistics"))
        )
    )
)

server <- function(input, output) {
    
    output$welcome <- renderText("FIFA World Cup, Wimbledon, the Olympics, what are they? The only sporting fixture people are truly clamouring over this summer is the Bristol Open Boules Championship.")

    # Winners table
    output$winners_table <- renderTable(
        striped = TRUE,
        bordered = TRUE,
        digits = 0,
        width = 400,{
        winners %>% 
            dplyr::arrange(desc(Year))
    })
    
    # Rankings table
    output$rankings_table <- renderDataTable({
        data %>%
            # Filter on input year and remove NA ranks in year as player did not play
            dplyr::filter(year == input$rankings_year,
                          !is.na(rank)) %>%
            # Arrange in ascending order of rank
            dplyr::arrange(rank) %>% 
            # Remove rank column as already numbered
            dplyr::select(-rank, -year) %>% 
            dplyr::rename(`Competitor Name` = "name",
                          `Extended Form (Rankings Last 3 Seasons)` = "form")
    })
}

shinyApp(ui, server)