## app.R ##
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(magrittr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readxl)
library(readr)
library(tibble)
library(DT)
library(scales)

data <-
    readxl::read_excel("www/data/Bristol Open Boules Rankings.xlsx") %>% 
    dplyr::rename(name = "Name") %>% 
    tidyr::pivot_longer(-name, names_to = "year", values_to = "rank") %>% 
    dplyr::mutate(year = as.numeric(year))

winners <-
    readxl::read_excel("www/data/winners.xlsx")

player_stats <-
    readxl::read_excel("www/data/player_stats.xlsx")

prize_money <-
    readxl::read_excel("www/data/Bristol Open Boules Finishing Positions.xlsx")

nationalities <-
    readr::read_csv("www/data/player_nationalities.csv")

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

# Attach player form to data
data <-
    dplyr::left_join(data,
                     player_form %>% 
                         dplyr::select(name, form),
                     by = "name")

#### Find player names
player_names <-
    data %>% 
    dplyr::select(name) %>% 
    dplyr::distinct() %>% 
    dplyr::arrange() %>% 
    dplyr::pull()


#### Complete player stats

# Add Grand Slams won
player_stats <-
    dplyr::left_join(player_stats,
                 winners %>% 
                     dplyr::group_by(Name) %>% 
                     dplyr::summarise("Grand Slams" = n()) %>% 
                     dplyr::ungroup(),
                 by = "Name") %>% 
    dplyr::mutate(`Grand Slams.x` = `Grand Slams.y`) %>% 
    dplyr::rename(`Grand Slams` = "Grand Slams.x") %>% 
    dplyr::mutate(`Grand Slams` = dplyr::case_when(is.na(`Grand Slams`) ~ 0,
                                                   TRUE ~ as.numeric(`Grand Slams`))) %>% 
    dplyr::select(-`Grand Slams.y`)

player_stats <-
    player_stats %>% 
    # Add Current Rank
    dplyr::mutate(`Current Rank` = with(data %>% dplyr::filter(year == max(year)), rank[match(player_stats$Name, name)])) %>% 
    # Add Best Rank
    dplyr::mutate(`Best Rank` = with(data %>% 
                                         dplyr::group_by(name) %>% 
                                         dplyr::summarise(rank = min(rank, na.rm = TRUE)) %>% 
                                         dplyr::ungroup(), rank[match(player_stats$Name, name)])) %>% 
    # Add Worst Rank
    dplyr::mutate(`Lowest Rank` = with(data %>% 
                                         dplyr::group_by(name) %>% 
                                         dplyr::summarise(rank = max(rank, na.rm = TRUE)) %>% 
                                         dplyr::ungroup(), rank[match(player_stats$Name, name)])) %>% 
    # Add Weeks at No. 1
    dplyr::mutate(`Weeks at No. 1` =
                      dplyr::case_when(Name ==
                                           "Glenn" ~ difftime(strptime(Sys.Date(), format = "%Y-%m-%d"),
                                                              strptime("2015-07-04", format = "%Y-%m-%d"),
                                                              units = "weeks") %>% 
                                           as.numeric() %>% 
                                           round(),
                                       TRUE ~ as.numeric(`Weeks at No. 1`)))
# Add Form Rank
year_one_weight <- 1
year_two_weight <- 2
year_three_weight <- 5
form_calculator <-
    data %>% 
    dplyr::select(-form) %>% 
    tidyr::pivot_wider(name,
                       names_from = "year",
                       values_from = "rank")
form_calculator <-
    form_calculator %>% 
    dplyr::select(1, ncol(form_calculator) - 2, ncol(form_calculator) - 1, ncol(form_calculator))
colnames(form_calculator) <- c("name", "year_one", "year_two", "year_three")
form_calculator <-
    form_calculator %>% 
    dplyr::mutate(year_one = dplyr::case_when(is.na(year_one) ~ year_one_weight,
                                              TRUE ~ year_one),
                  year_two = dplyr::case_when(is.na(year_two) ~ year_two_weight,
                                              TRUE ~ year_two),
                  year_three = dplyr::case_when(is.na(year_three) ~ year_two_weight,
                                                TRUE ~ year_three)) %>% 
    dplyr::mutate(form_rank = (year_one_weight / year_one + year_two_weight / year_two + year_three_weight / year_three) / 
                      sum(year_one_weight + year_two_weight + year_three_weight) * 1000) %>% 
    dplyr::mutate(form_rank = round(form_rank, 0)) %>% 
    dplyr::arrange(dplyr::desc(form_rank))
player_stats <-
    player_stats %>% 
    # Add Current Rank
    dplyr::mutate(`Form Ranking Points (max. 1000)` = with(form_calculator, form_rank[match(player_stats$Name, name)]))

# Calculate Prize Money
prize_money[prize_money == 1] <- 225000
prize_money[prize_money == 2] <- 112500
prize_money[prize_money == 3] <- 76800
prize_money[prize_money == 4] <- 56300
prize_money[prize_money == 5] <- 28100
prize_money[prize_money == 6] <- 26500
prize_money[prize_money == 7] <- 25000
prize_money[prize_money == 8] <- 22500
prize_money[prize_money == 9] <- 19900
prize_money[prize_money == 10] <- 17800
prize_money[prize_money == 11] <- 16000
prize_money[prize_money == 12] <- 14500
prize_money[prize_money == 13] <- 10000
prize_money[prize_money == 14] <- 5000
prize_money[prize_money == 15] <- 1250
prize_money[prize_money == 16] <- 750
prize_money[prize_money == 17] <- 125
prize_money[prize_money == 18] <- 49
prize_money[prize_money == 19] <- 10
prize_money[prize_money == 20] <- 5
prize_money[is.na(prize_money)] <- 0
#Total prize money
prize_money <-
    prize_money %>% 
    tidyr::pivot_longer(-Name,
                        names_to = "year",
                        values_to = "prize_money") %>% 
    dplyr::group_by(Name) %>% 
    dplyr::summarise(prize_money = sum(prize_money)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(prize_money = paste0("£", scales::comma(prize_money)))
player_stats <-
    player_stats %>% 
    # Add Prize Money
    dplyr::mutate(`Prize Money` = with(prize_money, prize_money[match(player_stats$Name, Name)]))

# Add Random Player Nationality
#Create random number to index to
player_stats <-
    player_stats %>% 
    dplyr::mutate(Nationality = runif(length(player_names), min = 1, max = nrow(nationalities)) %>% floor())
#Match random index to Nationality
player_stats <-
    player_stats %>% 
    dplyr::mutate(Nationality = with(nationalities, Nationality[match(player_stats$Nationality, Number)]))

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
                "<p style = 'text-align: center;'><small>Powered by GlennCorp Ltd.</small></p>",
                "<br>"
            )),
            menuItem("Home", tabName = "home", icon = icon("home")),
            menuItem("Results", tabName = "results", icon = icon("table"),
                     badgeLabel = "Coming Soon", badgeColor = "orange"),
            menuItem("Rankings", tabName = "rankings", icon = icon("trophy")),
            menuItem("Player Stats", tabName = "stats", icon = icon("chart-line"),
                     badgeLabel = "NEW", badgeColor = "green")
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
                    fluidRow(tableOutput("winners_table")),
                    br(),
                    h4(textOutput("sponsors"))
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
                    h2("Player Statistics"),
                    fluidRow(
                            title = "Compare Rankings",
                            dropdownButton(
                                tags$h3("Settings:"),
                                tags$h4("Compare Player Rankings Over Time"),
                                
                                selectInput(inputId = 'player_select',
                                            label = 'Select Competitor:',
                                            choices = unique(data$name),
                                            multiple = TRUE,
                                            selected =
                                                data %>% 
                                                dplyr::filter(year == max(data$year)) %>% 
                                                dplyr::arrange(rank) %>% 
                                                dplyr::select(name) %>% 
                                                head(3) %>% 
                                                dplyr::pull()),
                                
                                circle = TRUE, status = "danger",
                                icon = icon("gear"), width = "300px",
                                
                                tooltip = tooltipOptions(title = "Click to Update Settings")
                            ),
                            
                            plotOutput(outputId = 'plot_rankings')
                        # )
                    ),
                    br(),
                    fluidRow(
                        # Bottom Left
                        box(title = "Compare the Pair",
                            status = "primary",
                            solidHeader = TRUE,
                            selectInput(inputId = 'compare_player_one',
                                        label = 'Select Competitor',
                                        choices = unique(data$name),
                                        selected =
                                            data %>% 
                                            dplyr::filter(year == max(data$year)) %>% 
                                            dplyr::arrange(rank) %>% 
                                            dplyr::select(name) %>% 
                                            head(1) %>% 
                                            dplyr::pull()),
                            tableOutput("table_player_one")),
                        # Bottom Right
                        box(
                            title = "Compare the Pair",
                            
                            status = "primary",
                            solidHeader = TRUE,
                            selectInput(inputId = 'compare_player_two',
                                        label = 'Select Competitor',
                                        choices = unique(data$name),
                                        selected =
                                            data %>% 
                                            dplyr::filter(year == max(data$year)) %>% 
                                            dplyr::arrange(rank) %>% 
                                            dplyr::select(name) %>% 
                                            tail(1) %>% 
                                            dplyr::pull()),
                            tableOutput("table_player_two")
                        )
                    ))
        )
    )
)

server <- function(input, output) {
    
    #### Home pane
    
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
    
    output$sponsors <- renderText({"Proud partners of GlennCorp Ltd.\u2122 and The Yeandel Prestige Worldwide Initiative\u2122."})
    
    #### Rankings pane
    
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
    
    
    #### Player Stats pane
    
    selected_data <- reactive({
        data %>%
            dplyr::filter(name %in% input$player_select)
    })
    
    output$plot_rankings <- renderPlot({
        #browser()
        palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FFFFFF",
                  "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
        ggplot2::ggplot(data = selected_data(),
                        ggplot2::aes(x = year,
                                     y = rank,
                                     group = name,
                                     colour = name)) +
            #Specify the line type.
            ggplot2::geom_point(shape = 19,
                                size = 5) +
            ggplot2::geom_line(ggplot2::aes(linetype = name),
                               linetype  = "solid",
                               size = 2) +
            #Add title, source and axis labels.
            ggplot2::theme_grey(base_size = 12) +
            ggplot2::labs(x = "Year",
                          y = "World Ranking",
                          color = "Name") + 
            #Adjust format of plot background, axis colours, tick mark colours, title size etc.
            ggplot2::theme(panel.background = ggplot2::element_blank(),
                           panel.grid.major.x = ggplot2::element_line(colour = "grey"),
                           panel.grid.major.y = ggplot2::element_line(colour = "grey"),
                           plot.margin = ggplot2::unit(c(0, 0, 0, 0), "mm"),
                           plot.title = ggplot2::element_text(hjust = 0, size = 10),
                           #legend.position = "none",
                           legend.text = ggplot2::element_text(size = 14),
                           axis.line = ggplot2::element_line(colour = "black"),
                           axis.text = ggplot2::element_text(colour = "black", size = 12)) +
            #Reverse the y axus to display ranking in order of 1 - 12.
            ggplot2::scale_y_reverse(limits = c(max(selected_data()$rank, na.rm = TRUE), 1),
                                     breaks = seq(max(selected_data()$rank, na.rm = TRUE), 1, -1)) +
            #Cut the space from sides of plot, and remove the line and axis tick for the dummy column.
            ggplot2::scale_x_continuous(expand = c(0.01, 0.01),
                                        breaks = seq(min(selected_data()$year, na.rm = TRUE),
                                                     max(selected_data()$year, na.rm = TRUE),
                                                     1)) +
            ggplot2::scale_colour_manual(values = c("red",
                                                    #modutils::mod_pal_engagement[5],
                                                    "#7fc28f",
                                                    #modutils::mod_pal_engagement[2],
                                                    "#fcc10b",
                                                    #modutils::mod_pal_engagement[4],
                                                    "#ee7490",
                                                    #modutils::mod_pal_engagement[3],
                                                    "#00616c",
                                                    "green",
                                                    "yellow",
                                                    #modutils::mod_pal_engagement[1],
                                                    "#da5720",
                                                    "black",
                                                    #modutils::mod_pal_operational[1],
                                                    "#48626e",
                                                    #modutils::mod_pal_operational[2],
                                                    "#224671",
                                                    #modutils::mod_pal_operational[3],
                                                    "#59afae",
                                                    #modutils::mod_pal_operational[4],
                                                    "#8f4447",
                                                    #modutils::mod_pal_operational[5],
                                                    "#2c5625",
                                                    #modutils::mod_pal_operational[6]
                                                    "#9fa2a3",
                                                    "blue",
                                                    "purple",
                                                    "orange",
                                                    "pink",
                                                    "magenta",
                                                    "cyan"))
    })
    
    # Compare player one
    output$table_player_one <- renderTable(
        striped = TRUE,
        colnames = FALSE,
        bordered = TRUE,{
            player_stats %>% 
                dplyr::filter(Name == input$compare_player_one) %>%
                t() %>%
                as.data.frame() %>%
                tibble::rownames_to_column()
        })
    
    # Compare player two
    output$table_player_two <- renderTable(
        striped = TRUE,
        colnames = FALSE,
        bordered = TRUE,{
            player_stats %>% 
                dplyr::filter(Name == input$compare_player_two) %>%
                t() %>%
                as.data.frame() %>%
                tibble::rownames_to_column()
        })

}

shinyApp(ui, server)