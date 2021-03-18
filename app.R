
# Data Visualization: STAT 302 - final project 2021/02/20 -----------------


# load packages -----------------------------------------------------------

library(tidyverse)
library(DT)
library(shiny)
library(skimr)
library(viridis)
library(shinythemes)
library(shinycssloaders)
library(maps)

# read in dataset
imm_dat <- read_rds("data/processed/imm_dat_clean.rds")

# lat/long data from map_data
states <- map_data("state") %>%
    rename(state = region)

# join datasets based on Percentage of bills enacted
imm_dat_map <- imm_dat %>%
    mutate(state = tolower(state)) %>%
    mutate(enacted = ifelse(enacted == "Yes", 1, 0)) %>%
    group_by(state) %>%
    summarize(n = n(),
              passed = sum(enacted),
              prop = passed/n) %>%
    mutate(enacted = cut(prop,
                         breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1),
                         labels = c("0-20%", "20-40%", "40-60%", "60-80%", "80-100%"),
                         right = FALSE)) %>%
    left_join(states, by = "state")


# join datasets based on whether bills targets undocumented individuals
imm_dat_map_undoc <- imm_dat %>%
    mutate(state = tolower(state)) %>%
    mutate(targets_undocumented = ifelse(targets_undocumented == "Yes", 1, 0)) %>%
    group_by(state) %>%
    summarize(
        n = n(),
        passed = sum(targets_undocumented),
        prop = passed/n) %>%
    mutate(targets_undocumented = cut(prop,
                         breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1),
                         labels = c("0-20%", "20-40%", "40-60%", "60-80%", "80-100%"),
                         right = FALSE)) %>%
    left_join(states, by = "state")

# join datasets based on whether bills targets undocumented children
imm_dat_map_undoc_child <- imm_dat %>%
    mutate(state = tolower(state)) %>%
    mutate(targets_undocumented_children = ifelse(targets_undocumented_children == "Yes", 1, 0)) %>%
    group_by(state) %>%
    summarize(
        n = n(),
        passed = sum(targets_undocumented_children),
        prop = passed/n) %>%
    mutate(targets_undocumented_children = cut(prop,
                                      breaks = c(0, 0.05, 0.10, 0.15, 0.20, 0.25),
                                      labels = c("0-5%", "5-10%", "10-15%", "15-20%", "20-25%"),
                                      right = FALSE)) %>%
    left_join(states, by = "state")

# make a tibble with Percentage of bills passed in each state -- overall
imm_dat_state <- imm_dat_map %>%
    distinct(state, passed, n) %>%
    janitor::adorn_totals("row") %>%
    mutate(prop = round((passed / n) * 100, 2),
           state = str_to_title(state),
           state = ifelse(row_number() == 51, "All US States", state))

# make a tibble with Percentage of bills passed in each state -- undocumented individuals
imm_dat_state_undoc <- imm_dat_map_undoc %>%
    distinct(state, passed, n) %>%
    janitor::adorn_totals("row") %>%
    mutate(prop = round((passed / n) * 100, 2),
           state = str_to_title(state),
           state = ifelse(row_number() == 51, "All US States", state))

# make a tibble with Percentage of bills passed in each state -- undocumented children
imm_dat_state_undoc_child <- imm_dat_map_undoc_child %>%
    distinct(state, passed, n) %>%
    janitor::adorn_totals("row") %>%
    mutate(prop = round((passed / n) * 100, 2),
           state = str_to_title(state),
           state = ifelse(row_number() == 51, "All US States", state))

# create a vector with all states' names
state_names <- list(
    "All US States",
    "Alabama", "Arizona", "Arkansas", "California",
    "Colorado", "Connecticut", "Delaware", "Florida", "Georgia",
    "Idaho", "Illinois", "Indiana", "Iowa",
    "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland",
    "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri",
    "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey",
    "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio",
    "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina",
    "South Dakota", "Tennessee", "Texas", "Utah", "Vermont",
    "Virginia", "Washington", "Washington, DC", "West Virginia", "Wisconsin",
    "Wyoming")


# shiny -------------------------------------------------------------------
# user interface ----
ui <- fluidPage(
    shinythemes::themeSelector(),
    navbarPage(
        # app title ----
        title = "Immigration Legislation",
        # app theme ----
        theme = shinytheme("sandstone"),


        # US Map tab  -------------------------------------------------------------
        navbarMenu("Legislation US Maps",
                 icon = icon("globe-americas"),
                 
                 # overall map ----
                 tabPanel(
                     "Enacted Legislation",
                     fluid = TRUE,
                     
                     # Sidebar layout with a input and output definitions
                     sidebarLayout(
                         sidebarPanel(
                             # Input: Drop down options
                             selectInput("state",
                                         label = "Select a state in the US:",
                                         choices = state_names,
                                         selected = "All US States")
                         ),
    
    
                     mainPanel(
                         img(src = "overall_enacted.png", width = "800px"),
                         fluidRow(column(12,
                                         helpText("This map represents the percentage of immigration bills that was finally enacted after being introduced by a particular state.
                                                  The text below indicates the exact count and percentage of bills enacted."))),
                         textOutput(outputId = "state_text"),
                         br(),
                         br(),
                         dataTableOutput(outputId = "state_data")
                         )
                     )
                     ),
        
                # Undocumented individuals map ----
                tabPanel(
                    "Targets Undocumented Individuals",
                    fluid = TRUE,
                    
                    # Sidebar layout with a input and output definitions
                    sidebarLayout(
                        sidebarPanel(
                            # Input: Drop down options
                            selectInput("state_undoc",
                                        label = "Select a state in the US:",
                                        choices = state_names,
                                        selected = "All US States")
                        ),
                        
                        
                        mainPanel(
                            img(src = "undoc_indiv.png", width = "800px"),
                            fluidRow(column(12,
                                            helpText("This map represents the percentage of immigration bills introduced within a particular state that specifically targets undocumented individuals.
                                                          The text below indicates the exact percentage of bills introduced in the chosen state."))),
                            textOutput(outputId = "state_text_undoc"),
                            br(),
                            br(),
                            dataTableOutput(outputId = "state_data_undoc")
                        )
                    )
                ),
                
                # Undocumented CHILDREN map ----
                tabPanel(
                    "Targets Undocumented Children",
                    fluid = TRUE,
                    
                    # Sidebar layout with a input and output definitions
                    sidebarLayout(
                        sidebarPanel(
                            # Input: Drop down options
                            selectInput("state_undoc_child",
                                        label = "Select a state in the US:",
                                        choices = state_names,
                                        selected = "All US States")
                        ),
                        
                        
                        mainPanel(
                            img(src = "undoc_children.png", width = "800px"),
                            fluidRow(column(12,
                                            helpText("This map represents the percentage of immigration bills introduced within a particular state that specifically targets undocumented children
                                                          The text below indicates the exact percentage of bills introduced in the chosen state."))),
                            textOutput(outputId = "state_text_undoc_child"),
                            br(),
                            br(),
                            dataTableOutput(outputId = "state_data_undoc_child")
                        )
                    )
                )
                ),
        
        
        
        # bar plot tab ------------------------------------------------------------
        tabPanel("Comparisons",
                 icon = icon("chart-bar"),
                 fluid = TRUE,
                 # Sidebar layout with a input and output definitions
                 fluidRow(
                     column(4,
                            # Input: Drop down options
                           selectInput(inputId = "y_var",
                                     label = "Select Variable:",
                                     choices = list(
                                         "Decade when bill was introduced" = "decade",
                                         "Region where bill was introduced" = "region",
                                         "Enacted" = "enacted"),
                                     selected = "decade")),

                    column(4,
                           # Input: Radio buttons
                           radioButtons("bar_var",
                                      label = "Select Targeted Population:",
                                      choices = c(
                                          "Targets Undocumented",
                                          "Targets Undocumented Children",
                                          "Targets Legal Permanent Residents",
                                          "Targets Refugees and Asylees"
                                      ),
                                      selected = "Targets Undocumented"))
                         ),
                br(),
                fluidRow(
                    column(12,
                           withSpinner(plotOutput(outputId = "bar_plot"))
                           )
                     )
                ),

        # about tab --------------------------------------------------------------
        tabPanel(
            "About",
            fluid = TRUE,
            icon = icon("info-circle"),
            # add info about the project here ----
            fluidRow(
                column(6,
                       br(),
                       h4(p("About this Shiny Project:")),
                       h5(p("This project visualizes data about immigration legislation across various US states. The first tab titled 'Legislation US Maps' provides a graphical representation of three elements of immigration bills: enacted bills, bills that target undocumented individuals, and bills that target undocumented children.")),
                       h5(p("The second tab titled 'Comparisons' provides simple bar charts of the counts of bills introduced depending on the user's choice: decade or region of introduction, and number of bills enacted depedning on whether they targeted a certain population."),
                          p("The source code for this Shiny app is available ",
                            a("on GitHub", href = "https://github.com/apoorvashivaram/immigration_legislation"),
                            ".")),
                       br(),
                       h4(p("About the Dataset:")),
                       h5(p("The dataset was developed to catalog and classify all immigration-related legislation introduced in the 50 states of the United States of America since 1990. There are 12,660 bills in these data. ")),
                       h5(p("Citation:
                            Filindra, Alexandra and Shanna Pearson-Merkowitz, (2020) Database of State-Level Immigration Legislation, 1990-2015. Chicago: University of Illinois"),
                            ("Creation of the database was supported with a major grant from the Russell Sage Foundation and the Pew Charitable Trusts.")),
                       br()
                       ),
                column(6,
                       h4(p("About the Author:")),
                       img(src = "AS.jpeg", height = "200px"),
                       h5(p("Apoorva Shivaram is a PhD student in Cognitive Psychology and a Masters student in Statistics at Northwestern University."),
                          ("Over the past year, she has discovered her love for Data Science using R. This Shiny App is a project completed for one of her classes at Northwestern University."),
                          ("Shoutout to Prof. Kuyper, Prof. Coburn and the rest of the instructional team!")),
                       br()
                       )
                ),
            br(),
            hr(),
            h4("Sources:"),
            h6(p("Immigration Legislation Data from ",
                  a("Immigration Legislation",
                    href = "https://github.com/NUstat/immigration_legislation"), ".")),
            h6("Built with ",
               img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "15px"),
               " by ",
               img(src = "https://www.rstudio.com/wp-content/uploads/2014/07/RStudio-Logo-Blue-Gray.png", height = "15px"),
               ".")

        )
    )
)


# server ----
server <- function(input, output) {

    # maps of the data ---------------------------------------------------------

    # overall ----
    # return name of state chosen ----
    output$state_text <- renderText({
        paste0("You have selected ", input$state, "! ",
               "The number of bills passed by ", input$state,
               " between 1990 and 2015 is given in the table below.")
    })

    # return data table with bills passed in that state ----
    output$state_data <- renderDataTable({
        imm_dat_state %>%
            filter(state == input$state) %>%
            datatable(
                colnames = c(
                    "US state that considered this bill",
                    "Number of Bills Created",
                    "Number of Bills Passed",
                    "Percentage of Bills Passed")
                )
    })

    
    # undocumented individuals ----

    # return name of state chosen ----
    output$state_text_undoc <- renderText({
        paste0("You have selected ", input$state_undoc, "! ",
               "The number of bills introduced by ", input$state_undoc_child,
               " that targets undocumented individuals between 1990 and 2015 is given in the table below.")
    })

    # return data table with bills passed in that state ----
    output$state_data_undoc <- renderDataTable({
        imm_dat_state_undoc_child %>%
            filter(state == input$state_undoc) %>%
            datatable(
                colnames = c(
                    "US state that considered this bill",
                    "Number of Bills Created",
                    "Number of Bills Passed",
                    "Percentage of Bills Passed")
            )
    })
    
    
    # undocumented CHILDREN ----
    # return name of state chosen ----
    output$state_text_undoc_child <- renderText({
        paste0("You have selected ", input$state_undoc_child, "! ",
               "The number of bills introduced by ", input$state_undoc_child,
               " that targets undocumented children between 1990 and 2015 is given in the table below.")
    })
    
    # return data table with bills passed in that state ----
    output$state_data_undoc_child <- renderDataTable({
        imm_dat_state_undoc_child %>%
            filter(state == input$state_undoc_child) %>%
            datatable(
                colnames = c(
                    "US state that considered this bill",
                    "Number of Bills Created",
                    "Number of Bills Passed",
                    "Percentage of Bills Passed")
            )
    })
    
    
    
    # bar chart ----
    # fill variable: user supplied fill variable
    bar_var_dat <- reactive({
        switch(input$bar_var,
               "Targets Undocumented" = imm_dat$targets_undocumented,
               "Targets Undocumented Children" = imm_dat$targets_undocumented_children,
               "Targets Legal Permanent Residents" = imm_dat$targets_legal_permanent_residents,
               "Targets Refugees and Asylees" = imm_dat$targets_refugees_asylees
        )
    })

    # bar plot variables ----
    output$bar_plot <- renderPlot({

        # y-axis labels
        y_label <- switch(input$y_var,
                          "decade" = "Decade when bill was introduced",
                          "region" = "Region where bill was introduced",
                          "enacted" = "Was the bill enacted?"
        )

        # bar plot graph ----
        ggplot(imm_dat, aes_string(y = input$y_var)) +
            geom_bar(
                aes(fill = bar_var_dat()),
                width = 0.5,
                stat = "count",
                color = "gray80",
                position = position_dodge()
            ) +
            scale_fill_discrete(
                name = input$bar_var
            ) +
            scale_x_continuous(
                breaks = seq(0, 3500, 500),
                labels = seq(0, 3500, 500),
                limits = c(0, 3400),
                expand = c(0, 0)
            ) +
            labs(
                y = y_label,
                x = "Number of bills created"
            ) +
            theme_minimal() +
            theme(
                legend.direction = "horizontal",
                legend.position = "top",
                legend.justification = "center",
                legend.title.align = 0.5,
                legend.text = element_text(size = 13),
                legend.title = element_text(size = 18),
                plot.subtitle = element_text(hjust = 0.5),
                plot.background = element_rect(fill = "grey95", color = NA),
                axis.title = element_text(size = 18),
                axis.text = element_text(size = 15)
            ) +
            guides(fill = guide_legend(title.position = "top"))
    })

}


# run the application ----
shinyApp(ui = ui, server = server)



