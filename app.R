library(shiny)
library(tidyverse)
library(bslib)
library(here)
library(janitor)

#read in data
covid_food <- read_csv(here("data/covid_food.csv")) %>%
  clean_names()

#to avoid doubling US counts, delete the rows including US location
us_absent <- covid_food %>%
  filter(location != "US")

my_theme <- bs_theme(
  bg = "ivory",
  fg = "green",
  primary = "blue",
  base_font = font_google("Courier Prime")
)

ui <- fluidPage(theme = my_theme,
                navbarPage("COVID-19 Food Security",
                           tabPanel("Home",
                                    sidebarLayout(
                                      sidebarPanel(h4("Data summary:"),
                                                   h5("Data is provided by the US Census Bureau Pulse Survey and measures the impact of the coronavirus on food insecurity across the US. Attributes include: food survey responses, age, income level, mental health, education, etc. The survey dates span from April 2020 through August 2021.")

                                      ), # end sidebarPanel
                                      mainPanel(h4("Purpose of the app:"),
                                                h5("We hope to shed light on the feelings, circumstances, demographics, and food security of people in the United States during COVID-19 through visualizing:"),
                                                h5("- the change in survey responses in the U.S. based on age range"),
                                                h5("- the change in survey responses in California based on income levels"),
                                                h5("- anxiety levels by U.S. state for different weeks during the survey period"),
                                                h5("- the reasons for not working during the entire survey period for the U.S.")

                                      )

                                    ) # end sidebarLayout

                          ), #end tab 1
                           tabPanel("Age",
                                    sidebarLayout(
                                      sidebarPanel("WIDGET",
                                                   radioButtons(inputId = "pick_age",
                                                                label = h3("Age range:"),

                                                                #when using manual choices, plot will not show up
                                                                # choices = list("18-24" = 1,
                                                                #                "25-39" = 2,
                                                                #                "40-54" = 3,
                                                                #                "55-64" = 4,
                                                                #                "65 and above" = 5),
                                                                choices = unique(us_absent$age) #idk how to remove NA
                                                                #choices = unique(starwars$species)
                                                                # selected = 1

                                                   ) # end radioButtons
                                      ), # end sidebarPanel

                                      mainPanel("OUTPUT!",
                                                plotOutput("age_plot")
                                      )

                                    ) # end sidebarLayout

                           ), # end tabPanel thing 2

                           tabPanel("Income",
                                    sidebarLayout(
                                      sidebarPanel("WIDGET",
                                                   radioButtons(inputId = "pick_a", label = h3("Income level:"),
                                                                # choices = list("Less than $25,000" = 1,
                                                                #                "$25,000 - $34,999" = 2,
                                                                #                "$35,000 - $49,999" = 3,
                                                                #                "$50,000 - $74,999" = 4,
                                                                #                "$75,000 - $99,999" = 5,
                                                                #                "$100,000 - $149,999" = 6,
                                                                #                "$150,000 - $199,999" = 7,
                                                                #                "$150,000 - $199,999" = 8,
                                                                #                "Did not report" = 9
                                                                # ),
                                                                choices = unique(us_absent$income) #idk how to remove NA
                                                                # selected = 1

                                                   ) # end radioButtons
                                      ), # end sidebarPanel

                                      mainPanel("OUTPUT!",
                                                plotOutput("income_plot")
                                      )

                                    ) # end sidebarLayout

                           ) # end tabPanel thing 3
                           #
                           # tabPanel("Anxiety",
                           #          sidebarLayout(
                           #            sidebarPanel("WIDGETS",
                           #
                           #                         checkboxGroupInput(inputId = "pick_age",
                           #                                            label = h3("Anxiety frequency:"),
                           #                                            choices = list("Not at all" = 1,
                           #                                                           "Several days" = 2,
                           #                                                           "More than half the days" = 3,
                           #                                                           "Nearly every day" = 4,
                           #                                                           "Did not report" = 5)
                           #                         ), # end checkboxGroupInput
                           #                         sliderInput("slider2", label = h3("Weeks"), min = 1,
                           #                                      max = 36, value = c(1, 10)
                           #                                      ) #end sliderInput
                           #            ), # end sidebarPanel
                           #
                           #            mainPanel("OUTPUT!",
                           #                      plotOutput("sw_plot")
                           #            )
                           #
                           #          ) # end sidebarLayout
                           #
                           # ), # end tabPanel thing 4
                           # tabPanel("Work",
                           #          sidebarLayout(
                           #            sidebarPanel("WIDGET",
                           #                         selectInput("select", label = h3("Reason for not working:"),
                           #                                     choices = list("Did not want to be employed" = 1,
                           #                                                    "Sick with coronavirus symptoms" = 2,
                           #                                                    "Caring for someone with coronavirus symptoms" = 3,
                           #                                                    "Caring for children not in school or daycare" = 4,
                           #                                                    "Caring for an elderly person" = 5,
                           #                                                    "Sick (not coronavirus related) or disabled" = 6,
                           #                                                    "Retired" = 7,
                           #                                                    "Coronavirus pandemic related reduction in business (including furlough)" = 8,
                           #                                                    "Laid off due to coronavirus pandemic" = 9,
                           #                                                    "Employment closed temporarily due to the coronavirus pandemic" = 10,
                           #                                                    "Employment went out of business due to the coronavirus pandemic" = 11,
                           #                                                    "Other reason" = 12,
                           #                                                    "Did not report" = 13),
                           #                                     selected = 1
                           #                                     ) # end checkboxGroupInput
                           #            ), # end sidebarPanel
                           #
                           #            mainPanel("OUTPUT!",
                           #                      plotOutput("sw_plot")
                           #            )
                           #
                           #          ) # end sidebarLayout
                           #
                           # ), # end tabPanel thing 5
                ) # end navbarPage
) # end ui

server <- function(input, output) {

  #WIDGET 1 START
  widget1 <- reactive({
    us_absent %>%
      filter(age %in% input$pick_age) %>%
      group_by(week) %>%
      summarize(
        (sum(enough_of_the_kinds_of_food_wanted) + sum(enough_food_but_not_always_the_kinds_wanted))/
          (sum(enough_of_the_kinds_of_food_wanted) +
             sum(enough_food_but_not_always_the_kinds_wanted) +
             sum(sometimes_not_enough_to_eat) +
             sum(often_not_enough_to_eat) +
             sum(did_not_report)
           )
        ) %>%
      rename(enough_of_the_kinds_of_food_wanted_ratio = '`/`(...)')
    }) # end age_reactive

    output$age_plot <- renderPlot(
      ggplot(data = widget1(), aes(x = week, y = enough_of_the_kinds_of_food_wanted_ratio)) +
        geom_line(color = "darkgreen") +
        theme_minimal() +
        labs(y = "Number of People Indicating Enough Food", #label y
             x = "Week Number")
    ) # end output$age_plot
    #WIDGET 1 END


    # #WIDGET 2 START
    # widget1 <- reactive({
    #   us_absent %>%
    #     filter(age %in% input$pick_age) %>%
    #     group_by(week) %>%
    #     summarize(
    #       (sum(enough_of_the_kinds_of_food_wanted) + sum(enough_food_but_not_always_the_kinds_wanted))/
    #         (sum(enough_of_the_kinds_of_food_wanted) +
    #            sum(enough_food_but_not_always_the_kinds_wanted) +
    #            sum(sometimes_not_enough_to_eat) +
    #            sum(often_not_enough_to_eat) +
    #            sum(did_not_report)
    #         )
    #     ) %>%
    #     rename(enough_of_the_kinds_of_food_wanted_ratio = '`/`(...)')
    # }) # end food_reactive
    #
    # output$age_plot <- renderPlot(
    #   ggplot(data = widget1(), aes(x = week, y = enough_of_the_kinds_of_food_wanted_ratio)) +
    #     geom_line(color = "darkgreen") +
    #     theme_minimal() +
    #     labs(y = "Number of People Indicating Enough Food", #label y
    #          x = "Week Number")
    # ) # end output$income_plot
    # #WIDGET 2 END


    #WIDGET 3 START

    #WIDGET 3 END



    #WIDGET 4 START

    #WIDGET 4 END
}

# #original example
#
#   sw_reactive <- reactive({
#     starwars %>%
#       filter(species %in% input$pick_species)
#   }) # end sw_reactive
#
#   output$sw_plot <- renderPlot(
#     ggplot(data = sw_reactive(), aes(x = mass, y = height)) +
#       geom_point(aes(color = species))
#   ) # end output$sw_plot
#
#   #}) # end sw_reactive

#}

shinyApp(ui = ui, server = server)
