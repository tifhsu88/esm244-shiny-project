library(shiny)
library(tidyverse)
library(bslib)
library(here)
library(sf)
library(janitor)

# read in data
covid_food <- read_csv(here("data/covid_food.csv")) %>%
  clean_names()

# contains geometry of us states
#us_states <- read_csv(here("data/us_states.csv")) # erika created this but it loses sf features

us_states_map <- read_sf(here("data", "us_states", "cb_2018_us_state_5m.shp")) %>%
  clean_names() %>%
  rename(location = stusps) %>%
  filter(location != "VI",
         location != "GU",
         location != "MP",
         location != "AS",
         location != "PR",
         location != "HI",
         location != "AK") %>%
  #location %in% c("CA","OR","AZ","WA","NV","WY","UT","ID","CO","NM")) %>%
  select(location, geometry)

us_states_no_geom <- us_states_map %>%
   select(-geometry)

covid_food_merged_no_geom <- merge(covid_food, us_states_no_geom, by = "location")

#to avoid doubling US counts, delete the rows including US location
us_absent_no_geom <- covid_food_merged_no_geom %>%
  filter(location != "US")


us_absent <- covid_food %>%
  filter(location != "US")

# # for widget 3
plot_data <- us_absent %>%
  group_by(week, location, freq_feel_anxious) %>%
  drop_na(freq_feel_anxious) %>%
  mutate(totals = enough_of_the_kinds_of_food_wanted +
           enough_food_but_not_always_the_kinds_wanted +
           sometimes_not_enough_to_eat +
           often_not_enough_to_eat +
           did_not_report) %>%
  select(#enough_of_the_kinds_of_food_wanted,
    #enough_food_but_not_always_the_kinds_wanted,
    #sometimes_not_enough_to_eat,
    #often_not_enough_to_eat,
    #did_not_report,
    totals,
    freq_feel_anxious,
    location,
    week) %>%
  summarize(sum(totals)) %>%
  rename(responses = 'sum(totals)') %>%
  mutate(response_pct = responses / sum(responses) * 100)
  # filter(freq_feel_anxious == 'Not at all', week %in% 1:36)

covid_food_merged <- merge(plot_data, us_states_map, by = 'location')





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
                                                                choices = list("18 - 24",
                                                                               "25 - 39",
                                                                               "40 - 54",
                                                                               "55 - 64",
                                                                               "65 and above"),
                                                                # choices = unique(us_absent$age) #idk how to remove NA
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
                                                   radioButtons(inputId = "pick_income", label = h3("Income level:"),
                                                                choices = list("Less than $25,000",
                                                                               "$25,000 - $34,999",
                                                                               "$35,000 - $49,999",
                                                                               "$50,000 - $74,999",
                                                                               "$75,000 - $99,999",
                                                                               "$100,000 - $149,999",
                                                                               "$150,000 - $199,999",
                                                                               "$200,000 and above",
                                                                               "Did not report"),
                                                                # choices = unique(us_absent$income) #idk how to remove NA
                                                                # selected = 1

                                                   ) # end radioButtons
                                      ), # end sidebarPanel

                                      mainPanel("OUTPUT only in CA!",
                                                plotOutput("income_plot")
                                      )

                                    ) # end sidebarLayout

                           ), # end tabPanel thing 3

                           tabPanel("Anxiety",
                                    sidebarLayout(
                                      sidebarPanel("WIDGETS",

                                                   checkboxGroupInput(inputId = "pick_anxiety",
                                                                      label = h3("Anxiety frequency:"),
                                                                      choices = list("Not at all",
                                                                                     "Several days",
                                                                                     "More than half the days",
                                                                                     "Nearly every day",
                                                                                     "Did not report")
                                                                      # choices = unique(us_absent$freq_feel_anxious) #idk how to remove NA
                                                   ), # end checkboxGroupInput
                                                   sliderInput(inputId = "pick_week",
                                                               label = h3("Weeks"),
                                                               min = 1,
                                                               max = 36,
                                                               value = c(1, 10)
                                                                ) #end sliderInput
                                      ), # end sidebarPanel

                                      mainPanel("OUTPUT!",
                                                plotOutput("map_plot")
                                      )

                                    ) # end sidebarLayout

                           ), # end tabPanel thing 4
                           tabPanel("Work",
                                    sidebarLayout(
                                      sidebarPanel("WIDGET",
                                                   selectInput(inputId = "pick_reason", label = h3("Reason for not working:"),
                                                               choices = list("Did not want to be employed",
                                                                              "Sick with coronavirus symptoms",
                                                                              "Caring for someone with coronavirus symptoms",
                                                                              "Caring for children not in school or daycare",
                                                                              "Caring for an elderly person",
                                                                              "Sick (not coronavirus related) or disabled",
                                                                              "Retired" ,
                                                                              "Coronavirus pandemic related reduction in business (including furlough)",
                                                                              "Laid off due to coronavirus pandemic",
                                                                              "Employment closed temporarily due to the coronavirus pandemic",
                                                                              "Employment went out of business due to the coronavirus pandemic",
                                                                              "Other reason",
                                                                              "Did not report")
                                                               # choices = unique(us_absent_no_geom$reason_not_working) #idk how to remove NA
                                                               # selected = 1
                                                               ) # end checkboxGroupInput
                                      ), # end sidebarPanel

                                      mainPanel("OUTPUT!",
                                                plotOutput("work_plot")
                                      )

                                    ) # end sidebarLayout

                           ), # end tabPanel thing 5
                ) # end navbarPage
) # end ui

server <- function(input, output) {

  #WIDGET 1 START
  widget1 <- reactive({
    us_absent_no_geom %>%
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
        labs(y = "Percentage of People Indicating Enough Food", #label y
             x = "Week Number") +
        #ylim(.5, 1) +
        scale_y_continuous(labels = scales::percent, limits = c(.5, 1))
    ) # end output$age_plot
    #WIDGET 1 END


    #WIDGET 2 START
    widget2 <- reactive({
      us_absent_no_geom %>%
        filter(location == "CA") %>%
        filter(income %in% input$pick_income) %>%
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
    }) # end food_reactive

    output$income_plot <- renderPlot(
      ggplot(data = widget2(), aes(x = week, y = enough_of_the_kinds_of_food_wanted_ratio)) +
        geom_line(color = "darkgreen") +
        theme_minimal() +
        labs(y = "Percentage of People Indicating Enough Food", #label y
             x = "Week Number") +
      #ylim(0.5, 1)
        scale_y_continuous(labels = scales::percent, limits = c(.5, 1))

    ) # end output$income_plot
    #WIDGET 2 END



    #WIDGET 3 START
    widget3 <- reactive({
      covid_food_merged %>%
        filter(freq_feel_anxious %in% input$pick_anxiety,
               week %in% input$pick_week) %>%
        group_by(location, freq_feel_anxious) %>%
        mutate(mean_pct = mean(response_pct)) %>%
        select(location, mean_pct, freq_feel_anxious, geometry) %>%
        distinct() %>%
        group_by(location) %>%
        mutate(sum_anx = sum(mean_pct))
    }) # end food_reactive

    output$map_plot <- renderPlot(
      ggplot(data = widget3(), aes(geometry = geometry)) +
        geom_sf(aes(fill = sum_anx), color = "white", size = 0.1) +
        theme_void() +
        scale_fill_gradientn(colors = c("palegreen","green","darkgreen")) +
        theme(legend.title = element_blank()) +
        labs(title = "Cumulative sum of the mean percentage of survey responses over the course of the selected week(s) that were of the selected anxiety frequency (or frequencies)")
    ) # end output$map_plot
    #WIDGET 3 END



    #WIDGET 4 START
    widget4 <- reactive({
      us_absent_no_geom %>%
        filter(reason_not_working %in% input$pick_reason) %>%
        mutate(sum_1 = sum(enough_of_the_kinds_of_food_wanted)/
                 (sum(enough_of_the_kinds_of_food_wanted) +
                    sum(enough_food_but_not_always_the_kinds_wanted) +
                    sum(sometimes_not_enough_to_eat) +
                    sum(often_not_enough_to_eat) +
                    sum(did_not_report)),
               sum_2 = sum(enough_food_but_not_always_the_kinds_wanted)/
                 (sum(enough_of_the_kinds_of_food_wanted) +
                    sum(enough_food_but_not_always_the_kinds_wanted) +
                    sum(sometimes_not_enough_to_eat) +
                    sum(often_not_enough_to_eat) +
                    sum(did_not_report)),
               sum_3 = sum(sometimes_not_enough_to_eat)/
                 (sum(enough_of_the_kinds_of_food_wanted) +
                    sum(enough_food_but_not_always_the_kinds_wanted) +
                    sum(sometimes_not_enough_to_eat) +
                    sum(often_not_enough_to_eat) +
                    sum(did_not_report)),
               sum_4 = sum(often_not_enough_to_eat)/
                 (sum(enough_of_the_kinds_of_food_wanted) +
                    sum(enough_food_but_not_always_the_kinds_wanted) +
                    sum(sometimes_not_enough_to_eat) +
                    sum(often_not_enough_to_eat) +
                    sum(did_not_report)),
               sum_5 = sum(did_not_report)/
                 (sum(enough_of_the_kinds_of_food_wanted) +
                    sum(enough_food_but_not_always_the_kinds_wanted) +
                    sum(sometimes_not_enough_to_eat) +
                    sum(often_not_enough_to_eat) +
                    sum(did_not_report))) %>%
        select(sum_1, sum_2, sum_3, sum_4, sum_5) %>%
        head(1) %>%
        t() %>%
        as.data.frame() %>%
        rename(values = "1")%>%
        add_column(name = c("Enough - Wanted",
                            "Enough - Not Always Wanted",
                            "Sometimes Not Enough",
                            "Often Not Enough",
                            "Did not report"))
    }) # end food_reactive

    output$work_plot <- renderPlot(
      ggplot(data=widget4(), aes(x=reorder(name, values), y=values)) +
        geom_bar(fill = "darkgreen", stat = "identity") +
        coord_flip() +
        theme_minimal() +
        scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
        labs(x = "Survey Response",
             y = "Percentage of People Selected")
    ) # end output$work_plot
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
