library(shiny)
library(tidyverse)
library(bslib)
library(here)
library(sf)
library(janitor)
library(shinycssloaders) #loader icon when loading outputs
library(thematic) # adapts ggplot to shiny theme
library(shinyWidgets) # background gradient

# read in data
covid_food <- data.table::fread(here("data/covid_food.csv")) %>%
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
         location != "AK",
         location != "DC") %>%
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

#widget 5
first <- read_csv(here("data/1_27_widget_5_new.csv"))
#first <- read_csv("1_27_widget_5.csv")
first$response <- factor(first$response, levels = c("Did not report",
                                                    "Often Not Enough",
                                                    "Sometimes Not Enough",
                                                    "Enough - Not Always Wanted",
                                                    "Enough - Wanted"))
second <- read_csv(here("data/28_36_widget_5_new.csv"))
#second <- read_csv("28_36_widget_5.csv")
second$response <- factor(second$response, levels = c("Did not report",
                                                      "Often Not Enough",
                                                      "Sometimes Not Enough",
                                                      "Enough - Not Always Wanted",
                                                      "Enough - Wanted"))

### THEME ELEMENTS ----------------------

# my_theme <- bs_theme(
#   bg = "ivory",
#   fg = "green",
#   primary = "blue",
#   heading_font = font_google("Courier Prime"),
#   base_font = font_google("Jost")
# )

thematic::thematic_shiny() # adapts ggplot to shiny theme

### UI ----------------------

ui <- fluidPage(theme = "food.css",

                setBackgroundColor(
                  color = c("#c8e6cb", "#ffffe3"),
                  gradient = "linear",
                  direction = "top"
                ),



                navbarPage(h4(strong("COVID-19 Food Security")),
                           tabPanel(h5("Home"),

                                    titlePanel(h2(strong("Food Security in the face of COVID-19"), align = "center")),

                                     mainPanel(
                                      tabsetPanel(
                                        tabPanel(h4(strong("Purpose")),
                                                 p(strong("We hope to shed light on the feelings, circumstances, demographics, and food security of people in the United States during COVID-19 through visualizing:")),
                                                 p("- the change in having enough to eat (desired + less desired foods) in the U.S. based on age range"),
                                                 p("- the change in having enough to eat (desired + less desired foods) in California based on income levels"),
                                                 p("- anxiety levels by U.S. state for different weeks during the survey period"),
                                                 p("- the change in food security responses due to the reason for not working in the entire U.S. over the full survey period")),

                                        tabPanel(h4(strong("Data")),
                                                 "Data is provided by the US Census Bureau Pulse Survey and measures the impact of the coronavirus on food insecurity across the US. Attributes include: food survey responses, age, income level, mental health, education, etc. The survey dates span from April 2020 through August 2021.")
                                        # sidebarLayout(
                                    #   sidebarPanel(h2(strong("Data summary:")),
                                    #                ("Data is provided by the US Census Bureau Pulse Survey and measures the impact of the coronavirus on food insecurity across the US. Attributes include: food survey responses, age, income level, mental health, education, etc. The survey dates span from April 2020 through August 2021.")

                                      # ), # end sidebarPanel
                                      # mainPanel(h2(strong("Purpose of the app:")),
                                      #           h3("We hope to shed light on the feelings, circumstances, demographics, and food security of people in the United States during COVID-19 through visualizing:"),
                                      #           h3("- the change in having enough to eat (desired + less desired foods) in the U.S. based on age range"),
                                      #           h3("- the change in having enough to eat (desired + less desired foods) in California based on income levels"),
                                      #           h3("- anxiety levels by U.S. state for different weeks during the survey period"),
                                      #           h3("- the change in food security responses due to the reason for not working in the entire U.S. over the full survey period")

                                      )

                                    ) # end sidebarLayout

                          ), #end tab 1
                           tabPanel(h5("Age"),
                                    h2(strong("Age vs. Food Security"), align = "center"),
                                    sidebarLayout(
                                      sidebarPanel("Select an age range on the left to see how many people indicated enough food (sum of the responses “enough of the kinds of food wanted” and “enough food but not always the kinds wanted”) in that age range for the entire U.S. during each week of the survey period.",
                                                   radioButtons(inputId = "pick_age",
                                                                label = h3("Age range:"),

                                                                #when using manual choices, plot will not show up
                                                                choices = list("18 - 24",
                                                                               "25 - 39",
                                                                               "40 - 54",
                                                                               "55 - 64",
                                                                               "65 and above")

                                                   ) # end radioButtons
                                      ), # end sidebarPanel

                                      mainPanel(
                                                plotOutput("age_plot"),
                                                "Note: “week” in this survey does not always refer to 7 days; it can be anywhere from a 5 day period to a 12 day period. See the “Additional Information” tab to see what dates are associated with each week number. We have adjusted the y-axis to remain a fixed range for easier comparison when switching between the age ranges."
                                      )

                                    ) # end sidebarLayout

                           ), # end tabPanel thing 2

                           tabPanel(h5("Income"),
                                    h2(strong("Income vs. Food Security"), align = "center"),
                                    sidebarLayout(
                                      sidebarPanel("Select an income range on the left to see how many people indicated enough food (sum of the responses “enough of the kinds of food wanted” and “enough food but not always the kinds wanted”) in that age range for the entire U.S. during each week of the survey period.",
                                                   radioButtons(inputId = "pick_income", label = h3("Income level:"),
                                                                choices = list("Less than $25,000",
                                                                               "$25,000 - $34,999",
                                                                               "$35,000 - $49,999",
                                                                               "$50,000 - $74,999",
                                                                               "$75,000 - $99,999",
                                                                               "$100,000 - $149,999",
                                                                               "$150,000 - $199,999",
                                                                               "$200,000 and above",
                                                                               "Did not report")

                                                   ) # end radioButtons
                                      ), # end sidebarPanel

                                      mainPanel(plotOutput("income_plot"),
                                                em("Note: “week” in this survey does not always refer to 7 days; it can be anywhere from a 5 day period to a 12 day period. See the “Additional Information” tab to see what dates are associated with each week number. We have adjusted the y-axis to remain a fixed range for easier comparison when switching between the age ranges.")
                                      )

                                    ) # end sidebarLayout

                           ), # end tabPanel thing 3

                           tabPanel(h5("Anxiety"),
                                    h2(strong("Cumulative sum of the mean percentage of survey responses over the course of the selected week(s) that were of the selected anxiety frequency (or frequencies)")),
                                    "Select as many anxiety frequency level survey responses on the left to visualize them on a map of the U.S. You can also use the slider bar to select a week range (default is showing over the entire survey period: Week 1-36). Selecting one anxiety frequency level will visualize the mean number of responses from people in each state over the entirety of the selected weeks. Selecting more than one anxiety frequency will display the sum of the means of the two or more selected (ie. if every checkbox is filled, every state will display 100%, as 100% of survey responses for that week range would be accounted for). This map includes only the contiguous United States. Pay close attention to the legend bar for the gradient on the right side of the map every time you change your selection: a drastic color change may be indicating a relatively small percentage difference.",

                                    sidebarLayout(
                                      sidebarPanel(

                                                   checkboxGroupInput(inputId = "pick_anxiety",
                                                                      label = h3("Anxiety frequency:"),
                                                                      choices = list("Not at all",
                                                                                     "Several days",
                                                                                     "More than half the days",
                                                                                     "Nearly every day",
                                                                                     "Did not report")
                                                   ), # end checkboxGroupInput
                                                   sliderInput(inputId = "pick_week",
                                                               label = h3("Weeks:"),
                                                               min = 1,
                                                               max = 36,
                                                               value = c(1, 10)
                                                                ) #end sliderInput
                                      ), # end sidebarPanel

                                      mainPanel(plotOutput("map_plot"),
                                                "Note: “week” in this survey does not always refer to 7 days; it can be anywhere from a 5 day period to a 12 day period. See the “Additional Information” tab to see what dates are associated with each week number."

                                      )

                                    ) # end sidebarLayout

                           ), # end tabPanel thing 4
                           tabPanel(h5("Work"),
                                    h2(strong("Reason for not working vs. Food Security"), align = "center"),


                                    sidebarLayout(
                                      sidebarPanel("Select a reason for not working survey response from the dropdown on the left to see how many people indicated different levels of food security for the selected reason for not working over the entire survey period for the U.S.",
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
                                                               ) # end checkboxGroupInput
                                      ), # end sidebarPanel

                                      mainPanel(plotOutput("work_plot"),
                                                "Note: “week” in this survey does not always refer to 7 days; it can be anywhere from a 5 day period to a 12 day period. See the “Additional Information” tab to see what dates are associated with each week number."
                                      ) # end mainPanel thing 5

                                    ) # end sidebarLayout

                           ), # end tabPanel thing 5
                          tabPanel(h5("Work Stack"),
                                   sidebarLayout(
                                     sidebarPanel("WIDGET",
                                                  selectInput(inputId = "pick_reasonstack", label = h3("Week:"),
                                                              choices = 1:36)
                                                  # choices = unique(us_absent_no_geom$reason_not_working) #idk how to remove NA
                                                  # selected = 1
                                                  # end checkboxGroupInput
                                     ), # end sidebarPanel

                                     mainPanel("OUTPUT!",
                                               plotOutput("workstack_plot")
                                     ) # end mainPanel thing 6

                                   ) # end sidebarLayout

                          ), # end tabPanel thing 6

                          tabPanel(h5("Additional info"),

                                   titlePanel(h1(strong("fill in additional info"), align = "left")),

                                   sidebarLayout(
                                     sidebarPanel(h2(strong("Data summary:")),
                                                  ("blah blah")

                                     ), # end sidebarPanel

                                     mainPanel(h2(strong("Purpose of the app:")),
                                               h3("We hope to shed light on the feelings, circumstances, demographics, and food security of people in the United States during COVID-19 through visualizing:"),
                                               h3("- the change in having enough to eat (desired + less desired foods) in the U.S. based on age range"),
                                               h3("- the change in having enough to eat (desired + less desired foods) in California based on income levels"),
                                               h3("- anxiety levels by U.S. state for different weeks during the survey period"),
                                               h3("- the change in food security responses due to the reason for not working in the entire U.S. over the full survey period")



                                   ) # end sidebarLayout

                          ), #end tab 7
                ), # end tabPanel thing 7

                tabPanel(h5("About us"),

                         mainPanel(
                           h2(strong("Erika Egg"), align = "center"),
                           div(img(src = "erika_profile_pic.png"), style = "text-align: center;"),

                           p("Some people have an idea about how they want to spend their life from a very young age — a calling so-to-speak, beckoning them in a certain direction. I’ve never been that way: almost everything interests me. I’ve always wanted to embrace all of my different passions and try to find some way to make them all work together, which is exactly what I’ve been able to explore and formulate throughout my undergraduate education. I am now a fourth year undergraduate triple majoring in Environmental Studies, Linguistics (Emphasis in Speech and Language Technologies), and History of Art and Architecture (Emphasis in Architecture and Environment) at UCSB. Next year, I hope to attend a Masters program that will help me better understand the increasingly interdisciplinary nature of the world as it applies to urban dynamics and sustainability."),

                           h2(strong("Tiffany Hsu"), align = "center"),
                           div(img(src = "tiff_profile_pic.jpg"), style = "text-align: center;"),

                           "Perhaps my love for creativity stems from my passion for dancing, but I was excited to find a new method to express creativity through a more analytical lens with data science. The act of gathering information to draw conclusions aligns with my enjoyment of helping others and contributing to something larger than myself. I am currently in the process of finding out where my passions lie within the realm of environmental studies. Currently, my interests include utilizing spatial data to address large scale processes such as land use change. Thus, I intend to complete my major in environmental studies with a minor in spatial studies and eventually work towards a masters in an environmental science program."

                         ) # end mainPanel thing 7

                ) # end tabPanel thing 7

                ) # end navbarPage


) # end ui

### SERVER ----------------------

server <- function(input, output) {

  #WIDGET 1 START
  widget1 <- reactive({
    us_absent_no_geom %>%
      filter(age %in% input$pick_age) %>%
      group_by(week) %>%
      summarize(enough_of_the_kinds_of_food_wanted_ratio =
        (sum(enough_of_the_kinds_of_food_wanted) + sum(enough_food_but_not_always_the_kinds_wanted))/
          (sum(enough_of_the_kinds_of_food_wanted) +
             sum(enough_food_but_not_always_the_kinds_wanted) +
             sum(sometimes_not_enough_to_eat) +
             sum(often_not_enough_to_eat) +
             sum(did_not_report)
           )
        ) #%>%
      #rename(enough_of_the_kinds_of_food_wanted_ratio = '`/`(...)')
    }) # end age_reactive

    output$age_plot <- renderPlot(
      ggplot(data = widget1(), aes(x = week, y = enough_of_the_kinds_of_food_wanted_ratio)) +
        geom_line(color = "seagreen",
                  size = 1.5) +
        theme_minimal() +
        labs(y = "% of People Indicating Enough Food", #label y
             x = "Week Number") +
        #ylim(.5, 1) +
        scale_y_continuous(labels = scales::percent, limits = c(.5, 1)) +
        theme(text = element_text(family = "Courier",
                                  size = 15,
                                  face = "bold"),
              axis.text = element_text(size = 12))
      ) # end output$age_plot
    #WIDGET 1 END


    #WIDGET 2 START
    widget2 <- reactive({
      us_absent_no_geom %>%
        filter(location == "CA") %>%
        filter(income %in% input$pick_income) %>%
        group_by(week) %>%
        summarize(enough_of_the_kinds_of_food_wanted_ratio =
          (sum(enough_of_the_kinds_of_food_wanted) + sum(enough_food_but_not_always_the_kinds_wanted))/
            (sum(enough_of_the_kinds_of_food_wanted) +
               sum(enough_food_but_not_always_the_kinds_wanted) +
               sum(sometimes_not_enough_to_eat) +
               sum(often_not_enough_to_eat) +
               sum(did_not_report)
            )
        ) #%>%
       # rename(enough_of_the_kinds_of_food_wanted_ratio = '`/`(...)')
    }) # end food_reactive

    output$income_plot <- renderPlot(
      ggplot(data = widget2(), aes(x = week, y = enough_of_the_kinds_of_food_wanted_ratio)) +
        geom_line(color = "seagreen",
                  size = 1.5) +
        theme_minimal() +
        labs(y = "% of People Indicating Enough Food", #label y
             x = "Week Number") +
      #ylim(0.5, 1)
        scale_y_continuous(labels = scales::percent, limits = c(.5, 1)) +
        theme(text = element_text(family = "Courier",
                                  size = 15,
                                  face = "bold"),
              axis.text = element_text(size = 12))

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
        scale_fill_gradientn(colors = c("khaki","darkolivegreen2","seagreen")) +
        labs(fill = "% Survey Response") +
        theme(legend.title = element_text(size = 20),
              text = element_text(family = "Courier",
                                  size = 20,
                                  face = "bold"))
        #labs(title = "Cumulative sum of the mean percentage of survey responses over the course of the selected week(s) that were of the selected anxiety frequency (or frequencies)")
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
        rename(values = "V1")%>%
        add_column(name = c("Enough - Wanted",
                            "Enough - Not Always Wanted",
                            "Sometimes Not Enough",
                            "Often Not Enough",
                            "Did not report"))
    }) # end food_reactive

    output$work_plot <- renderPlot(
      ggplot(data=widget4(), aes(x=reorder(name, values), y=values)) +
        geom_bar(fill = "seagreen", stat = "identity") +
        coord_flip() +
        theme_minimal() +
        scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
        labs(x = "Survey Response",
             y = "Percentage of People Selected") +
        theme(text = element_text(family = "Courier",
                                  size = 15,
                                  face = "bold"),
              axis.text = element_text(size = 12))

    ) # end output$work_plot
    #WIDGET 4 END

    widget5 <- reactive({
      #if (input$pick_reasonstack <= 27) {
      #first %>%
      #pivot_longer(!c(reason, name), names_to="week",values_to="values") %>%
      #filter(week %in% input$pick_reasonstack) %>%
      #rename(response = name) #%>%
      #factor(first$response, levels = c("Did not report",
      #"Often Not Enough",
      #"Sometimes Not Enough",
      #"Enough - Not Always Wanted",
      #"Enough - Wanted"))
      #}

      if (input$pick_reasonstack < 28) {
        first %>%
          #pivot_longer(!c(reason, name), names_to="week",values_to="values") %>%
          filter(week %in% input$pick_reasonstack) #%>%
        #rename(response = name)  #%>%
        #factor(second$response, levels = c("Did not report",
        #"Often Not Enough",
        #"Sometimes Not Enough",
        #"Enough - Not Always Wanted",
        #"Enough - Wanted"))
      }

      else {
        second %>%
          #pivot_longer(!c(reason, name), names_to="week",values_to="values") %>%
          filter(week %in% input$pick_reasonstack) #%>%
        #rename(response = name) #%>%
        #factor(first$response, levels = c("Did not report",
        #"Often Not Enough",
        #"Sometimes Not Enough",
        #"Enough - Not Always Wanted",
        #"Enough - Wanted"))
      }

    }) # end food_reactive

    output$workstack_plot <- renderPlot(
      ggplot(data=widget5(), aes(fill=response, y=values, x=reason)) +
        geom_bar(position="stack", stat="identity") +
        theme_minimal() +
        scale_fill_manual(values = c("snow4", # did not report
                                     "indianred", #often not enough
                                     "orange", #sometimes not enough
                                     "palegreen3", # enough - not always wanted
                                     "seagreen")) + # enough wanted
        theme(text = element_text(family = "Courier",
                                  size = 20,
                                  face = "bold"),
              axis.text = element_text(size = 12))

    ) # end output$work_plot
    #WIDGET 5 END

}


shinyApp(ui = ui, server = server)
