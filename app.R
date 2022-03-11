###############################################
#### Food Security in the face of COVID-19 ####
###############################################

# Focusing on attributes like age range, income range, anxiety levels, and reasons for not working from April 2020 through August 2021, we hope to help individuals understand more about the drivers of food insecurity (especially during the pandemic) and be better prepared in addressing food insecurity in unexpected crises.

# Authors: Erika Egg, Tiffany Hsu


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

# AGE WIDGET DF
age_widget_df <- read_csv(here("data/age_ratios.csv"))

#I NCOME WIDGET DF
income_widget_df <- read_csv(here("data/income_ratios.csv"))

# MAP WIDGET DF
map_widget_df <- read_sf(here("data/map_anxiety.shp"))

# WORK WIDGET DFs
work_widget_df <- read_csv(here("data/work_ratios.csv"))

first <- read_csv(here("data/1_27_widget_5_new.csv"))
first$response <- factor(first$response, levels = c("Did not report",
                                                    "Often Not Enough",
                                                    "Sometimes Not Enough",
                                                    "Enough - Not Always Wanted",
                                                    "Enough - Wanted"))

second <- read_csv(here("data/28_36_widget_5_new.csv"))
second$response <- factor(second$response, levels = c("Did not report",
                                                      "Often Not Enough",
                                                      "Sometimes Not Enough",
                                                      "Enough - Not Always Wanted",
                                                      "Enough - Wanted"))

### THEME ELEMENTS ----------------------

thematic::thematic_shiny() # adapts ggplot to shiny theme

### UI ----------------------
ui <- fluidPage(theme = "food.css",

                setBackgroundColor(
                  color = c("#c8e6cb", "#ffffe3"),
                  gradient = "linear",
                  direction = "top"
                ),



                navbarPage(h4(strong("COVID-19 Food Security")),

### HOME tab ----------------------

                           tabPanel(h5("Home"),



                                    titlePanel(
                                      h2(strong("Food Security in the face of COVID-19"), align = "center")),
                                    div(img(src = "covid_molecule.png"), style="text-align: center;"),


                                     mainPanel(

                                      tabsetPanel(
                                        tabPanel(h4(strong("Purpose")),

                                                 br(),

                                                 p(("We aim to help inform everyone on what age groups (in the U.S.) and income ranges (in California) have been affected most by food insecurity during COVID-19, what levels of anxiety were felt during the time, and how peoples’ reasons for not working during the time impacted their food insecurity. This could help people understand more about the drivers of food insecurity (especially during a pandemic) and about sentiments surrounding COVID-19, so that we are better prepared to address food insecurity during unexpected crises.")),

                                                 br(),

                                                 p(strong("We hope to shed light on the feelings, circumstances, demographics, and food security of people in the United States during COVID-19 through visualizing:")),
                                                 p("- the change in having enough to eat (desired + less desired foods) in the contiguous U.S. based on age range"),
                                                 p("- the change in having enough to eat (desired + less desired foods) in California based on income levels"),
                                                 p("- anxiety levels by contiguous U.S. state for different weeks during the survey period"),
                                                 p("- the change in food security responses due to the reason for not working in the entire contiguous U.S. over the full survey period"),
                                                 p("- the change in food security responses due to the reason for not working in the entire contiguous U.S. by week, with only select reasons included and a stacked bar graph format for side-by-side comparison between reasons"),

                                        ), # end tabPanel

                                        tabPanel(h4(strong("Data")),

                                                 #covid food data source
                                                 em(h2(strong("COVID-19 Food Security Data:"))),
                                                 (em("The dataset cleaned by Jack Ogozaly was downloaded from")),
                                                 a(href="https://www.kaggle.com/jackogozaly/pulse-survey-food-insecurity-data",
                                                   "Kaggle"),
                                                   em("and measures the impact of the coronavirus on food insecurity across the US. Attributes relate to: food survey responses, age, income level, mental health, education, etc. The survey dates span from April 2020 through August 2021."),
                                                 em("The original form of this data can be found at the"),
                                                 a(href="https://www.census.gov/programs-surveys/household-pulse-survey/data.html",
                                                   "US Census Bureau's Pulse Survey website."),

                                                 # shapefile source
                                                 em(h2(strong("United States Map Shapefile:"))),
                                                 (em("Data is provided by the")),
                                                 a(href="https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html",
                                                   "US Census Bureau."),
                                                 em("We selected the US state boundaries from 2018 at the 5m resolution level. States included in the shapefile data were used to filter for only states in the contiguous U.S. from the US Census Bureau Pulse Survey to ensure consistency throughout all widgets.")

                                                 ), # end tab panel

                                        tabPanel(h4(strong("Bias and Transparency")),

                                                 br(),

                                                 p(strong("Adressing bias and transparency:")),
                                                 p("We hope that our FAQ and explanations throughout our tabs inform on the process we took to prepare the data for display and make clear the information we are actually displaying. We recognize that some of the data collection situations (like how options for reasons for not working change multiple times throughout the survey period) are not ideal, but, through our explanations and wrangling, we hope to have optimized what information you can glean from the data. If you have any further questions about our process, please do not hesitate to reach out."),
                                        ) # end tabPanel

                                      ) # end tabset panel

                                    ) # end mainpanel

                          ), #end tab 1

  ### AGE tab----------------------

                           tabPanel(h5("Age"),
                                    h2(strong("Age vs. Food Security"), align = "center"),
                                    sidebarLayout(
                                      sidebarPanel("Select an age range on the left to see how many people indicated enough food (sum of the responses “enough_of_the_kinds_of_food_wanted” and “enough_food_but_not_always_the_kinds_wanted”) in that age range for the entire U.S. during each week of the survey period.",
                                                   radioButtons(inputId = "pick_age",
                                                                label = h3(strong("Age range:")),

                                                                #when using manual choices, plot will not show up
                                                                choices = list("18 - 24",
                                                                               "25 - 39",
                                                                               "40 - 54",
                                                                               "55 - 64",
                                                                               "65 and above")

                                                   ) # end radioButtons
                                      ), # end sidebarPanel

                                      mainPanel(
                                        withSpinner(plotOutput("age_plot"),
                                                    color = getOption("spinner.color", default = "#FFAA33"),
                                                    type = getOption("spinner.type", default = 4)),
                                        em("Note: “week” in this survey does not always refer to 7 days; it can be anywhere from a 5 day period to a 12 day period. See the “FAQ” tab to see what dates are associated with each week number. We have adjusted the y-axis to remain a fixed range for easier comparison when switching between the age ranges.")
                                      ) #end mainPanel

                                    ) # end sidebarLayout

                           ), # end tabPanel thing 2

  ### INCOME tab----------------------

                           tabPanel(h5("Income"),
                                    h2(strong("Income vs. Food Security"), align = "center"),
                                    sidebarLayout(
                                      sidebarPanel("Select an income range on the left to see how many people indicated enough food (sum of the responses “enough_of_the_kinds_of_food_wanted” and “enough_food_but_not_always_the_kinds_wanted”) in that income range for California during each week of the survey period.",
                                                   radioButtons(inputId = "pick_income",
                                                                label = h3(strong("Income level:")),
                                                                choices = list("Less than $25,000",
                                                                               "$25,000 - $34,999",
                                                                               "$35,000 - $49,999",
                                                                               "$50,000 - $74,999",
                                                                               "$75,000 - $99,999",
                                                                               "$100,000 - $149,999",
                                                                               "$150,000 - $199,999",
                                                                               "$200,000 and above")
                                                   ) # end radioButtons

                                      ), # end sidebarPanel

                                      mainPanel(
                                        withSpinner(plotOutput("income_plot"),
                                                  color = getOption("spinner.color", default = "#FFAA33"),
                                                  type = getOption("spinner.type", default = 4)),

                                                em("Note: “week” in this survey does not always refer to 7 days; it can be anywhere from a 5 day period to a 12 day period. See the “FAQ” tab to see what dates are associated with each week number. We have adjusted the y-axis to remain a fixed range for easier comparison when switching between the income ranges.")
                                      ) #end mainPanel

                                    ) # end sidebarLayout

                           ), # end tabPanel thing 3

  ### ANXIETY tab----------------------

                         tabPanel(h5("Anxiety"),
                                    h3(strong("Average percentage of survey responses over the course of the selected week(s) that were of the selected anxiety frequency (or frequencies)")),
                                    "Select as many anxiety frequency level survey responses on the left to visualize them on a map of the U.S. You can also use the slider bar to select a week range (default is showing over the entire survey period: Week 1-36). Selecting one anxiety frequency level will visualize the mean number of responses from people in each state over the entirety of the selected weeks. Selecting more than one anxiety frequency will display the sum of the means of the two or more selected (ie. if every checkbox is filled, every state will display 100%, as 100% of survey responses for that week range would be accounted for). This map includes only the contiguous United States. Pay close attention to the legend bar for the gradient on the right side of the map every time you change your selection: a drastic color change may be indicating a relatively small percentage difference.",

                                    sidebarLayout(
                                      sidebarPanel(
                                                   checkboxGroupInput(inputId = "pick_anxiety",
                                                                      label = h3(strong("Anxiety frequency:")),
                                                                      choices = list("Not at all",
                                                                                     "Several days",
                                                                                     "More than half the days",
                                                                                     "Nearly every day",
                                                                                     "Did not report"),
                                                                      select = "Not at all"
                                                   ), # end checkboxGroupInput

                                                   sliderInput(inputId = "pick_week",
                                                               label = h3(strong("Weeks:")),
                                                               min = 1,
                                                               max = 36,
                                                               value = c(1, 36)
                                                                ) #end sliderInput
                                      ), # end sidebarPanel

                                      mainPanel(
                                        withSpinner(plotOutput("map_plot"),
                                                            color = getOption("spinner.color", default = "#FFAA33"),
                                                            type = getOption("spinner.type", default = 4)),
                                                em("Note: “week” in this survey does not always refer to 7 days; it can be anywhere from a 5 day period to a 12 day period. See the 'FAQ' tab to see what dates are associated with each week number.")

                                      ) # end mainPanel

                                    ) # end sidebarLayout

                           ), # end tabPanel thing 4

  ### WORK tab----------------------

                            tabPanel(h5("Work"),
                                     h2(strong("Reason for not working vs. Food Security"), align = "center"),
                                     mainPanel(
                                       tabsetPanel(
                                         tabPanel(h3(("Entire survey period")),
                                                  sidebarLayout(
                                                    sidebarPanel("Select a reason for not working survey response from the dropdown on the left to see how many people indicated different levels of food security for the selected reason for not working over the entire survey period for the contiguous U.S.",
                                                                 selectInput(inputId = "pick_reason",
                                                                             label = h3(strong("Reason for not working:")),
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
                                                                                            "Did not report",
                                                                                            "I was concerned about getting or spreading the coronavirus",
                                                                                            "I was caring for someone or sick myself with coronavirus symptoms",
                                                                                            "I was laid off or furloughed due to coronavirus pandemic",
                                                                                            "I did not have tranportation to work")
                                                                 ) # end checkboxGroupInput

                                                    ), # end sidebarPanel

                                                    mainPanel(
                                                      withSpinner(plotOutput("work_plot"),
                                                                color = getOption("spinner.color", default = "#FFAA33"),
                                                                type = getOption("spinner.type", default = 4)),
                                                              em("If you would like to see the exact changes made to each reason and what weeks each reason represents, please view the “FAQ” tab. Note: “week” in this survey does not always refer to 7 days; it can be anywhere from a 5 day period to a 12 day period. See the “FAQ” tab to see what dates are associated with each week number.")
                                                    ) # end mainPanel thing 5

                                                  ) # end sidebarLayout

                                         ), # end first work tab


                                         tabPanel(h3(("By week")),
                                                  sidebarLayout(
                                                    sidebarPanel("Select a week from the dropdown on the left to see how survey responses were distributed for reasons for not working:",
                                                                 selectInput(inputId = "pick_reasonstack",
                                                                             label = h3(strong("Week:")),
                                                                             choices = 1:36)
                                                    ), # end sidebarPanel

                                                    mainPanel(
                                                      withSpinner(plotOutput("workstack_plot"),
                                                                  color = getOption("spinner.color", default = "#FFAA33"),
                                                                  type = getOption("spinner.type", default = 4)),

                                                              br(),
                                                              br(),

                                                              p("'Did not want to be employed', 'Sick with coronavirus symptoms',  'Caring for someone with coronavirus symptoms', 'Caring for children not in school or daycare', and 'Sick (not coronavirus related) or disabled' through week 27 and then 'Did not want to be employed', 'I was caring for someone or sick myself with coronavirus symptoms', 'Caring for children not in school or daycare', and 'Sick (not coronavirus related) or disabled' for week 28 onwards (when 'Sick with coronavirus symptoms' and 'Caring for someone with coronavirus symptoms' were combined into one collective reason)"),

                                                              em("Note: “week” in this survey does not always refer to 7 days; it can be anywhere from a 5 day period to a 12 day period. See the “FAQ” tab to see what dates are associated with each week number. If you would like more information on our wrangling of the changes in options to select for reasons for not working throughout the survey period, please view the “FAQ” tab.")


                                                    ) # end mainPanel thing 6

                                                  ) # end sidebarLayout

                                         ) # end tab panel

                                       ) # end tabset panel

                                     ) # end mainpanel

                           ), # end tabPanel thing 5

                          ### ADDITIONAL INFO tab----------------------

                          tabPanel(h5("FAQ"),

                                   titlePanel(h2(strong("Frequently asked questions:"), align = "center")),

                                   mainPanel(

                                     # start FAQs
                                     em(h4(strong("How are you defining food security here?"))),

                                     br(),

                                     p("For our purposes, we are saying that someone has food security if they answered “Enough of the kinds of food wanted” or if they answered “Enough food but not always the kinds wanted” as their survey response. Therefore, in our Age and Income tabs, indicating enough food is represented by the sum of these responses."),

                                     br(),

                                     em(h4(strong("Which US states are included in these outputs?"))),

                                     br(),

                                     p("For any tab that indicates data for the contiguous U.S. was used, we took information (ie. survey responses) broken down by state (excluding Alaska and Hawaii) and combined them (such as by summing) to get, for example, total survey responses in the U.S. We did not use the information already included in the dataset for the entire U.S. in order to keep the data used throughout the tabs consistent, as we wanted to use only the contiguous U.S. for our map output."),

                                     br(),

                                     em(h4(strong("What does a “week” represent in this survey?"))),

                                     br(),

                                     p("Weeks varied in length throughout the survey period, as well as excluded some dates. Below you can see exactly which dates correspond with each week number:"),

                                       p("[Week 1] April 23 - May 5 2020"),
                                       p("[Week 2] May 7 - May 12 2020"),
                                       p("[Week 3] May 14 - May 19 2020"),
                                       p("[Week 4] May 21 - May 26 2020"),
                                       p("[Week 5] May 28 - June 2 2020"),
                                       p("[Week 6] June 4 - June 9 2020"),
                                       p("[Week 7] June 11 - June 16 2020"),
                                       p("[Week 8] June 18 - June 23 2020"),
                                       p("[Week 9] June 25 - June 30 2020"),
                                       p("[Week 10] July 2 - July 7 2020"),
                                       p("[Week 11] July 9 - July 14 2020"),
                                       p("[Week 12] July 16 - July 21 2020"),
                                       p("[Week 13] August 19 - August 31 2020"),
                                       p("[Week 14] September 2 - September 14 2020"),
                                       p("[Week 15] September 16 - September 28 2020"),
                                       p("[Week 16] September 30 - October 12 2020"),
                                       p("[Week 17] October 14 - October 26 2020"),
                                       p("[Week 18] October 28 - November 9 2020"),
                                       p("[Week 19] November 11 - November 23 2020"),
                                       p("[Week 20] November 25 - December 7 2020"),
                                       p("[Week 21] December 9 - December 21 2020"),
                                       p("[Week 22] January 6 - January 18 2021"),
                                       p("[Week 23] January 20 - February 1 2021"),
                                       p("[Week 24] February 3 - February 15 2021"),
                                       p('[Week 25] February 17 - March 1 2021'),
                                       p("[Week 26] March 3 - March 15 2021"),
                                       p("[Week 27] March 17 - March 29 2021"),
                                       p("[Week 28] April 14 - April 26 2021"),
                                       p("[Week 29] April 28 - May 10 2021"),
                                       p("[Week 30] May 12 - May 24 2021"),
                                       p("[Week 31] May 26 - June 7 2021"),
                                       p("[Week 32] June 9 - June 21 2021"),
                                       p("[Week 33] June 23 - July 5 2021"),
                                       p("[Week 34] July 21 - August 2 2021"),
                                       p("[Week 35] August 4 - August 16 2021"),
                                       p("[Week 36] August 18 - August 30  2021"),

                                     br(),

                                     em(h4(strong("Could you explain more about how the reasons for not working were combined and what weeks are included for each reason?"))),
                                     br(),
                                     p("All of the unique reasons originally listed in the dataframe and the weeks associated with them are below:"),

                                     p("[1] Did not want to be employed: Weeks 1-12"),
                                     p("[2] Sick with coronavirus symptoms: Weeks 1-12"),
                                     p("[3] Caring for someone with coronavirus symptoms: Weeks 1-12"),
                                     p("[4] Caring for children not in school or daycare: Weeks 1-12"),
                                     p("[5] Caring for an elderly person: Weeks 1-12"),
                                     p("[6] Sick (not coronavirus related) or disabled: Weeks 1-12"),
                                     p("[7] Retired: Weeks 1-12"),
                                     p("[8] Coronavirus pandemic related reduction in business (including furlough): Weeks 1-12"),
                                     p("[9] Laid off due to coronavirus pandemic: Weeks 1-12"),
                                     p("[10] Employment closed temporarily due to the coronavirus pandemic: Weeks 1-12"),
                                     p("[11] Employment went out of business due to the coronavirus pandemic: Weeks 1-12"),
                                     p("[12] Other reason: Weeks 1-36"),
                                     p("[13] Did not report: Weeks 1-27 "),
                                     p("[14] I did not want to be employed at this time: Weeks 13-36"),
                                     p("[15] I was sick with coronavirus symptoms: Weeks 13-27"),
                                     p("[16] I was caring for someone with coronavirus symptoms: Weeks 13-27"),
                                     p("[17] I was caring for children not in school or daycare: Weeks 13-36"),
                                     p("[18] I was caring for an elderly person: Weeks 13-36"),
                                     p("[19] I was concerned about getting or spreading the coronavirus: Weeks 13-36"),
                                     p("[20] I was sick (not coronavirus related) or disabled: Weeks 13-36"),
                                     p("[21] I am retired: Weeks 13-36"),
                                     p("[22] My employer experienced a reduction in business (including furlough) due to coronavirus pandemic: Weeks 13-27"),
                                     p("[23] I was laid off due to coronavirus pandemic: Weeks 13-27"),
                                     p("[24] My employer closed temporarily due to the coronavirus pandemic: Weeks 13-36"),
                                     p("[25] My employer went out of business due to the coronavirus pandemic: Weeks 13-36"),
                                     p("[26] I was caring for someone or sick myself with coronavirus symptoms: Weeks 28-36"),
                                     p("[27] I was laid off or furloughed due to coronavirus pandemic: Weeks 28-36"),
                                     p("[28] I did not have tranportation to work;Note: typo was in the original data - should be “transportation: Weeks 28-36"),
                                     p("[29] Did not report reason: Weeks 28-36"),

                                     br(),

                                     h4(strong("How they were combined one-to-one (if applicable):")),
                                     br(),

                                     p("Data from 14 was added under 1’s label (Did not want to be employed) so that it now represents Weeks 1-36"),
                                     p("Data from 15 was added under 2’s label (Sick with coronavirus symptoms) so that it now represents Weeks 1-27*"),
                                     p("Data from 16 was added under 3’s label (Caring for someone with coronavirus symptoms) so that it now represents Weeks 1-27*"),
                                     p("Data from 17 was added under 4’s label (Caring for children not in school or daycare) so that it now represents Weeks 1-36"),
                                     p("Data from 18 was added under 5’s label (Caring for an elderly person) so that it now represents Weeks 1-36"),
                                     p("Data from 20 was added under 6’s label (Sick (not coronavirus related) or disabled) so that it now represents Weeks 1-36"),
                                     p("Data from 21 was added under 7’s label (Retired) so that it now represents Weeks 1-36"),
                                     p("Data from 22 was added under 8’s label (Coronavirus pandemic related reduction in business (including furlough)) so that it now represents Weeks 1-27*"),
                                     p("Data from 23 was added under 9’s label (Laid off due to coronavirus pandemic) so that it now represents Weeks 1-27*"),
                                     p("Data from 24 was added under 10’s label (Employment closed temporarily due to the coronavirus pandemic) so that it now represents Weeks 1-36"),
                                     p("Data from 25 was added under 11’s label (Employment went out of business due to the coronavirus pandemic) so that it now represents Weeks 1-36"),
                                     p("Data from 29 was added under 13’s label (Did not report) so that it now represents Weeks 1-36"),

                                     br(),

                                     h4(strong("One-to-one combinations that resulted in data for Weeks 1-27 rather than all 36 weeks have an asterisk. A new combined reason represents two separate reasons for these as follows in Weeks 28-36:")),
                                     br(),

                                     em(strong("Sick with coronavirus symptoms")), "and", em(strong("Caring for someone with coronavirus symptoms")), "combine into option", em(strong("I was caring for someone or sick myself with coronavirus symptoms")),
                                     em(strong("Coronavirus pandemic related reduction in business (including furlough)")), "and", em(strong("Laid off due to coronavirus pandemic")), " combine into ", em(strong("I was laid off or furloughed due to coronavirus pandemic")),

                                     br(),
                                     br(),

                                     p("Since wrapping the separate reasons into the combined reasons would lose some of the specificity of our data, in addition to it not being completely clear if they are equivalents (ie. “Coronavirus pandemic related reduction in business” might not necessarily mean furlough, and the combined reason only specifies furlough), we left Weeks 28-36 with the combined reason and Weeks 1-27 with the more specific, separated reasons instead of creating full 36 week data for these."),

                                     br(),

                                     (h4(strong("Our new set of unique reasons (after our changes) and the weeks now associated with them are summarized below for clarity (reasons still without full 36 week data have asterisks):"))),

                                     br(),

                                     p("[1] Did not want to be employed: Weeks 1-36"),
                                     p("[2] Sick with coronavirus symptoms: Weeks 1-27*"),
                                     p("[3] Caring for someone with coronavirus symptoms: Weeks 1-27*"),
                                     p("[4] Caring for children not in school or daycare: Weeks 1-36"),
                                     p("[5] Caring for an elderly person: Weeks 1-36"),
                                     p("[6] Sick (not coronavirus related) or disabled: Weeks 1-36"),
                                     p("[7] Retired: Weeks 1-36"),
                                     p("[8] Coronavirus pandemic related reduction in business (including furlough): Weeks 1-27*"),
                                     p("[9] Laid off due to coronavirus pandemic: Weeks 1-27*"),
                                     p("[10] Employment closed temporarily due to the coronavirus pandemic: Weeks 1-36"),
                                     p("[11] Employment went out of business due to the coronavirus pandemic: Weeks 1-36"),
                                     p("[12] Other reason: Weeks 1-36"),
                                     p("[13] Did not report: Weeks 1-36 "),
                                     p("[14] I was concerned about getting or spreading the coronavirus: Weeks 27-36*"),
                                     p("[15] I was caring for someone or sick myself with coronavirus symptoms: Weeks 27-36*"),
                                     p("[16] I was laid off or furloughed due to coronavirus pandemic: Weeks 27-36*"),
                                     p("[17] I did not have tranportation to work: Weeks 27-36*"),

                                     br(),

                                     br()

                                   ) # end mainPanel

                          ), #end tab 7

  ### ABOUT US tab----------------------

                tabPanel(h5("About us"),

                         mainPanel(

                           h2(strong(a(href="https://eeerika.github.io/egg_personal_site/",
                             "Erika Egg")), align = "center"),
                           div(img(src = "erika_profile_pic.png"), style = "text-align: center;"),

                           br(),

                           p("Some people have an idea about how they want to spend their life from a very young age — a calling so-to-speak, beckoning them in a certain direction. I’ve never been that way: almost everything interests me. I’ve always wanted to embrace all of my different passions and try to find some way to make them all work together, which is exactly what I’ve been able to explore and formulate throughout my undergraduate education. I am now a fourth year undergraduate triple majoring in Environmental Studies, Linguistics (Emphasis in Speech and Language Technologies), and History of Art and Architecture (Emphasis in Architecture and Environment) at UCSB. Next year, I hope to attend a Masters program that will help me better understand the increasingly interdisciplinary nature of the world as it applies to urban dynamics and sustainability."),

                           h2(strong(a(href="https://tifhsu88.github.io/tifhsu88/index.html",
                                       "Tiffany Hsu")), align = "center"),
                           div(img(src = "tiff_profile_pic.jpg"), style = "text-align: center;"),

                           br(),

                           p("Perhaps my love for creativity stems from my passion for dancing, but I was excited to find a new method to express creativity through a more analytical lens with data science. The act of gathering information to draw conclusions aligns with my enjoyment of helping others and contributing to something larger than myself. I am currently in the process of finding out where my passions lie within the realm of environmental studies. Currently, my interests include utilizing spatial data to address large scale processes such as land use change. Thus, I intend to complete my major in environmental studies with a minor in spatial studies and eventually work towards a masters in an environmental science program.")

                         ) # end mainPanel thing 7

                ) # end tabPanel thing 7

                ) # end navbarPage

) # end ui

### SERVER ----------------------

server <- function(input, output) {

  ### age reactive ----------------------

  widget1 <- reactive({
    age_widget_df %>%
      filter(age %in% input$pick_age)
  }) # end age_reactive

    output$age_plot <- renderPlot(
      ggplot(data = widget1(), aes(x = week, y = enough_of_the_kinds_of_food_wanted_ratio)) +
        geom_line(color = "orange",
                  size = 1.5) +
        theme_minimal() +
        labs(y = "% of People Indicating Enough Food",
             x = "Week Number") +
        scale_y_continuous(labels = scales::percent, limits = c(.5, 1)) +
        theme(text = element_text(family = "Courier",
                                  size = 15,
                                  face = "bold"),
              axis.text = element_text(size = 12))
      ) # end output$age_plot

    ### income reactive ----------------------

    widget2 <- reactive({
      income_widget_df %>%
        filter(income %in% input$pick_income)
    }) # end income_reactive

    output$income_plot <- renderPlot(
      ggplot(data = widget2(), aes(x = week, y = enough_of_the_kinds_of_food_wanted_ratio)) +
        geom_line(color = "orange",
                  size = 1.5) +
        theme_minimal() +
        labs(y = "% of People Indicating Enough Food",
             x = "Week Number") +
        scale_y_continuous(labels = scales::percent, limits = c(.5, 1)) +
        theme(text = element_text(family = "Courier",
                                  size = 15,
                                  face = "bold"),
              axis.text = element_text(size = 12))

    ) # end output$income_plot

    ### map reactive ----------------------

    widget3 <- reactive({
      map_widget_df %>%
        filter(frq_fl_ %in% input$pick_anxiety,
               week %in% input$pick_week) %>%
            group_by(locatin, frq_fl_) %>%
            mutate(mean_pct = mean(rspns_p)) %>%
            select(locatin, mean_pct, frq_fl_, geometry) %>%
            distinct() %>%
            group_by(locatin) %>%
            mutate(sum_anx = sum(mean_pct))
    }) # end map_reactive

    output$map_plot <- renderPlot(
      ggplot(data = widget3(), aes(geometry = geometry)) +
        geom_sf(aes(fill = sum_anx), color = "white", size = 0.1) +
        theme_void() +
        scale_fill_gradientn(colors = c("khaki","darkolivegreen2","seagreen")) +
        labs(fill = "% of Survey Responses") +
        theme(legend.title = element_text(size = 18),
              text = element_text(family = "Courier",
                                  size = 20,
                                  face = "bold"))
    ) # end output$map_plot
    #WIDGET 3 END


    ### work reactive ----------------------

    widget4 <- reactive({
      work_widget_df %>%
        filter(reason_not_working %in% input$pick_reason)
    }) # end work_reactive

    output$work_plot <- renderPlot(
      ggplot(data=widget4(), aes(x=factor(food_security_level, level = c("Did not report",
                                                          "Enough - Wanted",
                                                          "Enough - Not Always Wanted",
                                                          "Sometimes Not Enough",
                                                          "Often Not Enough")),
                                 y=value)) +
        geom_bar(fill = "orange", stat = "identity") +
        coord_flip() +
        theme_minimal() +
        scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
        labs(x = "Survey Response",
             y = "% of People Selected") +
        theme(text = element_text(family = "Courier",
                                  size = 15,
                                  face = "bold"),
              axis.text = element_text(size = 12))

    ) # end output$work_plot

    ### work stack reactive ----------------------
    widget5 <- reactive({

      pick_week <- as.numeric(input$pick_reasonstack)
      if (pick_week < 28) {
        first %>%
          filter(week %in% pick_week)
      }

      else {
        second %>%
          filter(week %in% pick_week)
      }

    }) # end work_stack_reactive

    output$workstack_plot <- renderPlot(
      ggplot(data=widget5(), aes(fill=response, y=values, x=reason)) +
        geom_bar(position="stack", stat="identity") +
        theme_minimal() +
        scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
        scale_x_discrete(labels = c("Children",
                                    "Caring \nfor COVID",
                                    "COVID \nsymptoms",
                                    "Unemployed \nby choice",
                                    "Sick \nnot COVID")) +
        scale_fill_manual(labels = c("Did not \nreport",
                                     "Often \nnot \nenough",
                                     "Sometimes \nnot \nenough",
                                     "Enough - \nnot always \nwanted",
                                     "Enough \nwanted"),
                          values = c("snow4", # did not report
                                     "indianred", #often not enough
                                     "orange", #sometimes not enough
                                     "palegreen3", # enough - not always wanted
                                     "seagreen")) + # enough wanted
        labs(fill = "",
             x = "Reason",
             y = "% of People Selected") +
        theme(text = element_text(family = "Courier",
                                  size = 15,
                                  face = "bold"),
              axis.text = element_text(size = 12),
              legend.text = element_text(size = 10),
              legend.position = "bottom")

    ) # end output$work_stack_plot

}


shinyApp(ui = ui, server = server)
