# Install and load the shiny package
if (!require("shiny")) install.packages("shiny")
if (!require("shinysurveys")) install.packages("shinysurveys")
if (!require("jpeg")) install.packages("jpeg")
if (!require("grid")) install.packages("grid")
library(jpeg)
library(shiny)
library(shinysurveys)
library(grid)

## Load image for Q 11 ## 
zones_11 <- readJPEG("C:/Users/bgura/Desktop/FW fishing survey/management_zones.jpg")
grid.raster(zones_11)



# Define the UI
ui <- fluidPage(
  titlePanel("2024-2025 Freshwater Fishing Survey"),
  
  mainPanel(
    uiOutput("questionsUI"),
    br(),
    actionButton("prevBtn", "Previous", class = "btn-primary"),
    actionButton("nextBtn", "Next", class = "btn-primary"),
    br(),
    br(),
    textOutput("thankYou")
  )
)

# Define the server logic
server <- function(input, output, session) {
  
  # Reactive value to track the current section
  currentSection <- reactiveVal(1)
  
  
  # Section titles
  sections <- c("General Questions", "MyCatch and Fishing Apps", "Activities", "Motivations","Major Purchases/Investments", "Freshwater Packages", "Demographics", "Comments")
  
  # Skip to Question 13 logic based on q2 input
  observeEvent(input$q2, {
    if (input$q2 == 0) {  # If the user selects "No" for Question 2
      currentSection(which(sections == "Motivations"))  # Skip to the section with Question 13
    }
  })
  #skip to q8 for q3
  observeEvent(input$q3, {
    if (input$q3 == 0) {  # If the user selects "No" for Question 3
      currentSection(which(sections == "Activities"))  # Skip to the section with Question 8
    }
  })
  
  # Add reactive value for total fishing days
  totalFishingDays <- reactive({
    # Get all fishing days inputs
    days <- sapply(1:12, function(i) {
      day_value <- input[[paste0("daysFishing_", i)]]
      # Convert to numeric, treating empty or non-numeric as 0
      if (is.null(day_value) || day_value == "" || !grepl("^\\d+$", day_value)) {
        return(0)
      }
      return(as.numeric(day_value))
    })
    # Sum all days
    sum(days, na.rm = TRUE)
  })
  
  
  
  # Define the survey questions for each section
  output$questionsUI <- renderUI({
    switch(sections[currentSection()],
           
           # General Questions Section
           "General Questions" = tagList(
             h3("General Questions"),
             # Question 1
             radioButtons("q1", "1. Did you complete a 2024-2025 iSEA survey distributed by Fisheries and Oceans Canada?",
                          choices = c("Yes" = 1, "No" = 0),
                          selected = character(0)),
             # Question 2
             radioButtons("q2", "2. Did you fish in British Columbia freshwater under your April 2024 to March 2025 freshwater fishing license?",
                          choices = c("Yes" = 1, "No (skip to 13)" = 0),
                          selected = character(0)),
           ),
           
           # MyCatch Section
           "MyCatch and Fishing Apps" = tagList(
             # Question 3
             h3("MyCatch and Fishing Apps"),
             radioButtons("q3", "3. Do you actively use the MyCatch app or other fishing apps to record your fishing activities?",
                          choices = c("Only MyCatch" = 3, "MyCatch and other apps (specify)" = 2, "Only other apps (specify)" = 1, "No (skip to 8)" = 0),
                          selected = character(0)),
             conditionalPanel(
               condition = "input.q3 == 1",
               textInput("q3_specify", "Please specify the other apps:")
             ),
             conditionalPanel(
               condition = "input.q3 == 2",
               textInput("q3_specify", "Please specify the other apps:")
             ),
             
             # Question 4
             selectInput("q4", 
                         "4. What percentage of fishing trips do you report on fishing apps?", 
                         choices = c(" " = 0, "1-10%" = 1, "11-20%" = 2, "21-30%" = 3, "31-40%" = 4, "41-50%" = 5, "51 - 60%" = 6, "61-70%" = 7, "71-80%" = 8, "81-90%" = 9, "91-99%" = 10, "100%" = 11),
                         selected = NULL),
             
             # Question 5
             radioButtons("q5_yesno", 
                          "5. If you catch no fish on a trip, do you still record that trip?", 
                          choices = c("Yes" = "yes", "No" = "no"),
                          selected = character(0)),
             conditionalPanel(
               condition = "input.q5_yesno == 'yes' || input.q5_yesno == 'no'",
               textInput("q5_explain", "Please explain:")
             ),
             # Question 6
             radioButtons("q6_yesno", 
                          "6. Do you record every fish caught on fishing apps?", 
                          choices = c("Yes" = "yes", "No" = "no"),
                          selected = character(0)),
             conditionalPanel(
               condition = "input.q6_yesno == 'yes' || input.q6_yesno == 'no'",
               textInput("q6_explain", "Please explain:")
             ),
             # Question 7
             radioButtons("q7_yesno", 
                          "7. Do you record every fish released on fishing apps?", 
                          choices = c("Yes" = "yes", "No" = "no"),
                          selected = character(0)),
             conditionalPanel(
               condition = "input.q7_yesno == 'yes' || input.q7_yesno == 'no'",
               textInput("q7_explain", "Please explain:")
             ),
           ),
           
           # Activities Section
           "Activities" = tagList(
             h3("Activities"),
             # Question 8
             h4("8. How many days per month did you fish in freshwater? For each month, provide approximate numbers of fish caught and fish released across all species. A particular date on which you fished recreationally for any length of time counts as one day fished"),
             
             # Table with alternate row colors
             fluidRow(
               column(12,
                      tags$head(
                        tags$style(HTML("
             #fishingTable table {
               width: 100%;
               border-collapse: collapse;
             }
             #fishingTable table tr:nth-child(odd) {
               background-color: #f2f2f2; /* Light grey for odd rows */
             }
             #fishingTable table tr:nth-child(even) {
               background-color: #ffffff; /* White for even rows */
             }
             #fishingTable table th {
               position: sticky;
               top: 0;
               z-index: 1;
               background-color: #6495ED; /* Cornflower Blue */
               color: white;
               padding: 8px;
               text-align: left;
               border: 1px solid #ddd;
             }
             #fishingTable table td {
               padding: 8px;
               text-align: left;
               border: 1px solid #ddd;
             }
           "))
                      ),
                      tableOutput("fishingTable") # This will use the CSS for styling
               )
             ),
             
             
             br(),
             
             
             # Question 9
             numericInput("q9", "9. On average, how many hours per day did you fish in freshwater in British Columbia on the days you fished between April 2024 and March 2025? ", value = 0, min = 0, max = 24),
             
             # Question 10
             checkboxGroupInput(
               "q10",
               "10. Please select all species you fished for (targeted) between April 2024 and March 2025 while fishing in freshwater:",
               choices = c(
                 "Sockeye Salmon", "Chinook Salmon", "Coho Salmon", "Pink Salmon", "Chum Salmon",
                 "Rainbow Trout", "Cutthroat Trout", "Lake Trout", "Steelhead", "Brook Trout",
                 "Dolly Varden/Bull Trout", "Other Trout", "Kokanee", "Grayling", "Burbot",
                 "Walleye", "White Sturgeon", "Smallmouth/largemouth bass", 
                 "Panfish (sunfish/bluegill/pumpkinseed)", "Perch", "Pike", "Other species"
               )
             ),
             
             br(),
             # Question 11
             tagList(
               h4(
                 "11. Please indicate which management zones (1-8) you fished in freshwater in each month fished ",
                 tags$a(
                   href = "management_zones.jpg",  # Link to the image in the 'www' folder
                   target = "_blank",              
                   tags$img(src = "management_zones.jpg", width = "200px", alt = "[View Map]")  # Clickable image
                 )
               ),
               
               fluidRow(
                 column(12,
                        # CSS styling for alternating row colors
                        tags$head(
                          tags$style(HTML("
               #zonesTable table {
                 width: 100%;
                 border-collapse: collapse;
               }
               #zonesTable table tr:nth-child(odd) {
                 background-color: #f2f2f2; /* Light grey for odd rows */
               }
               #zonesTable table tr:nth-child(even) {
                 background-color: #ffffff; /* White for even rows */
               }
               #zonesTable table th {
                 position: sticky;
                 top: 0;
                 z-index: 1;
                 background-color: #6495ED; /* Cornflower Blue */
                 color: white;
                 padding: 8px;
                 text-align: left;
                 border: 1px solid #ddd;
               }
               #zonesTable table td {
                 padding: 8px;
                 text-align: left;
                 border: 1px solid #ddd;
               }
             "))
                        ),
                        tableOutput("zonesTable") # Applies CSS styling for alternating row colors
                 )
               ),
             ),
             
             
             
             # Question 12
             h4(sprintf("12. Earlier you mentioned you fished %d days between April 2024 and March 2025. Please enter a value between 0 and %d of those days that you spent fishing in freshwater from the following options:",
                        totalFishingDays(), totalFishingDays())),
             
             # Add validation for the sum of days
             textInput("q12_private_boat", sprintf("Fishing from a private boat (Enter a number between 0 and %d)", totalFishingDays())),
             textInput("q12_shore", sprintf("Fishing from shore (Enter a number between 0 and %d)", totalFishingDays())),
             textInput("q12_guide_charter", sprintf("Fishing with a guide/charter (Enter a number between 0 and %d)", totalFishingDays())),
             
             # Add validation message
             uiOutput("q12_validation")
           ),
           
           # Motivations Section 
           "Motivations" = tagList(
             # Question 13 
             h4("13. Please rate the importance of each of the following motivations for your fishing trips:"),
             
             # Add CSS for table styling
             tags$head(
               tags$style(HTML("
                 .motivation-table-container {
                   max-height: 500px;
                   overflow-y: auto;
                   border: 1px solid #ddd;
                 }
                 .motivation-table {
                   width: 100%;
                   border-collapse: collapse;
                   margin: 20px 0;
                   table-layout: fixed;
                 }
                 .motivation-table th, .motivation-table td {
                   border: 1px solid #ddd;
                   padding: 12px;
                   text-align: left;
                 }
                 .motivation-table th:first-child {
                 width: 30%;
                 }
                 .motivation-table th:last-child {
                 width: 70%;
                 }
                
                 .motivation-table th {
                   background-color: cornflowerblue;
                   color: white;
                   position: sticky;
                   top: 0;
                   z-index: 2;
                 }
                 .motivation-table tr:nth-child(even) {
                   background-color: #f9f9f9;
                 }
                 .motivation-table tr:nth-child(odd) {
                   background-color: #ffffff;
                 }
                 .radio-group {
                   display: flex;
                   justify-content: space-between;
                   width: 100%;
                   gap: 10px;
                 }
               "))
             ),
             
             # Scrollable div for table
             div(
               class = "motivation-table-container",
               uiOutput("motivationTable")
             )
           ),
           
           # Major Purchases/Investments Section
           "Major Purchases/Investments" = tagList(
             h3("Major Purchases/Investments"),
             
             # Question 14
             radioButtons("q14", "14. Did you or any member of your household make any major purchases or investments in British Columbia between April 2024 and March 2025 related in whole or in part to fishing in freshwater? (e.g., fishing rods, boats, motors, 4x4s, cabins, camping gear, trailers, etc.)
Please note: Purchases of fishing supplies (lures, line, tackle, bait, etc.) are covered in another question",
                          choices = c("Yes" = 1, "No" = 0),
                          selected = character(0)),
             
             # Question 15 
             conditionalPanel(
               condition = "input.q14 == 1",
               h4("15. For each investment category, please indicate the amount of money spent in British Columbia between April 2024 and March 2025 by you and members of your household and estimate the percentage of the total amount you consider was directly attributed to freshwater fishing. If no amount was spent, please enter 0."),
               
               # Add CSS for frozen headers and alternating row colors
               tags$head(
                 tags$style(HTML("
                   .frozen-table-container {
                     max-height: 400px;
                     overflow-y: auto;
                     border: 1px solid #ddd;
                   }
                   .frozen-table {
                     width: 100%;
                     border-collapse: collapse;
                   }
                   .frozen-table th {
                     position: sticky;
                     top: 0;
                     background-color: cornflowerblue;
                     color: white;
                     padding: 8px;
                     border: 1px solid #ddd;
                     text-align: left;
                   }
                   .frozen-table td {
                     padding: 8px;
                     border: 1px solid #ddd;
                     text-align: left;
                   }
                   .frozen-table tr:nth-child(even) {
                     background-color: #f9f9f9;
                   }
                   .frozen-table tr:nth-child(odd) {
                     background-color: #ffffff;
                   }
                 "))
               ),
               
               # Table container
               div(
                 class = "frozen-table-container",
                 uiOutput("investmentTable")
               )
             )
             
             
             
           ),
           
           
           # Freshwater Packages Section
           "Freshwater Packages" = tagList(
             h3("Freshwater Packages"),
             #Descriptive Text
             p("This question refers only to packages purchased in British Columbia from a fishing lodge, guide or outfitter (or their agent), which include a complete range of services such as lodging, food, transportation, etc."),
             
             # Question 16
             radioButtons("q16", "16. Did you or any member of your household purchase package deals (e.g., fishing and accommodation, all-inclusive trips, guided group trips, etc.) related to fishing in British Columbia freshwater?",
                          choices = c("Yes" = 1, "No" = 0),
                          selected = character(0)),
             
             # Question 17
             conditionalPanel(
               condition = "input.q16 == 1",
               h4("17. Excluding expenditures on major purchases and package deals, please estimate the amount of money you and other members of your household spent on the following to fish within British Columbia between April 2024 and March 2025. If no amount was spent, please enter a '0'."),
               
               # Add CSS for frozen headers and alternating row colors
               tags$head(
                 tags$style(HTML("
                 .frozen-table-container {
                   max-height: 400px;
                   overflow-y: auto;
                   border: 1px solid #ddd;
                 }
                 .frozen-table {
                   width: 100%;
                   border-collapse: collapse;
                 }
                 .frozen-table th {
                   position: sticky;
                   top: 0;
                   background-color: cornflowerblue;
                   color: white;
                   padding: 8px;
                   border: 1px solid #ddd;
                   text-align: left;
                 }
                 .frozen-table td {
                   padding: 8px;
                   border: 1px solid #ddd;
                   text-align: left;
                 }
                 .frozen-table tr:nth-child(even) {
                   background-color: #f9f9f9;
                 }
                 .frozen-table tr:nth-child(odd) {
                   background-color: #ffffff;
                 }
               "))
               ),
               
               # Table container
               div(
                 class = "frozen-table-container",
                 uiOutput("packageTable")
               )
             )
           ),
           
           
           
           
           # Demographics Section
           "Demographics" = tagList(
             h3("Demographics"),
             p("In order to analyse this survey in a meaningful way, we require some personal information about you and your household. This will allow us to accurately estimate the number of anglers in the province, their use of the resource and the economic benefits generated by recreational fishing in freshwater in British Columbia. Your responses will remain confidential."),
             # Question 18
             selectInput("q18", "18. Which of the following age groups do you belong to?",
                         choices = c("","Under 18", "18-29", "30-39", "40-49", "50-59", 
                                     "60-69", "70-79", "80-89", "90 or over", "Prefer not to answer"), selected = ""),
             # Question 19
             radioButtons("q19", "19. What is your gender?",
                          choices = c("Male", "Female", "Transgender", "Non-binary", "Other", "Prefer not to answer"),
                          selected = character(0)),
             # Question 20
             numericInput("q20", "20. How many members (16 years and over) of your household held a BC freshwater fishing license in 2024-2025?", value = 0, min = 0),
             # Question 21
             numericInput("q21", "21. How many members of your household under 16 years of age fished in BC freshwater in 2024/2025?", value = 0, min = 0)
           ),
           
           # Comments Section + Email entry 
           "Comments" = tagList(
             h3("Comments"),
             # Question 22
             textAreaInput("q22", "22. Please add any additional comments here:"),
             textAreaInput("q23", '23. If you would like to be included in the random prize draw for 1 of 5 $100 Cabelas giftcards, please enter your email below.')
           )
    )
  })
  
  # Create months list with specified labels Q 8
  months <- c("April 2024", "May 2024", "June 2024", "July 2024", "August 2024",
              "September 2024", "October 2024", "November 2024", "December 2024",
              "January 2025", "February 2025", "March 2025")
  
  # Render the fishing days table dynamically for question 8
  output$fishingTable <- renderUI({
    table_tags <- tags$table(
      class = "frozen-table",
      tags$thead(
        tags$tr(
          tags$th("Month"),
          tags$th("Total Days Fishing"),
          tags$th("Approximate Number of Fish Caught"),
          tags$th("Approximate Number of Fish Harvested")
        )
      ),
      tags$tbody(
        lapply(1:12, function(i) {
          tags$tr(
            tags$td(months[i]),
            tags$td(
              textInput(
                inputId = paste0("daysFishing_", i),
                label = NULL,
                value = input[[paste0("daysFishing_", i)]] %||% "",
                placeholder = "Enter days"
              )
            ),
            tags$td(
              textInput(
                inputId = paste0("fishCaught_", i),
                label = NULL,
                value = input[[paste0("fishCaught_", i)]] %||% "",
                placeholder = "Enter number"
              )
            ),
            tags$td(
              textInput(
                inputId = paste0("fishHarvested_", i),
                label = NULL,
                value = input[[paste0("fishHarvested_", i)]] %||% "",
                placeholder = "Enter number"
              )
            )
          )
        })
      )
    )
    return(table_tags)
  })
  
  # Render the management zones table dynamically for question 11
  output$zonesTable <- renderTable({
    data.frame(
      Month = months,
      `Areas Fished` = sapply(1:12, function(i) {
        as.character(textInput(paste0("areasFished_", i), label = NULL, placeholder = "Enter zones (e.g., 1-8)"))
      }),
      `Fished on Rivers` = sapply(1:12, function(i) {
        as.character(selectInput(paste0("fishedRivers_", i), label = NULL, choices = c("", "Yes", "No"), selected = NULL))
      }),
      `Fished on Lakes` = sapply(1:12, function(i) {
        as.character(selectInput(paste0("fishedLakes_", i), label = NULL, choices = c("", "Yes", "No"), selected = NULL))
      }),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }, sanitize.text.function = function(x) x)
  
  # Handle survey submissions
  observeEvent(input$submitQ8, {
    responses <- lapply(1:12, function(i) {
      list(
        month = months[i],
        daysFishing = input[[paste0("daysFishing_", i)]],
        fishCaught = input[[paste0("fishCaught_", i)]],
        fishHarvested = input[[paste0("fishHarvested_", i)]]
      )
    })
    print(responses)
    output$confirmationQ8 <- renderText("Thank you for submitting your fishing activity data!")
  })
  
  observeEvent(input$submitQ11, {
    responses <- lapply(1:12, function(i) {
      list(
        month = months[i],
        areasFished = input[[paste0("areasFished_", i)]],
        fishedRivers = input[[paste0("fishedRivers_", i)]],
        fishedLakes = input[[paste0("fishedLakes_", i)]]
      )
    })
    print(responses)
    output$confirmationQ11 <- renderText("Thank you for submitting your management zone data!")
  })
  
  
  ## Q 12 validation 
  output$q12_validation <- renderUI({
    # Convert inputs to numeric, treating invalid inputs as 0
    private_boat <- as.numeric(if(is.null(input$q12_private_boat) || input$q12_private_boat == "") 0 else input$q12_private_boat)
    shore <- as.numeric(if(is.null(input$q12_shore) || input$q12_shore == "") 0 else input$q12_shore)
    guide_charter <- as.numeric(if(is.null(input$q12_guide_charter) || input$q12_guide_charter == "") 0 else input$q12_guide_charter)
    
    total_entered <- sum(private_boat, shore, guide_charter, na.rm = TRUE)
    
    if (total_entered > totalFishingDays()) {
      tags$p(style = "color: red;", 
             sprintf("The total number of days (%d) exceeds the total fishing days reported (%d)", 
                     total_entered, totalFishingDays()))
    } else {
      tags$p(style = "color: green;",
             sprintf("Total days entered: %d out of %d", 
                     total_entered, totalFishingDays()))
    }
  })
  
  # Render section for Q 13 
  # Data for Question 13 (Motivations)
  motivations <- c(
    "To catch fish for eating",
    "To catch a large fish",
    "To catch many fish",
    "Availability of hatchery fish",
    "For the challenge or sport of fishing",
    "To get away from the regular routine",
    "For relaxation",
    "To bring your family closer together",
    "For companionship",
    "To improve your fishing skills",
    "To be close to nature",
    "Opportunity to keep cultural and family traditions",
    "For livelihood/employment opportunities"
  )
  
  # Define Likert scale options
  likert_options <- c("Not at all important", "Somewhat important", "Important", "Very important", "Extremely important")
  
  # Replace the existing MotivationsTable output with this new version
  output$motivationTable <- renderUI({
    table_tags <- tags$table(
      class = "motivation-table",
      tags$thead(
        tags$tr(
          tags$th("Motivation"),
          tags$th("Rating")
        )
      ),
      tags$tbody(
        lapply(seq_along(motivations), function(i) {
          tags$tr(
            tags$td(motivations[i]),
            tags$td(
              radioButtons(
                inputId = paste0("motivation_", i),
                label = NULL,
                choices = likert_options,
                selected = character(0),
                inline = TRUE
              )
            )
          )
        })
      )
    )
    return(table_tags)
  })
  
  
  # Define investment categories for Q15
  investmentCategories <- c(
    "Fishing equipment (rods, reels, fish finders, etc.)",
    "Camping equipment (tents, camper trailers, etc.)",
    "Purchased new boating equipment (boats, motors, trailers, etc.)",
    "Purchased used boating equipment (boats, motors, trailers, etc.)",
    "Purchased new special vehicles (4x4, camper truck, ATV, snowmobile, etc.)",
    "Purchased used special vehicles (4x4, camper truck, ATV, snowmobile, etc.)",
    "Land-buildings (cabins, cottages, lands, etc.)",
    "Other (special clothing, waders, etc.)"
  )
  
  # Render the investment table dynamically for Q15
  output$investmentTable <- renderUI({
    tagList(
      tags$table(
        class = "frozen-table",
        tags$thead(
          tags$tr(
            tags$th("Investment Category"),
            tags$th("Amount spent in British Columbia ($)"),
            tags$th("% Attributed to fishing in freshwater (%)")
          )
        ),
        tags$tbody(
          lapply(seq_along(investmentCategories), function(i) {
            tags$tr(
              class = ifelse(i %% 2 == 0, "gray-background", "white-background"),
              tags$td(investmentCategories[i]),
              tags$td(
                numericInput(
                  inputId = paste0("amount_", i),
                  label = NULL,
                  value = 0,
                  min = 0,
                  step = 0.01,
                  width = "100%"
                )
              ),
              tags$td(
                numericInput(
                  inputId = paste0("percent_", i),
                  label = NULL,
                  value = 0,
                  min = 0,
                  max = 100,
                  step = 0.01,
                  width = "100%"
                )
              )
            )
          })
        )
      )
    )
  })
  
  # Add to the data collection logic in your next/submit button handlers:
  collectQ15Responses <- function() {
    if (input$q14 == 1) {
      responses <- lapply(seq_along(investmentCategories), function(i) {
        list(
          category = investmentCategories[i],
          amount = input[[paste0("amount_", i)]],
          percent = input[[paste0("percent_", i)]]
        )
      })
      return(do.call(rbind, lapply(responses, as.data.frame)))
    }
    return(NULL)
  }
  
  
  
  
  # Define package categories for Q17
  packageCategories <- c(
    "Accommodation (hotels, motels, cottage rentals, etc.)",
    "Campsite fees (private, provincial, etc.)",
    "Food (groceries, restaurant meals, alcoholic beverages)",
    "Travel costs within British Columbia for freshwater fishing: Vehicle",
    "Travel costs within British Columbia for freshwater fishing: Ferry",
    "Travel costs within British Columbia for freshwater fishing: Airfare (not previously reported in this survey)",
    "Travel costs within British Columbia for freshwater fishing: Other (not including personal boating costs)",
    "Household-owned boat costs (gas, repairs, launch/ramp fees, moorage, insurance, etc.)",
    "Shared ownership boat costs (including only your share of expenses)",
    "Rentals for fishing (boats, gear, etc.)",
    "Fishing supplies (lures, line, tackle, bait, etc.)",
    "Guide services (not previously reported in this survey)",
    "Fishing license fees (permits, stamps, etc.)",
    "Access fees (park fees, etc.)",
    "Other (specify)"
  )
  
  # Render the package table dynamically for Q17
  output$packageTable <- renderUI({
    tagList(
      tags$table(
        class = "frozen-table",
        tags$thead(
          tags$tr(
            tags$th("Package Category"),
            tags$th("Amount spent in British Columbia ($)")
          )
        ),
        tags$tbody(
          lapply(seq_along(packageCategories), function(i) {
            tags$tr(
              class = ifelse(i %% 2 == 0, "gray-background", "white-background"),
              tags$td(packageCategories[i]),
              tags$td(
                numericInput(
                  inputId = paste0("amount_", i),
                  label = NULL,
                  value = 0,
                  min = 0,
                  step = 0.01,
                  width = "100%"
                )
              )
            )
          })
        )
      )
    )
  })
  
  # Add to the data collection logic in your next/submit button handlers:
  collectQ17Responses <- function() {
    responses <- lapply(seq_along(packageCategories), function(i) {
      list(
        category = packageCategories[i],
        amount = input[[paste0("amount_", i)]]
      )
    })
    return(do.call(rbind, lapply(responses, as.data.frame)))
  }
  
  
  # Navigation logic for moving between sections
  observeEvent(input$nextBtn, {
    if (currentSection() < length(sections)) {
      currentSection(currentSection() + 1)
    } else {
      output$thankYou <- renderText("Thank you for completing the survey!")
    }
  })
  
  observeEvent(input$prevBtn, {
    if (currentSection() > 1) {
      currentSection(currentSection() - 1)
    }
  })
}


# Run the app
shinyApp(ui = ui, server = server)

