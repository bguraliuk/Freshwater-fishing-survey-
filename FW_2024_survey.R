# Install and load the shiny package
if (!require("shiny")) install.packages("shiny")
if (!require("shinysurveys")) install.packages("shinysurveys")
library(shiny)
library(shinysurveys)


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
  sections <- c("General Questions", "MyCatch", "Activities", "Motivations","Economics", "Freshwater Packages", "Demographics", "Comments")
  
  # Define the survey questions for each section
  output$questionsUI <- renderUI({
    switch(sections[currentSection()],
           
           # General Questions Section
           "General Questions" = tagList(
             h3("General Questions"),
             radioButtons("q1", "1. Did you complete a 2024-2025 iSEA survey distributed by Fisheries and Oceans Canada?",
                          choices = c("Yes" = 1, "No" = 0)),
             radioButtons("q2", "2. Did you fish in British Columbia freshwater under your April 2024 to March 2025 freshwater fishing license?",
                          choices = c("Yes" = 1, "No" = 0))
           ),
           
           # MyCatch Section
           "MyCatch" = tagList(
             h3("MyCatch"),
             radioButtons("q3", "3. Do you actively use the MyCatch app or other fishing apps to record your fishing activities?",
                          choices = c("Yes" = 3, "MyCatch and other apps" = 2, "Only other apps (specify)" = 1, "No" = 0)),
             conditionalPanel(
               condition = "input.q3 == 1",
               textInput("q3_specify", "Please specify the other apps:")
             ),
             textInput("q4", "4. What proportion of fishing trips do you report on fishing apps?"),
             textInput("q5", "5. If you catch no fish on a trip, do you still record that trip? Please explain:"),
             textInput("q6", "6. Do you record every fish caught on fishing apps? Please explain:"),
             textInput("q7", "7. Do you record every fish released on fishing apps? Please explain:")
           ),
           
           # Activities Section
           "Activities" = tagList(
             h3("Activities"),
             
             h4("8. How many days per month did you fish in freshwater?"),
             fluidRow(
               column(12,
                      tableOutput("fishingTable")
               )
             ),
             fluidRow(
               column(12,
                      actionButton("submitQ8", "Submit"),
                      textOutput("confirmationQ8")
               )
             ),
             
             br(),
             
             numericInput("q9", "9. How many hours per day were spent fishing on BC freshwater trips?", value = 0, min = 0),
             
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
             
             h4("11. Please indicate which management zones (1-9) you fished in freshwater in each month fished [LINKED TO MAP]"),
             fluidRow(
               column(12,
                      tableOutput("zonesTable")
               )
             ),
             fluidRow(
               column(12,
                      actionButton("submitQ11", "Submit"),
                      textOutput("confirmationQ11")
               )
             ),
             textInput("q12", "12. Earlier you mentioned you fished [X] days between April 2024 and March 2025. Please enter a value between 0 and [X]  of those days that you spent fishing in freshwater from a private boat, fishing from shore and/or fishing with a guide/charter."),
           ),
           
           # Motivations Section 
           "Motivations" = tagList(
             h3("Motivations"),
             h4("13. Please rate the importance of each of the following motivations for your fishing trips:"),
             fluidRow(
               column(12,
                      tableOutput("MotivationsTable")
               )
             ),
             fluidRow(
               column(12,
                      actionButton("submitQ13", "Submit"),
                      textOutput("confirmationQ13")
               )
             )
           ),
           
           # Economics Section
           "Economics" = tagList(
             h3("Economics"),
             
             # Question 14
             radioButtons("q14", "14. Did you or any member of your household make any major purchases or investments in British Columbia between April 2024 and March 2025 related in whole or in part to fishing in freshwater? (e.g., fishing rods, boats, motors, 4x4s, cabins, camping gear, trailers, etc.)
  Please note: Purchases of fishing supplies (lures, line, tackle, bait, etc.) are covered in another question",
                          choices = c("Yes" = 1, "No" = 0)),
             
             # Question 15
             conditionalPanel(
               condition = "input.q14 == 1",
               h4("15. For each investment category, please indicate the amount of money spent in British Columbia between April 2024 and March 2025 by you and members of your household, and estimate the percentage of the total amount attributed to freshwater fishing:"),
               lapply(seq_along(Economics), function(i) {
                 tagList(
                   h5(Economics[i]),
                   numericInput(paste0("amount_", i), "Amount spent ($):", value = 0, min = 0, step = 0.01),
                   numericInput(paste0("percentage_", i), "Percentage attributed to freshwater fishing (%):", value = 0, min = 0, max = 100, step = 1)
                 )
               }),
               actionButton("submitQ15", "Submit Question 15")
             ),
           ),
           # Freshwater Packages Section
           "Freshwater Packages" = tagList(
             h3("Freshwater Packages"),
             
             # Question 16
             radioButtons("q16", "16. Did you or any member of your household purchase package deals (e.g., fishing and accommodation, all-inclusive trips, guided group trips, etc.) related to fishing in British Columbia freshwater?",
                          choices = c("Yes" = 1, "No" = 0)),
             
             conditionalPanel(
               condition = "input.q16 == 1",
               textInput("q16_details", "Please describe the package deals purchased:")
             ),
             
             # Question 17
             h4("17. Excluding expenditures on major purchases and package deals, please estimate the amount of money you and other members of your household spent on the following to fish within British Columbia between April 2024 and March 2025.
                If no amount was spent, please enter a ‘0’."),
             
             uiOutput("investmentTable"),  # Dynamically rendered table
             actionButton("submitQ17", "Submit Question 17"),
             textOutput("confirmationQ17")
           ),
           
           
           
           
           # Demographics Section
           "Demographics" = tagList(
             h3("Demographics"),
             selectInput("q18", "18. Which of the following age groups do you belong to?",
                         choices = c("Under 18", "18-29", "30-39", "40-49", "50-59", 
                                     "60-69", "70-79", "80-89", "90 or over", "Prefer not to answer")),
             radioButtons("q19", "19. What is your gender?",
                          choices = c("Male", "Female", "Transgender", "Non-binary", "Other", "Prefer not to answer")),
             numericInput("q20", "20. How many members (16 years and over) of your household held a BC freshwater fishing license in 2024-2025?", value = 0, min = 0),
             numericInput("q21", "21. How many members of your household under 16 years of age fished in BC freshwater in 2024/2025?", value = 0, min = 0)
           ),
           
           # Comments Section
           "Comments" = tagList(
             h3("Comments"),
             textAreaInput("q22", "22. Please add any additional comments here:")
           )
    )
  })
  
  # Create months list with specified labels
  months <- c("April 2024", "May 2024", "June 2024", "July 2024", "August 2024",
              "September 2024", "October 2024", "November 2024", "December 2024",
              "January 2025", "February 2025", "March 2025")
  
  # Render the fishing days table dynamically for question 8
  output$fishingTable <- renderTable({
    data.frame(
      Month = months,
      `Total Days Fishing` = sapply(1:12, function(i) {
        as.character(textInput(paste0("daysFishing_", i), label = NULL, placeholder = "Enter days"))
      }),
      `Approximate Number of Fish Caught` = sapply(1:12, function(i) {
        as.character(textInput(paste0("fishCaught_", i), label = NULL, placeholder = "Enter number"))
      }),
      `Approximate Number of Fish Harvested` = sapply(1:12, function(i) {
        as.character(textInput(paste0("fishHarvested_", i), label = NULL, placeholder = "Enter number"))
      }),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }, sanitize.text.function = function(x) x)
  
  # Render the management zones table dynamically for question 11
  output$zonesTable <- renderTable({
    data.frame(
      Month = months,
      `Areas Fished` = sapply(1:12, function(i) {
        as.character(textInput(paste0("areasFished_", i), label = NULL, placeholder = "Enter zones (e.g., 1-9)"))
      }),
      `Fished on Rivers` = sapply(1:12, function(i) {
        as.character(selectInput(paste0("fishedRivers_", i), label = NULL, choices = c("Yes", "No"), selected = NULL))
      }),
      `Fished on Lakes` = sapply(1:12, function(i) {
        as.character(selectInput(paste0("fishedLakes_", i), label = NULL, choices = c("Yes", "No"), selected = NULL))
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
    "To improve your fishing skills"
  )
  
  importanceLevels <- c(
    "Not at all important",
    "Somewhat important",
    "Important",
    "Very important",
    "Extremely important"
  )
  
  # Render the table dynamically with dropdowns for Question 13
  output$MotivationsTable <- renderTable({
    data.frame(
      Motivation = motivations,
      Importance = sapply(seq_along(motivations), function(i) {
        as.character(selectInput(
          inputId = paste0("motivation_", i),
          label = NULL,
          choices = importanceLevels,
          selected = ""
        ))
      }),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }, sanitize.text.function = function(x) x)
  
  # Handle submission for Question 13
  observeEvent(input$submitQ13, {
    responses <- lapply(seq_along(motivations), function(i) {
      list(
        motivation = motivations[i],
        importance = input[[paste0("motivation_", i)]]
      )
    })
    
    # Handle submission for Question 15
    observeEvent(input$submitQ15, {
      responses <- lapply(seq_along(Economics), function(i) {
        list(
          category = Economics[i],
          amount = input[[paste0("amount_", i)]],
          percentage = input[[paste0("percentage_", i)]]
        )
      })
      print(responses)
      showNotification("Thank you for submitting your Economics data!")
    })
    
    # Print responses in the console
    print(responses)
    
    # Confirmation message
    output$confirmationQ13 <- renderText("Thank you for submitting your responses!")
  })
  
  # Updated Server Code for Question 17
  # Define investment categories for Q17
  investmentCategories <- list(
    "Accommodation (hotels, motels, cottage rentals, etc.)",
    "Campsite fees (private, provincial, etc.)",
    "Food (groceries, restaurant meals, alcoholic beverages)",
    list(
      heading = "Travel costs within British Columbia for freshwater fishing",
      subcategories = c("Vehicle", "Ferry", "Airfare (not previously reported in this survey)", "Other (not including personal boating costs)")
    ),
    "Household owned boat costs (gas, repairs, launch/ramp fees, moorage, insurance, etc.)",
    "Shared ownership boat costs (including only your share of expenses)",
    "Rentals for fishing (boats, gear, etc.)",
    "Fishing supplies (lures, line, tackle, bait, etc.)",
    "Guide services (not previously reported in this survey)",
    "Fishing license fees (permits, stamps, etc.)",
    "Access fees (park fees, etc.)",
    "Other (specify)"
  )
  
  # Render the investment table dynamically
  output$investmentTable <- renderUI({
    tagList(
      lapply(seq_along(investmentCategories), function(i) {
        if (is.list(investmentCategories[[i]])) {
          tagList(
            h5(investmentCategories[[i]]$heading),  # Subcategory heading
            lapply(seq_along(investmentCategories[[i]]$subcategories), function(j) {
              fluidRow(
                column(8, h6(paste0("- ", investmentCategories[[i]]$subcategories[j]))),  # Subcategory name
                column(4, numericInput(
                  inputId = paste0("amount_", i, "_", j),  # Unique input ID
                  label = NULL,
                  value = 0,
                  min = 0,
                  step = 0.01
                ))
              )
            })
          )
        } else {
          fluidRow(
            column(8, h5(investmentCategories[[i]])),  # Category name
            column(4, numericInput(
              inputId = paste0("amount_", i),  # Unique input ID
              label = NULL,
              value = 0,
              min = 0,
              step = 0.01
            ))
          )
        }
      })
    )
  })
  
  # Handle submission for Question 17
  observeEvent(input$submitQ17, {
    # Collect responses
    responses <- lapply(seq_along(investmentCategories), function(i) {
      if (is.list(investmentCategories[[i]])) {
        lapply(seq_along(investmentCategories[[i]]$subcategories), function(j) {
          list(
            category = paste0(investmentCategories[[i]]$heading, " - ", investmentCategories[[i]]$subcategories[j]),
            amount = input[[paste0("amount_", i, "_", j)]]
          )
        })
      } else {
        list(
          category = investmentCategories[[i]],
          amount = input[[paste0("amount_", i)]]
        )
      }
    })
    
    # Flatten the responses
    responses <- do.call(c, responses)
    
    # Log responses (for debugging or saving)
    print(responses)
    
    # Confirmation message
    output$confirmationQ17 <- renderText("Thank you for submitting your expenditures data!")
  })
  
  
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


## R app ##
rsconnect::setAccountInfo(name='bguraliuk',
                          token='3EAE3198A7AB91FEB83E77D4678EEC68',
                          secret='jDecEpZyFFL4rmNQiwaVSIWfQ9HmWGEo0YeNhv9t')
library(rsconnect)
rsconnect::deployApp('3EAE3198A7AB91FEB83E77D4678EEC68')

