# Install and load required packages
if (!require("shiny")) install.packages("shiny")
if (!require("shinysurveys")) install.packages("shinysurveys")
if (!require("jpeg")) install.packages("jpeg")
if (!require("grid")) install.packages("grid")
if (!require("rsconnect")) install.packages("rsconnect")
if (!require("shinyjs")) install.packages("shinyjs")
if (!require("lubridate")) install.packages("lubridate")

# Load libraries
library(jpeg)
library(shiny)
library(shinysurveys)
library(grid)
library(rsconnect)
library(shinyjs)
library(lubridate)

# Define global variables
# Months list with specified labels
months <- c("April 2024", "May 2024", "June 2024", "July 2024", "August 2024",
            "September 2024", "October 2024", "November 2024", "December 2024",
            "January 2025", "February 2025", "March 2025")

# Define motivation options for Q13
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

# CSS for all tables
tableCSS <- "
  .data-table {
    width: 100%;
    border-collapse: collapse;
    margin-bottom: 20px;
  }
  .data-table th {
    position: sticky;
    top: 0;
    background-color: #6495ED;
    color: white;
    padding: 8px;
    text-align: left;
    border: 1px solid #ddd;
  }
  .data-table td {
    padding: 8px;
    text-align: left;
    border: 1px solid #ddd;
  }
  .data-table tr:nth-child(odd) {
    background-color: #f2f2f2;
  }
  .data-table tr:nth-child(even) {
    background-color: #ffffff;
  }
  .table-container {
    max-height: 500px;
    overflow-y: auto;
    border: 1px solid #ddd;
    margin-bottom: 20px;
  }
  .radio-group {
    display: flex;
    gap: 20px;
  }
  .radio-label {
    margin: 0 5px;
    white-space: nowrap;
    font-weight: normal;
  }
  .checkbox-group {
    display: flex;
    flex-wrap: wrap;
    gap: 4px;
  }
  .checkbox-item {
    display: flex;
    align-items: center;
    white-space: nowrap;
  }
  .checkbox-item input[type='checkbox'] {
    margin-right: 5px;
  }
"

# CSS for modal dialog
modalCSS <- "
  .modal {
    display: none;
    position: fixed;
    z-index: 1000;
    left: 0;
    top: 0;
    width: 100%;
    height: 100%;
    overflow: auto;
    background-color: rgba(0,0,0,0.4);
  }
  .modal-content {
    background-color: #fefefe;
    margin: 15% auto;
    padding: 20px;
    border: 1px solid #888;
    width: 80%;
    max-width: 400px;
    border-radius: 5px;
  }
  .modal-buttons {
    display: flex;
    justify-content: flex-end;
    gap: 10px;
    margin-top: 20px;
  }
  .modal-buttons button {
    padding: 8px 16px;
    cursor: pointer;
  }
"

# Then update your tableCSS assignment to:
tableCSS <- paste(tableCSS, modalCSS)

# Define the UI
ui <- fluidPage(
  shinyjs::useShinyjs(),
  tags$head(tags$style(HTML(tableCSS))),
  
  titlePanel("2024-2025 Freshwater Fishing Survey"),
  
  # Consent form panel
  conditionalPanel(
    condition = "!input.consent || !input.startSurvey",
    wellPanel(
      style = "max-width: 800px; margin: 0 auto;",
      img(src = "logo_1.png", style = "width: 100%; margin-top: 20px;"),
      h2("Consent Form"),
      h3("How do angler app users compare with the broader fishing public?"),
      
      p("You have been invited to take part in the current research study to determine similarities and differences between all licensed freshwater anglers in British Columbia (BC) and users of fishing apps like yourself. You have been selected to participate because you are a registered user of the MyCatch fishing app, which is popular in BC, and we would like to ask you about your background and experiences relating to freshwater fishing in BC. The survey will take approximately", tags$b("10-15 minutes")),
      br(),
      
      p("We are inviting you to take a survey for research. This survey is completely voluntary. There are no negative consequences if you don't want to participate. Once you start the survey, you are free to stop at any time. By consenting, you do not waive any rights to legal recourse in the event of research-related harm."),
      
      h4("Purpose:"),
      p("This study aims to compare activities, economic expenditures, and demographics between angler app users and the broader freshwater fishing public in BC."),
      
      h4("Procedure:"),
      p("We will ask you to complete a short  (", tags$b("10-15 minute"),") survey, designed to mimic essential elements of the iSEA (internet Socio-Economic) survey administered by Fisheries and Oceans Canada (DFO) on behalf of DFO and the BC Ministry of Water, Land and Resource Stewardship (WLRS). The current survey is composed of four parts, asking questions related to: MyCatch app use, your fishing activities, expenditures, and basic demographic information."),
      
      h4("Risks:"),
      tags$ul(
        tags$li("You may find some questions to be personal or upsetting. You can skip them or quit the survey at any time."),
        tags$li("Online data being hacked or intercepted: We use secure systems but cannot completely eliminate this risk."),
        tags$li("Breach of confidentiality: Data is anonymous and stored on encrypted servers.")
      ),
      
      h4("Benefits:"),
      p("Fishing apps are a valuable means of keeping tabs on fish populations across the province, but because these data are opt-in (you have to choose whether to report your activity), they may not be representative of everyone's experience. This survey will help understand whether, and how, app users may differ from the broader fishing public, which can help provide more accurate information to biologists tasked with providing healthy and productive fish populations in BC."),
      
      h4("Compensation:"),
      p("$100 gift cards to Cabela's / Bass Pro Shops will be given out randomly to three participants."),
      tags$ul(
        tags$li("To be eligible for this prize, you will need to provide a method of contacting you to notify you that you have won. This information will be stored separately and will not be linked to your survey responses in any way."),
        tags$li("Participation in the prize draw is voluntary and you may continue to take the survey without entering the draw.")
      ),
      
      h4("Contact:"),
      p("For questions about your rights as a participant, contact the Director, SFU Office of Research Ethics at dore@sfu.ca or 778-782-6593."),
      
      h4("To participate, you must:"),
      tags$ul(
        tags$li(tags$b("Be at least 18 years old")),
        tags$li(tags$b("Have a valid BC freshwater fishing license"))
      ),
      
      br(),
      checkboxInput("consent", tags$b("I have read and understand the above information and agree to participate in this study."), FALSE),
      
      conditionalPanel(
        condition = "input.consent",
        actionButton("startSurvey", "Start Survey", class = "btn-primary",
                     style = "font-size: 18px; padding: 10px 20px;")
      ),
      
      br(),
      img(src = "logo_1.png", style = "width: 100%; margin-top: 20px;"),
      
      div(
        style = "display: flex; justify-content: center; margin-top: 10px;",
        img(src = "logo_gov.png", style = "width: 50%; max-width: 200px; height: auto;")
      )
    )
  ),
  
  # Main survey panel - only shown after consent
  conditionalPanel(
    condition = "input.consent && input.startSurvey",
    mainPanel(
      # Add a notification area for feedback messages
      div(id = "notification-area", style = "margin-bottom: 15px;"),
      
      # Section title and navigation indicator
      uiOutput("sectionTitleUI"),
      
      # Main questionnaire area
      uiOutput("questionsUI"),
      br(),
      
      # Navigation buttons with disabled state when saving
      div(style = "display: flex; justify-content: space-between; width: 200px;",
          actionButton("prevBtn", "Previous", class = "btn-primary"),
          actionButton("nextBtn", "Next", class = "btn-primary")
      ),
      br(),
      textOutput("thankYou")
    )
  )
)

# Define the server logic
server <- function(input, output, session) {
  # Reactive value to track the current section
  currentSection <- reactiveVal(1)
  
  # Create reactive values to store all responses
  responses <- reactiveValues()
  
  # Flag to indicate if navigation is in progress (saving)
  savingInProgress <- reactiveVal(FALSE)
  
  # Section titles
  sections <- c("General Questions", "MyCatch and Fishing Apps", "Activities1", "Activities2", 
                "Activities3", "Activities3b", "Activities4", "Motivations", 
                "Major Purchases/Investments", "Freshwater Packages", "Demographics", 
                "Comments")
  
  # Function to collect all input values from the current section
  collectCurrentSectionData <- function() {
    # Get the current section name
    current_section_name <- sections[currentSection()]
    
    # Define which input IDs to collect for each section
    section_inputs <- switch(current_section_name,
                             "General Questions" = c("q1", "q2"),
                             "MyCatch and Fishing Apps" = c("q3", "q3_specify", "q4", "q5_yesno", "q5_explain", "q6_yesno", "q6_explain", "q7_yesno", "q7_explain"),
                             "Activities1" = c(paste0("daysFishing_", 1:12), paste0("fishCaught_", 1:12), paste0("fishHarvested_", 1:12)),
                             "Activities2" = c("q9", "q10"),
                             "Activities3" = {
                               # Get the active months
                               active_months <- sapply(1:12, function(i) {
                                 days <- input[[paste0("daysFishing_", i)]]
                                 !is.null(days) && !is.na(as.numeric(days)) && as.numeric(days) > 0
                               })
                               active_month_indices <- which(active_months)
                               
                               # Collect region inputs for active months
                               regions <- c("1", "2", "3", "4", "5", "6", "7a", "7b", "8")
                               region_inputs <- unlist(lapply(active_month_indices, function(i) {
                                 paste0("regionsFished_", i, "_", regions)
                               }))
                               
                               region_inputs
                             },
                             "Activities3b" = {
                               # Collect waterbody inputs based on selected regions
                               active_months <- sapply(1:12, function(i) {
                                 days <- input[[paste0("daysFishing_", i)]]
                                 !is.null(days) && !is.na(as.numeric(days)) && as.numeric(days) > 0
                               })
                               active_month_indices <- which(active_months)
                               
                               waterbody_inputs <- c()
                               for (i in active_month_indices) {
                                 for (region in c("1", "2", "3", "4", "5", "6", "7a", "7b", "8")) {
                                   if (isTRUE(input[[paste0("regionsFished_", i, "_", region)]])) {
                                     waterbody_inputs <- c(waterbody_inputs, paste0("waterbody_", i, "_", region))
                                   }
                                 }
                               }
                               
                               waterbody_inputs
                             },
                             "Activities4" = c("q12_guide_charter", "q12_shore", "q12_private_boat"),
                             "Motivations" = paste0("motivation_", 1:length(motivations)),
                             "Major Purchases/Investments" = c("q14", if (isTRUE(input$q14 == 1)) c(
                               paste0("amount_", 1:length(investmentCategories)), 
                               paste0("percent_", 1:length(investmentCategories))
                             ) else c()),
                             "Freshwater Packages" = c("q16", if (isTRUE(input$q16 == 1)) c(
                               paste0("packageAmount_", 1:length(packageCategories))
                             ) else c()),
                             "Demographics" = c("q18", "q19", "q20", "q21"),
                             "Comments" = c("q22", "q23"),
                             "Thank You" = c()
    )
    
    # Collect the values for the defined inputs
    section_data <- list()
    for (id in section_inputs) {
      if (!is.null(input[[id]])) {
        section_data[[id]] <- input[[id]]
      }
    }
    
    return(section_data)
  }
  
  # Function to save all data from the current section to responses
  saveCurrentSectionData <- function() {
    # Set the saving flag
    savingInProgress(TRUE)
    
    # Show a notification
    showNotification("Saving responses...", id = "saving", type = "message")
    
    # Collect and save the data
    section_data <- collectCurrentSectionData()
    for (name in names(section_data)) {
      responses[[name]] <- section_data[[name]]
    }
    
    # Clear the notification and reset the flag
    removeNotification("saving")
    savingInProgress(FALSE)
    
    
    # Show success notification
    showNotification("Responses saved successfully!", id = "save_success", type = "message")
    # Remove the success notification after 1 second
    invalidateLater(1000)
    removeNotification("save_success")
  }
  
  # Helper function to get saved value
  getSavedValue <- function(inputId, default = NULL) {
    if (!is.null(responses[[inputId]])) {
      return(responses[[inputId]])
    }
    return(default)
  }
  
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
  
  # Process the "more_than_10" value for analysis
  observeEvent(input$q9, {
    if (!is.null(input$q9) && input$q9 == "more_than_10") {
      # For analysis purposes, you might want to store this as a specific numeric value
      # or keep it as a special string value depending on how you'll analyze the data
      responses[["q9_original"]] <- "more_than_10"
      # Optionally store a numeric value for analysis
      responses[["q9_numeric"]] <- 11  # or another appropriate value for ">10"
    } else if (!is.null(input$q9)) {
      # For regular numeric values
      responses[["q9_original"]] <- input$q9
      responses[["q9_numeric"]] <- as.numeric(input$q9)
    }
  })
  
  # Display section title with progress
  output$sectionTitleUI <- renderUI({
    div(
      style = "margin-bottom: 20px;",
      div(
        style = "background-color: #f0f0f0; height: 10px; border-radius: 5px; margin-top: 5px;",
        div(
          style = sprintf("background-color: #6495ED; height: 100%%; width: %s%%; border-radius: 5px;", 
                          min(100, round(100 * (currentSection() - 1) / (length(sections) - 1), 0))),
          ""
        )
      )
    )
  })
  
  
  # Submit button handler
  observeEvent(input$submitBtn, {
    # Disable the submit button to prevent multiple submissions
    shinyjs::disable("submitBtn")
    
    # Show loading message
    showNotification("Submitting your survey...", id = "submitting", type = "message")
    
    # Save current section data
    saveCurrentSectionData()
    
    # Simulate submission process (add your actual submission code here)
    Sys.sleep(1)  # Simulate processing time
    
    # Remove the notification
    removeNotification("submitting")
    
    # Show success message
    showNotification("Survey submitted successfully!", id = "submit_success", type = "message")
    
    # Hide the survey content and buttons
    shinyjs::hide("questionsUI")
    shinyjs::hide("prevBtn")
    shinyjs::hide("nextBtn")
    shinyjs::hide("submitBtn")
    
    # Show thank you message
    shinyjs::show("thankYouMessage")
    
    # Scroll to top to show thank you message
    runjs("window.scrollTo({ top: 0, behavior: 'smooth' });")
    
    # Optional: Here you would add code to actually submit the data to a database or file
    # For example: saveRDS(responses, "survey_responses.rds")
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
                          selected = getSavedValue("q1", character(0))),
             
             # Question 2
             radioButtons("q2", "2. Did you fish in British Columbia freshwater under your April 2024 to March 2025 freshwater fishing license?",
                          choices = c("Yes" = 1, "No (skip to 13)" = 0),
                          selected = getSavedValue("q2", character(0)))
           ),
           
           # MyCatch Section
           "MyCatch and Fishing Apps" = tagList(
             h3("MyCatch and Fishing Apps"),
             # Question 3
             radioButtons("q3", "3. Do you actively use the MyCatch app or other fishing apps to record your fishing activities?",
                          choices = c("Only MyCatch" = 3, "MyCatch and other apps (specify)" = 2, 
                                      "Only other apps (specify)" = 1, "No (skip to 8)" = 0),
                          selected = getSavedValue("q3", character(0))),
             
             conditionalPanel(
               condition = "input.q3 == 1 || input.q3 == 2",
               textInput("q3_specify", "Please specify the other apps:",
                         value = getSavedValue("q3_specify", ""))
             ),
             
             # Question 4
             selectInput("q4", 
                         "4. What percentage of fishing trips do you report on fishing apps?", 
                         choices = c(" " = 0, "1-10%" = 1, "11-20%" = 2, "21-30%" = 3, 
                                     "31-40%" = 4, "41-50%" = 5, "51-60%" = 6, "61-70%" = 7, 
                                     "71-80%" = 8, "81-90%" = 9, "91-99%" = 10, "100%" = 11),
                         selected = getSavedValue("q4")),
             
             # Question 5
             radioButtons("q5_yesno", 
                          "5. If you catch no fish on a trip, do you still record that trip?", 
                          choices = c("Yes" = "yes", "No" = "no"),
                          selected = getSavedValue("q5_yesno", character(0))),
             
             conditionalPanel(
               condition = "input.q5_yesno == 'yes' || input.q5_yesno == 'no'",
               textInput("q5_explain", "Please explain:",
                         value = getSavedValue("q5_explain", ""))
             ),
             
             # Question 6
             radioButtons("q6_yesno", 
                          "6. Do you record every fish caught on fishing apps?", 
                          choices = c("Yes" = "yes", "No" = "no"),
                          selected = getSavedValue("q6_yesno", character(0))),
             
             conditionalPanel(
               condition = "input.q6_yesno == 'yes' || input.q6_yesno == 'no'",
               textInput("q6_explain", "Please explain:",
                         value = getSavedValue("q6_explain", ""))
             ),
             
             # Question 7
             radioButtons("q7_yesno", 
                          "7. Do you record every fish released on fishing apps?", 
                          choices = c("Yes" = "yes", "No" = "no"),
                          selected = getSavedValue("q7_yesno", character(0))),
             
             conditionalPanel(
               condition = "input.q7_yesno == 'yes' || input.q7_yesno == 'no'",
               textInput("q7_explain", "Please explain:",
                         value = getSavedValue("q7_explain", ""))
             )
           ),
           
           # Activities Section - Part 1
           "Activities1" = tagList(
             h3("Freshwater Fishing Activities"),
             # Question 8
             tags$strong("8. How many days per month did you fish in freshwater? For each month, provide approximate numbers of fish caught and fish harvested across all species. A particular date on which you fished recreationally for any length of time counts as one day fished"),
             
             # Table with consistent styling
             div(id = "fishingTable", class = "table-container", 
                 tags$style(HTML("
                   #fishingTableOutput table {
                     width: 100%;
                     border-collapse: collapse;
                   }
                   #fishingTableOutput table th {
                     background-color: #6495ED;
                     color: white;
                     padding: 8px;
                     border: 1px solid #ddd;
                     text-align: left;
                   }
                   #fishingTableOutput table td {
                     padding: 8px;
                     border: 1px solid #ddd;
                   }
                   #fishingTableOutput table tr:nth-child(odd) {
                     background-color: #f2f2f2;
                   }
                   #fishingTableOutput table tr:nth-child(even) {
                     background-color: #ffffff;
                   }
                 ")),
                 tableOutput("fishingTableOutput")
             )
           ),
           
           # Activities Section - Part 2
           "Activities2" = tagList(
             h3("Freshwater Fishing Activities"),
             # Question 9
             tags$div(
               id = "q9_container",
               tags$label("9. On average, how many hours per day did you fish in freshwater in British Columbia on the days you fished between April 2024 and March 2025?"),
               tags$div(
                 style = "display: flex; align-items: center;",
                 selectInput("q9", 
                             label = NULL,
                             choices = c(0:10, ">10" = "more_than_10"), 
                             selected = getSavedValue("q9", 0),
                             width = "150px")
               ),
               
               # Modal for confirmation
               tags$div(
                 id = "confirmModal",
                 class = "modal",
                 style = "display: none; position: fixed; z-index: 1000; left: 0; top: 0; width: 100%; height: 100%; overflow: auto; background-color: rgba(0,0,0,0.4);",
                 tags$div(
                   class = "modal-content",
                   style = "background-color: #fefefe; margin: 15% auto; padding: 20px; border: 1px solid #888; width: 80%; max-width: 400px; border-radius: 5px;",
                   tags$h4("Confirmation"),
                   tags$p("Are you sure you fished more than 10 hours on average per day?"),
                   tags$div(
                     style = "display: flex; justify-content: flex-end; gap: 10px; margin-top: 20px;",
                     tags$button(
                       id = "cancelBtn",
                       class = "btn btn-default",
                       "No, let me change"
                     ),
                     tags$button(
                       id = "confirmBtn",
                       class = "btn btn-primary",
                       "Yes, I'm sure"
                     )
                   )
                 )
               )
             ),
             
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
               ),
               selected = getSavedValue("q10", NULL)
             ),
             # Add JavaScript for modal behavior
             tags$script(HTML("
    $(document).ready(function() {
      // Function to handle modal display
      function handleModal() {
        var selectedValue = $('#q9').val();
        if (selectedValue === 'more_than_10') {
          $('#confirmModal').show();
        }
      }
      
      // Watch for changes to the select input
      $('#q9').change(handleModal);
      
      // Handle cancel button click
      $('#cancelBtn').click(function() {
        $('#confirmModal').hide();
        $('#q9').val('10'); // Reset to 10 hours
      });
      
      // Handle confirm button click
      $('#confirmBtn').click(function() {
        $('#confirmModal').hide();
        // Keep the >10 hours selected
      });
      
      // Close modal if clicked outside
      $(window).click(function(event) {
        if ($(event.target).is('#confirmModal')) {
          $('#confirmModal').hide();
          $('#q9').val('10'); // Reset to 10 hours
        }
      });
    });
  "))
           ),
           
           # Activities Section - Part 3
           "Activities3" = tagList(
             h3("Freshwater Fishing Activities"),
             # Question 11
             tags$strong(
               HTML(paste(
                 "11. Please indicate which management regions (1 - 8) you fished in freshwater in each month fished",
                 tags$span(
                   style = "color: #2b579a;",
                   textOutput("monthsFishedText", inline = TRUE)
                 ),
                 "(click map to enlarge)"
               ))
             ),
             br(),
             tags$a(
               href = "management_zones.jpg",  
               target = "_blank",
               tags$img(src = "management_zones.jpg", width = "200px", alt = "[View Map]")
             ),
             
             # Regions table
             div(id = "regionsTable", class = "table-container", 
                 tags$style(HTML("
                   #regionsTableOutput table {
                     width: 100%;
                     border-collapse: collapse;
                   }
                   #regionsTableOutput table th {
                     background-color: #6495ED;
                     color: white;
                     padding: 8px;
                     border: 1px solid #ddd;
                     text-align: left;
                   }
                   #regionsTableOutput table td {
                     padding: 8px;
                     border: 1px solid #ddd;
                   }
                   #regionsTableOutput table tr:nth-child(odd) {
                     background-color: #f2f2f2;
                   }
                   #regionsTableOutput table tr:nth-child(even) {
                     background-color: #ffffff;
                   }
                 ")),
                 tableOutput("regionsTableOutput")
             )
           ),
           
           # Activities Section - Part 3b
           "Activities3b" = tagList(
             h3("Freshwater Fishing Activities"),
             # Question 11b
             tags$strong(
               "11b. For each month and management region you selected above, please indicate whether you fished in rivers, lakes, or both:"
             ),
             br(),
             tags$a(
               href = "management_zones.jpg",  
               target = "_blank",
               tags$img(src = "management_zones.jpg", width = "200px", alt = "[View Map]")
             ),
             
             # Waterbody table
             div(id = "waterbodyTable", class = "table-container", 
                 tags$style(HTML("
                   #waterbodyTableOutput table {
                     width: 100%;
                     border-collapse: collapse;
                   }
                   #waterbodyTableOutput table th {
                     background-color: #6495ED;
                     color: white;
                     padding: 8px;
                     border: 1px solid #ddd;
                     text-align: left;
                   }
                   #waterbodyTableOutput table td {
                     padding: 8px;
                     border: 1px solid #ddd;
                   }
                   #waterbodyTableOutput table tr:nth-child(odd) {
                     background-color: #f2f2f2;
                   }
                   #waterbodyTableOutput table tr:nth-child(even) {
                     background-color: #ffffff;
                   }
                 ")),
                 tableOutput("waterbodyTableOutput")
             )
           ),
           
           # Activities Section - Part 4
           "Activities4" = tagList(
             h3("Freshwater Fishing Activities"),
             # Question 12
             tags$div(
               style = "margin-bottom: 20px;",
               tags$strong(sprintf("12. Earlier you mentioned you fished %d days between April 2024 and March 2025. Please enter a value between 0 and %d of those days that you spent fishing in freshwater from the following options:",
                                   totalFishingDays(), totalFishingDays()))
             ),
             
             # Add validation for the sum of days
             selectInput("q12_guide_charter", 
                         sprintf("Fishing with a guide/charter (Enter a number between 0 and %d)", totalFishingDays()),
                         choices = 0:totalFishingDays(),
                         selected = getSavedValue("q12_guide_charter", "0")),
             
             selectInput("q12_shore", 
                         sprintf("Fishing from shore (Enter a number between 0 and %d)", totalFishingDays()),
                         choices = 0:totalFishingDays(),
                         selected = getSavedValue("q12_shore", "0")),
             
             selectInput("q12_private_boat", 
                         sprintf("Fishing from a private boat (Enter a number between 0 and %d)", totalFishingDays()),
                         choices = 0:totalFishingDays(),
                         selected = getSavedValue("q12_private_boat", "0")),
             
             # Q12 validation message
             uiOutput("q12_validation")
           ),
           
           # Motivations Section 
           "Motivations" = tagList(
             h3("Motivations for Fishing"),
             
             # Question 13 
             tags$strong("13. Please rate the importance of each of the following motivations for your fishing trips:"),
             
             # Add specific CSS for radio buttons in this section
             tags$style(HTML("
               .motivation-radio .shiny-options-group {
                 display: flex;
                 flex-direction: row;
                 justify-content: space-between;
                 width: 100%;
               }
               .motivation-radio .shiny-options-group .radio-inline {
                 margin-right: 5px;
                 white-space: nowrap;
               }
               .motivation-radio .shiny-options-group .radio-inline input[type='radio'] {
                 margin-right: 3px;
               }
               .motivation-radio label {
                 font-weight: normal;
                 margin-bottom: 0;
               }
             ")),
             
             # Simple script to remember scroll position
             tags$script(HTML("
               $(document).ready(function() {
                 // Save scroll position when user scrolls
                 $('#motivationTable').on('scroll', function() {
                   sessionStorage.setItem('motivationScroll', $(this).scrollTop());
                 });
                 
                 // Restore scroll position from session storage
                 var savedScroll = sessionStorage.getItem('motivationScroll');
                 if (savedScroll) {
                   setTimeout(function() {
                     $('#motivationTable').
                     $('#motivationTable').scrollTop(savedScroll);
                   }, 200);
                 }
               });
             ")),
             
             # Motivations table - using shiny inputs with custom class
             div(id = "motivationTable", class = "table-container", 
                 tags$table(
                   class = "data-table",
                   style = "width: 100%;",
                   tags$thead(
                     tags$tr(
                       tags$th(style = "width: 40%;", "Motivation"),
                       tags$th(style = "width: 60%;", "Rating")
                     )
                   ),
                   tags$tbody(
                     lapply(seq_along(motivations), function(i) {
                       tags$tr(
                         tags$td(motivations[i]),
                         tags$td(
                           div(class = "motivation-radio",
                               radioButtons(
                                 inputId = paste0("motivation_", i),
                                 label = NULL,
                                 choices = likert_options,
                                 selected = getSavedValue(paste0("motivation_", i), character(0)),
                                 inline = TRUE
                               )
                           )
                         )
                       )
                     })
                   )
                 )
             )
           ),
           
           # Major Purchases/Investments Section
           "Major Purchases/Investments" = tagList(
             h3("Major Purchases/Investments"),
             
             # Question 14
             radioButtons("q14", paste("14. Did you or any member of your household make any major purchases or investments in British Columbia",
                                       "between April 2024 and March 2025 related in whole or in part to fishing in freshwater?",
                                       "(e.g., fishing rods, boats, motors, 4x4s, cabins, camping gear, trailers, etc.)",
                                       "Please note: Purchases of fishing supplies (lures, line, tackle, bait, etc.) are covered in another question"),
                          choices = c("Yes" = 1, "No" = 0),
                          selected = getSavedValue("q14", character(0)),
                          inline = FALSE,
                          width = "100%"
             ),
             
             # Question 15 
             conditionalPanel(
               condition = "input.q14 == 1",
               tags$strong("15. For each investment category, please indicate the amount of money spent in British Columbia between April 2024 and March 2025 by you and members of your household and estimate the percentage of the total amount you consider was directly attributed to freshwater fishing. If no amount was spent, please enter '0'."),
               
               # Investment table
               div(id = "investmentTable", class = "table-container",
                   tags$table(
                     class = "data-table",
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
                           tags$td(investmentCategories[i]),
                           tags$td(
                             numericInput(
                               inputId = paste0("amount_", i),
                               label = NULL,
                               value = getSavedValue(paste0("amount_", i), 0),
                               min = 0,
                               step = 0.01,
                               width = "100%"
                             )
                           ),
                           tags$td(
                             numericInput(
                               inputId = paste0("percent_", i),
                               label = NULL,
                               value = getSavedValue(paste0("percent_", i), 0),
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
             )
           ),
           
           # Freshwater Packages Section
           "Freshwater Packages" = tagList(
             h3("Freshwater Fishing Packages"),
             
             # Descriptive Text
             tags$div(
               style = "margin-bottom: 20px;",
               tags$strong("This question refers only to packages purchased in British Columbia from a fishing lodge, guide or outfitter (or their agent), which include a complete range of services such as lodging, food, transportation, etc.")
             ),
             
             # Question 16
             radioButtons("q16", "16. Did you or any member of your household purchase package deals (e.g., fishing and accommodation, all-inclusive trips, guided group trips, etc.) related to fishing in British Columbia freshwater?",
                          choices = c("Yes" = 1, "No" = 0),
                          selected = getSavedValue("q16", character(0)),
                          inline = FALSE,
                          width = "100%"
             ),
             
             # Question 17
             conditionalPanel(
               condition = "input.q16 == 1",
               tags$strong("17. Excluding expenditures on major purchases and package deals, please estimate the amount of money you and other members of your household spent on the following to fish Freshwater within British Columbia between April 2024 and March 2025. If no amount was spent, please enter a '0'."),
               
               # Package table
               div(id = "packageTable", class = "table-container",
                   tags$table(
                     class = "data-table",
                     tags$thead(
                       tags$tr(
                         tags$th("Package Category"),
                         tags$th("Amount spent in British Columbia ($)")
                       )
                     ),
                     tags$tbody(
                       lapply(seq_along(packageCategories), function(i) {
                         tags$tr(
                           tags$td(packageCategories[i]),
                           tags$td(
                             numericInput(
                               inputId = paste0("packageAmount_", i),
                               label = NULL,
                               value = getSavedValue(paste0("packageAmount_", i), 0),
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
             )
           ),
           
           # Demographics Section
           "Demographics" = tagList(
             h3("Demographic Questions"),
             
             tags$div(
               style = "margin-bottom: 20px;", 
               tags$strong("In order to analyse this survey in a meaningful way, we require some personal information about you and your household. This will allow us to accurately estimate the number of anglers in the province, their use of the resource and the economic benefits generated by recreational fishing in freshwater in British Columbia. Your responses will remain confidential.")
             ),
             
             # Question 18
             selectInput("q18", "18. Which of the following age groups do you belong to?",
                         choices = c("", "Under 18", "18-29", "30-39", "40-49", "50-59", 
                                     "60-69", "70-79", "80-89", "90 or over", "Prefer not to answer"), 
                         selected = getSavedValue("q18")),
             
             # Question 19
             radioButtons("q19", "19. What is your gender?",
                          choices = c("Male", "Female", "Transgender", "Non-binary", "Other", "Prefer not to answer"),
                          selected = getSavedValue("q19", character(0))),
             
             # Question 20
             numericInput("q20", "20. How many members (16 years and over) of your household held a BC freshwater fishing license in 2024-2025?", 
                          value = getSavedValue("q20", 0), min = 0),
             
             # Question 21
             numericInput("q21", "21. How many members of your household under 16 years of age fished in BC freshwater in 2024/2025?",
                          value = getSavedValue("q21", 0), min = 0)
           ),
           
           # Comments Section + Email entry 
           "Comments" = tagList(
             h3("Comments"),
             
             # Question 22
             textAreaInput("q22", "22. Please add any additional comments here:",
                           value = getSavedValue("q22", "")),
             
             # Question 23
             textAreaInput("q23", "23. If you would like to be included in the random prize draw for 1 of 3 $100 Cabelas / Bass Pro Shops giftcards, please enter your email below.",
                           value = getSavedValue("q23", "")),
             # Add space before submit button
             br(),
             
             # Submit Button
             div(
               style = "text-align: center; margin-top: 20px;",
               actionButton("submitBtn", "Submit Survey", 
                            class = "btn-success",
                            style = "font-size: 18px; padding: 10px 30px;")
             ),
             
             # Hidden Thank You message (will be shown after submission)
             div(
               id = "thankYouMessage",
               style = "display: none; text-align: center; padding: 30px; margin-top: 20px;",
               h2("Thank you for completing the survey."),
               p("Your responses have been submitted successfully."),
               p("You may close this window.")
             )
           ),
           
    )
  })
  
  # Render the fishing days table dynamically for question 8
  output$fishingTableOutput <- renderTable({
    # Create a data frame for the table
    data.frame(
      Month = month.name[((1:12 + 3) %% 12) + 1],  # April to March
      `Total Days Fishing` = sapply(1:12, function(i) {
        as.character(
          selectInput(
            inputId = paste0("daysFishing_", i),
            label = NULL,
            choices = c("", 0:31),  # Days in month will be limited client-side
            selected = getSavedValue(paste0("daysFishing_", i), "")
          )
        )
      }),
      `Approximate Number of Fish Caught` = sapply(1:12, function(i) {
        as.character(
          textInput(
            inputId = paste0("fishCaught_", i),
            label = NULL,
            value = getSavedValue(paste0("fishCaught_", i), ""),
            placeholder = "Enter number"
          )
        )
      }),
      `Approximate Number of Fish Harvested` = sapply(1:12, function(i) {
        as.character(
          textInput(
            inputId = paste0("fishHarvested_", i),
            label = NULL,
            value = getSavedValue(paste0("fishHarvested_", i), ""),
            placeholder = "Enter number"
          )
        )
      }),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }, sanitize.text.function = function(x) x, 
  class = "data-table", 
  rownames = FALSE, 
  colnames = TRUE, 
  width = "100%")
  
  # Display months where fishing occurred for question 11
  output$monthsFishedText <- renderText({
    # Get months where fishing days were entered
    active_months <- sapply(1:12, function(i) {
      days <- input[[paste0("daysFishing_", i)]]
      !is.null(days) && !is.na(as.numeric(days)) && as.numeric(days) > 0
    })
    
    # Get the names of months with activity
    fished_months <- months[active_months]
    
    if(length(fished_months) > 0) {
      return(sprintf(": [%s]", paste(fished_months, collapse = ", ")))
    } else {
      return("")
    }
  })
  
  # Render the management regions table for question 11
  output$regionsTableOutput <- renderTable({
    # Get months where fishing days were entered
    active_months <- sapply(1:12, function(i) {
      days <- input[[paste0("daysFishing_", i)]]
      # Check if days is not NULL and greater than 0
      !is.null(days) && !is.na(as.numeric(days)) && as.numeric(days) > 0
    })
    
    # Only include months where fishing occurred
    filtered_months <- months[active_months]
    
    # If no months have fishing days, return message
    if(length(filtered_months) == 0) {
      return(data.frame(
        Message = "Please enter fishing days in question 8 first.",
        stringsAsFactors = FALSE
      ))
    }
    
    # Create table only for months with fishing activity
    data.frame(
      Month = filtered_months,
      `Regions Fished` = sapply(which(active_months), function(i) {
        as.character(
          tags$div(
            class = "checkbox-group",
            style = "display: flex; flex-direction: row; gap: 5px;",
            lapply(c("1", "2", "3", "4", "5", "6", "7a", "7b", "8"), function(region) {
              tags$div(
                class = "checkbox-item",
                style = "display: flex; align-items: center;",
                checkboxInput(
                  paste0("regionsFished_", i, "_", region),
                  label = paste("Region", region),
                  value = getSavedValue(paste0("regionsFished_", i, "_", region), FALSE)
                )
              )
            })
          )
        )
      }),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }, sanitize.text.function = function(x) x,
  class = "data-table", 
  rownames = FALSE, 
  colnames = TRUE, 
  width = "100%")
  
  # Render the waterbody table for question 11b
  output$waterbodyTableOutput <- renderTable({
    # Get months where fishing days were entered
    active_months <- sapply(1:12, function(i) {
      days <- input[[paste0("daysFishing_", i)]]
      !is.null(days) && !is.na(as.numeric(days)) && as.numeric(days) > 0
    })
    
    # Create empty data frame to store results
    results <- data.frame(
      Month = character(),
      Region = character(),
      Waterbody = character(),
      stringsAsFactors = FALSE
    )
    
    # Only process months where fishing occurred
    active_month_indices <- which(active_months)
    
    # For each active month and region, check if it was selected in question 11
    for(i in active_month_indices) {
      for(region in c("1", "2", "3", "4", "5", "6", "7a", "7b", "8")) {
        if(isTRUE(input[[paste0("regionsFished_", i, "_", region)]])) {
          results <- rbind(results, data.frame(
            Month = months[i],
            Region = paste("Region", region),
            Waterbody = as.character(
              tags$div(
                style = "display: flex; gap: 10px;",
                selectInput(
                  paste0("waterbody_", i, "_", region),
                  label = NULL,
                  choices = c(
                    "Select" = "",
                    "Rivers only" = "rivers",
                    "Lakes only" = "lakes",
                    "Both rivers and lakes" = "both"
                  ),
                  selected = getSavedValue(paste0("waterbody_", i, "_", region), ""),
                  width = "200px"
                )
              )
            )
          ))
        }
      }
    }
    
    if(nrow(results) == 0) {
      results <- data.frame(
        Message = "Please select fishing regions for the months where you fished.",
        stringsAsFactors = FALSE
      )
    }
    
    return(results)
  }, sanitize.text.function = function(x) x,
  class = "data-table", 
  rownames = FALSE, 
  colnames = TRUE, 
  width = "100%")
  
  # Question 12 validation message
  output$q12_validation <- renderUI({
    # Convert inputs to numeric
    private_boat <- as.numeric(input$q12_private_boat)
    shore <- as.numeric(input$q12_shore)
    guide_charter <- as.numeric(input$q12_guide_charter)
    
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
  
  # Skip logic based on q2 input
  observeEvent(input$q2, {
    if (input$q2 == 0) {  # If the user selects "No" for Question 2
      # Save the current section data before skipping
      saveCurrentSectionData()
      # Skip to the section with Question 13
      currentSection(which(sections == "Motivations"))
    }
  })
  
  # Skip logic based on q3 input
  observeEvent(input$q3, {
    if (input$q3 == 0) {  # If the user selects "No" for Question 3
      # Save the current section data before skipping
      saveCurrentSectionData()
      # Skip to the section with Question 8
      currentSection(which(sections == "Activities1"))
    }
  })
  
  # Navigation logic for moving between sections
  observeEvent(input$nextBtn, {
    # Disable buttons to prevent double-clicking
    shinyjs::disable("nextBtn")
    shinyjs::disable("prevBtn")
    
    # Save current section data first
    saveCurrentSectionData()
    
    # Then navigate to next section
    if (currentSection() < length(sections)) {
      currentSection(currentSection() + 1)
      # Scroll to top of page
      runjs("window.scrollTo({ top: 0, behavior: 'smooth' });")
    }
    
    # Re-enable buttons
    shinyjs::enable("nextBtn")
    shinyjs::enable("prevBtn")
  })
  
  # Navigation logic for previous button
  observeEvent(input$prevBtn, {
    # Disable buttons to prevent double-clicking
    shinyjs::disable("prevBtn")
    shinyjs::disable("nextBtn")
    
    # Save current section data first
    saveCurrentSectionData()
    
    # Then navigate to previous section
    if (currentSection() > 1 && currentSection() != length(sections)) {
      currentSection(currentSection() - 1)
      # Scroll to top of page
      runjs("window.scrollTo({ top: 0, behavior: 'smooth' });")
    }
    
    # Re-enable buttons
    shinyjs::enable("prevBtn")
    shinyjs::enable("nextBtn")
  })
  
}

# Run the app
shinyApp(ui = ui, server = server)