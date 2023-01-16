##############################################
## App for using the pivot_wider() function ##
##############################################

# Load required packages and R files
library(shiny)
library(shinyBS) # info on hover 
library(DT) # select-able data tables
library(tidyverse) # pivot functions
source("design_elements.R")

# Define path to save csv files
path = getwd()

ui_pivot_wider = function(){
  sidebarLayout(
    
    # Sidebar panel for inputs
    sidebarPanel(
      style = paste("position: fixed; 
                    overflow: auto; 
                    width:30%;
                    margin-bottom:50px;
                    background-color:", lightblue),
      
      # Load original csv
      fileInput("pw_input_file", "Choose CSV File",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")
      ),
      
      # Set columns "names from"
      strong("Names From"),
      br(),
      actionButton(
        "pw_button_names_from", "Set Names",
        style = paste("background-color:", orange)
        ),
      bsTooltip(id = "pw_button_names_from", 
                title = "Values of the selected columns will be used as names for the new columns."),
      
      # Set columns "values from"
      br(),
      br(),
      strong("Values From"),
      br(),
      actionButton(
        "pw_button_values_from", "Set Values",
        style = paste("background-color:", pink)
        ),
      bsTooltip(id = "pw_button_values_from", 
                title = "Values of the selected columns will be used as values for the new columns."),
      
      # Button to save new csv
      br(),
      br(),
      strong("Create CSV"),
      br(),
      downloadButton("pw_button_save_csv", "Save CSV")
    ),
    
    # Main panel for rendering the data tables
    mainPanel(
      
      # Original data 
      h2("Original Data"),
      
      # Step by step
      p("💡 Columns are selected by clicking on their values"),
      p("1️⃣ Select a csv file"),
      p("2️⃣ Select columns to create new columns from and click \"Set Names From!\"", tag_name = "test_id"),
      p("3️⃣ Select columns to take values from and click \"Set Values From!\""),
      p("4️⃣ Check the preview"),
      p("5️⃣ Create your new csv file"),
      
      # Original data table
      DT::dataTableOutput("pw_raw_table"),
      
      # Preview table
      h2("Preview"),
      p("This is how your data will look like after pivoting."),
      DT::dataTableOutput("pw_preview_table"), 
      
      # Speech bubble for used code
      witch_speaks_code(
        h3_text = "Fun Fact!", 
        h2_text = "You are using this code:", 
        code_text = textOutput("pw_tidy_code")
        )
    )
  )
}

# Server
server_pivot_wider = function(input, output, session){
  
  # Load CSV
  pw_raw_dat = reactive({
    
    # Validate file
    input_file <- input$pw_input_file
    if (is.null(input_file))
      return(NULL)
    
    # Read file
    read.csv(input_file$datapath)
  })
  
  
  # Render original data table
  output$pw_raw_table <- DT::renderDataTable({
    
    # Validate data frame
    if (is.null(pw_raw_dat())){
      return(NULL)
    }
    
    # Render data table
    datatable(pw_raw_dat(), 
              extensions = "Select", 
              selection = list(target = "column"), 
              options = list(ordering = FALSE, searching = FALSE)) %>% 
      formatStyle(
        pw_names_from, # format names from orange
        backgroundColor = orange) %>% 
      formatStyle(
        pw_values_from, # format values from pink
        backgroundColor = pink)
    
  }) 
  
  # Selected columns
  pw_names_from <- NULL
  pw_values_from <- NULL
  makeReactiveBinding("pw_names_from")
  makeReactiveBinding("pw_values_from")
  
  pw_selected_columns = reactive({
    pw_selected_columns = names(pw_raw_dat()[input$pw_raw_table_columns_selected])
    
  })
  
  # Update Selected Columns
  observeEvent(input$pw_button_names_from, {
    
    # Set names from to current selection
    pw_names_from <<- pw_selected_columns()
    
    # Clear current selection
    DT_proxy = dataTableProxy("pw_raw_table")
    reloadData(DT_proxy, clearSelection = "all")
  })
  
  observeEvent(input$pw_button_values_from, {
    # Set values from to current selection
    pw_values_from <<- pw_selected_columns()
    
    # Clear current selection
    DT_proxy = dataTableProxy("pw_raw_table")
    reloadData(DT_proxy, clearSelection = "all")
    DT_proxy 
  })
  
  # Render preview data table according to input and selected columns
  pw_preview <- reactive({
    
    # Validate data frame and selected columns
    if (is.null(pw_raw_dat()) | is.null(pw_names_from) | is.null(pw_values_from)){
      return(NULL)
    }
    
    # Create preview data frame
    preview = pw_raw_dat() %>% 
      # Reformat to wide
      pivot_wider(
        names_from = pw_names_from, 
        values_from = pw_values_from
        ) 
  })
  
  # Render preview table
  output$pw_preview_table <- DT::renderDataTable({
    datatable(pw_preview(),
              options = list(ordering = TRUE, searching = TRUE))
  })
  
  # Save preview to csv
  output$pw_button_save_csv <- downloadHandler(
    filename = "filename.csv",
    content = function(file) {
      write.csv(pw_preview(), file, row.names = FALSE)
    }
  )
  
  # Render used tidyverse code
  output$pw_tidy_code = renderText({
    paste("preview <- raw_dat %>% pivot_wider(names_from = c(", paste0('"', paste(pw_names_from, collapse='", "'), '"'), "), values_from = c(", paste0('"', paste(pw_values_from, collapse='", "'), '"'), "))", sep="")
  })
}