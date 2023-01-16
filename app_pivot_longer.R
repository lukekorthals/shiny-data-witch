##############################################
## App for using the pivot_longer() function ##
##############################################

# Load required packages and R files
library(shiny)
library(shinyBS) # info on hover 
library(DT) # select-able data tables
library(tidyverse) # pivot functions
source("design_elements.R")

# Define path to save csv files
path = getwd()

# UI 
ui_pivot_longer = function(){
  sidebarLayout(
    
    # Sidebar panel for inputs
    sidebarPanel(
      style = paste("position: fixed; 
      overflow: auto; 
      width:30%;
      margin-bottom:50px;
      opacity: o.5;
      background-color: ", lightblue),

      # Load original csv
      fileInput("pl_input_file", p("Choose CSV File"),
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")
      ),
      
      # Inputs to rename reformatted columns
      textInput("pl_text_names_to", p("Rename \"Names\" Column"), 
                value = "Names"),
      bsTooltip(id = "pl_text_names_to", 
                title = "This column containes the names of the selcted columns"),
      textInput("pl_text_values_to", p("Rename \"Values\" Column"), 
                value = "Values"),
      bsTooltip(id = "pl_text_values_to", 
                title = "This column containes the values of the selcted columns"),
      
      # Button to save new csv
      strong("Create CSV"),
      br(),
      downloadButton("pl_button_save_csv", "Save CSV")
    ),
    
    # Main panel for rendering the data tables
    mainPanel(

      # Original Data
      h2("Original Data"),
      
      
      # Step by step
      p("💡 Columns are selected by clicking on their values"),
      p("1️⃣ Select a csv file"),
      p("2️⃣ Select columns to reformat"),
      p("3️⃣ Check the preview"),
      p("4️⃣ Rename the columns \"Names\" and \"Values\""),
      p("5️⃣ Create your new csv file"),
      
      # Original data table
      DT::dataTableOutput("pl_raw_table"),
      
      # Preview table
      h2("Preview"),
      p("This is how your data will look like after reformating"),
      DT::dataTableOutput("pl_preview_table"),
      
      # Speech bubble for used code
      witch_speaks_code( # imported from design_elements.R
        h3_text = "Fun Fact!", 
        h2_text = "You are using this code:", 
        code_text = textOutput("pl_tidy_code")
        )
    )
  )
}

# Server
server_pivot_longer = function(input, output, session){
  
  # Load CSV
  pl_raw_dat = reactive({
    
    # Validate file
    input_file <- input$pl_input_file
    if (is.null(input_file)){
      return(NULL)
    }

    # Read file
    read.csv(input_file$datapath)
  })
  
  # Render original data table
  output$pl_raw_table <- DT::renderDataTable({
    
    # Validate data frame
    if (is.null(pl_raw_dat())){
      return(NULL)
    }
    
    # Render data table
    datatable(pl_raw_dat(), 
              extensions = "Select", 
              selection = list(target = "column"), 
              options = list(ordering = TRUE, searching = TRUE)
              ) 
  })
  
  # Selected columns
  pl_selected_columns = reactive({
    pl_selected_columns = names(pl_raw_dat()[input$pl_raw_table_columns_selected])
  })
  
  # Render preview data table according to input and selected columns
  pl_preview <- reactive({
    
    # Validate data frame and selected columns
    if(is.null(pl_raw_dat()) | length(pl_selected_columns()) == 0) {
      return(NULL)
    }
    
    # Create preview data frame
    preview <- pl_raw_dat() %>% 
      # Reformat to long 
      pivot_longer(
        pl_raw_dat(), 
        cols = pl_selected_columns(), # selected columns
        names_to = input$pl_text_names_to, # set column name
        values_to = input$pl_text_values_to # set column name
        )
  })
  
  # Render preview table
  output$pl_preview_table <- DT::renderDataTable({
    datatable(pl_preview(),
              options = list(ordering = TRUE, searching = TRUE)
              )
  })
  
  # Save preview to csv
  output$pl_button_save_csv <- downloadHandler(
    filename = "data_long.csv",
    content = function(file) {
           write.csv(pl_preview(), file, row.names = FALSE)
         }
    )
  
  # Render used tidyverse code
  output$pl_tidy_code = renderText({
    paste("preview <- raw_dat %>% pivot_longer(raw_dat, cols = c(", paste0('"', paste(pl_selected_columns(), collapse='", "'), '"'), "), names_to = \"", input$pl_text_names_to, "\", values_to = \"",  input$pl_text_values_to, "\")", sep="")
  })
}
