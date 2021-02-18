library(shiny)
library(shinyChakraSlider)
library(tidyverse)
library(DT)
library(shinyjs)


ui <- fluidPage(
  
  useShinyjs(),
  
  title = "Creating Scrapable Datasets from Google Audioset",
  
  dataTableOutput("new_csv_dt"),
  
  hr(),
  
  fluidRow(
    column(3,
           h2("File Management"),
           # file upload 
           fileInput(
             "newFile",
             "Upload .csv File",
             multiple = FALSE,
             accept = c('csv', 'comma-separated-values','.csv')
           ),
           
           # number of segments
           numericInput(
             "numSegments",
             "Number of Segments",
             value = 0,
             min = 0,
             max = 0
           ),
           
           sliderInput("sliderNumSegments",
                             label = "",
                             value = 0, min = 0, max = 0, step = 1
           ),
          
           
           actionButton("processFile", "Apply Changes"),
           
           downloadButton("downloadFile", "Download File")
    ),
    
    column(6,
           h2("Label Filtering"),
           
           textInput(
             "includeLabels",
             "Include Labels - Separate by Comma",
             width = '90%'
           ),
           
           textInput(
             "excludeLabels",
             "Exclude Labels - Separate by Comma",
             width = '90%'
           ),
           ),
    
    column(3,
           h2("Search Labels"),
           dataTableOutput("label_dt")
    )
  )
  
)

# Define server logic for random distribution app ----
server <- function(input, output, session) {
  
  csv_with_labels <- NULL
  download_csv <- read.csv("blank.csv")
  last_processed_filename <- ""
  
  add_labels <- function(x, labelDict) {
    numRows <- nrow(x)
    for (row in 1:nrow(x)) {
      # parse label_ids to vector
      all_label_ids <- as.character(x[row, 'positive_labels'])
      all_label_ids <- unlist(strsplit(all_label_ids, ","))
      
      numLabels <- length(all_label_ids)
      natural_lang_labels <- character()
      
      # add all natural language labels to a vector
      # which will be collapsed to a single string later
      for (i in 1:numLabels) {
        label_id <- str_trim(all_label_ids[[i]])
        natural_label <- labelDict[label_id, 'label']
        natural_lang_labels <- c(natural_lang_labels, natural_label)
      }
      
      # collapse vector of natural language labels and assign to row
      x[row, 'decoded_labels'] <- paste(natural_lang_labels, collapse = ", ")
      setProgress(
        row/numRows,
        detail = paste("Row ", row, " of ", numRows)
        )
    }
    
    x
  }
  
  filter_by_label <- function(x, include, mode = 'include') {
    
    numRows <- nrow(x)
    
    print(paste("Looking for", include))
    
    for (row in 1:numRows) {
      from_data <- tolower(str_trim(unlist(str_split(x[row, 'decoded_labels'], ","))))
      
      matches <- (from_data %in% include)
      
      for (i in 1:length(matches)) {
        if (isTRUE(matches[i])) {
          x[row, 'include'] <- TRUE
          break
        }
      }
      
      if (is.null(x[row, 'include']) || is.na(x[row, 'include'])) {
        x[row, 'include'] <- FALSE
      }
      
      setProgress(
        row/numRows,
        detail = paste("Row ", row, " of ", numRows)
      )
    }
    
    if (mode == 'include') {
      x <- x %>%
        filter(include == TRUE) %>%
        select(!include)
    }
    
    else {
      x <- x %>%
        filter(include == FALSE) %>%
        select(!include)
    }
    
    x
  }
  
  output$new_csv_dt = renderDataTable(
    datatable(as.data.frame(read.csv("blank.csv")))
  )
  
  observeEvent(input$newFile, {
    #print(nrow(currentCSV()))
    
    disable("numSegments")
    disable("sliderNumSegments")
    
    withProgress(
      csv_with_labels <<- add_labels(currentCSV(), label_matches),
      message = "Decoding File Labels..."
      )
    output$new_csv_dt = renderDataTable(
      datatable(csv_with_labels)
    )
    
    enable("numSegments")
    enable("sliderNumSegments")
    
    updateNumericInput(session, "numSegments", 
                       value = nrow(currentCSV()), max = nrow(currentCSV()))
    updateSliderInput(session, "sliderNumSegments", 
                      value = nrow(currentCSV()), max = nrow(currentCSV()))
    
  })
  
  observeEvent(input$sliderNumSegments, {
    updateNumericInput(session, "numSegments", value = input$sliderNumSegments)
  })
  
  observeEvent(input$numSegments, {
    updateSliderInput(session, "sliderNumSegments", value = input$numSegments)
  })
  
  observeEvent(input$processFile, {
    
    # cut execution here if no file was uploaded
    # or the uploaded file was not done processing
    if (is.null(csv_with_labels)) {
      return()
    }
    
    #update filename
    last_processed_filename <<- "segments"
    
    processedCSV <- csv_with_labels
    
    # if the user gave labels to include, parse others out
    if (str_trim(input$includeLabels) != "") {
      include <- tolower(str_trim(unlist(str_split(input$includeLabels, ","))))
      #print(paste("Including labels", include))
      
      # update filename
      last_processed_filename <<- paste(last_processed_filename, "_incl=",paste(include, collapse = ","), sep="")
      
      processedCSV <- withProgress(
        filter_by_label(processedCSV, include, 'include'),
        message = "Filtering by included labels..."
          )
    }
    
    # if the user gave labels to exclude, parse them out
    # at a glance, this seems repetitive, but because
    # many segments have multiple sounds/labels,
    # there may be some bundled together in audio segments
    # that they wish to explicitly exclude, despite that the
    # desired sounds is included
    if (str_trim(input$excludeLabels) != "") {
      #print("Excluding")
      exclude <- str_trim(unlist(str_split(input$excludeLabels, ",")))
      
      # update filename
      last_processed_filename <<- paste(last_processed_filename, "_excl=", paste(exclude, collapse = ","), sep = "")
      
      processedCSV <- withProgress(
        filter_by_label(processedCSV, exclude, 'exclude'),
        message = "Filtering by excluded labels..."
      )
      
    }
    
    # only use selected number of segments
    numSamples <- input$numSegments
    if (numSamples > nrow(processedCSV)) {
      numSamples <- nrow(processedCSV)
    }
    
    last_processed_filename <<- paste(last_processed_filename, "_numSegments=", numSamples, ".csv", sep = "")
    
    processedCSV <- processedCSV %>%
      sample_n(numSamples)
    
    download_csv <<- processedCSV
    output$new_csv_dt = renderDataTable(
      datatable(processedCSV)
    )
  })
  
  label_matches <- as.data.frame(read_csv("label_matches.csv")) %>%
    arrange(label)
  rownames(label_matches) <- label_matches[, 'label_id']
  
  currentCSV <- reactive({
    as.data.frame(read.csv(input$newFile$datapath))
  })
  
  
  maxSegs <- reactive({
    nrows(currentCSV())
  })
  
  # DEFINE OUTPUTS
  
  output$allLabels <- renderTable({
    label_table <- label_matches %>%
      rename(
        "ID" = label_id,
        "Label" = label
      )
    
  })
  
  output$label_dt = renderDataTable(
    datatable(
      label_matches %>%
        rename(
          ID = label_id,
          Label = label
        ),
              rownames = FALSE)
  )
  
  
  
  output$data <- renderTable({
    currentCSV() %>%
      head()
  })
  
  output$downloadFile <- downloadHandler(
    filename = function() {
      paste(last_processed_filename)
    },
    content = function(file) {
      write.csv(download_csv, file, row.names = FALSE, quote = FALSE)
    },
    contentType = "text/csv"
  )
  
}

shinyApp(ui = ui, server = server)

