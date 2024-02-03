library(shiny)
library(shinydashboard)
library(synthesisr)
library(shinybusy)
library(openalexR)
library(openalex)
library(tidyverse)

source('build_ris.R')
source('search_openAlex.R')
source('reconstruct_abstract.R')

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Repair missing and incomplete abstracts in RIS files"),
    br(),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          fileInput("file", 
                    "RIS file upload", 
                    multiple = FALSE, 
                    accept = c('.ris', '.txt')),
          uiOutput('fixbutton'),
          br(),
          uiOutput('downloadbutton'),
          add_busy_spinner(spin = "fading-circle", color = "#e43235", margins = c(70, 20), position='bottom-left')
        ),

        # Show a plot of the generated distribution
        mainPanel(
          textOutput('report')
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  rv <- reactiveValues()
  
  observeEvent(input$file,{
    validate(need(input$file != "", "Select your bibliographic file to upload..."))
    
    #do nothing if file is not uploaded
    if (is.null(input$file)) {
      return(NULL)
    } else {
      
      #upload file
      rv$input <- read_refs(input$file$datapath) #read in the RIS file as a df
      
      dois <- rv$input$doi #set the doi column
      doisLIST <- data.frame(ids=dois, type=rep('doi', length(dois))) #create an input dataframe with identifier type
      rv$doisLIST <- na.omit(doisLIST) #remove NAs
      
      #create report
      rv$report <- paste0('Your file contains ', nrow(rv$input), ' records. ', nrow(rv$doisLIST), ' DOIs were identified.')
    }
    
    #render report
    output$report <- renderText({
      rv$report
    })
    
    #render fix button UI
    output$fixbutton <- renderUI({
      if (is.null(input$file)) return(NULL)
      tagList(
        br(),
        actionButton('repair', 'Repair RIS')
      )
    })
    
    observeEvent(input$repair,{
      
      openalex_polite("neal_haddaway@hotmail.com")
      rv$results <- search_openAlex(rv$doisLIST) #search OA for dois
      print(paste0(length(rv$results), ' potential matches were found on OpenAlex'))
      
      #reconstruct abstracts
      abstracts <- data.frame(doi = NULL, abstract = NULL) #reconstruct abstracts
      for (i in 1:length(rv$results)){
        tryCatch({
          ab_new <- reconstruct_abstract(rv$results[[i]])
          doi_new <- names(rv$results[i])
          row <- data.frame(doi = doi_new, abstract = ab_new)
        }, error=function(e){ab_new <<- NA})
        abstracts <- rbind(abstracts, row)
      }
      abstracts <- abstracts[!duplicated(abstracts),] #remove duplicate entries
      print(paste0(nrow(abstracts), ' of these records possessed abstracts'))
      
      #replace all abstracts with those from Open Alex
      output <- merge(rv$input, abstracts, by.x = "doi", 
                      by.y = "doi", all.x = TRUE, all.y = FALSE)
      output <- output %>% #merge two abstract columns, replacing NAs with partial abstracts
        mutate(abstract.y = coalesce(abstract.y, abstract.x))
      output$abstract.x <- output$abstract.y #replace original partial abstract column with new data
      names(output)[names(output) == 'abstract.x'] <- 'abstract' #rename
      output <- subset(output, select = -c(abstract.y)) #remove merged abstract column
      print(paste0('Your data have been repaired to include these ', nrow(abstracts), ' complete abstracts'))
      
      #rebuild RIS file
      rv$ris <- build_ris(output, save=FALSE)
      print('Your RIS file has been built')
      rv$report <- paste0(rv$report, ' ', length(rv$results), ' potential matches were found on OpenAlex. ', nrow(abstracts), ' of these records possessed abstracts. Your data have been repaired.')
      
      
    })
    
    #render download button UI
    output$downloadbutton <- renderUI({
      if (is.null(rv$ris)) return(NULL)
      tagList(
        downloadButton('download', 'Download repaired RIS')
      )
    })
    
    #download repaired RIS file
    output$download <- downloadHandler(
      filename = function(){
        paste("REPAIRED-", Sys.Date(), ".ris", sep = "")
      },
      content = function(file) {
        write.table(rv$ris, file, row.names = FALSE, col.names = FALSE)
        
      }
    )
    
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
