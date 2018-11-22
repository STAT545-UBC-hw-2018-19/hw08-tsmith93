library(shiny)
library(ggplot2)
library(tidyverse)
library(shinythemes)
library(knitr)
library(DT)
library(stringr)

#create object for dataset you are using
bcl <- read.csv("bcl-data.csv", stringsAsFactors = FALSE)

#this section describes the user interface i.e. what your user will be interacting with
#fluidPage() scales the app components in realtime to ensure all the available browser width is used
ui <- fluidPage(
  #theme customizes the overall app aesthetic
  theme = shinytheme("lumen"),
  #give the page a title
  titlePanel("BC Liquor Store Price Explorer",
             #this changes the tab title for your browser
             windowTitle = "BCL Exploration App"),
  #customize the look of your sidebar
  sidebarLayout(
    sidebarPanel(
      h4(
        #give users more insight on how to use app
        "Interact with these tools to find the product you desire"
      ),
      #add a break so users are not visually overwhelmed
      br(),
      #title for slider
      sliderInput("priceInput", "What's your price range?",
                  #range of slider and units
                  min = 0, max = 1000, c(50, 200), pre = "C$"),
      #describe check box
      checkboxGroupInput("typeInput", "What type of alcohol are you looking for?",
                         #choices for checkbox
                         choices = c("BEER", "REFRESHMENT", "SPIRITS", "WINE"),
                         #default checked boxes
                         selected = c("BEER", "REFRESHMENT", "SPIRITS", "WINE")),
      #add checkbox to filter results by lowest price first (default is highest first ;))
      checkboxInput("sortInput", "Start with lowest prices", # an option to sort the results table by price.
                                  value= FALSE,
                                  width= NULL),
      #link iu to server
      uiOutput("countryOutput"),
      #allow users to select country
      conditionalPanel(condition = "input.countryOption", uiOutput("countryOption"))
    ),
    #add ui content to main panel
    mainPanel(
      #use this header to describe findings (see server section for how)
      h3(textOutput("findings")),
      #add breaks
      br(),
      #add option for users to export findings
      downloadButton("downloader", "Save List"),
      #add breaks
      br(), br(),
      #separate table and plot into different tabs
      tabsetPanel( 
        #title plot tab and define content
        tabPanel("Visualizer", plotOutput("price_plot")),
        #title the table tab and define content
        tabPanel("List", DT::dataTableOutput("search")),
      #add fun image encouraing users to drink responsibly  
      img(src="liqua.jpg", width = "100%")
    )
    )
  )
)

#this section feeds desired data into ui
server <- function(input, output) {
  #link output to the country variable
  output$countryOutput <- renderUI({
    #link data to the output and give explanation to user
    selectInput("countryInput", "Product of which country?",
                #define choices
                choices = c(bcl$Country),
                #give default
                selected = "CANADA")
  })  
  
  #ensure results update automatically with reactive
  filtered <- reactive({
    #allow result to update to country selection
    if (is.null(input$countryInput)) {
      return(NULL)
    }
    #allow price sorting and selection
    if (input$sortInput) {
    bcl %>%
        #allow users to select minimum threshold of data
      filter(Price >= input$priceInput[1],
             #allow users to select upper threshold of data filter
             Price <= input$priceInput[2],
             #allow users to select type of alcohol
             Type %in% input$typeInput,
             #allow users to seect specific country
             Country == input$countryInput) %>% 
        #arrange price in ascending order
        arrange(Price)
    } else {
      #call on dataframe
      bcl %>%
        filter(Price >= input$priceInput[1],
               Price <= input$priceInput[2],
               Type %in% input$typeInput, #to choose more than one product type.
               Country == input$countryInput) %>% 
        #arranfe price by descending order
        arrange(desc(Price))
    }
  })
  
  #allow users to specify data in plot
  output$price_plot <- renderPlot({
    if (is.null(filtered())) {
      return()
    }
    #make ggplot with price on x axis, and fill based on type
    ggplot(filtered(), aes(Price, color = Type, fill = Type)) +
      #manually enter palette
      scale_fill_manual(values=cbPalette) +
      geom_histogram(colour = "black") + 
      scale_x_continuous() +
      #set plot theme
      theme_bw() +
      #add fancy axes labels
      labs(x="Product Price", y="Number of Products")
  })
  
  #give table outpute, DT makes table interactive
  output$search <- DT::renderDataTable({
    if (input$sortInput==TRUE) {
      #filter data by price
    filtered() %>% 
        arrange(Price)
    }
    else filtered()
  })
  
  #let users see how many results they have
  output$findings <- renderText({
    #define the phrase used for user to see, nrow(filtered()) gives the specific number of results based on search
    paste0("Check out these ", nrow(filtered()), " products  ")
  })
  
  #make app more interactive by allowing users to download results 
  output$downloader <- downloadHandler( 
    filename = function(){
      "bcl-data.csv"
    },
    #this code allows users to download based on their search criteria
    content = function(resultz){
      write.csv(filtered(),resultz)
    }
  )
}

#tie it all together!
shinyApp(ui = ui, server = server)
