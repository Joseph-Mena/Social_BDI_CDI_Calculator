##### SOCIAL BDI/CDI CALCULATOR R SHINY APPLICATION #####
##### CODE WRITTEN AND MAINTAINED BY JOSEPH MENA #####

library(shiny)
library(tidyverse)
library(magrittr)
library(httr)
library(jsonlite)
library(shinythemes)
library(tools)

#options(rsconnect.max.bundle.size=800000000000)

ui <- fluidPage(theme=shinytheme("cosmo"),
                
                
                
                # Application title
                titlePanel(strong("Social BDI/CDI Calculator")),
                
                
                sidebarLayout(
                  sidebarPanel(
                    
                    helpText("Discover a brand's organic presence in social media by state relative to the state's population (BDI) and compare it with the social presence of its category (CDI).")
                    ,
                    
                    helpText("First, enter your brand:"),
                    helpText("Tip: If an entry has more than one meaning, like Dawn or Tide, then try making your search a little more specific, like 'Tide Detergent' or the boolean 'Tide AND Detergent'")
                    ,
                    
                    textInput("brandsearch", "Search Brand:", ""),
                    br(),
                    helpText("Next enter the category the brand is in:"),
                    
                    helpText("Tip: try to keep your search in everyday laymen's terms (e.g. instead of 'salty snacks', just search 'chips')"),
                    
                    textInput("categorysearch", "Search Category:", ""),
                    
                    
                    actionButton("button", "Calculate")
                    
                    #br(),
                    #br(),
                    
                    #uiOutput("outputSlider"),  
                    #uiOutput("outputSlider2")  
                    
                  
                  ),
                  
                  mainPanel(
                    
                    
                    
                    conditionalPanel(
                      condition = "input.button > 0", radioButtons(inputId = "radiobuttons", "Select output:", c("Sortable Table", "Download Link"), selected = "Sortable Table", inline = TRUE)
                    ),
                    
                   
                    conditionalPanel( 
                      condition = "input.button > 0 && input.radiobuttons == 'Sortable Table'",
                      helpText("A BDI or CDI over 100 indicates a brand or category is being strongly discussed online in a state relative to its population, and underperforming when below 100"), helpText("Tip: Set BDI range to maximum of 100 and CDI range to minimum of 100 and sort table by least-to-greatest Difference to identify states where interest in a category is high but interest in the particular brand is lacking",  br(), br(), fluidRow(align = "center", splitLayout(div(style="display: inline-block;vertical-align:top; width: 150px;", uiOutput("outputSlider")), div(style="display: inline-block;vertical-align:top; width: 150px;", uiOutput("outputSlider2")))))
                    ),
                    
                    conditionalPanel( 
                      condition = "input.radiobuttons == 'Sortable Table'", dataTableOutput("table1")),
                    
                    conditionalPanel(
                      condition = "input.button > 0 && input.radiobuttons == 'Download Link'", strong(helpText("Click below to download CSV data of Social BDI/CDI by State which can then be opened with Excel")), downloadButton("downloadData1", "Download CSV"))
                    
                    
                    
                  )
                  
                )
                
)  



server <- function(input,output)  {
  
  observeEvent(input$button,{
    
    showModal(modalDialog("Loading data...This shouldn't take long...", footer=NULL))
    
    #data derived from API call
    
    #components of API request to concatenate, including authentication and parameters
    base <- "https://atlas.infegy.com/api/v2/"
    endpoint <- "states"
    api_key <- ### THIRD-PARTY PRIVATE KEY ###
    
    #query <- "pringles"
    #query2 <- "chips"
    
    query <- reactive(gsub(" ","%20", input$brandsearch))
    query2 <- reactive(gsub(" ","%20", input$categorysearch))
    
    
    start_date <- "start_date=1%20year%20ago"
    end_date <- "end_date=now"
    countries <- "countries=US"
    
    organic_only <- "%20NOT%20(ad%20OR%20advertiser%20OR%20advertisement%20OR%20promo%20OR%20promotional%20OR%20promoted%20OR%20promote%20OR%20promotion%20OR%20propaganda%20OR%20endorsement%20OR%20commercial%20OR%20coupon%20OR%20%22paid%20media%22%20OR%20%22paid%20advertising%22%20OR%20%23ad%20OR%20%23advertising%20OR%20%23advertisement%20OR%20%23promotional%20OR%20%23promo%20OR%20%23promotion%20OR%20%23endorsement%20OR%20%23propaganda%20OR%20%23commercial%20OR%20%23coupon%20OR%20%23marketing%20OR%20%23digitalmarketing%20OR%20%23socialmediamarketing%20OR%20%23contentmarketing%20OR%20%23SMM%20OR%20%23onlinemarketing%20OR%20%23marketingdigital%20OR%20%23paidmedia%20OR%20%23paidadvertising)"
    
    #full request
    call1 <- paste0(base,endpoint,"?",api_key,"&","query=",query(),organic_only,"&",start_date,"&",end_date,"&",countries)
    call2 <- paste0(base,endpoint,"?",api_key,"&","query=",query2(),organic_only,"&",start_date,"&",end_date,"&",countries)
    
    
    #use httr's GET function to request infegy API
    get_query_states <- GET(call1)
    get_query_states2 <- GET(call2)
    
    
    #use httr's content function to deserialize/convert raw data from call into JSON format
    get_query_states_text <- httr::content(get_query_states, "text")
    get_query_states_text2 <- httr::content(get_query_states2, "text")
    
    
    #parse the JSON using the jsonlite package
    get_query_states_json <- fromJSON(get_query_states_text, flatten = TRUE)
    get_query_states_json2 <- fromJSON(get_query_states_text2, flatten = TRUE)
    
    
    #converting output component of states into data frame
    get_query_states_df <- as.data.frame(get_query_states_json$output)   
    get_query_states_df2 <- as.data.frame(get_query_states_json2$output)   
    
    
    library(tidyverse)
    library(magrittr)
    

    BDI_df <- get_query_states_df %>%
      select(name,sources,population) %>%
      filter(!name %in% c('Hawaii','Alaska','American Samoa','Puerto Rico','Virgin Islands','District of Columbia','')) %>%
      mutate_if(is.numeric, funs(./sum(.))) %>% 
      mutate(BDI = (sources/population)*100) %>%
      select(name,BDI)
    
    
    CDI_df <- get_query_states_df2 %>%
      select(name,sources,population) %>%
      filter(!name %in% c('Hawaii','Alaska','American Samoa','Puerto Rico','Virgin Islands','District of Columbia','')) %>%
      mutate_if(is.numeric, funs(./sum(.))) %>% 
      mutate(CDI = (sources/population)*100) %>%
      select(name,CDI)
    
    
    Combined_df <- BDI_df %>%
      left_join(CDI_df, by="name") %>%
      rename(State = name) %>%
      mutate(Difference = BDI - CDI) %>%
      mutate_if(is.numeric, round)
    
    
    
    output$table1 <- renderDataTable({
      Combined_df %>%
        filter(BDI>input$BDI[1], BDI<input$BDI[2]) %>%
        filter(CDI>input$CDI[1], CDI<input$CDI[2])
    })
    
    
    
    output$outputSlider <- renderUI({
      sliderInput(inputId = "BDI",
                  label = "BDI range",
                  min = 0,
                  max = max(Combined_df$BDI + 1, na.rm = TRUE),
                  value = c(0, max(Combined_df$BDI + 1, na.rm = TRUE)))
    })
    
    output$outputSlider2 <- renderUI({
      sliderInput(inputId = "CDI",
                  label = "CDI range",
                  min = 0,
                  max = max(Combined_df$CDI + 1, na.rm = TRUE),
                  value = c(0, max(Combined_df$CDI + 1, na.rm = TRUE)))
    })
    
    
    
    
    
    output$downloadData1 <- downloadHandler(
      filename = function() {
        paste(input$brandsearch, "_", input$categorysearch, "_social_BDI_CDI_by_state", ".csv", sep = "")
      },
      content = function(file) {
        write.csv(Combined_df, file, row.names = FALSE)
      }
    )
    
    removeModal()
    
    
  }) 
  
}   



shinyApp(ui = ui, server = server)

#remove(list = ls())
