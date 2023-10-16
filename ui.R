
ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css?family=Inter"),
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$title("FI Report")
  ),
  
  # Application title
  titlePanel(div(img(src="InQube Logo.png", height = 50),
                 "FI Report",
                 span(span(textOutput('user', inline = T), ", ",
                           a(href = "https://login.shinyapps.io/logout", "Logout"),
                           style = "float:right;"), br(),
                      span(span('Data Refreshed at: ', style = "font-style: italic;"), 
                           textOutput('tm', inline = T),
                           style = "float:right;"), br(),
                      style = 'font-size: 13px; vertical-align:middle; float:right;')
  )
  ),
  inputPanel(
    pickerInput("customer", label = "Customer",
                choices = '',
                options = pickerOptions(actionsBox = T, liveSearch = T, maxOptions = 100),
                multiple = T),
    
    pickerInput("season", label = "Season",
                choices = '',
                options = pickerOptions(actionsBox = T, liveSearch = T, maxOptions = 100),
                multiple = T),
    
    pickerInput("plant", label = "Plant",
                choices = '',
                options = pickerOptions(actionsBox = T, liveSearch = T),
                multiple = T),
    
    pickerInput("extract.style", label = "Style Extract",
                choices = '',
                options = pickerOptions(actionsBox = T, liveSearch = T),
                multiple = T),
    
    # pickerInput("full.style", label = "Style Full",
    #             choices = '',
    #             selected = '',
    #             options = pickerOptions(actionsBox = T, liveSearch = T),
    #             multiple = T),
    
    pickerInput("year", label = "Year",
                choices = '',
                options = pickerOptions(actionsBox = T, liveSearch = T),
                multiple = T),
    
    pickerInput("month", label = "Month",
                choices = '',
                options = pickerOptions(actionsBox = T, liveSearch = T),
                multiple = T),
    
    
    
    
  ),
  br(),
  tabsetPanel(type = 'pills',id='md',
              tabPanel('Total Style List',
                       # inputPanel(
                       #   pickerInput("customer", label = "Customer",
                       #               choices = '',
                       #               options = pickerOptions(actionsBox = T, liveSearch = T, maxOptions = 100),
                       #               multiple = T),
                       #   
                       #   pickerInput("season", label = "Season",
                       #               choices = '',
                       #               selected = '',
                       #               options = pickerOptions(actionsBox = T, liveSearch = T, maxOptions = 100),
                       #               multiple = T),
                       #   
                       #   pickerInput("plant", label = "Plant",
                       #               choices = '',
                       #               selected = '',
                       #               options = pickerOptions(actionsBox = T, liveSearch = T),
                       #               multiple = T),
                       #   
                       #   pickerInput("extract.style", label = "Style Extract",
                       #               choices = '',
                       #               selected = '',
                       #               options = pickerOptions(actionsBox = T, liveSearch = T),
                       #               multiple = T),
                       #   
                       #   pickerInput("full.style", label = "Style Full",
                       #               choices = '',
                       #               selected = '',
                       #               options = pickerOptions(actionsBox = T, liveSearch = T),
                       #               multiple = T),
                       #   
                       #   pickerInput("year", label = "Year",
                       #               choices = '',
                       #               selected = '',
                       #               options = pickerOptions(actionsBox = T, liveSearch = T),
                       #               multiple = T),
                       #   
                       #   pickerInput("month", label = "Month",
                       #               choices = '',
                       #               selected = '',
                       #               options = pickerOptions(actionsBox = T, liveSearch = T),
                       #               multiple = T),
                       #   
                       # 
                       #   
                       #   
                       # ),
                       br(),
                       fluidRow(
                         downloadButton('download.total.style.list', label = 'Download', style = "float: left;"),
                         
                       ),
                       br(),
                       withSpinner(reactableOutput('total.style.list.tbl'), proxy.height = '150px', type = 5, size = 0.5),
                       
              ),
              
              tabPanel('Style Count & Repeat Style',
                       # 
                       # fluidRow(tags$style(".small-box{border-radius: 15px;}"),
                       #          valueBoxOutput('actual.sales.value',width=3)
                       #   
                       # ),
                       br(),
                       fluidRow(
                         downloadButton('download.repeat.style.list', label = 'Download', style = "float: left;"),
                       ),
                       br(),
                       withSpinner(reactableOutput('repeat.style.list.tbl'), proxy.height = '150px', type = 5, size = 0.5),
                      
                       ),
              tabPanel('Material Details',
                       
                       pickerInput("md.week", label = "Week",
                                   choices = '',
                                   options = pickerOptions(actionsBox = T, liveSearch = T),
                                   multiple = F),
                       
                       br(),
                       fluidRow(
                         downloadButton('download.material.details', label = 'Download', style = "float: left;"),
                       ),

                       br(),
                       # withSpinner(reactableOutput('material.details.tbl'), proxy.height = '150px', type = 5, size = 0.5),

                       ),
              tabPanel('Material Details Summary',
                       
                       pickerInput("mds.week", label = "Week",
                                   choices = '',
                                   options = pickerOptions(actionsBox = T, liveSearch = T),
                                   multiple = F),
                       br(),
                       fluidRow(
                         downloadButton('download.material.details.summary', label = 'Download', style = "float: left;"),
                       ),

                       br(),
                       # withSpinner(reactableOutput('material.details.summary.tbl'), proxy.height = '150px', type = 5, size = 0.5),

              ),


  ),
  
  hr()
  
)