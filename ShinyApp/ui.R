fluidPage(
  navbarPage(
    "New York City Choropleths of Violations per Restaurant",
    tabPanel("NYC and Boroughs",
             sidebarPanel(
               selectInput("year","Year:", choices=year),
               selectInput("borough", "Borough:", choices=borough),
               selectInput("violation", "Violation type:", choices=viotype)
             ),
             mainPanel(
               fluidRow(
                 column(5, h3("New York City Borough Boundaries"),plotOutput("choroBoro")),
                 column(5, h3("Borough Zoom"),plotOutput("choroZip")),
                 column(2, h3("Violations per Restaurant"),
                        plotOutput("legend"))
               )
             )),
    tabPanel("NYC 2014-2017",
             sidebarPanel(
               selectInput("viob","Violation Type:", choices=viotype),
               h4("Violations per Restaurant"),
               plotOutput("legendb")
             ),
             mainPanel(
               fluidRow(
                 column(5, h4("2014"),plotOutput("Boro14")),
                 column(5, h4("2015"),plotOutput("Boro15"))
               ),
               fluidRow(
                 column(5, h4("2016"),plotOutput("Boro16")),
                 column(5, h4("2017"),plotOutput("Boro17"))
               )
             )),
    tabPanel("Boroughs 2014-2017",
             sidebarPanel(
               selectInput("vioz","Violation Type:", choices=viotype),
               selectInput("boroz","Borough:", choices=borough),
               plotOutput("legendz")
             ),
             mainPanel(
               fluidRow(
                 column(5, h4("2014"),plotOutput("Zip14")),
                 column(5, h4("2015"),plotOutput("Zip15"))
               ),
               fluidRow(
                 column(5, h4("2016"),plotOutput("Zip16")),
                 column(5, h4("2017"),plotOutput("Zip17"))
               )
             ))
  )
)