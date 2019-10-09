fluidPage(
  titlePanel("New York City Choropleths of Violations per Restaurant"),
  
  fluidRow(
    column(4, wellPanel(
      selectInput("year","Year:", choices=year)
    )),
    column(4, wellPanel(
      selectInput("borough", "Borough:", choices=borough)
    )),
    column(4, wellPanel(
      selectInput("violation", "Violation type:", choices=viotype)
    ))
  ),
  fluidRow(
    column(5, wellPanel(
      h4("New York City Borough Boundaries"),
      plotOutput("choroBoro")
    )),
    column(5, wellPanel(
      h4("Borough"),
      plotOutput("choroZip")
    )),
    column(2, wellPanel(
      "Violations per Restaurant",
      plotOutput("legend")
    ))
  )
)
