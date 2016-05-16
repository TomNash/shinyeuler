shinyUI(fluidPage(
  titlePanel("Euler Diagram Maker"),
  sidebarLayout(
    sidebarPanel(
      helpText("Create area-proportional Euler diagrams then click on each region of the diagram to see what elements comprise it."),
      textInput("title", "Plot Title", "Title", width="75%"),
      textInput("list.A.name", "List A Name:", "A", width="75%"),
      strong("List A Contents:"),
      br(),
      tags$textarea(id="listA", rows=3, cols=40,""),
      textInput("list.B.name", "List B Name:", "B", width="75%"),
      strong("List B Contents:"),
      br(),
      tags$textarea(id="listB", rows=3, cols=40,""),
      br(),br(),
      downloadButton("downloadPlot", label = "Download Plot"), 
      br(),
      # textInput("list.C.name", "(optional) List C Name:", "C", width="75%"),
      # strong("List C Contents:"),
      # br(),
      # tags$textarea(id="list3", rows=3, cols=40,""),
      h5("Made possible with the R package", a("venneuler",
                                                  href="https://cran.r-project.org/web/packages/venneuler/",
                                                  target="_blank"))
    ),
    mainPanel(
      column(8, align="center",
             plotOutput("plot", click = "plot_click")),
      uiOutput("info")
    )
  )
))
