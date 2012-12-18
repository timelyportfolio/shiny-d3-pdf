shinyUI(pageWithSidebar(
  headerPanel('Downloading Data with Shiny'),
  sidebarPanel(
    helpText("An Example of R creating a pdf and then delivering with Shiny to Download.  This 
            is a very crude example in serious need of refinement.  Shiny has prebuilt functions
            for delivering R plots through .png.  Ideally, we could use knitr knit2pdf to
            build a pdf and then send it."),
    downloadLink('downloadPdf', 'Download Performance File')
  ),
  mainPanel(
  )
))