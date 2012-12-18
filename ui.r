shinyUI(pageWithSidebar(
  headerPanel('Downloading Data with Shiny'),
  sidebarPanel(
    helpText("An Example of R creating a pdf and then delivering with Shiny to Download.  This 
            is a very crude example in serious need of refinement and should be considered
            highly experimental.  Shiny has prebuilt functions with plotOutput
            for delivering R plots through .png, but sometimes we might like to deliver more than .png.
            Ideally, we could use knitr's knit2pdf function to access the power of latex
            to build a multi-page pdf real-time and then send it to the browser."),
    downloadLink('downloadPdf', 'Download Performance File')
  ),
  mainPanel(
  )
))