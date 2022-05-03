#' @title Generate SOPHiA DDM validation report
#' @description Generate SOPHiA DDM validation report via Shiny app
#'
#' @import shiny
#' @import shinyFiles
#' @importFrom digest digest
#' @importFrom rmarkdown render
#' @importFrom fs path_home
#' @export

ddmValidation <- function() {

  filePanel <- function(ftype, label) {
    ns <- NS(ftype)
    wellPanel(
      tags$h4(label),
      shinyFilesButton(ns('add'), 'Select file(s)', 'Select file(s)', TRUE),
      actionButton(ns('clear'), label = "Clear selection"),
      tags$br(),
      textOutput(outputId = ns("print"))
    )
  }

  fileServer <- function(ftype, roots, paths) {
    moduleServer(
      id = ftype,
      function(input, output, session) {
        shinyFileChoose(input, "add", roots = roots, session = session)
        observeEvent(input$add, {
          selFls <- parseFilePaths(roots, input$add)$datapath
          newFls <- setdiff(selFls, isolate(paths[[ftype]]))
          paths[[ftype]] <- c(isolate(paths[[ftype]]), newFls)
        })
        observeEvent(input$clear, {
          paths[[ftype]] <- vector("character")
        })
        output$print <- renderText(paths[[ftype]], sep = "\n")
        paths
      }
    )
  }

  ui <- fluidPage(
    titlePanel("Compare SOPHiA Versions"),
    textInput("author", label = "Report author", placeholder = "Your name"),
    filePanel("v0", "Old version variant tables"),
    filePanel("v1", "New version variant tables"),
    filePanel("e0", "Old version exon coverage"),
    filePanel("e1", "New version exon coverage"),
    tags$br(),
    downloadButton("report", "Generate report"),
  )

  server <- function(input, output, session) {
    volumes <- c(Home = fs::path_home(), getVolumes()())
    paths <- reactiveValues(v0 = vector("character"),
                            v1 = vector("character"),
                            e0 = vector("character"),
                            e1 = vector("character"))

    paths <- fileServer(ftype = "v0", paths = paths, roots = volumes)
    paths <- fileServer(ftype = "v1", paths = paths, roots = volumes)
    paths <- fileServer(ftype = "e0", paths = paths, roots = volumes)
    paths <- fileServer(ftype = "e1", paths = paths, roots = volumes)

    output$report <- downloadHandler(
      filename = function() {
        paste0("sophia_validation_report_", Sys.Date(), ".html")
      },
      content = function(file) {
        rmarkdown::render(
          input = getTemplate("ddmValidation"),
          output_file = file,
          params = list(var0path = paths$v0,
                        var1path = paths$v1,
                        exn0path = paths$e0,
                        exn1path = paths$e1,
                        user = input$author),
          envir = new.env()
        )
      }
    )

  }

  shinyApp(ui, server)

}

