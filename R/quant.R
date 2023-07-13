quantUI <- function(id, quants = c("none", "tmt6", "tmt10", "tmt11", "tmt16", "tmt18"))
{
  ns <- NS(id)

  tagList(
    fluidRow(
      column(4, selectInput(NS(id, "quant"), "Quantitation", quants, selected = "none")),
    ),
    uiOutput(NS(id, "tmt")),
    actionButton(NS(id, "reset"), "Reset", class = "btn-danger"),
  )
}


quantServer <- function(id, quants = c("none", "tmt6", "tmt10", "tmt11", "tmt16", "tmt18"))
{
  moduleServer(
    id,
    function(input, output, session) {
      observeEvent(input$reset, {
        updateSelectInput(session, "quant", "Quantitation", quants, selected = "none")
        updateNumericInput(session, "ppm_reporters", "Reporter tolerance (ppm)", value = 10, min = 1)
        updateNumericInput(session, "tmt_reporter_lower", "Reporter lower bound", value = 126.1, min = 0)
        updateNumericInput(session, "tmt_reporter_upper", "Reporter upper bound", value = 135.2, min = 0)
      })

      quant <- reactive(input$quant)
      observe({
        if (grepl("^tmt", quant())) {
          output$tmt <- renderUI({
            fluidRow(
              column(4, numericInput(NS(id, "ppm_reporters"), label = "Reporter tolerance (ppm)",
                                     value = 10, min = 1)),
              column(4, numericInput(NS(id, "tmt_reporter_lower"), label = "Reporter lower bound",
                                     value = 126.1, min = 0)),
              column(4, numericInput(NS(id, "tmt_reporter_upper"), label = "Reporter upper bound",
                                     value = 135.2, min = 0)),
            )
          })
        }
      })

      list(quant = reactive(input$quant),
           ppm_reporters = reactive(input$ppm_reporters),
           tmt_reporter_lower = reactive(input$tmt_reporter_lower),
           tmt_reporter_upper = reactive(input$tmt_reporter_upper)
      )
    }
  )
}


