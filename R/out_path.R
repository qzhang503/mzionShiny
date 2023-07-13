outpath_UI <- function(id)
{
  ns <- NS(id)
  tagList(actionButton(ns("button_open"), "Output path"))
}


outpath_Server <- function(id)
{
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)
    current_path <- reactiveVal()
    current_path(root_path <- "~")
    return_path <- reactiveVal()
    return_path(root_path)

    # Selection modal
    observeEvent(input$button_open, {
      showModal(
        modalDialog(
          title = "Select an output path",
          p(strong("Current path: "), textOutput(ns("current_path"), inline = TRUE)),
          fluidRow(
            column(2, actionButton(ns("button_back"), "Back")),
            column(4, selectInput(ns("dir"), label = NULL, choices = "Please select"))
          ),

          footer <- tagList(modalButton("Cancel"), actionButton(ns("ok"), "OK"))
        )
      )
      new_choices <- c("Please select", dir(current_path()))
      updateSelectInput(inputId = "dir", choices = new_choices)
    })

    # back button
    observeEvent(input$button_back, {
      if(current_path() != root_path){
        current_path(dirname(current_path()))
        new_choices = c("Please select", dir(current_path()))
        updateSelectInput(inputId = "dir", choices = new_choices)
      }
    })

    # OK button
    observeEvent(input$ok, {
      return_path(current_path())
      removeModal()
    })

    # update directory
    observeEvent(input$dir, {
      if(input$dir != "Please select"){
        current_path(file.path(current_path(), input$dir))
      }
      new_choices <- c("Please select", dir(current_path()))
      updateSelectInput(inputId = "dir", choices = new_choices)
    })

    # display directory
    output$current_path = renderText({ current_path() })

    return(reactive(return_path()))
  })
}

