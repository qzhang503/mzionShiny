#' User interface of MGF parameters
#'
#' @param id Namespace identifier.
#' @param type_ms2ions Type of MS2 ions.
map_ms2UI <- function(id, type_ms2ions = c("by", "ax", "cz"))
{
  ns <- NS(id)

  sidebarLayout(
    sidebarPanel(
      shinyFiles::shinyFilesButton(NS(id, "btn_file"), "Select a PSM file", "Select Files",
                                   multiple = FALSE,style = "background-color: #f5f5f5"),
      textInput(NS(id, "in_name"), NULL, value = "", placeholder = "~/Mzion/Project/psmQ.txt"),
      uiOutput(NS(id, "raws")),
      numericInput(NS(id, "scan"), "Scan number", 1),
      numericInput(NS(id, "rank"), "PSM rank", 1, min = 1),
      selectInput(NS(id, "type_ms2ions"), "Type of MS2", type_ms2ions, selected = "by"),
      checkboxInput(NS(id, "is_decoy"), "Is decoy", value = FALSE),
      textInput(NS(id, "out_name"), "Output file name", placeholder = file.path("bar.png")),
      fluidRow(
        column(3, numericInput(NS(id, "width "), "Image width ", 12), style = "width: 160px;"),
        column(3, numericInput(NS(id, "height"), "Image height", 8), style = "width: 160px;"),
      ),
      actionButton(NS(id, "view"), "View", style = "background-color:#41ab5d; width:200px;
                 font-size:100%; color:white"),
      width = 4
    ),
    mainPanel(
      DT::DTOutput(NS(id, "table")),
      plotOutput(NS(id, "plot")),
      width = 8
    )
  )
}


#' Server-side processing of MGF parameters
#'
#' @param id Namespace identifier.
#' @param type_ms2ions Type of MS2 ions.
#' @param cols PSM columns for displaying.
map_ms2Server <- function(id, type_ms2ions = c("by", "ax", "cz"),
                          cols = c("prot_acc", "raw_file", "pep_seq", "pep_scan_num",
                                   "pep_isdecoy", "pep_rank", "pep_score"))
{
  moduleServer(
    id,
    function(input, output, session) {
      volume0 <- c(Home = fs::path_home_r(), Home_win = fs::path_home(), shinyFiles::getVolumes()())
      volumes <- reactive(c(Project = input$out_path, volume0))

      ## PSM
      observeEvent(input$btn_file, ignoreInit = TRUE, {
        shinyFiles::shinyFileChoose(input, "btn_file", roots = volumes(), session = session,
                                    filetypes = c(text = "txt"))
        fileinfo <- shinyFiles::parseFilePaths(volumes(), input$btn_file)
        fileinfo$datapath <- gsub("\\\\", "/", fileinfo$datapath)
        updateTextInput(session = session, inputId = "in_name", label = NULL,
                        value = unname(fileinfo$datapath ))
      })

      psms <- eventReactive(input$in_name, ignoreInit = TRUE, {
        if (file.exists(input$in_name))
          suppressWarnings(readr::read_tsv(input$in_name, col_types = mzion::get_mzion_coltypes()))
        else
          NULL
      })

      observeEvent(psms(), {
        if (nrow(psms())) {
          ok_cols <- cols[cols %in% names(psms())]
          output$table <- DT::renderDT(psms()[, ok_cols], filter = "bottom",
                                       selection = "single",
                                       options = list(pageLength = 5))
        }

        if (length(raws <- sort(unique(psms()$raw_file)))) {
          output$raws <- renderUI({
            selectInput(NS(id, "raws"), "MS files", raws)
          })
        }
      })

      btn_view <- eventReactive(input$view, {
        shinyjs::toggleState("view")
        ans <- mzion::mapMS2ions(out_path = gsub("(.*)/[^/]+", "\\1", input$in_name),
                                 in_name = gsub(".*/([^/]+)", "\\1", input$in_name),
                                 out_name = input$out_name, raw_file = input$raws,
                                 scan = input$scan, rank = input$rank,
                                 is_decoy = input$is_decoy,
                                 type_ms2ions = input$type_ms2ions,
                                 width = input$width, height = input$height)
        shinyjs::toggleState("view")

        ans
      })

      observeEvent(input$table_rows_selected, {
        # single row guaranteed by selection = "single"
        if (!is.null(i <- input$table_rows_selected)) {
          row <- psms()[i, , drop = FALSE]
          updateSelectInput(session, "raws", label = NULL, choices = NULL, selected = row$raw_file)
          updateNumericInput(session, "scan", NULL, row$pep_scan_num)
          updateNumericInput(session, "rank", NULL, row$pep_rank)
          updateCheckboxInput(session, "is_decoy", NULL, row$pep_isdecoy)
        }
      })

      output$plot <- renderPlot(btn_view()$p)
    }
  )
}


