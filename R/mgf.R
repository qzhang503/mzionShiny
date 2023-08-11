#' User interface of MGF parameters
#'
#' @param id Namespace identifier.
#' @param quant Quantitation method.
mgfUI <- function(id, quant = c("none", "tmt6", "tmt10", "tmt11", "tmt16", "tmt18"))
{
  ns <- NS(id)

  tagList(
    fluidRow(
      column(4, selectInput(NS(id, "quant"), "Quantitation", quant, selected = "none")),
    ),
    uiOutput(NS(id, "tmt")),
    fluidRow(
      column(4, shinyFiles::shinyDirButton(NS(id, "select_outpath"), "Output path", "Please select a folder",
                                           style = "background-color: #f5f5f5")),
      column(12, textInput(NS(id, "out_path"), label = NULL, value = "~", placeholder = file.path("~/Mzion/My_Project"))),
      column(4, shinyFiles::shinyDirButton(NS(id, "select_mgfpath"), "MGF path", "Please select a folder",
                                           style = "background-color: #f5f5f5")),
      column(12, textInput(NS(id, "mgf_path"), label = NULL, value = "~", placeholder = file.path("~/Mzion/My_Project/mgf"))),
      column(4, shinyFiles::shinyDirButton(NS(id, "select_cachepath"), "Cache folder", "Please select a folder",
                                           style = "background-color: #f5f5f5")),
      column(12, textInput(NS(id, ".path_cache"), label = NULL, value = formals(mzion::matchMS)$.path_cache)),
    ),
    fluidRow(
      column(4, numericInput(NS(id, "min_ms1_charge"), "Min MS1 charge state", 2)),
      column(4, numericInput(NS(id, "max_ms1_charge"), "Max MS1 charge state", 6)),
      column(4, numericInput(NS(id, "min_ms2mass"), "Min MS2 mass", 115)),
      column(4, numericInput(NS(id, "max_ms2mass"), "Max MS2 mass", 4500)),
      column(4, numericInput(NS(id, "min_scan_num"), "Min scan number", 1)),
      column(4, numericInput(NS(id, "max_scan_num"), "Max scan number", Inf)),
      column(4, numericInput(NS(id, "min_ret_time"), "Min retention time", 0)),
      column(4, numericInput(NS(id, "max_ret_time"), "Max retention time", Inf)),
      column(4, numericInput(NS(id, "topn_ms2ions"), "Top-N features", 100)),
      column(4, checkboxInput(NS(id, "exclude_reporter_region"), "Exclude reporter region", value = FALSE)),
    ),
    checkboxInput(NS(id, "calib_ms1mass"), "Mass calibration"),
    conditionalPanel(
      condition = "input.calib_ms1mass == true",
      ns = ns,
      numericInput(NS(id, "ppm_ms1calib"), "Calibration mass tolerance (ppm)", 10),
    ),
    checkboxInput(NS(id, "cut_ms2ions"), "Cut MS2 by regions"),
    conditionalPanel(
      condition = "input.cut_ms2ions == true",
      ns = ns,
      textInput(NS(id, "topn_ms2ion_cuts"), "Cuts (m/z = percent)", value = NA,
                placeholder = "`1000` = 90, `1100` = 5, `4500` = 5"),
    ),
    actionButton(NS(id, "reset"), "Reset",
                 style = "width:70px; background-color:#c51b8a; border-color:#f0f0f0; color:white",
                 title = "Reset values in the current tab"),
  )
}


#' Server-side processing of MGF parameters
#'
#' @param id Namespace identifier.
#' @param quant Quantitation method.
mgfServer <- function(id, quant = c("none", "tmt6", "tmt10", "tmt11", "tmt16", "tmt18"))
{
  moduleServer(
    id,
    function(input, output, session) {
      volumes <- c(Home = fs::path_home_r(), Home_win = fs::path_home(), shinyFiles::getVolumes()())

      # Output path
      shinyFiles::shinyDirChoose(input, "select_outpath", roots = volumes, session = session,
                                 restrictions = system.file(package = "base"))
      out_path <- reactive({
        if (is.integer(input$select_outpath))
          NULL
        else {
          fileinfo <- shinyFiles::parseDirPath(volumes, input$select_outpath)
          fileinfo <- gsub("\\\\", "/", fileinfo)
        }
      })

      observeEvent(input$select_outpath, {
        updateTextInput(session = session, inputId = "out_path", label = NULL, value = out_path())
      })

      # MGF path
      shinyFiles::shinyDirChoose(input, "select_mgfpath", roots = volumes, session = session,
                                 restrictions = system.file(package = "base"))
      mgf_path <- reactive({
        if (is.integer(input$select_mgfpath))
          NULL
        else
          gsub("\\\\", "/", shinyFiles::parseDirPath(volumes, input$select_mgfpath))
      })

      observeEvent(input$select_mgfpath, {
        updateTextInput(session = session, inputId = "mgf_path", label = NULL, value = mgf_path())
      })

      # Cache folder
      shinyFiles::shinyDirChoose(input, "select_cachepath", roots = volumes, session = session,
                                 restrictions = system.file(package = "base"), allowDirCreate = FALSE)
      .path_cache <- reactive({
        if (is.integer(input$select_cachepath))
          NULL
        else
          gsub("\\\\", "/", shinyFiles::parseDirPath(volumes, input$select_cachepath))
      })

      observeEvent(input$select_cachepath, {
        updateTextInput(session = session, inputId = ".path_cache", label = NULL, value = .path_cache())
      })

      ## Quant
      observeEvent(input$quant, {
        if (grepl("^tmt", input$quant)) {
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

      observeEvent(input$reset, {
        updateTextInput(session, "out_path", label = NULL, value = "~",
                        placeholder = file.path("~/Mzion/My_Project"))
        updateTextInput(session, "mgf_path", label = NULL, value = "~",
                        placeholder = "~/Mzion/My_project/mgf")
        updateTextInput(session, ".path_cache", NULL,
                        value = formals(mzion::matchMS)$.path_cache)
        updateNumericInput(session, "min_ms1_charge", "Min MS1 charge state", 2)
        updateNumericInput(session, "max_ms1_charge", "Max MS1 charge state", 6)
        updateNumericInput(session, "min_ms2mass", "Min MS2 mass", 115)
        updateNumericInput(session, "max_ms2mass", "Max MS2 mass", 4500)
        updateNumericInput(session, "min_scan_num", "Min scan number", 1)
        updateNumericInput(session, "max_scan_num", "Max scan number", Inf)
        updateNumericInput(session, "min_ret_time", "Min retention time", 0)
        updateNumericInput(session, "max_ret_time", "Max retention time", Inf)
        updateNumericInput(session, "topn_ms2ions", "Top-N features", 100)
        updateCheckboxInput(session, "exclude_reporter_region", "Exclude reporter region", value = FALSE)
        updateCheckboxInput(session, "calib_ms1mass", "Mass calibration", value = FALSE)
        updateNumericInput(session, "ppm_ms1calib", "Calibration mass tolerance (ppm)", 10)
        updateCheckboxInput(session, "cut_ms2ions", "Cut MS2 by regions", value = FALSE)
        updateTextInput(session, "topn_ms2ion_cuts", "Cuts (m/z = percent)", value = NA,
                        placeholder = "`1000` = 90, `1100` = 5, `4500` = 5")
        updateSelectInput(session, "quant", "Quantitation", quant, selected = "none")
        updateNumericInput(session, "ppm_reporters", "Reporter tolerance (ppm)", value = 10, min = 1)
        updateNumericInput(session, "tmt_reporter_lower", "Reporter lower bound", value = 126.1, min = 0)
        updateNumericInput(session, "tmt_reporter_upper", "Reporter upper bound", value = 135.2, min = 0)
      })

      list(mgf_path = reactive(input$mgf_path),
           min_ms1_charge = reactive(input$min_ms1_charge),
           max_ms1_charge = reactive(input$max_ms1_charge),
           min_ms2mass = reactive(input$min_ms2mass),
           max_ms2mass = reactive(input$max_ms2mass),
           min_scan_num = reactive(input$min_scan_num),
           max_scan_num = reactive(input$max_scan_num),
           min_ret_time = reactive(input$min_ret_time),
           max_ret_time = reactive(input$max_ret_time),
           topn_ms2ions = reactive(input$topn_ms2ions),
           exclude_reporter_region = reactive(input$exclude_reporter_region),
           calib_ms1mass = reactive(input$calib_ms1mass),
           ppm_ms1calib = reactive(input$ppm_ms1calib),
           cut_ms2ions = reactive(input$cut_ms2ions),
           topn_ms2ion_cuts = reactive(input$topn_ms2ion_cuts),
           .path_cache = reactive(input$.path_cache),
           out_path = reactive(input$out_path),
           quant = reactive(input$quant),
           ppm_reporters = reactive(input$ppm_reporters),
           tmt_reporter_lower = reactive(input$tmt_reporter_lower),
           tmt_reporter_upper = reactive(input$tmt_reporter_upper)
      )
    }
  )
}


