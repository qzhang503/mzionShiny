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
      column(4, shinyFiles::shinyDirButton(NS(id, "select_mgfpath"), "Peaklist path", "Please select a folder",
                                           style = "background-color: #f5f5f5") |>
               bslib::tooltip("mzML (no zlib compression) or MGF")),
      column(12, textInput(NS(id, "mgf_path"), label = NULL, value = "~", placeholder = file.path("~/Mzion/My_Project/mgf"))),
      column(4, shinyFiles::shinyDirButton(NS(id, "select_cachepath"), "Cache folder", "Please select a folder",
                                           style = "background-color: #f5f5f5")),
      column(12, textInput(NS(id, ".path_cache"), label = NULL, value = formals(mzion::matchMS)$.path_cache)),
    ),
    # hr(),
    fluidRow(
      column(4, numericInput(NS(id, "topn_ms2ions"), "Top-N features", 150)),
      column(4, numericInput(NS(id, "min_ms1_charge"), "Min MS1 charge state", 2, 1)),
      column(4, numericInput(NS(id, "max_ms1_charge"), "Max MS1 charge state", 4, 2)),
      column(4, numericInput(NS(id, "min_ms2mass"), "Min MS2 mass", 115)),
      column(4, numericInput(NS(id, "max_ms2mass"), "Max MS2 mass", 4500)),
      column(4, numericInput(NS(id, "min_scan_num"), "Min scan number", 1)),
      column(4, numericInput(NS(id, "max_scan_num"), "Max scan number", Inf)),
      column(4, numericInput(NS(id, "min_ret_time"), "Min retention time", 0)),
      column(4, numericInput(NS(id, "max_ret_time"), "Max retention time", Inf)),
    ),
    checkboxInput(NS(id, "is_mdda"), "Chimeric precursors", value = TRUE) |>
      bslib::tooltip("Require mzML format"),
    conditionalPanel(
      condition = "input.is_mdda == true",
      ns = ns,
      fluidRow(
        column(4, numericInput(NS(id, "maxn_mdda_precurs"), "Number of precursors", 1, min = 0) |>
                 bslib::tooltip("0 - apply MSConvert defaults; 1 - apply Mzion de-isotoping; > 1 consider chimeric precursors")),
        column(4, numericInput(NS(id, "n_mdda_flanks"), "Number of flanking MS1 spectra", 6, min = 1) |>
                 bslib::tooltip("E.g. 6 preceding and 6 following MS1 spectra")),
        column(4, numericInput(NS(id, "ppm_ms1_deisotope"), "MS1 tolerance (ppm)", 8, min = 1)),
        column(4, numericInput(NS(id, "grad_isotope"), "Isotope-envelope gradient", 1.6, min = 1)),
        column(4, numericInput(NS(id, "fct_iso2"), "Fuzzy isotope-envelope gradient", 3, min = 1)),
        column(4, checkboxInput(NS(id, "use_defpeaks"), "Include defaults", value = FALSE) |>
                 bslib::tooltip("Use MSConvert precursors if not determined by Mzion")),
      ),
    ),
    checkboxInput(NS(id, "deisotope_ms2"), "De-isotope MS2 spectra", value = TRUE),
    conditionalPanel(
      condition = "input.deisotope_ms2 == true",
      ns = ns,
      fluidRow(
        column(4, numericInput(NS(id, "ppm_ms2_deisotope"), "MS2 tolerance (ppm)", 8, min = 1)),
        column(4, numericInput(NS(id, "max_ms2_charge"), "Max MS2 charge state", 3, min = 2, max = 4)),
      ),
    ),
    hr(),
    checkboxInput(NS(id, "calib_ms1mass"), "Calibrate masses"),
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
    checkboxInput(NS(id, "exclude_reporter_region"), "Exclude reporter region", value = FALSE),
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

      # Peaklist path
      shinyFiles::shinyDirChoose(input, "select_mgfpath", roots = volumes, session = session,
                                 restrictions = system.file(package = "base"))
      mgf_path <- reactive({
        if (is.integer(input$select_mgfpath))
          NULL
        else
          gsub("\\\\", "/", shinyFiles::parseDirPath(volumes, input$select_mgfpath))
      })

      observeEvent(input$select_mgfpath, {
        bslib::update_tooltip(session$ns("select_mgfpath"))
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
              column(4, numericInput(NS(id, "ppm_reporters"),
                                     label = "Reporter tolerance (ppm)",
                                     # tags$span(style="color: #969696; border: #969696","Reporter tolerance (ppm)"),
                                     value = 10, min = 1)),
              column(4, numericInput(NS(id, "tmt_reporter_lower"),
                                     label = "Reporter lower bound (m/z)",
                                     # tags$span(style="color: #969696; border: #969696","Reporter lower bound (m/z)"),
                                     value = 126.1, min = 0)),
              column(4, numericInput(NS(id, "tmt_reporter_upper"),
                                     label = "Reporter upper bound (m/z)",
                                     # tags$span(style="color: #969696; border: #969696","Reporter upper bound (m/z)"),
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
        updateNumericInput(session, "max_ms1_charge", "Max MS1 charge state", 4)
        updateNumericInput(session, "min_ms2mass", "Min MS2 mass", 115)
        updateNumericInput(session, "max_ms2mass", "Max MS2 mass", 4500)
        updateNumericInput(session, "min_scan_num", "Min scan number", 1)
        updateNumericInput(session, "max_scan_num", "Max scan number", Inf)
        updateNumericInput(session, "min_ret_time", "Min retention time", 0)
        updateNumericInput(session, "max_ret_time", "Max retention time", Inf)
        updateNumericInput(session, "topn_ms2ions", "Top-N features", 150)

        updateNumericInput(session, "n_mdda_flanks", "Number of flanking MS1 spectra", 6)
        updateNumericInput(session, "ppm_ms1_deisotope", "MS1 tolerance (ppm)", 8)
        updateNumericInput(session, "ppm_ms2_deisotope", "MS2 tolerance (ppm)", 8)
        updateNumericInput(session, "max_ms2_charge", "Max MS2 charge state", 3)
        updateCheckboxInput(session, "is_mdda", "Chimeric precursors", value = TRUE)
        updateNumericInput(session, "grad_isotope", "Isotope-envelope gradient", 1.6)
        updateNumericInput(session, "fct_iso2", "Fuzzy isotope-envelope gradient", 3)
        updateNumericInput(session, "maxn_mdda_precurs", "Number of precursors", 1)
        updateCheckboxInput(session, "use_defpeaks", "Include defaults", value = FALSE)
        updateCheckboxInput(session, "deisotope_ms2", "De-isotope MS2 spectra", value = TRUE)
        updateCheckboxInput(session, "exclude_reporter_region", "Exclude reporter region", value = FALSE)
        updateCheckboxInput(session, "calib_ms1mass", "Calibrate masses", value = FALSE)
        updateNumericInput(session, "ppm_ms1calib", "Calibration mass tolerance (ppm)", 10)
        updateCheckboxInput(session, "cut_ms2ions", "Cut MS2 by regions", value = FALSE)
        updateTextInput(session, "topn_ms2ion_cuts", "Cuts (m/z = percent)", value = NA,
                        placeholder = "`1000` = 90, `1100` = 5, `4500` = 5")
        updateSelectInput(session, "quant", "Quantitation", quant, selected = "none")
        updateNumericInput(session, "ppm_reporters", "Reporter tolerance (ppm)", value = 10, min = 1)
        updateNumericInput(session, "tmt_reporter_lower", "Reporter lower bound (m/z)", value = 126.1, min = 0)
        updateNumericInput(session, "tmt_reporter_upper", "Reporter upper bound (m/z)", value = 135.2, min = 0)
      })

      # deisotope_ms2 <- eventReactive(input$ppm_ms2_deisotope, if (input$ppm_ms2_deisotope > 0) TRUE else FALSE)

      observeEvent(input$maxn_mdda_precurs, {
        bslib::update_tooltip(
          session$ns("maxn_mdda_precurs"),
        )
      })

      # observeEvent(input$maxn_mdda_precurs, {
      #   if (input$maxn_mdda_precurs <= 0) {
      #     updateCheckboxInput(session, "is_mdda", "Chimeric precursors (mzML)", value = FALSE)
      #   }
      # })
      observeEvent(input$is_mdda, {
        bslib::update_tooltip(session$ns("is_mdda"))

        if (!input$is_mdda) {
          updateNumericInput(session, "maxn_mdda_precurs", "Number of precursors", 0)
        }
      })

      observeEvent(input$use_defpeaks, {
        bslib::update_tooltip(session$ns("use_defpeaks"))
      })

      observeEvent(input$n_mdda_flanks, {
        bslib::update_tooltip(session$ns("n_mdda_flanks"))
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
           # deisotope_ms2 = deisotope_ms2,
           deisotope_ms2 = reactive(input$deisotope_ms2),
           max_ms2_charge = reactive(input$max_ms2_charge),
           # is_mdda = reactive(input$is_mdda),
           use_defpeaks = reactive(input$use_defpeaks),
           ppm_ms1_deisotope = reactive(input$ppm_ms1_deisotope),
           ppm_ms2_deisotope = reactive(input$ppm_ms2_deisotope),
           n_mdda_flanks = reactive(input$n_mdda_flanks),
           grad_isotope = reactive(input$grad_isotope),
           fct_iso2 = reactive(input$fct_iso2),
           maxn_mdda_precurs = reactive(input$maxn_mdda_precurs),
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


