#' User interface of search parameters
#'
#' @param id Namespace identifier.
searchUI <- function(id)
{
  tagList(
    fluidRow(
      column(4, numericInput(NS(id, "min_len"), label = "Min peptide length", value = 7, min = 1)),
      column(4, numericInput(NS(id, "max_len"), label = "Max peptide length", value = 40, min = 1)),
      column(4, numericInput(NS(id, "max_miss"), label = "Max mis-cleavages", value = 2, min = 0)),
      column(4, numericInput(NS(id, "min_mass"), "Min precursor mass", value = 200, min = 1)),
      column(4, numericInput(NS(id, "max_mass"), "Max precursor mass", value = 4500, min = 500)),
      column(4, numericInput(NS(id, "ppm_ms1"), "MS1 tolerance (ppm)", value = 20, min = 1)),
      column(4, numericInput(NS(id, "maxn_vmods_setscombi"), "Max modification sets",value = 512, min = 1) |>
               bslib::tooltip("Maximum sets of combinatorial modifications")),
      column(4, numericInput(NS(id, "maxn_vmods_per_pep"), "Max variable modifications",
                             value = 5, min = 1)),
      column(4, numericInput(NS(id, "maxn_sites_per_vmod"), "Max variable modifications per site",
                             value = 3, min = 1)),
      column(4, numericInput(NS(id, "maxn_vmods_sitescombi_per_pep"), "Max position permutations",
                             value = 64, min = 1)),
      column(4, numericInput(NS(id, "maxn_fnl_per_seq"), "Max neutral losses (fixed)",
                             value = 3, min = 1)),
      column(4, numericInput(NS(id, "maxn_vnl_per_seq"), "Max neutral losses (variable)",
                             value = 3, min = 1)),
      column(4, numericInput(NS(id, "min_ms2mass"), "Min MS2 mass", value = 115)),
      column(4, numericInput(NS(id, "max_ms2mass"), "Max MS2 mass", value = 4500)),
      column(4, numericInput(NS(id, "ppm_ms2"), "MS2 tolerance (ppm)", value = 20)),
      column(4, numericInput(NS(id, "minn_ms2"), "Min MS2 features", value = 6)),
    ),
    selectInput(NS(id, "type_ms2ions"), "MS2 fragments", c("by", "ax", "cz"), selected = "by"),
    selectInput(NS(id, "enzyme"), "Enzyme", enzymes, selected = "Trypsin_P"),
    # uiOutput(NS(id, "nes")),
    numericInput(NS(id, "noenzyme_maxn"), "Max span of peptide lengths", value = 0) |>
      bslib::tooltip(paste0("To circumvent RAM limits for search at noenzyme specificity. ",
                            "E.g. at the minimum peptide length of 7, ",
                            "a setting of 15 corresponds to a sectional searchs at lengths 7-21, 22-36 etc. ",
                            "The span will be automatically determined at the setting of 0.")),
    checkboxInput(NS(id, "customenzyme"), "Custom enzyme") |>
      bslib::tooltip("Enter a regular expression"),
    conditionalPanel(
      condition = "input.customenzyme == true",
      ns = NS(id),
      fluidRow(
        column(4, textInput(NS(id, "custom_enzymeC"), "C-term specificity") |>
                 bslib::tooltip("Trypsin: ([KR]{1})([^P]{1})")),
        column(4, textInput(NS(id, "custom_enzymeN"), "N-term specificity") |>
                 bslib::tooltip("GluN: ([E]{1})")),
      )
    ),
    actionButton(NS(id, "reset"), "Reset",
                 style = "width:70px; background-color:#c51b8a; border-color:#f0f0f0; color:white",
                 title = "Reset values in the current tab"),
  )
}


#' Server-side processing of search parameters
#'
#' @param id Namespace identifier.
searchServer <- function(id)
{
  moduleServer(
    id,
    function(input, output, session) {
      observeEvent(input$reset, {
        updateNumericInput(session, "min_len", "Min peptide length", value = 7, min = 1)
        updateNumericInput(session, "max_len", "Max peptide length", value = 40, min = 1)
        updateNumericInput(session, "max_miss", "Max mis-cleavages", value = 2, min = 0)
        updateNumericInput(session, "min_mass", "Min precursor mass", value = 200, min = 1)
        updateNumericInput(session, "max_mass", "Max precursor mass", value = 4500, min = 500)
        updateNumericInput(session, "ppm_ms1", "MS1 tolerance (ppm)", 20, 1)
        updateNumericInput(session, "maxn_vmods_setscombi", "Max modification sets", value = 512, min = 1)
        updateNumericInput(session, "maxn_vmods_per_pep", "Max variable modifications", value = 5, min = 1)
        updateNumericInput(session, "maxn_sites_per_vmod", "Max variable modifications per site", value = 3, min = 1)
        updateNumericInput(session, "maxn_vmods_sitescombi_per_pep", "Max position permutations", 64, 1)
        updateNumericInput(session, "maxn_fnl_per_seq", "Max neutral losses (fixed)", 3, 1)
        updateNumericInput(session, "maxn_vnl_per_seq", "Max neutral losses (variable)", 3, 1)
        updateNumericInput(session, "min_ms2mass", "Min MS2 mass", 115)
        updateNumericInput(session, "max_ms2mass", "Max MS2 mass", 4500)
        updateNumericInput(session, "ppm_ms2", "MS2 tolerance (ppm)", 20)
        updateNumericInput(session, "minn_ms2", "Min MS2 features", 6)
        updateSelectInput(session, "type_ms2ions", "MS2 fragments", c("by", "ax", "cz"), selected = "by")
        updateSelectInput(session, "enzyme", "Enzyme", enzymes, selected = "Trypsin_P")
        updateNumericInput(session, "noenzyme_maxn", "Max span of peptide lengths", value = 0)
        updateCheckboxInput(session, "customenzyme", "Custom enzyme", value = FALSE)
        updateTextInput(session, "custom_enzymeC", "C-term specificity", value = "")
        updateTextInput(session, "custom_enzymeN", "N-term specificity", value = "")
      })

      if (FALSE) {
        enzyme <- reactive(input$enzyme)
        observe({
          if (enzyme() == "Noenzyme") {
            output$nes <- renderUI({
              fluidRow(
                column(8, numericInput(NS(id, "noenzyme_maxn"),
                                       "Maximum span of peptide lengths",
                                       value = 0)),
              )
            })
          }
        })
      }

      if (FALSE) {
        output$nes <- renderUI({
          x <- numericInput(NS(id, "noenzyme_maxn"),
                            "Maximum span of peptide lengths",
                            value = dplyr::coalesce(input$noenzyme_maxn, 0))

          if (input$enzyme != "Noenzyme") {
            x <- div(style = "visibility:hidden;", x)
          }

          x
        })
      }

      observeEvent(input$maxn_vmods_setscombi, {
        bslib::update_tooltip(session$ns("maxn_vmods_setscombi"))
      })

      observeEvent(input$noenzyme_maxn, {
        bslib::update_tooltip(session$ns("noenzyme_maxn"))
      })

      observeEvent(input$customenzyme, {
        if (!input$customenzyme) {
          updateTextInput(session, "custom_enzymeC", "C-term specificity", value = "")
          updateTextInput(session, "custom_enzymeN", "N-term specificity", value = "")
        }
      })


      list(min_len = reactive(input$min_len),
           max_len = reactive(input$max_len),
           max_miss = reactive(input$max_miss),
           min_mass = reactive(input$min_mass),
           max_mass = reactive(input$max_mass),
           ppm_ms1 = reactive(input$ppm_ms1),
           maxn_vmods_setscombi = reactive(input$maxn_vmods_setscombi),
           maxn_vmods_per_pep = reactive(input$maxn_vmods_per_pep),
           maxn_sites_per_vmod = reactive(input$maxn_sites_per_vmod),
           maxn_vmods_sitescombi_per_pep = reactive(input$maxn_vmods_sitescombi_per_pep),
           maxn_fnl_per_seq = reactive(input$maxn_fnl_per_seq),
           maxn_vnl_per_seq = reactive(input$maxn_vnl_per_seq),
           min_ms2mass = reactive(input$min_ms2mass),
           max_ms2mass = reactive(input$max_ms2mass),
           ppm_ms2 = reactive(input$ppm_ms2),
           minn_ms2 = reactive(input$minn_ms2),
           type_ms2ions = reactive(input$type_ms2ions),
           enzyme = reactive(input$enzyme),
           noenzyme_maxn = reactive(input$noenzyme_maxn),
           custom_enzymeC = reactive(input$custom_enzymeC),
           custom_enzymeN = reactive(input$custom_enzymeN)
        )
    }
  )
}


