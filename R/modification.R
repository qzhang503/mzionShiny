#' User interface of modification parameters
#'
#' @param id Namespace identifier.
modUI <- function(id)
{
  ns <- NS(id)
  tagList(
    checkboxInput(ns("use_short_mods"), "Short list", value = TRUE),
    fluidRow(
      column(4, selectInput(ns("fixedmods"), "Fixed modifications",  short_mods,
                            selected = "Carbamidomethyl (Anywhere = C)", multiple = TRUE)),
      column(4, selectInput(ns("varmods"), "Variable modifications", short_mods,
                            selected = c("Acetyl (Protein N-term = N-term)", "Oxidation (Anywhere = M)",
                                         "Deamidated (Anywhere = N)", "Deamidated (Anywhere = Q)"),
                            multiple = TRUE)),
      column(4, selectInput(ns("locmods"), "Localizations", short_mods,
                            selected = c("Oxidation (Anywhere = M)", "Deamidated (Anywhere = N)",
                                         "Deamidated (Anywhere = Q)"), multiple = TRUE)),
    ),
    column(4, sliderInput(ns("n_13c"), "Numbers of 13C", value = c(0, 0), min = -1, max = 3)),
    column(4, checkboxInput(NS(id, "rm_dup_term_anywhere"),
                            paste0("Remove the same-site combinations at both terminal and anywhere positions ",
                                   "(e.g., N-term Q and Anywhere Q)"), value = TRUE)),

    checkboxInput(ns("use_ms1notches"), "Precursor off-sets"),
    conditionalPanel(
      condition = "input.use_ms1notches == true",
      ns = ns,
      fluidRow(
        column(4, textInput(ns("ms1_notches"), "Values", placeholder = "-97.976896, -79.96633")),
      )
    ),
    checkboxInput(ns("use_ms1neulosses"), "Precursor neutral losses"),
    conditionalPanel(
      condition = "input.use_ms1neulosses == true",
      ns = ns,
      fluidRow(
        column(4, selectInput(ns("ms1_neulosses"), "Values", selected = NULL,
                              choices = short_mods, multiple = TRUE)),
      )
    ),
    conditionalPanel(
      condition = "input.use_ms1notches == true || input.use_ms1neulosses == true",
      ns = ns,
      fluidRow(
        column(4, numericInput(ns("maxn_neulosses_fnl"), "Max fixed NLs", value = 2, min = 1)),
        column(4, numericInput(ns("maxn_neulosses_vnl"), "Max variable NLs", value = 2, min = 1))
      )
    ),
    checkboxInput(ns("isolabs"), "Isotope labels"),
    conditionalPanel(
      condition = "input.isolabs == true",
      ns = ns,
      fluidRow(
        column(4, selectInput(ns("fixedlabs"), "Fixed labels", umods_labs$modification, multiple = TRUE)),
        column(4, selectInput(ns("varlabs"), "Variable labels", umods_labs$modification, multiple = TRUE)),
      )
    ),
    # multiple motifs later...
    # checkboxInput(ns("pepmotifs"), "Peptide motifs"),
    # conditionalPanel(
    #   condition = "input.pepmotifs == true",
    #   ns = ns,
    #   fluidRow(
    #     column(4, selectInput(ns("nm_mod_motifs"), "Modification", short_mods)),
    #     column(4, textInput(ns("val_mod_motifs"), "Motif", NULL, placeholder = "NM")),
    #   )
    # ),
    actionButton(ns("reset"), "Reset", class = "btn-danger"),
  )
}


#' Server-side processing of search parameters
#'
#' @param id Namespace identifier.
modServer <- function(id)
{
  moduleServer(
    id,
    function(input, output, session) {
      use_short_mods <- reactive(input$use_short_mods)
      observeEvent(use_short_mods(), {
        choices <- if (use_short_mods()) short_mods else umods$modification
        updateSelectInput(session = session, inputId = "fixedmods", choices = choices,
                          selected = "Carbamidomethyl (Anywhere = C)")
        updateSelectInput(session = session, inputId = "varmods", choices = choices,
                          selected = c("Acetyl (Protein N-term = N-term)", "Oxidation (Anywhere = M)",
                                       "Deamidated (Anywhere = N)", "Deamidated (Anywhere = Q)"))
        updateSelectInput(session = session, inputId = "locmods", choices = choices,
                          selected = c("Oxidation (Anywhere = M)", "Deamidated (Anywhere = N)",
                                       "Deamidated (Anywhere = Q)"))
        updateSelectInput(session = session, inputId = "ms1_neulosses", choices = choices,
                          selected = NULL)
      })

      observeEvent(input$reset, {
        updateCheckboxInput(session, "use_short_mods", "Short list", value = TRUE)
        updateSelectInput(session, "fixedmods", "Fixed modifications",  umods_short$modification,
                          selected = "Carbamidomethyl (Anywhere = C)")
        updateSelectInput(session, "varmods", "Variable modifications", umods_short$modification,
                          selected = c("Acetyl (Protein N-term = N-term)", "Oxidation (Anywhere = M)",
                                       "Deamidated (Anywhere = N)", "Deamidated (Anywhere = Q)"))
        updateSelectInput(session, "locmods", "Localizations", umods_short$modification,
                          selected = c("Oxidation (Anywhere = M)", "Deamidated (Anywhere = N)",
                                       "Deamidated (Anywhere = Q)"))
        updateSliderInput(session, "n_13c", "Numbers of 13C", value = c(0, 0), min = -1, max = 3)
        updateCheckboxInput(session, "rm_dup_term_anywhere",
                            paste0("Remove the same-site combinations at both terminal and anywhere positions ",
                                   "(e.g., N-term Q and Anywhere Q)"), value = FALSE)
        updateCheckboxInput(session, "use_ms1notches", "Precursor off-sets", value = FALSE)
        updateCheckboxInput(session, "use_ms1neulosses", "Precursor neutral losses", value = FALSE)
        updateCheckboxInput(session, "isolabs", "Isotope labels", value = FALSE)
        updateCheckboxInput(session, "pepmotifs", "Peptide motifs", value = FALSE)
        updateTextInput(session, "ms1_notches", "Values", value = "", placeholder = "-97.976896, -79.96633")
        updateNumericInput(session, "maxn_neulosses_fnl", "Max fixed NLs", value = 2, min = 1)
        updateNumericInput(session, "maxn_neulosses_vnl", "Max variable NLs", value = 2, min = 1)
        updateSelectInput(session, "ms1_neulosses", "Values", umods_short$modification, selected = NULL)
        # updateSelectInput(session, "nm_mod_motifs", "Modification", umods_short$modification)
        # updateTextInput(session, "val_mod_motifs", value = "", placeholder = "NM")
        updateSelectInput(session, "fixedlabs", "Fixed labels",  umods_labs$modification)
        updateSelectInput(session, "varlabs", "Variable labels", umods_labs$modification)
      })

      notches_and_nls <- reactive(list(input$use_ms1notches, input$use_ms1neulosses))
      observeEvent(notches_and_nls(), {
        if (input$use_ms1notches && input$use_ms1neulosses) {
          print("Select either Precursor off-sets or Precursor neutral losses, not both.")
          updateCheckboxInput(session, "use_ms1neulosses", "Precursor neutral losses", value = FALSE)
        }
      })

      list(fixedmods = reactive(input$fixedmods),
           varmods = reactive(input$varmods),
           locmods = reactive(input$locmods),
           rm_dup_term_anywhere = reactive(input$rm_dup_term_anywhere),
           n_13c = reactive(input$n_13c),
           ms1_notches = reactive(input$ms1_notches),
           maxn_neulosses_fnl = reactive(input$maxn_neulosses_fnl),
           maxn_neulosses_vnl = reactive(input$maxn_neulosses_vnl),
           ms1_neulosses = reactive(input$ms1_neulosses),
           # nm_mod_motifs = reactive(input$nm_mod_motifs),
           # val_mod_motifs = reactive(input$val_mod_motifs),
           fixedlabs = reactive(input$fixedlabs),
           varlabs = reactive(input$varlabs))
    }
  )
}


