#' User interface of FDR parameters
#'
#' @param id Namespace identifier.
fdrUI <- function(id)
{
  tagList(
    fluidRow(
      column(4, numericInput(NS(id, "target_fdr"), "Target FDR", value = 0.01, min = 1E-10)),
      column(4, selectInput(NS(id, "fdr_type"), "FDR type", c("protein", "peptide", "psm"))),
      column(4, numericInput(NS(id, "max_pepscores_co"), "Score threshold to warrent a PSM significance", value = 50, min = 0)),
      column(4, numericInput(NS(id, "min_pepscores_co"), "Score threshold to discard a PSM significance", value = 0, min = 0)),
      column(4, numericInput(NS(id, "max_protscores_co"), "Upper limit in protein score cut-offs", value = Inf, min = 0)),
      column(4, numericInput(NS(id, "max_protnpep_co"), "Max number of peptides to warrant a protein significance", value = 10, min = 1)),
      column(4, selectInput(NS(id, "fdr_group"), "FDR group", fdr_groups, selected = "base")),
      column(4, selectInput(NS(id, "nes_fdr_group"), "FDR group (NES)", nes_fdr_groups, selected = "base")),
      column(4, numericInput(NS(id, "topn_mods_per_seq"), "Top-N matches per sequence", value = 1, min = 1)),
      column(4, numericInput(NS(id, "topn_seqs_per_query"), "Top-N sequences per query", value = 1), min = 1),
      column(4, checkboxInput(NS(id, "svm_reproc"), "Percolator", value = FALSE)),
    ),

    # checkboxInput(NS(id, "svm_reproc"), "Percolator"),
    # conditionalPanel(
    #   condition = "input.svm_reproc == true",
    #   ns = NS(id),
    #   fluidRow(
    #     column(3, selectInput(NS(id, "svm_kernel"), "SVM kernel", c("radial", "linear"))),
    #   ),
    #   fluidRow(
    #     column(3, checkboxInput(NS(id, "pep_score"), "Peptide score", value = TRUE)),
    #     column(3, checkboxInput(NS(id, "pep_ret_range"), "Retention time", value = TRUE)),
    #     column(3, checkboxInput(NS(id, "pep_delta"), "MS1 mass error", value = TRUE)),
    #     column(3, checkboxInput(NS(id, "pep_n_ms2"), "Number of MS2 features", value = TRUE)),
    #     column(3, checkboxInput(NS(id, "pep_expect"), "Peptide expectation", value = TRUE)),
    #     column(3, checkboxInput(NS(id, "pep_exp_mz"), "Experimental m/z", value = TRUE)),
    #     column(3, checkboxInput(NS(id, "pep_exp_mr"), "Experimental molecular weight", value = TRUE)),
    #     column(3, checkboxInput(NS(id, "pep_tot_int"), "Precursor intensity", value = TRUE)),
    #     column(3, checkboxInput(NS(id, "pep_n_matches2"), "Number of secondary features", value = TRUE)),
    #     column(3, checkboxInput(NS(id, "pep_ms2_deltas_mean"), "Mean MS2 mass error", value = TRUE)),
    #   ),
    # ),

    actionButton(NS(id, "reset"), "Reset", class = "btn-danger"),
  )
}


#' Server-side processing of FDR parameters
#'
#' @param id Namespace identifier.
fdrServer <- function(id)
{
  moduleServer(
    id,
    function(input, output, session) {
      observeEvent(input$reset, {
        updateNumericInput(session, "target_fdr", "Target FDR", value = 0.01, min = 1E-10)
        updateSelectInput(session, "fdr_type", "FDR type", c("protein", "peptide", "psm"))
        updateNumericInput(session, "max_pepscores_co", "Score threshold to warrent a PSM significance", value = 50, min = 0)
        updateNumericInput(session, "min_pepscores_co", "Score threshold to discard a PSM significance", value = 0, min = 0)
        updateNumericInput(session, "max_protscores_co", "Upper limit in protein score cut-offs", value = Inf, min = 0)
        updateNumericInput(session, "max_protnpep_co", "Max number of peptides to warrant a protein significance", value = 10, min = 1)
        updateSelectInput(session, "fdr_group", "FDR group", fdr_groups, selected = "base")
        updateSelectInput(session, "nes_fdr_group", "FDR group (NES)", nes_fdr_groups, selected = "base")
        updateNumericInput(session, "topn_mods_per_seq", "Top-N matches per sequence", value = 1, min = 1)
        updateNumericInput(session, "topn_seqs_per_query", "Top-N sequences per query", value = 1, min = 1)
        updateCheckboxInput(session, "svm_reproc", "Percolator", value = FALSE)
      })

      list(target_fdr = reactive(input$target_fdr),
           fdr_type = reactive(input$fdr_type),
           max_pepscores_co = reactive(input$max_pepscores_co),
           min_pepscores_co = reactive(input$min_pepscores_co),
           max_protscores_co = reactive(input$max_protscores_co),
           max_protnpep_co = reactive(input$max_protnpep_co),
           fdr_group = reactive(input$fdr_group),
           nes_fdr_group = reactive(input$nes_fdr_group),
           topn_mods_per_seq = reactive(input$topn_mods_per_seq),
           topn_seqs_per_query = reactive(input$topn_seqs_per_query),
           svm_reproc = reactive(input$svm_reproc)
      )
    }
  )
}


