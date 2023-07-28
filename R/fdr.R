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
    ),

    checkboxInput(NS(id, "svm_reproc"), "Percolator", value = FALSE),
    conditionalPanel(
      condition = "input.svm_reproc == true",
      ns = NS(id),
      fluidRow(
        column(3, selectInput(NS(id, "svm_kernel"), "SVM kernel", c("radial", "linear"))),
      ),
      fluidRow(
        column(3, checkboxInput(NS(id, "use_pep_score"), "Peptide score", value = TRUE)),
        column(3, checkboxInput(NS(id, "use_pep_ret_range"), "Retention time", value = TRUE)),
        column(3, checkboxInput(NS(id, "use_pep_delta"), "MS1 mass error", value = TRUE)),
        column(3, checkboxInput(NS(id, "use_pep_n_ms2"), "Number of MS2 features", value = TRUE)),
        column(3, checkboxInput(NS(id, "use_pep_expect"), "Peptide expectation", value = TRUE)),
        column(3, checkboxInput(NS(id, "use_pep_exp_mz"), "Experimental m/z", value = TRUE)),
        column(3, checkboxInput(NS(id, "use_pep_exp_mr"), "Experimental molecular weight", value = TRUE)),
        column(3, checkboxInput(NS(id, "use_pep_tot_int"), "Precursor intensity", value = TRUE)),
        column(3, checkboxInput(NS(id, "use_pep_n_matches2"), "Number of secondary features", value = TRUE)),
        column(3, checkboxInput(NS(id, "use_pep_ms2_deltas_mean"), "Mean MS2 mass error", value = TRUE)),
      ),
    ),
    actionButton(NS(id, "reset"), "Reset",
                 style = "width:70px; background-color:#c51b8a; border-color:#f0f0f0; color:white",
                 title = "Reset values in the current tab"),
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
        updateSelectInput(session, "svm_kernel", "SVM kernel", c("radial", "linear"), selected = "radial")
        updateCheckboxInput(session, "use_pep_score", "Peptide score", value = TRUE)
        updateCheckboxInput(session, "use_pep_ret_range", "Retention time", value = TRUE)
        updateCheckboxInput(session, "use_pep_delta", "MS1 mass error", value = TRUE)
        updateCheckboxInput(session, "use_pep_n_ms2", "Number of MS2 features", value = TRUE)
        updateCheckboxInput(session, "use_pep_expect", "Peptide expectation", value = TRUE)
        updateCheckboxInput(session, "use_pep_exp_mz", "Experimental m/z", value = TRUE)
        updateCheckboxInput(session, "use_pep_exp_mr", "Experimental molecular weight", value = TRUE)
        updateCheckboxInput(session, "use_pep_tot_int", "Precursor intensity", value = TRUE)
        updateCheckboxInput(session, "use_pep_n_matches2", "Number of secondary features", value = TRUE)
        updateCheckboxInput(session, "use_pep_ms2_deltas_mean", "Mean MS2 mass error", value = TRUE)
      })

      p1 <- eventReactive(input$use_pep_score, if (input$use_pep_score) "pep_score" else NULL)
      p2 <- eventReactive(input$use_pep_ret_range, if (input$use_pep_ret_range) "pep_ret_range" else NULL)
      p3 <- eventReactive(input$use_pep_delta, if (input$use_pep_delta) "pep_delta" else NULL)
      p4 <- eventReactive(input$use_pep_n_ms2, if (input$use_pep_n_ms2) "pep_n_ms2" else NULL)
      p5 <- eventReactive(input$use_pep_expect, if (input$use_pep_expect) "pep_expect" else NULL)
      p6 <- eventReactive(input$use_pep_exp_mz, if (input$use_pep_exp_mz) "pep_exp_mz" else NULL)
      p7 <- eventReactive(input$use_pep_exp_mr, if (input$use_pep_exp_mr) "pep_exp_mr" else NULL)
      p8 <- eventReactive(input$use_pep_tot_int, if (input$use_pep_tot_int) "pep_tot_int" else NULL)
      p9 <- eventReactive(input$use_pep_n_matches2, if (input$use_pep_n_matches2) "pep_n_matches2" else NULL)
      p10 <- eventReactive(input$use_pep_ms2_deltas_mean, if (input$use_pep_ms2_deltas_mean) "pep_ms2_deltas_mean" else NULL)

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
           svm_reproc = reactive(input$svm_reproc) ,
           svm_kernel = reactive(input$svm_kernel) ,
           svm_feats = reactive(c(p1(), p2(), p3(), p4(), p5(), p6(), p7(), p8(), p9(), p10())),
           use_pep_score <- reactive(input$use_pep_score),
           use_pep_ret_range <- reactive(input$use_pep_ret_range),
           use_pep_delta <- reactive(input$use_pep_delta),
           use_pep_n_ms2 <- reactive(input$use_pep_n_ms2),
           use_pep_expect <- reactive(input$use_pep_expect),
           use_pep_exp_mz <- reactive(input$use_pep_exp_mz),
           use_pep_exp_mr <- reactive(input$use_pep_exp_mr),
           use_pep_tot_int <- reactive(input$use_pep_tot_int),
           use_pep_n_matches2 <- reactive(input$use_pep_n_matches2),
           use_pep_ms2_deltas_mean <- reactive(input$use_pep_ms2_deltas_mean)
      )
    }
  )
}


