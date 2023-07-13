#' User interface of FASTA parameters
#'
#' @param id Namespace identifier.
fastaUI <- function(id)
{
  tagList(
    fluidRow(
      # shinyFilesButton(NS(id, "files"), "Select FASTA files", "Select Files", multiple = TRUE),

      column(4, checkboxInput(NS(id, "use_ms1_cache"), "Use cache", value = TRUE)),
      column(12, numericInput(NS(id, "n"), "Number of databases", 1, min = 1)),
      uiOutput(NS(id, "dbs")),
      # textOutput(NS(id, "fastas")),
    ),
  )
}


#' Server-side processing of FASTA parameters
#'
#' @param id Namespace identifier.
#' @param accessions Accession types.
fastaServer <- function(id,
                        accessions = c("uniprot_acc", "uniprot_id", "refseq_acc", "other"))
{
  moduleServer(
    id,
    function(input, output, session) {
      # roots <- getVolumes()
      # shinyFileChoose(input, "files", roots = roots, filetypes = c("fasta", "fas"))
      # files <- reactive(parseFilePaths(roots, input$files)[, "datapath", drop = TRUE])

      fas_ids <- reactive(paste0("fasta_", seq_len(input$n)))
      acc_ids <- reactive(paste0("acc_type_", seq_len(input$n)))
      pat_ids <- reactive(paste0("acc_pattern_", seq_len(input$n)))
      use_cuspat_ids <- reactive(paste0("use_custom_pat_", seq_len(input$n)))

      output$dbs <- renderUI({
        mapply(function (x1, x2, x3, x4) {
          n <- gsub(".*_(\\d+)$", "\\1", x1)
          fluidRow(
            column(6, textInput(NS(id, x1), paste("Database", n),
                                value = isolate(input[[x1]]) %||% "",
                                placeholder = "~/Mzion/DB/my.fasta")),
            column(3, radioButtons(NS(id, x2), "Accession type", accessions)),
            column(3, textInput(NS(id, x3), "Accession pattern (optional)",
                                value = isolate(input[[x3]]) %||% "",
                                placeholder = "Regular expression for \"other\"")),
            # column(4, checkboxInput(NS(id, x4), "Custom accession")),
          )
        }, fas_ids(), acc_ids(), pat_ids(), use_cuspat_ids())
      })

      fastas <- reactive({
        ans <- unlist(lapply(fas_ids(), function (x) input[[x]] %||% ""))
        # ans[ans == ""] <- NULL
        ans
      })
      # output$fastas <- renderPrint({ fastas() })

      acc_types <- reactive({
        ans <- unlist(lapply(acc_ids(), function (x) input[[x]] %||% ""))
        # ans[ans == ""] <- NULL
        ans
      })

      acc_patterns <- reactive({
        ans <- unlist(lapply(pat_ids(), function (x) input[[x]] %||% ""))
        # ans[ans == ""] <- NULL
        ans
      })

      use_ms1_cache <- reactive(input$use_ms1_cache)

      list(# files = files,
           fasta = fastas,
           acc_type = acc_types,
           acc_pattern = acc_patterns,
           use_ms1_cache = use_ms1_cache)
    }
  )
}


