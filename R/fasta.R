#' User interface of FASTA parameters
#'
#' @param id Namespace identifier.
fastaUI0 <- function(id)
{
  tagList(
    fluidRow(
      column(3, checkboxInput(NS(id, "use_ms1_cache"), "Use cache", value = TRUE)),
    ),
    fluidRow(
      column(3, numericInput(NS(id, "n"), "Number of fastas", 1, min = 1)),
    ),
    uiOutput(NS(id, "dbs")),
    fluidRow(
      column(3, shinyFiles::shinyFilesButton(NS(id, "btn_fasta"), "Load FASTA", "Please select a file",
                                             filetype = list(text = "fasta"),
                                             multiple = FALSE, viewtype = "detail")),
    ),
  )
}


#' Server-side processing of FASTA parameters
#'
#' @param id Namespace identifier.
#' @param accessions Accession types.
fastaServer0 <- function(id, accessions = c("uniprot_acc", "uniprot_id", "refseq_acc", "other"))
{
  moduleServer(
    id,
    function(input, output, session) {
      fas_ids <- reactive(paste0("fasta_", seq_len(input$n)))
      acc_ids <- reactive(paste0("acc_type_", seq_len(input$n)))
      pat_ids <- reactive(paste0("acc_pattern_", seq_len(input$n)))
      use_cuspat_ids <- reactive(paste0("use_custom_pat_", seq_len(input$n)))

      output$dbs <- renderUI({
        mapply(function (x1, x2, x3, x4) {
          idx <- gsub(".*_(\\d+)$", "\\1", x1)
          fixedRow(
            # column(2, shinyFiles::shinyFilesButton(NS(id, x0), "Load FASTA", "Please select a file",
            #                                        filetype = list(text = "fasta"),
            #                                        multiple = FALSE, viewtype = "detail")),
            fluidRow(
              column(4, textInput(NS(id, x1), label = paste("Database", idx),
                                  value = isolate(input[[x1]]) %||% "",
                                  placeholder = "~/Mzion/DB/my.fasta")),
              column(3, radioButtons(NS(id, x2), "Accession type", accessions,
                                     selected = isolate(input[[x2]]))),
              column(3, textInput(NS(id, x3), "Accession pattern (optional)",
                                  value = isolate(input[[x3]]) %||% "",
                                  placeholder = "Regular expression for \"other\"")),
              # column(4, checkboxInput(NS(id, x4), "Custom accession")),
            ),
          )
        }, fas_ids(), acc_ids(), pat_ids(), use_cuspat_ids())
      })

      volumes <- c(Home = fs::path_home_r(), Home_win = fs::path_home(),
                   "R Installation" = R.home(), shinyFiles::getVolumes()())

      observeEvent(input[["btn_fasta"]], {
        shinyFiles::shinyFileChoose(input, "btn_fasta", roots = volumes, session = session)

        if (is.integer(input[["btn_fasta"]]))
          NULL
        else {
          fileinfo <- shinyFiles::parseFilePaths(volumes, input[["btn_fasta"]])
          fileinfo <- gsub("\\\\", "/", unname(fileinfo[["datapath"]]))

          for (i in seq_len(input$n)) {
            if (input[[paste0("fasta_", i)]] == "")
              break
          }

          updateTextInput(session, paste0("fasta_", i), NULL, value = fileinfo)
        }
      })

      fastas <- reactive({
        ans <- unlist(lapply(fas_ids(), function (x) input[[x]] %||% ""))
        ans <- ans[ans != ""]
      })
      acc_types <- reactive({
        ans <- unlist(lapply(acc_ids(), function (x) input[[x]] %||% ""))
        ans <- ans[ans != ""]
      })
      acc_patterns <- reactive({
        ans <- unlist(lapply(pat_ids(), function (x) input[[x]] %||% ""))
        ans <- ans <- ans[ans != ""]
      })

      use_ms1_cache <- reactive(input$use_ms1_cache)

      list(fasta = fastas,
           acc_type = acc_types,
           acc_pattern = acc_patterns,
           use_ms1_cache = use_ms1_cache,
           n = reactive(input$n))
    }
  )
}



#' User interface of FASTA parameters
#'
#' @param id Namespace identifier.
#' @param accessions Protein accession types.
fastaUI <- function(id, accessions = c("uniprot_acc", "uniprot_id", "refseq_acc", "other"))
{
  ns <- NS(id)

  tagList(
    fluidRow(
      column(4, checkboxInput(NS(id, "use_ms1_cache"), "Use cache", value = TRUE)),
    ),
    fluidRow(
      column(8,
             shinyFiles::shinyFilesButton(NS(id, "select_1"), "Select a FASTA file", "Select Files",
                                             multiple = FALSE, style = "background-color: #f5f5f5"),
             textInput(NS(id, "fasta_1"), "Database 1", value = "", placeholder = file.path("~/Mzion/DB/db1.fasta")),
             radioButtons(NS(id, "acc_type_1"), "Accession type 1", accessions),
             checkboxInput(NS(id, "custom_accpat_1"), "Custom accession 1"),
             conditionalPanel(
               condition = "input.custom_accpat_1 == true",
               ns = ns,
               textInput(NS(id, "acc_pattern_1"), "Accession pattern 1"), NULL),
      ),
    ),
    br(),
    fluidRow(
      column(8,
             shinyFiles::shinyFilesButton(NS(id, "select_2"), "Select a FASTA file", "Select Files",
                                          multiple = FALSE, style = "background-color: #f5f5f5"),
             textInput(NS(id, "fasta_2"), "Database 2", value = "", placeholder = file.path("~/Mzion/DB/db2.fasta")),
             radioButtons(NS(id, "acc_type_2"), "Accession type 2", accessions),
             checkboxInput(NS(id, "custom_accpat_2"), "Custom accession 2"),
             conditionalPanel(
               condition = "input.custom_accpat_2 == true",
               ns = ns,
               textInput(NS(id, "acc_pattern_2"), "Accession pattern 2"), NULL),
      ),
    ),
    br(),
    fluidRow(
      column(8,
             shinyFiles::shinyFilesButton(NS(id, "select_3"), "Select a FASTA file", "Select Files",
                                          multiple = FALSE, style = "background-color: #f5f5f5"),
             textInput(NS(id, "fasta_3"), "Database 3", value = "", placeholder = file.path("~/Mzion/DB/db3.fasta")),
             radioButtons(NS(id, "acc_type_3"), "Accession type 3", accessions),
             checkboxInput(NS(id, "custom_accpat_3"), "Custom accession 3"),
             conditionalPanel(
               condition = "input.custom_accpat_3 == true",
               ns = ns,
               textInput(NS(id, "acc_pattern_3"), "Accession pattern 3"), NULL),
      ),
    ),
    br(),
    fluidRow(
      column(8,
             shinyFiles::shinyFilesButton(NS(id, "select_4"), "Select a FASTA file", "Select Files",
                                          multiple = FALSE, style = "background-color: #f5f5f5"),
             textInput(NS(id, "fasta_4"), "Database 4", value = "", placeholder = file.path("~/Mzion/DB/db4.fasta")),
             radioButtons(NS(id, "acc_type_4"), "Accession type 4", accessions),
             checkboxInput(NS(id, "custom_accpat_4"), "Custom accession 4"),
             conditionalPanel(
               condition = "input.custom_accpat_4 == true",
               ns = ns,
               textInput(NS(id, "acc_pattern_4"), "Accession pattern 4"), NULL),
      ),
    ),
    br(),
    fluidRow(
      column(8,
             shinyFiles::shinyFilesButton(NS(id, "select_5"), "Select a FASTA file", "Select Files",
                                          multiple = FALSE, style = "background-color: #f5f5f5"),
             textInput(NS(id, "fasta_5"), "Database 5", value = "", placeholder = file.path("~/Mzion/DB/db5.fasta")),
             radioButtons(NS(id, "acc_type_5"), "Accession type 5", accessions),
             checkboxInput(NS(id, "custom_accpat_5"), "Custom accession 5"),
             conditionalPanel(
               condition = "input.custom_accpat_5 == true",
               ns = ns,
               textInput(NS(id, "acc_pattern_5"), "Accession pattern 5"), NULL),
      ),
    ),
    br(),
  )
}


#' Server-side processing of FASTA parameters
#'
#' @param id Namespace identifier.
#' @param accessions Accession types.
fastaServer <- function(id, accessions = c("uniprot_acc", "uniprot_id", "refseq_acc", "other"))
{
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      volumes <- c(Home = fs::path_home_r(), Home_win = fs::path_home(), shinyFiles::getVolumes()())

      shinyFiles::shinyFileChoose(input, "select_1", roots = volumes, filetypes = c("fasta", "fas"))
      fasta_1 <- reactive(unname(shinyFiles::parseFilePaths(volumes, input$select_1)[, "datapath", drop = TRUE]))
      observeEvent(input$select_1, { updateTextInput(session, "fasta_1", NULL, value = fasta_1()) })

      shinyFiles::shinyFileChoose(input, "select_2", roots = volumes, filetypes = c("fasta", "fas"))
      fasta_2 <- reactive(unname(shinyFiles::parseFilePaths(volumes, input$select_2)[, "datapath", drop = TRUE]))
      observeEvent(input$select_2, { updateTextInput(session, "fasta_2", NULL, value = fasta_2()) })

      shinyFiles::shinyFileChoose(input, "select_3", roots = volumes, filetypes = c("fasta", "fas"))
      fasta_3 <- reactive(unname(shinyFiles::parseFilePaths(volumes, input$select_3)[, "datapath", drop = TRUE]))
      observeEvent(input$select_3, { updateTextInput(session, "fasta_3", NULL, value = fasta_3()) })

      shinyFiles::shinyFileChoose(input, "select_4", roots = volumes, filetypes = c("fasta", "fas"))
      fasta_4 <- reactive(unname(shinyFiles::parseFilePaths(volumes, input$select_4)[, "datapath", drop = TRUE]))
      observeEvent(input$select_4, { updateTextInput(session, "fasta_4", NULL, value = fasta_4()) })

      shinyFiles::shinyFileChoose(input, "select_5", roots = volumes, filetypes = c("fasta", "fas"))
      fasta_5 <- reactive(unname(shinyFiles::parseFilePaths(volumes, input$select_5)[, "datapath", drop = TRUE]))
      observeEvent(input$select_5, { updateTextInput(session, "fasta_5", NULL, value = fasta_5()) })

      fas_ids <- reactive(paste0("fasta_", 1:5))
      acc_ids <- reactive(paste0("acc_type_", 1:5))
      pat_ids <- reactive(paste0("acc_pattern_", 1:5))
      use_cuspat_ids <- reactive(paste0("use_custom_pat_", 1:5))

      fastas <- reactive({ unlist(lapply(fas_ids(), function (x) input[[x]] %||% "")) })
      acc_types <- reactive({ unlist(lapply(acc_ids(), function (x) input[[x]] %||% "")) })
      acc_patterns <- reactive({ unlist(lapply(pat_ids(), function (x) input[[x]] %||% "")) })
      use_ms1_cache <- reactive(input$use_ms1_cache)

      list(fasta = fastas,
           acc_type = acc_types,
           acc_pattern = acc_patterns,
           use_ms1_cache = use_ms1_cache)
    }
  )
}


