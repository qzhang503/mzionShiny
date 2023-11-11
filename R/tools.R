#' User interface of MGF parameters
#'
#' @param id Namespace identifier.
#' @param sites Sites of amino-acid residues.
#' @param positions Positions of modifications.
add_unimodUI <- function(id,
                     sites = c("A", "C", "D", "E", "F", "G", "H", "I", "K",
                               "L", "M", "N", "P", "Q", "R", "S", "T", "W",
                               "Y", "V", "U", "B", "X", "Z", "N-term",
                               "C-term"),
                     positions = c("Anywhere", "Any N-term", "Protein N-term",
                                   "Any C-term", "Protein C-term"))
{
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        textInput(NS(id, "title"), label = "Title",
                  placeholder = "TMT10plexNterm+Gln->pyro-Glu") |>
          bslib::tooltip("Do-not-use-space"),
        textInput(NS(id, "full_name"), label = "Description",
                  placeholder = "Additive N-term TMT10 and Gln->pyro-Glu"),
        textInput(NS(id, "composition"), label = "Composition",
                  placeholder = "H(17) C(8) 13C(4) 15N O(2)"),
      ),
      fluidRow(
        selectInput(NS(id, "site"), "AA site", sites, selected = NULL, multiple = FALSE),
        selectInput(NS(id, "position"), "Modification position", positions, selected = NULL, multiple = FALSE),
        textInput(NS(id, "neuloss"), label = "Neutral loss", placeholder = "H(4) C O S"),
      ),
      fluidRow(
        actionButton(NS(id, "load"), "Load", class = "btn-success", title = "Loads all Unimod entries."),
        actionButton(NS(id, "add"),  "Add",  class = "btn-warning", title = "Adds a Unimod entry"),
      ),
      textOutput(NS(id, "msg")),
      width = 4
    ),
    mainPanel(
      DT::DTOutput(NS(id, "table")),
      width = 8
    )
  )
}


#' Server-side processing of MGF parameters
#'
#' @param id Namespace identifier.
add_unimodServer <- function(id)
{
  moduleServer(
    id,
    function(input, output, session) {
      umods <- reactiveVal()

      observeEvent(input$table_rows_selected, {
        if (!is.null(i <- input$table_rows_selected)) {
          # stopifnot(length(i) == 1L)
          row <- umods()[i, , drop = FALSE]
          updateTextInput(session, "title", NULL, value = row$title)
          updateTextInput(session, "full_name", NULL, value = row$full_name)
          updateTextInput(session, "composition", NULL, value = row$composition)
          updateSelectInput(session, "site", label = NULL, choices = NULL, selected = row$site)
          updateSelectInput(session, "position", label = NULL, choices = NULL, selected = row$position)
          updateTextInput(session, "neuloss", NULL, value = row$neuloss)
        }
      })

      observeEvent(input$add, {
        if (input$neuloss == "") {
          m1 <- list(mono_mass = "0", avge_mass = "0")
          nl <- "0"
        }
        else {
          m1 <- tryCatch(mzion::calc_unimod_compmass(input$neuloss), error = function(e) NULL)
          nl <- input$neuloss

          if (is.null(m1)) {
            output$msg <- renderText(
              paste0("Ignore Unimod neutral loss: ",input$neuloss, ". ",
                     "Please check the formula."))
            m1 <- list(mono_mass = "0", avge_mass = "0")
            nl <- "0"
          }
        }

        m0 <- tryCatch(mzion::calc_unimod_compmass(input$composition), error = function(e) NULL)

        if (is.null(m0)) {
          output$msg <- renderText(
            paste0("Cannot add Unimod: ",input$composition, ". ",
                   "Please check the formula."))
        }
        else {
          mzion::add_unimod(header      = c(title       = input$title,
                                            full_name   = input$full_name),
                            specificity = c(site        = input$site,
                                            position    = input$position),
                            delta       = c(mono_mass   = as.character(m0$mono_mass),
                                            avge_mass   = as.character(m0$avge_mass),
                                            composition = input$composition),
                            neuloss     = c(mono_mass   = as.character(m1[["mono_mass"]]),
                                            avge_mass   = as.character(m1[["avge_mass"]]),
                                            composition = nl))

          output$msg <- renderText(paste0("Unimod added: ", input$title, "."))
          # umods(mzion::table_unimods())
          # output$table <- DT::renderDT(umods(), filter = "bottom", selection = "single", options = list(pageLength = 5))
        }
      })

      observeEvent(input$load, {
        umods(mzion::table_unimods())
        output$table <- DT::renderDT(umods(), filter = "bottom",
                                     selection = "single",
                                     options = list(pageLength = 5))
      })

      observeEvent(input$title, {
        bslib::update_tooltip(session$ns("title"))
      })
    }
  )
}


#' User interface of MGF parameters
#'
#' @param id Namespace identifier.
#' @param sites Sites of amino-acid residues.
#' @param positions Positions of modifications.
remove_unimodUI <- function(id,
                            sites = c("A", "C", "D", "E", "F", "G", "H", "I", "K",
                                      "L", "M", "N", "P", "Q", "R", "S", "T", "W",
                                      "Y", "V", "U", "B", "X", "Z", "N-term",
                                      "C-term"),
                            positions = c("Anywhere", "Any N-term", "Protein N-term",
                                          "Any C-term", "Protein C-term"))
{
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        textInput(NS(id, "title"), label = "Title", placeholder = "TMT10plexNterm+Gln->pyro-Glu"),
        selectInput(NS(id, "site"), "AA site", sites, selected = NULL, multiple = FALSE),
        selectInput(NS(id, "position"), "Modification position", positions, selected = NULL, multiple = FALSE),
      ),
      fluidRow(
        actionButton(NS(id, "load"), "Load", class = "btn-success", title = "Loads all Unimod entries."),
        actionButton(NS(id, "remove"), "Remove", class = "btn-danger", title = "Removes a Unimod entry"),
      ),
      textOutput(NS(id, "msg")),
      width = 4
    ),
    mainPanel(
      DT::DTOutput(NS(id, "table")),
      width = 8
    )
  )
}


#' Server-side processing of MGF parameters
#'
#' @param id Namespace identifier.
remove_unimodServer <- function(id)
{
  moduleServer(
    id,
    function(input, output, session) {
      umods <- reactiveVal()

      observeEvent(input$table_rows_selected, {
        if (!is.null(i <- input$table_rows_selected)) {
          # stopifnot(length(i) == 1L)
          row <- umods()[i, , drop = FALSE]
          updateTextInput(session, "title", NULL, value = row$title)
          updateTextInput(session, "site", NULL, value = row$site)
          updateTextInput(session, "position", NULL, value = row$position)
        }
      })

      observeEvent(input$remove, {
        mzion::remove_unimod(header      = c(title       = input$title),
                             specificity = c(site        = input$site,
                                             position    = input$position))

        output$msg <- renderText(paste0("Unimod removed: ", input$title, "."))
        # umods(mzion::table_unimods())
        # output$table <- DT::renderDT(umods(), filter = "bottom", selection = "single", options = list(pageLength = 5))
      })

      observeEvent(input$load, {
        umods(mzion::table_unimods())
        output$table <- DT::renderDT(umods(), filter = "bottom",
                                     selection = "single",
                                     options = list(pageLength = 5))
      })
    }
  )
}


#' User interface of MGF parameters
#'
#' Find (neutral losses of) a Unimod.
#'
#' @param id Namespace identifier.
find_unimodUI <- function(id)
{
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        textInput(NS(id, "unimod"), label = "Unimod",
                  placeholder = "Phospho (S)"),
        actionButton(NS(id, "load"), "Load", class = "btn-success", title = "Loads all Unimod entries."),
        actionButton(NS(id, "find"), "Find", class = "btn-warning",
                     title = "Finds a Unimod and applicable neutral losses"),
        h2(""),
        tableOutput(NS(id, "umod"))
      ),
      textOutput(NS(id, "msg")),
      width = 4
    ),
    mainPanel(
      DT::DTOutput(NS(id, "table")),
      width = 8
    )
  )
}


#' Server-side processing of MGF parameters
#'
#' @param id Namespace identifier.
find_unimodServer <- function(id)
{
  moduleServer(
    id,
    function(input, output, session) {
      umods <- reactiveVal()

      observeEvent(input$table_rows_selected, {
        if (!is.null(i <- input$table_rows_selected)) {
          # stopifnot(length(i) == 1L)
          row <- umods()[i, , drop = FALSE]
          updateTextInput(session, "unimod", NULL, value = row$modification)
        }
      })

      umod <- eventReactive(input$find, {
        x <- tryCatch(mzion::find_unimod(unimod = input$unimod),
                 error = function(e) NULL)

        if (is.null(x)) {
          output$msg <- renderText(paste0("Unimod not found: ", input$unimod, "."))
        }
        else {
          x <- suppressWarnings(data.frame(x)) # uneven lengths of x
          x$monomass <- as.character(x$monomass)
          x$nl <- as.character(x$nl)
          x <- dplyr::rename(x, mono_mass = monomass, site = position_site)
        }

        x
      })
      output$umod <- renderTable(umod())

      observeEvent(input$load, {
        umods(mzion::table_unimods() |>
                dplyr::mutate(modification = paste0(title, " (", position, " = ", site, ")")))

        output$table <- DT::renderDT(umods(), filter = "bottom",
                                     selection = "single",
                                     options = list(pageLength = 5))
      })
    }
  )
}


