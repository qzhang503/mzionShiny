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
  ns <- NS(id)

  tagList(
    fluidRow(
      column(4, textInput(NS(id, "title"), label = "Title (do-not-use-space)",
                           placeholder = "TMT10plexNterm+Gln->pyro-Glu"),),
      column(4, textInput(NS(id, "full_name"), label = "Description",
                           placeholder = "Additive N-term TMT10 and Gln->pyro-Glu"),),
      column(4, textInput(NS(id, "composition"), label = "Composition",
                           placeholder = "H(17) C(8) 13C(4) 15N O(2)"),),
    ),
    fluidRow(
      column(4, selectInput(NS(id, "site"), "Site", sites, selected = NULL, multiple = FALSE)),
      column(4, selectInput(NS(id, "position"), "Position", positions, selected = NULL, multiple = FALSE)),
      column(4, textInput(NS(id, "neuloss"), label = "Neutral loss",
                          placeholder = "H(4) C O S"),),
    ),
    fluidRow(
      column(4, actionButton(NS(id, "add"), "Add", class = "btn-success",
                             title = "Calculates the mass of a chemical formula"),),
    ),
    fluidRow(
      h2(""),
      column(12, dataTableOutput(NS(id, "umods")),),
      column(4, actionButton(NS(id, "reload"), "Reload", class = "btn-success"),),
    ),
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
      if (length(excls <- which(names(umods) %in% c("modification", "short"))))
        umods <- umods[, -excls, drop = FALSE]

      umods <- umods |>
        dplyr::rename(Title = title, Description = full_name, Site = site,
                      Position = position, Mono_mass = mono_masses,
                      Composition = composition)

      output$umods <- renderDataTable(umods, options = list(pageLength = 5))

      observeEvent(input$add, {
        m0 <- mzion::calc_unimod_compmass(input$composition)
        m1 <- mzion::calc_unimod_compmass(input$neuloss)

        if (input$title != "") {
          x <- mzion::add_unimod(header      = c(title       = input$title,
                                                 full_name   = input$full_name),
                                 specificity = c(site        = input$site,
                                                 position    = input$position),
                                 delta       = c(mono_mass   = as.character(m0$mono_mass),
                                                 avge_mass   = as.character(m0$avge_mass),
                                                 composition = input$composition),
                                 neuloss     = c(mono_mass   = as.character(m1$mono_mass),
                                                 avge_mass   = as.character(m1$avge_mass),
                                                 composition = input$neuloss))
        }
      })

      observeEvent(input$reload, {
        umods <- mzion::table_unimods()
        output$umods <- renderDataTable(umods, options = list(pageLength = 5))
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
  ns <- NS(id)

  tagList(
    fluidRow(
      column(4, textInput(NS(id, "title"), label = "Title",
                           placeholder = "TMT10plexNterm+Gln->pyro-Glu"),),
      column(4, selectInput(NS(id, "site"), "Site", sites, selected = NULL, multiple = FALSE)),
      column(4, selectInput(NS(id, "position"), "Position", positions, selected = NULL, multiple = FALSE)),

    ),
    fluidRow(
      column(4, actionButton(NS(id, "remove"), "Remove", class = "btn-danger",
                              title = "Calculates the mass of a chemical formula"),),
    ),
    fluidRow(
      h2(""),
      dataTableOutput(NS(id, "umods")),
      column(12, actionButton(NS(id, "reload"), "Reload", class = "btn-success"),),
    ),
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
      if (length(excls <- which(names(umods) %in% c("modification", "short"))))
        umods <- umods[, -excls, drop = FALSE]

      umods <- umods |>
        dplyr::rename(Title = title, Description = full_name, Site = site,
                      Position = position, Mono_mass = mono_masses,
                      Composition = composition)

      output$umods <- renderDataTable(umods, options = list(pageLength = 5))

      observeEvent(input$remove, {
        if (input$title != "") {
          x <- mzion::remove_unimod(header      = c(title       = input$title),
                                    specificity = c(site        = input$site,
                                                    position    = input$position))
        }
      })

      observeEvent(input$reload, {
        umods <- mzion::table_unimods()
        output$umods <- renderDataTable(umods, options = list(pageLength = 5))
      })
    }
  )
}


#' User interface of MGF parameters
#'
#' @param id Namespace identifier.
find_unimodUI <- function(id)
{
  ns <- NS(id)

  tagList(
    fluidRow(
      column(4, textInput(NS(id, "unimod"), label = "Unimod",
                          placeholder = "Phospho (S)"),),
    ),
    fluidRow(
      column(4, actionButton(NS(id, "find"), "Find", class = "btn-success",
                             title = "Finds a Unimod and applicable neutral losses"),),
    ),
    fluidRow(
      h2(""),
      column(12, tableOutput(NS(id, "umod"))),
    ),
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
      umod <- eventReactive(input$find, {
        if (input$unimod != "") {
          x <- mzion::find_unimod(unimod = input$unimod)
          x <- suppressWarnings(data.frame(x)) # uneven lengths of x
          x$monomass <- as.character(x$monomass)
          x$nl <- as.character(x$nl)
          x
        }
      })

      output$umod <- renderTable(umod())
    }
  )
}

