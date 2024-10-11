# tags$style("#Bookmarking .fa-link {color:#E87722}")

# library(mzion)
# library(shinyFiles)
# library(bslib)
# library(shinyjs)

#' The application user-interface
#'
#' @param request Request.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    shinyjs::useShinyjs(),

    # titlePanel("DDA"),

    navbarPage(
      # shinyjs::useShinyjs(),
      # theme = bslib::bs_theme(bootswatch = "cerulean"),
      # theme = bslib::bs_theme(preset = "shiny"),
      theme = bslib::bs_theme(),
      # title = a("Mzion", href = "https://github.com/qzhang503/mzion/"),
      title = "Mzion",
      id = "Mzion",

      tabPanel(
        "Peaks",
        mgfUI("mgf"),
        # textOutput("out_path"),
        # textOutput("mgf_path"),
        # textOutput(".path_cache"),
        # textOutput("min_ms1_charge"),
        # textOutput("max_ms1_charge"),
        # textOutput("min_ms2mass"),
        # textOutput("max_ms2mass"),
        # textOutput("min_scan_num"),
        # textOutput("max_scan_num_new"),
        # textOutput("min_ret_time"),
        # textOutput("max_ret_time_new"),
        # textOutput("topn_ms2ions"),
        # textOutput("exclude_reporter_region"),
        # textOutput("calib_masses"),
        # textOutput("ppm_ms1calib"),
        # textOutput("topn_ms2ion_cuts_new"),
        # textOutput("quant"),
        # textOutput("ppm_reporters"),
        # textOutput("tmt_reporter_lower"),
        # textOutput("tmt_reporter_upper"),
      ),
      tabPanel(
        "Databases",
        fastaUI("fasta"),
        # textOutput("fasta"),
        # textOutput("acc_type"),
        # textOutput("acc_pattern"),
        # textOutput("use_ms1_cache"),
        # tableOutput("files"),
        # textOutput("cached_fasta"),
        # textOutput("cached_acctype"),
        # textOutput("cached_accpat"),
      ),
      tabPanel(
        "Modifications",
        modUI("mod"),
        # textOutput("fixedmods"),
        # textOutput("varmods"),
        # textOutput("locmods"),
        # textOutput("rm_dup_term_anywhere"),
        # textOutput("n_13c_new"),
        # textOutput("n_13c"),
        # textOutput("ms1_notches"),
        # textOutput("maxn_neulosses_fnl"),
        # textOutput("maxn_neulosses_vnl"),
        # textOutput("ms1_neulosses"),
        # textOutput("fixedlabs"),
        # textOutput("varlabs"),
        # textOutput("nm_mod_motifs"),
        # textOutput("val_mod_motifs"),
      ),
      tabPanel(
        "Searches",
        searchUI("search"),
        # textOutput("min_len"),
        # textOutput("max_len"),
        # textOutput("max_miss"),
        # textOutput("min_mass"),
        # textOutput("max_mass"),
        # textOutput("ppm_ms1"),
        # textOutput("maxn_vmods_setscombi"),
        # textOutput("maxn_vmods_per_pep"),
        # textOutput("maxn_sites_per_vmod"),
        # textOutput("maxn_vmods_sitescombi_per_pep"),
        # textOutput("maxn_fnl_per_seq"),
        # textOutput("maxn_vnl_per_seq"),
        # textOutput("min_ms2mass"),
        # textOutput("max_ms2mass"),
        # textOutput("ppm_ms2"),
        # textOutput("minn_ms2"),
        # textOutput("type_ms2ions"),
        # textOutput("enzyme"),
        # textOutput("noenzyme_maxn"),
        # textOutput("custom_enzyme"),
      ),
      tabPanel(
        "FDR",
        fdrUI("fdr"),
        # textOutput("target_fdr"),
        # textOutput("fdr_type"),
        # textOutput("max_pepscores_co"),
        # textOutput("min_pepscores_co"),
        # textOutput("max_protscores_co_new"),
        # textOutput("max_protnpep_co"),
        # textOutput("fdr_group"),
        # textOutput("nes_fdr_group"),
        # textOutput("topn_mods_per_seq"),
        # textOutput("topn_seqs_per_query"),
        # textOutput("svm_reproc"),
      ),
      tabPanel(
        "Exec",
        fixedRow(
          align = "center",
          column(
            1,
            splitLayout(cellWidths = 140,
                        shinyFiles::shinyFilesButton("loadpars", "Reload", "Please select a file",
                                                     filetype = list(text = "pars"), multiple = FALSE,
                                                     viewtype = "detail",
                                                     style = "background-color:#43a2ca; width:120px;
                                                     font-size:100%; color:white") |>
                          bslib::tooltip("Reload search parameters or pre-compiled workflows (directory: Workflows)"),
                        shinyFiles::shinySaveButton("savepars", "Save", "Save file",
                                                    filetype = list(text = "pars"), viewtype = "icon",
                                                    style = "background-color:#43a2ca; width:120px;
                                                    font-size:100%; color:white") |>
                          bslib::tooltip("Save search parameters"),
                        actionButton("submit", "Submit", style = "background-color:#41ab5d; width:120px;
                             font-size:100%; color:white"),
                        actionButton("cancel", "Cancel", style = "background-color:#a50f15; width:120px;
                             font-size:100%; color:white"),
            )
          ),
        ),
      ),
      navbarMenu("Tools",
                 tabPanel(
                   "Add Unimod",
                   add_unimodUI("add_unimod"),
                 ),
                 tabPanel(
                   "Remove Unimod",
                   remove_unimodUI("remove_unimod"),
                 ),
                 tabPanel(
                   "Find Unimod (NL)",
                   find_unimodUI("find_unimod"),
                 ),
                 tabPanel(
                   "View PSM",
                   map_ms2UI("map_ms2ions"),
                 ),
                 icon = icon("wrench"),
      ),
      ###
      # footer = fixedRow(
      #   align = "center",
      #   br(),
      #   column(
      #     1,
      #     splitLayout(cellWidths = 70,
      #                 shinyFiles::shinyFilesButton("loadpars", "Reload", "Please select a file",
      #                                              filetype = list(text = "pars"), multiple = FALSE,
      #                                              viewtype = "detail",
      #                                              style = "background-color:#43a2ca; width:70px; font-size:100%; color:white"),
      #                 shinyFiles::shinySaveButton("savepars", "Save", "Save file",
      #                                             filetype = list(text = "pars"), viewtype = "icon",
      #                                             style = "background-color:#43a2ca; width:70px; font-size:100%; color:white"),
      #                 actionButton("submit", "Submit", style = "background-color:#41ab5d; width:70px;
      #                              font-size:100%; color:white"),
      #                 actionButton("cancel", "Cancel", style = "background-color:#a50f15; width:70px;
      #                              font-size:100%; color:white"),
      #     )
      #   ),
      # ),
      ###
    ),
  )
}


#' Add external Resources to the Application
#'
#' This function is internally used to add external resources inside the Shiny
#' application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "mzionShiny"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}

