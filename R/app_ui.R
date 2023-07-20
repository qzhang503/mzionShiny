# tags$style("#Bookmarking .fa-link {color:#E87722}")

library(mzion)
library(shinyFiles)
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

    # Your application UI logic
    h1("Mzion"),
    titlePanel("DDA"),

    fluidPage(
      ###
      shinyjs::useShinyjs(),
      ###
      navlistPanel(
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
          # textOutput("calib_ms1mass"),
          # textOutput("ppm_ms1calib"),
          # textOutput("topn_ms2ion_cuts_new"),
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
          "Search parameters",
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
          "Quantitation",
          quantUI("quant"),
          # textOutput("quant"),
          # textOutput("ppm_reporters"),
          # textOutput("tmt_reporter_lower"),
          # textOutput("tmt_reporter_upper"),
        ),
        # tabPanel(
        #   "Foo",
        #   outpath_UI("db"),
        # ),
      ),
      fluidRow(
        shinyFiles::shinySaveButton("savepars", "Save parameters", "Save file",
                                    filetype = list(text = "pars"), viewtype = "icon"),
        shinyFiles::shinyFilesButton("loadpars", "Load parameters", "Please select a file",
                                     filetype = list(text = "pars"), multiple = FALSE, viewtype = "detail"),
        # bookmarkButton(
        #   label = "Bookmarking",
        #   title = "Save this application's state and get a URL for sharing or re-analysis."),
      ),
      h3(""),
      fluidRow(
        actionButton("submit", "Submit", class = "btn-lg btn-success"),
        actionButton("cancel", "Cancel", class = "btn-lg btn-danger"),
      ),
    )
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

