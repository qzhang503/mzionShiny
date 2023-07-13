#' Extract numeric values
#'
#' @param text A text string.
extract_nums <- function(text) {
  text  <- gsub(",\\s*", ",", text)
  split <- strsplit(text, ",", fixed = FALSE)[[1]]
  as.numeric(split)
}


#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session)
{
  ## FASTA
  res_fasta <- fastaServer("fasta")
  fasta <- res_fasta$fasta
  acc_type <- res_fasta$acc_type
  acc_pattern <- res_fasta$acc_pattern
  use_ms1_cache <- res_fasta$use_ms1_cache

  # output$fasta <- renderPrint({ paste0("fasta: ", fasta()) })
  # output$acc_type <- renderPrint({ paste0("acc_type: ", acc_type()) })
  # output$acc_pattern <- renderPrint({ paste0("acc_pattern: ", acc_pattern()) })
  # output$use_ms1_cache <- renderPrint({ paste0("use_ms1_cache: ", use_ms1_cache()) })

  ###
  # files <- res_fasta$files
  # output$files <- renderTable(files())
  ###

  ## modifications
  mods <- modServer("mod")
  fixedmods <- mods$fixedmods
  varmods <- mods$varmods
  locmods <- mods$locmods
  rm_dup_term_anywhere <- mods$rm_dup_term_anywhere
  n_13c <- mods$n_13c
  ms1_notches <- mods$ms1_notches
  maxn_neulosses_fnl <- mods$maxn_neulosses_fnl
  maxn_neulosses_vnl <- mods$maxn_neulosses_vnl
  ms1_neulosses <- mods$ms1_neulosses
  # nm_mod_motifs <- mods$nm_mod_motifs
  # val_mod_motifs <- mods$val_mod_motifs
  fixedlabs <- mods$fixedlabs
  varlabs <- mods$varlabs

  # output$fixedmods <- renderPrint(paste0("fixedmods: ", c(fixedmods()) ))
  # output$varmods <- renderPrint(paste0("varmods: ", c(varmods()) ))
  # output$locmods <- renderPrint(paste0("locmods: ", c(locmods()) ))
  # output$rm_dup_term_anywhere <- renderPrint(paste0("rm_dup_term_anywhere: ", rm_dup_term_anywhere() ))
  # output$n_13c <- renderPrint(paste0("n_13c: ", c(n_13c_new())))
  # output$ms1_notches <- renderPrint(paste0("ms1_notches: ", c(ms1_notches()) ))
  # output$maxn_neulosses_fnl <- renderPrint(paste0("maxn_neulosses_fnl: ", c(maxn_neulosses_fnl()) ))
  # output$maxn_neulosses_vnl <- renderPrint(paste0("maxn_neulosses_vnl: ", c(maxn_neulosses_vnl()) ))
  # output$ms1_neulosses <- renderPrint(paste0("ms1_neulosses: ", c(ms1_neulosses()) ))
  # output$fixedlabs <- renderPrint(paste0("fixedlabs: ", c(fixedlabs()) ))
  # output$varlabs <- renderPrint(paste0("varlabs: ", c(varlabs()) ))
  # output$nm_mod_motifs <- renderPrint(paste0("nm_mod_motifs: ", nm_mod_motifs() )) # single value currently
  # output$val_mod_motifs <- renderPrint(paste0("val_mod_motifs: ", c(val_mod_motifs()) ))

  n_13c_new <- reactive(seq(n_13c()[1], n_13c()[2]))
  ms1_notches_num <- reactive(extract_nums(ms1_notches()))
  # output$mod_motifs <- renderText({
  #   vals <- mods$val_mod_motifs
  #   names(vals) <- mods$nm_mod_motifs
  #   vals
  # })

  ## mgfs
  mgfs <- mgfServer("mgf")
  out_path <- mgfs$out_path
  .path_cache <- mgfs$.path_cache
  mgf_path <- mgfs$mgf_path
  min_ms1_charge <- mgfs$min_ms1_charge
  max_ms1_charge <- mgfs$max_ms1_charge
  min_ms2mass <- mgfs$min_ms2mass
  max_ms2mass <- mgfs$max_ms2mass
  min_scan_num <- mgfs$min_scan_num
  max_scan_num <- mgfs$max_scan_num
  min_ret_time <- mgfs$min_ret_time
  max_ret_time <- mgfs$max_ret_time
  topn_ms2ions <- mgfs$topn_ms2ions
  exclude_reporter_region <- mgfs$exclude_reporter_region
  calib_ms1mass <- mgfs$calib_ms1mass
  ppm_ms1calib <- mgfs$ppm_ms1calib
  topn_ms2ion_cuts <- mgfs$topn_ms2ion_cuts

  max_scan_num_new <- reactive({ if (is.na(max_scan_num())) Inf else max_scan_num() })
  max_ret_time_new <- reactive({ if (is.na(max_ret_time())) Inf else max_ret_time() })
  topn_ms2ion_cuts_new <- reactive({ if (topn_ms2ion_cuts() == "") NA else topn_ms2ion_cuts() })

  # output$out_path <- renderPrint(paste0("out_path: ", out_path() ))
  # output$mgf_path <- renderPrint(paste0("mgf_path: ", mgf_path() ))
  # output$.path_cache <- renderPrint(paste0(".path_cache: ", .path_cache() ))
  # output$min_ms1_charge <-renderPrint(paste0("min_ms1_charge: ", min_ms1_charge() ))
  # output$max_ms1_charge <-renderPrint(paste0("max_ms1_charge: ", max_ms1_charge() ))
  # output$min_ms2mass <-renderPrint(paste0("min_ms2mass: ", min_ms2mass() ))
  # output$max_ms2mass <-renderPrint(paste0("max_ms2mass: ", max_ms2mass() ))
  # output$min_scan_num <-renderPrint(paste0("min_scan_num: ", min_scan_num() ))
  # output$min_ret_time <-renderPrint(paste0("min_ret_time: ", min_ret_time() ))
  # output$topn_ms2ions <-renderPrint(paste0("topn_ms2ions: ", topn_ms2ions() ))
  # output$exclude_reporter_region <-renderPrint(paste0("exclude_reporter_region: ", exclude_reporter_region() ))
  # output$calib_ms1mass <-renderPrint(paste0("calib_ms1mass: ", calib_ms1mass() ))
  # output$ppm_ms1calib <-renderPrint(paste0("ppm_ms1calib: ", ppm_ms1calib() ))
  # output$max_scan_num_new <-renderPrint(paste0("max_scan_num: ", max_scan_num_new() ))
  # output$max_ret_time_new <-renderPrint(paste0("max_ret_time: ", max_ret_time_new() ))
  # output$topn_ms2ion_cuts_new <-renderPrint(paste0("topn_ms2ion_cuts: ", topn_ms2ion_cuts_new() ))

  ## searches
  searches <- searchServer("search")
  min_len <- searches$min_len
  max_len <- searches$max_len
  max_miss <- searches$max_miss
  min_mass <- searches$min_mass
  max_mass <- searches$max_mass
  ppm_ms1 <- searches$ppm_ms1
  maxn_vmods_setscombi <- searches$maxn_vmods_setscombi
  maxn_vmods_per_pep <- searches$maxn_vmods_per_pep
  maxn_sites_per_vmod <- searches$maxn_sites_per_vmod
  maxn_vmods_sitescombi_per_pep <- searches$maxn_vmods_sitescombi_per_pep
  maxn_fnl_per_seq <- searches$maxn_fnl_per_seq
  maxn_vnl_per_seq <- searches$maxn_vnl_per_seq
  min_ms2mass <- searches$min_ms2mass
  max_ms2mass <- searches$max_ms2mass
  ppm_ms2 <- searches$ppm_ms2
  minn_ms2 <- searches$minn_ms2
  type_ms2ions <- searches$type_ms2ions
  enzyme <- searches$enzyme
  noenzyme_maxn <- searches$noenzyme_maxn
  custom_enzyme <- searches$custom_enzyme

  # output$min_len <- renderPrint(paste0("min_len: ", min_len() ))
  # output$max_len <- renderPrint(paste0("max_len: ", max_len() ))
  # output$max_miss <- renderPrint(paste0("max_miss: ", max_miss() ))
  # output$min_mass <- renderPrint(paste0("min_mass: ", min_mass() ))
  # output$max_mass <- renderPrint(paste0("max_mass: ", max_mass() ))
  # output$ppm_ms1 <- renderPrint(paste0("ppm_ms1: ", ppm_ms1() ))
  # output$maxn_vmods_setscombi <- renderPrint(paste0("maxn_vmods_setscombi: ", maxn_vmods_setscombi() ))
  # output$maxn_vmods_per_pep <- renderPrint(paste0("maxn_vmods_per_pep: ", maxn_vmods_per_pep() ))
  # output$maxn_sites_per_vmod <- renderPrint(paste0("maxn_sites_per_vmod: ", maxn_sites_per_vmod() ))
  # output$maxn_vmods_sitescombi_per_pep <- renderPrint(paste0("maxn_vmods_sitescombi_per_pep: ", maxn_vmods_sitescombi_per_pep() ))
  # output$maxn_fnl_per_seq <- renderPrint(paste0("maxn_fnl_per_seq: ", maxn_fnl_per_seq() ))
  # output$maxn_vnl_per_seq <- renderPrint(paste0("maxn_vnl_per_seq: ", maxn_vnl_per_seq() ))
  # output$min_ms2mass <- renderPrint(paste0("min_ms2mass: ", min_ms2mass() ))
  # output$max_ms2mass <- renderPrint(paste0("max_ms2mass: ", max_ms2mass() ))
  # output$ppm_ms2 <- renderPrint(paste0("ppm_ms2: ", ppm_ms2() ))
  # output$minn_ms2 <- renderPrint(paste0("minn_ms2: ", minn_ms2() ))
  # output$type_ms2ions <- renderPrint(paste0("type_ms2ions: ", type_ms2ions() ))
  # output$enzyme <- renderPrint(paste0("enzyme: ", enzyme() ))
  # output$noenzyme_maxn <- renderPrint(paste0("noenzyme_maxn: ", noenzyme_maxn() ))
  # output$custom_enzyme <- renderPrint(paste0("custom_enzyme: ", custom_enzyme() ))

  ## FDR
  fdrs <- fdrServer("fdr")
  target_fdr <- fdrs$target_fdr
  fdr_type <- fdrs$fdr_type
  max_pepscores_co <- fdrs$max_pepscores_co
  min_pepscores_co <- fdrs$min_pepscores_co
  max_protscores_co <- fdrs$max_protscores_co
  max_protnpep_co <- fdrs$max_protnpep_co
  fdr_group <- fdrs$fdr_group
  nes_fdr_group <- fdrs$nes_fdr_group
  topn_mods_per_seq <- fdrs$topn_mods_per_seq
  topn_seqs_per_query <- fdrs$topn_seqs_per_query
  svm_reproc <- fdrs$svm_reproc

  max_protscores_co_new <- reactive({ if (is.na(max_protscores_co())) Inf else max_protscores_co() })

  # output$target_fdr <- renderPrint(paste0("target_fdr: ", target_fdr() ))
  # output$fdr_type <- renderPrint(paste0("fdr_type: ", fdr_type() ))
  # output$max_pepscores_co <- renderPrint(paste0("max_pepscores_co: ", max_pepscores_co() ))
  # output$min_pepscores_co <- renderPrint(paste0("min_pepscores_co: ", min_pepscores_co() ))
  # output$max_protscores_co_new <- renderPrint(paste0("max_protscores_co: ", max_protscores_co_new() ))
  # output$max_protnpep_co <- renderPrint(paste0("max_protnpep_co: ", max_protnpep_co() ))
  # output$fdr_group <- renderPrint(paste0("fdr_group: ", fdr_group() ))
  # output$nes_fdr_group <- renderPrint(paste0("nes_fdr_group: ", nes_fdr_group() ))
  # output$topn_mods_per_seq <- renderPrint(paste0("topn_mods_per_seq: ", topn_mods_per_seq() ))
  # output$topn_seqs_per_query <- renderPrint(paste0("topn_seqs_per_query: ", topn_seqs_per_query() ))
  # output$svm_reproc <- renderPrint(paste0("svm_reproc: ", svm_reproc() ))

  ## Quant
  quants <- quantServer("quant")
  quant <- quants$quant
  ppm_reporters <- quants$ppm_reporters
  tmt_reporter_lower <- quants$tmt_reporter_lower
  tmt_reporter_upper <- quants$tmt_reporter_upper

  # output$quant <- renderPrint(paste0("quant: ", quant() ))
  # output$ppm_reporters <- renderPrint(paste0("ppm_reporters: ", ppm_reporters() ))
  # output$tmt_reporter_lower <- renderPrint(paste0("tmt_reporter_lower: ", tmt_reporter_lower() ))
  # output$tmt_reporter_upper <- renderPrint(paste0("tmt_reporter_upper: ", tmt_reporter_upper() ))

  ## Foo
  # outpath_Server("db")

  ## Bookmarking
  # https://cran.r-project.org/web/packages/shinyjqui/vignettes/save-and-restore.html

  df <- eventReactive(input$submit, {
    updateActionButton(session, "submit", "Running", icon = icon("redo"))
    # saveRDS(pars, file.path(out_path(), "Calls", "shinypar.rds"))

    mzion::matchMS(
      out_path = out_path(),
      mgf_path = mgf_path(),
      fasta = fasta(),
      acc_type = acc_type(),
      acc_pattern = acc_pattern(), # NULL
      fixedmods = fixedmods(),
      varmods = varmods(),
      rm_dup_term_anywhere = rm_dup_term_anywhere(),
      ms1_neulosses = ms1_neulosses(), # NULL
      maxn_neulosses_fnl = maxn_neulosses_fnl(),
      maxn_neulosses_vnl = maxn_neulosses_vnl(),
      fixedlabs = fixedlabs(), # NULL
      varlabs = varlabs(), # NULL
      locmods = locmods(),
      # mod_motifs = mod_motifs(), # NULL
      enzyme = enzyme(),
      custom_enzyme = custom_enzyme(), # c(Cterm = NULL, Nterm = NULL)
      nes_fdr_group = nes_fdr_group(),
      noenzyme_maxn = noenzyme_maxn(),
      # maxn_fasta_seqs = 200000L,
      maxn_vmods_setscombi = maxn_vmods_setscombi(),
      maxn_vmods_per_pep = maxn_vmods_per_pep(),
      maxn_sites_per_vmod = maxn_sites_per_vmod(),
      maxn_fnl_per_seq = maxn_fnl_per_seq(),
      maxn_vnl_per_seq = maxn_vnl_per_seq(),
      maxn_vmods_sitescombi_per_pep = maxn_vmods_sitescombi_per_pep(),
      min_len = min_len(),
      max_len = max_len(),
      max_miss = max_miss(),
      min_mass = min_mass(),
      max_mass = max_mass(),
      ppm_ms1 = ppm_ms1(),
      n_13c = n_13c_new(),
      ms1_notches = ms1_notches_num(),
      # par_groups = NULL,
      # silac_mix = NULL,
      type_ms2ions = type_ms2ions(),
      min_ms2mass = min_ms2mass(),
      max_ms2mass = max_ms2mass(),
      minn_ms2 = minn_ms2(),
      ppm_ms2 = ppm_ms2(),
      tmt_reporter_lower = tmt_reporter_lower(),
      tmt_reporter_upper = tmt_reporter_upper(),
      exclude_reporter_region = exclude_reporter_region(),
      # index_mgf_ms2 = FALSE,
      ppm_reporters = ppm_reporters(),
      quant = quant(),
      target_fdr = target_fdr(),
      fdr_type = fdr_type(),
      fdr_group = fdr_group(),
      max_pepscores_co = max_pepscores_co(),
      min_pepscores_co = min_pepscores_co(),
      max_protscores_co = max_protscores_co_new(),
      max_protnpep_co = max_protnpep_co(),
      # method_prot_es_co = "median",
      # soft_secions = FALSE,
      topn_mods_per_seq = topn_mods_per_seq(),
      topn_seqs_per_query = topn_seqs_per_query(),
      # combine_tier_three = FALSE,
      # max_n_prots = 60000L,

      use_ms1_cache = use_ms1_cache(),
      .path_cache = .path_cache(),
      # .path_fasta = NULL,
      topn_ms2ions = topn_ms2ions(),
      topn_ms2ion_cuts = topn_ms2ion_cuts_new(), # NA
      min_ms1_charge = min_ms1_charge(),
      max_ms1_charge = max_ms1_charge(),
      min_scan_num = min_scan_num(),
      max_scan_num = max_scan_num_new(), # .Machine$integer.max
      min_ret_time = min_ret_time(),
      max_ret_time = max_ret_time_new(), # Inf
      calib_ms1mass = calib_ms1mass(),
      ppm_ms1calib = ppm_ms1calib(),
      # add_ms2theos = FALSE,
      # add_ms2theos2 = FALSE,
      # add_ms2moverzs = FALSE,
      # add_ms2ints = FALSE,

      svm_reproc = svm_reproc(),
      # svm_kernel = "radial",
      # svm_feats = c("pep_score", "pep_ret_range", "pep_delta", "pep_n_ms2",
      #               "pep_expect", "pep_exp_mz", "pep_exp_mr", "pep_tot_int",
      #               "pep_n_matches2", "pep_ms2_deltas_mean"),
      # svm_cv = TRUE,
      # svm_k = 3L,
      # svm_costs = c(0.1, 0.3, 1, 3, 10),
      # svm_def_cost = 1,
      # svm_iters = 10L,

      # by_modules = TRUE,
      # digits = 4L,
    )



    # updateActionButton(session, "submit", label = "Submit")
  })

  observeEvent(df(), {
    output$df <- if (is.data.frame(df())) renderTable(df()) else renderText(df())
  })

  # modelDialog() before empty folder...
}


