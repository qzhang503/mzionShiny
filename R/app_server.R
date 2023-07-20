#' Extract numeric values
#'
#' @param text A text string.
extract_nums <- function(text) {
  # if (text == "") return(0)
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
  n_fasta <- res_fasta$n
  # output$fasta <- renderPrint({ paste0("fasta: ", fasta()) })
  # output$acc_type <- renderPrint({ paste0("acc_type: ", acc_type()) })
  # output$acc_pattern <- renderPrint({ paste0("acc_pattern: ", acc_pattern()) })
  # output$use_ms1_cache <- renderPrint({ paste0("use_ms1_cache: ", use_ms1_cache()) })

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
  use_short_mods <- mods$use_short_mods # for re-loading
  use_ms1notches = mods$use_ms1notches
  use_ms1neulosses = mods$use_ms1neulosses
  isolabs = mods$isolabs
  # output$fixedmods <- renderPrint(paste0("fixedmods: ", c(fixedmods()) ))
  # output$varmods <- renderPrint(paste0("varmods: ", c(varmods()) ))
  # output$locmods <- renderPrint(paste0("locmods: ", c(locmods()) ))
  # output$rm_dup_term_anywhere <- renderPrint(paste0("rm_dup_term_anywhere: ", rm_dup_term_anywhere() ))
  # output$n_13c <- renderPrint(paste0("n_13c: ", c(n_13c_new())))
  # output$n_13c <- renderPrint(paste0("n_13c: ", c(n_13c())))
  # output$ms1_notches <- renderPrint(paste0("ms1_notches: ", c(ms1_notches()) ))
  # output$maxn_neulosses_fnl <- renderPrint(paste0("maxn_neulosses_fnl: ", c(maxn_neulosses_fnl()) ))
  # output$maxn_neulosses_vnl <- renderPrint(paste0("maxn_neulosses_vnl: ", c(maxn_neulosses_vnl()) ))
  # output$ms1_neulosses <- renderPrint(paste0("ms1_neulosses: ", c(ms1_neulosses()) ))
  # output$fixedlabs <- renderPrint(paste0("fixedlabs: ", c(fixedlabs()) ))
  # output$varlabs <- renderPrint(paste0("varlabs: ", c(varlabs()) ))
  # output$nm_mod_motifs <- renderPrint(paste0("nm_mod_motifs: ", nm_mod_motifs() )) # single value currently
  # output$val_mod_motifs <- renderPrint(paste0("val_mod_motifs: ", c(val_mod_motifs()) ))
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
  max_scan_num <- mgfs$max_scan_num # NA
  min_ret_time <- mgfs$min_ret_time
  max_ret_time <- mgfs$max_ret_time # NA
  topn_ms2ions <- mgfs$topn_ms2ions
  exclude_reporter_region <- mgfs$exclude_reporter_region
  calib_ms1mass <- mgfs$calib_ms1mass
  ppm_ms1calib <- mgfs$ppm_ms1calib
  cut_ms2ions <- mgfs$cut_ms2ions
  topn_ms2ion_cuts <- mgfs$topn_ms2ion_cuts # ""
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
  max_protscores_co <- fdrs$max_protscores_co # NA
  max_protnpep_co <- fdrs$max_protnpep_co
  fdr_group <- fdrs$fdr_group
  nes_fdr_group <- fdrs$nes_fdr_group
  topn_mods_per_seq <- fdrs$topn_mods_per_seq
  topn_seqs_per_query <- fdrs$topn_seqs_per_query
  svm_reproc <- fdrs$svm_reproc
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

  ## Mzion compatibles
  max_protscores_co_new <- reactive({ if (is.na(max_protscores_co())) Inf else max_protscores_co() })
  max_scan_num_new <- reactive({ if (is.na(max_scan_num())) .Machine$integer.max else max_scan_num() })
  max_ret_time_new <- reactive({ if (is.na(max_ret_time())) Inf else max_ret_time() })
  topn_ms2ion_cuts_new <- reactive({ if (topn_ms2ion_cuts() == "") NA else topn_ms2ion_cuts() })
  n_13c_new <- reactive(seq(n_13c()[1], n_13c()[length(n_13c())])) # c(1, 1) -> c(1)
  noenzyme_maxn_new <- reactive({ if (is.null(noenzyme_maxn())) 0L else noenzyme_maxn() })

  ms1_notches_num <- reactive({
    nums <- extract_nums(ms1_notches())
    nums <- if (length(nums) && is.numeric(nums)) nums else 0
  })

  ## mzion::matchMS parameters
  pars <- reactive({
    list(
      out_path = out_path(),
      mgf_path = mgf_path(),
      fasta = fasta(),
      acc_type = acc_type(),
      acc_pattern = acc_pattern(), # NULL
      fixedmods = fixedmods(), # NULL if empty
      varmods = varmods(), # NULL if empty
      rm_dup_term_anywhere = rm_dup_term_anywhere(),
      ms1_neulosses = ms1_neulosses(), # NULL if empty
      maxn_neulosses_fnl = maxn_neulosses_fnl(),
      maxn_neulosses_vnl = maxn_neulosses_vnl(),
      fixedlabs = fixedlabs(), # NULL
      varlabs = varlabs(), # NULL
      locmods = locmods(), # NULL if empty
      # mod_motifs = mod_motifs(), # NULL
      enzyme = enzyme(),
      custom_enzyme = custom_enzyme(), # "" -> matchMS: c(Cterm = NULL, Nterm = NULL)
      nes_fdr_group = nes_fdr_group(),
      noenzyme_maxn = noenzyme_maxn_new(), # NULL -> matchMS: 0L
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

      svm_reproc = svm_reproc()
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
  })

  ## Save parameters
  volume0 <- c(Home = fs::path_home_r(), Home_win = fs::path_home(), shinyFiles::getVolumes()())
  volumes <- reactive(c(Project = out_path(), volume0))

  observeEvent(input$savepars, {
    # back to Shiny formats for reloading
    pars0 <- pars()
    pars0[["max_protscores_co"]] <- max_protscores_co() # NA
    pars0[["max_scan_num"]] <- max_scan_num() # NA
    pars0[["max_ret_time"]] <- max_ret_time() # NA
    pars0[["topn_ms2ion_cuts"]] <- topn_ms2ion_cuts() # ""
    pars0[["n_13c"]] <- n_13c() # c(1, 1)
    pars0[["ms1_notches"]] <- ms1_notches() # ""
    pars0[["noenzyme_maxn"]] <- noenzyme_maxn()

    if (out_path() == "") {
      print("Set up a working direcotry first")
    }
    else {
      shinyFiles::shinyFileSave(input, "savepars", roots = volumes(), session = session,
                                restrictions = system.file(package = "base"))
      fileinfo <- shinyFiles::parseSavePath(volumes(), input$savepars)

      if (nrow(fileinfo))
        saveRDS(pars0, fileinfo$datapath)
    }
  })

  ## Reload parameters
  # use observeEvent(input$loadpars) if no need of cached_pars
  cached_pars <- eventReactive(input$loadpars, {
    shinyFiles::shinyFileChoose(input, "loadpars", roots = volumes(), session = session)
    fileinfo <- shinyFiles::parseFilePaths(volumes(), input$loadpars)
    if (nrow(fileinfo)) readRDS(fileinfo$datapath) else NULL
  })

  observeEvent(cached_pars(), {
    if (!is.null(cached_pars())) {
      ## FASTA
      cache_fas <- cached_pars()$fasta
      cache_acctypes <- cached_pars()$acc_type
      cache_accpats <- cached_pars()$acc_pattern
      if (!length(cache_accpats)) cache_accpats <- vector("list", length(cache_fas))

      updateCheckboxInput(session, NS("fasta", "use_ms1_cache"), "Use cache",
                          value = cached_pars()$use_ms1_cache)
      # updateNumericInput(session, NS("fasta", "n"), "Number of databases", value = length(cache_fas))

      idxes <- seq_along(cache_fas)
      fas_ids <- paste0("fasta_", idxes)
      acc_ids <- paste0("acc_type_", idxes)
      pat_ids <- paste0("acc_pattern_", idxes)
      # use_cuspat_ids <- paste0("use_custom_pat_", idxes)
      accessions <- c("uniprot_acc", "uniprot_id", "refseq_acc", "other")

      for (i in idxes) {
        updateTextInput(session, NS("fasta", fas_ids[[i]]), paste("Database", i), value = cache_fas[[i]])
        updateRadioButtons(session, NS("fasta", acc_ids[[i]]), "Accession type",
                           accessions, selected = cache_acctypes[[i]])
        updateTextInput(session, NS("fasta", pat_ids[[i]]), "Accession pattern (optional)",
                        value = cache_accpats[[i]], placeholder = "Regular expression for \"other\"")
      }

      ## MGF
      # if out_path changed -> cached_pars not found...
      updateTextInput(session, NS("mgf", "out_path"), "Output path", value = cached_pars()$out_path,
                      placeholder = file.path("~/Mzion/My_Project"))
      updateTextInput(session, NS("mgf", "mgf_path"), "MGF path", value = cached_pars()$mgf_path,
                      placeholder = "~/Mzion/My_project/mgf")
      updateTextInput(session, NS("mgf", ".path_cache"), "Default cache folder",
                      value = cached_pars()$.path_cache)
      updateNumericInput(session, NS("mgf", "min_ms1_charge"), "Min MS1 charge state",
                         value = cached_pars()$min_ms1_charge)
      updateNumericInput(session, NS("mgf", "max_ms1_charge"), "Max MS1 charge state",
                         value = cached_pars()$max_ms1_charge)
      updateNumericInput(session, NS("mgf", "min_ms2mass"), "Min MS2 mass", value = cached_pars()$min_ms2mass)
      updateNumericInput(session, NS("mgf", "max_ms2mass"), "Max MS2 mass", value = cached_pars()$max_ms2mass)
      updateNumericInput(session, NS("mgf", "min_scan_num"), "Min scan number", value = cached_pars()$min_scan_num)
      updateNumericInput(session, NS("mgf", "max_scan_num"), "Max scan number", value = cached_pars()$max_scan_num)
      updateNumericInput(session, NS("mgf", "min_ret_time"), "Min retention time", value = cached_pars()$min_ret_time)
      updateNumericInput(session, NS("mgf", "max_ret_time"), "Max retention time", value = cached_pars()$max_ret_time)
      updateNumericInput(session, NS("mgf", "topn_ms2ions"), "Top-N features", value = cached_pars()$topn_ms2ions)
      updateCheckboxInput(session, NS("mgf", "exclude_reporter_region"), "Exclude reporter region",
                          value = cached_pars()$exclude_reporter_region)
      updateCheckboxInput(session, NS("mgf", "calib_ms1mass"), "Mass calibration", value = cached_pars()$calib_ms1mass)
      updateNumericInput(session, NS("mgf", "ppm_ms1calib"), "Calibration mass tolerance (ppm)",
                         value = cached_pars()$ppm_ms1calib)
      updateCheckboxInput(session, NS("mgf", "cut_ms2ions"), "Cut MS2 by regions", cached_pars()$cut_ms2ions)
      updateTextInput(session, NS("mgf", "topn_ms2ion_cuts"), "Cuts (m/z = percent)",
                      value = cached_pars()$topn_ms2ion_cuts,
                      placeholder = "`1000` = 90, `1100` = 5, `4500` = 5")

      ## mod
      choices <- if (use_short_mods()) short_mods else umods$modification
      updateCheckboxInput(session, NS("mod", "use_short_mods"), "Short list",
                          value = cached_pars()$use_short_mods)
      updateSelectInput(session, NS("mod", "fixedmods"), "Fixed modifications",
                        choices, selected = cached_pars()$fixedmods)
      updateSelectInput(session, NS("mod", "varmods"), "Variable modifications",
                        choices, selected = cached_pars()$varmods)
      updateSelectInput(session, NS("mod", "locmods"), "Localizations",
                        choices, selected = cached_pars()$locmods)
      updateSliderInput(session, NS("mod", "n_13c"), "Numbers of 13C",
                        value = cached_pars()$n_13c, min = -1, max = 3)
      updateCheckboxInput(session, NS("mod", "rm_dup_term_anywhere"),
                          paste0("Remove the same-site combinations at both terminal and anywhere positions ",
                                 "(e.g., N-term Q and Anywhere Q)"),
                          value = cached_pars()$rm_dup_term_anywhere)
      # cached_pars()$use_ms1notches, use_ms1neulosses and isolabs are NULL
      updateCheckboxInput(session, NS("mod", "use_ms1notches"), "Precursor off-sets",
                          value = cached_pars()$use_ms1notches)
      updateCheckboxInput(session, NS("mod", "use_ms1neulosses"), "Precursor neutral losses",
                          value = cached_pars()$use_ms1neulosses)
      updateCheckboxInput(session, NS("mod", "isolabs"), "Isotope labels", value = cached_pars()$isolabs)
      updateCheckboxInput(session, NS("mod", "pepmotifs"), "Peptide motifs", value = cached_pars()$pepmotifs)
      updateTextInput(session, NS("mod", "ms1_notches"), "Values", value = cached_pars()$ms1_notches,
                      placeholder = "-97.976896, -79.96633")
      updateNumericInput(session, NS("mod", "maxn_neulosses_fnl"), "Max fixed NLs",
                         value = cached_pars()$maxn_neulosses_fnl, min = 1)
      updateNumericInput(session, NS("mod", "maxn_neulosses_vnl"), "Max variable NLs",
                         value = cached_pars()$maxn_neulosses_vnl, min = 1)
      updateSelectInput(session, NS("mod", "ms1_neulosses"), "Values",
                        choices, selected = cached_pars()$ms1_neulosses)
      updateSelectInput(session, NS("mod", "fixedlabs"), "Fixed labels",
                        umods_labs$modification, selected = cached_pars()$fixedlabs)
      updateSelectInput(session, NS("mod", "varlabs"), "Variable labels",
                        umods_labs$modification, selected = cached_pars()$varlabs)

      ## search
      updateNumericInput(session, NS("search", "min_len"), "Min peptide length",
                         value = cached_pars()$min_len, min = 1)
      updateNumericInput(session, NS("search", "max_len"), "Max peptide length",
                         value = cached_pars()$max_len, min = 1)
      updateNumericInput(session, NS("search", "max_miss"), "Max mis-cleavages",
                         value = cached_pars()$max_miss, min = 0)
      updateNumericInput(session, NS("search", "min_mass"), "Min precursor mass",
                         value = cached_pars()$min_mass, min = 1)
      updateNumericInput(session, NS("search", "max_mass"), "Max precursor mass",
                         value = cached_pars()$max_mass, min = 500)
      updateNumericInput(session, NS("search", "ppm_ms1"), "MS1 tolerance (ppm)",
                         value = cached_pars()$ppm_ms1, min = 1)
      updateNumericInput(session, NS("search", "maxn_vmods_setscombi"), "Max combinations in modification sets",
                         value = cached_pars()$maxn_vmods_setscombi, min = 1)
      updateNumericInput(session, NS("search", "maxn_vmods_per_pep"), "Max variable modifications",
                         value = cached_pars()$maxn_vmods_per_pep, min = 1)
      updateNumericInput(session, NS("search", "maxn_sites_per_vmod"), "Max variable modifications per site",
                         value = cached_pars()$maxn_sites_per_vmod, min = 1)
      updateNumericInput(session, NS("search", "maxn_vmods_sitescombi_per_pep"), "Max position permutations",
                         value = cached_pars()$maxn_vmods_sitescombi_per_pep, min = 1)
      updateNumericInput(session, NS("search", "maxn_fnl_per_seq"), "Max neutral losses (fixed)",
                         value = cached_pars()$maxn_fnl_per_seq, min = 1)
      updateNumericInput(session, NS("search", "maxn_vnl_per_seq"), "Max neutral losses (variable)",
                         value = cached_pars()$maxn_vnl_per_seq, min = 1)
      updateNumericInput(session, NS("search", "min_ms2mass"), "Min MS2 mass", value = cached_pars()$min_ms2mass)
      updateNumericInput(session, NS("search", "max_ms2mass"), "Max MS2 mass", value = cached_pars()$max_ms2mass)
      updateNumericInput(session, NS("search", "ppm_ms2"), "MS2 tolerance (ppm)", value = cached_pars()$ppm_ms2)
      updateNumericInput(session, NS("search", "minn_ms2"), "Min MS2 features", value = cached_pars()$minn_ms2)
      updateSelectInput(session, NS("search", "type_ms2ions"), "MS2 fragments",
                        c("by", "ax", "cz"), selected = cached_pars()$type_ms2ions)
      updateSelectInput(session, NS("search", "enzyme"), "Enzyme", enzymes, selected = cached_pars()$enzyme)
      updateCheckboxInput(session, NS("search", "customenzyme"), "Custom enzyme", value = cached_pars()$customenzyme)
      # no update? the same as fasta need to load the parameter file twice
      updateNumericInput(session, NS("search", "noenzyme_maxn"),
                         paste0("Max number of peptide lengths for a section ",
                                "(e.g., lengths 7-21, 22-36, ... at a value of 15)"),
                         value = cached_pars()$noenzyme_maxn)
      updateTextInput(session, NS("search", "custom_enzyme"), "Specificity (in regular expression)",
                      value = cached_pars()$custom_enzyme, placeholder = "Cterm = NULL, Nterm = NULL")

      ## FDR
      updateNumericInput(session, NS("fdr", "target_fdr"), "Target FDR", value = cached_pars()$target_fdr, min = 1E-10)
      updateSelectInput(session,  NS("fdr", "fdr_type"), "FDR type", choices = c("protein", "peptide", "psm"),
                        selected = cached_pars()$fdr_type)
      updateNumericInput(session, NS("fdr", "max_pepscores_co"), "Score threshold to warrent a PSM significance",
                         value = cached_pars()$max_pepscores_co, min = 0)
      updateNumericInput(session, NS("fdr", "min_pepscores_co"), "Score threshold to discard a PSM significance",
                         value = cached_pars()$min_pepscores_co, min = 0)
      updateNumericInput(session, NS("fdr", "max_protscores_co"), "Upper limit in protein score cut-offs",
                         value = cached_pars()$max_protscores_co, min = 0)
      updateNumericInput(session, NS("fdr", "max_protnpep_co"), "Max number of peptides to warrant a protein significance",
                         value = cached_pars()$max_protnpep_co, min = 1)
      updateSelectInput(session, NS("fdr", "fdr_group"), "FDR group", fdr_groups,
                        selected = cached_pars()$fdr_group)
      updateSelectInput(session, NS("fdr", "nes_fdr_group"), "FDR group (NES)", nes_fdr_groups,
                        selected = cached_pars()$nes_fdr_group)
      updateNumericInput(session, NS("fdr", "topn_mods_per_seq"), "Top-N matches per sequence",
                         value = cached_pars()$topn_mods_per_seq, min = 1)
      updateNumericInput(session, NS("fdr", "topn_seqs_per_query"), "Top-N sequences per query",
                         value = cached_pars()$topn_seqs_per_query, min = 1)
      updateCheckboxInput(session, NS("fdr", "svm_reproc"), "Percolator", value = cached_pars()$svm_reproc)

      ## quantitation
      updateSelectInput(session, NS("quant", "quant"), "Quantitation",
                        choices = c("none", "tmt6", "tmt10", "tmt11", "tmt16", "tmt18"),
                        selected = cached_pars()$quant)
      updateNumericInput(session, NS("quant", "ppm_reporters"), "Reporter tolerance (ppm)",
                         value = cached_pars()$ppm_reporters, min = 1)
      updateNumericInput(session, NS("quant", "tmt_reporter_lower"), "Reporter lower bound",
                         value = cached_pars()$tmt_reporter_lower, min = 0)
      updateNumericInput(session, NS("quant", "tmt_reporter_upper"), "Reporter upper bound",
                         value = cached_pars()$tmt_reporter_upper, min = 0)
    }
  })

  ## Submit
  btn_submit <- eventReactive(input$submit, {
    shinyjs::toggleState("submit")

    sink(file.path(out_path(), "mzpars.txt"))
    print(pars())
    sink()

    if (!file.exists(fi_pars <- file.path(out_path(), "mz.pars")))
      saveRDS(fi_pars)

    do.call(mzion::matchMS, pars())
    shinyjs::toggleState("submit")
  })
  observe(btn_submit())

  btn_cancel <- observeEvent(input$cancel, ignoreInit = TRUE, {
    stop("Process interrupted.", call. = FALSE)
  })
}


