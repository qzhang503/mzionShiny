short_mods <- c("Carbamidomethyl (Anywhere = C)",
                "Acetyl (Protein N-term = N-term)",
                "Acetyl (Anywhere = K)", "Carbamyl (Anywhere = K)",
                "Diglycyl (Anywhere = K)", "TMT6plex+Digly (Anywhere = K)",
                "Oxidation (Anywhere = M)", "Deamidated (Anywhere = N)",
                "Deamidated (Anywhere = Q)", "Gln->pyro-Glu (N-term = Q)",
                "Phospho (Anywhere = S)", "Phospho (Anywhere = T)",
                "Phospho (Anywhere = Y)",
                "TMT6plex (Anywhere = K)", "TMT6plex (Any N-term = N-term)",
                "TMT10plex (Anywhere = K)", "TMT10plex (Any N-term = N-term)",
                "TMT11plex (Anywhere = K)", "TMT11plex (Any N-term = N-term)",
                "TMT16plex (Anywhere = K)", "TMT16plex (Any N-term = N-term)",
                "TMT18plex (Anywhere = K)", "TMT18plex (Any N-term = N-term)")

# table_unimods()
.temp_dir <- tempdir()

umods <- mzion::table_unimods(file.path(.temp_dir, "unimods.txt")) |>
  dplyr::mutate(modification = paste0(title, " (", position, " = ", site, ")")) |>
  dplyr::mutate(short = ifelse(modification %in% short_mods, TRUE, FALSE))

umods_short <- umods |> dplyr::filter(short) # used in modification.R
umods_labs  <- umods |> dplyr::filter(grepl("^Label:", title))

enzymes <- c("Trypsin_P", "Trypsin", "LysC", "LysN", "ArgC", "LysC_P",
             "Chymotrypsin", "GluC", "GluN", "AspC", "AspN", "SemiTrypsin_P",
             "SemiTrypsin", "SemiLysC", "SemiLysN", "SemiArgC", "SemiLysC_P",
             "SemiChymotrypsin", "SemiGluC", "SemiGluN", "SemiAspC", "SemiAspN",
             "Noenzyme", "Nodigest")

fdr_groups <- c("base", "all", "top3")
nes_fdr_groups <- c("base", "base_cterm_tryptic", "base_cterm_nontryptic",
                    "all","all_cterm_tryptic", "all_cterm_nontryptic", "top3",
                    "top3_cterm_tryptic", "top3_cterm_nontryptic")
