# +++++++++++++++++++++++++++++++++++++
# ----- PROCESS MODEL PERFORMANCE -----
# +++++++++++++++++++++++++++++++++++++
# FC20230501

# load libraries
library(tidyverse)
library(stringr)
library(data.table)
library(knitr)
library(kableExtra)
library(stringdist)
library(Hmisc)
library(here)


# +++++++++++++++++++++++++++++++++++++++++++++++++++
# (1) Compute metrics by group and spellchecker -----
# +++++++++++++++++++++++++++++++++++++++++++++++++++

# load performance data
data_non_DL <- fread(here("data/model_performance/synth_data_comb_non_DL_results.csv")) %>% data.frame()
data_non_DL_main <- data_non_DL[!grepl(data_non_DL$experiment, pattern = "_mr|_mrs|_dr"), ] # drop prefix experiments, don't have data on them for the deep learning spellchecker
# data_DL <- fread(here("data/model_performance/synth_data_comb_incl_DL_results.csv")) %>% data.frame()
data_DL_1 <- read_csv(here("data/model_performance/synth_data_comb_incl_intermediate_DL_results1.csv")) %>% data.frame()
data_DL_2 <- fread(here("data/model_performance/synth_data_comb_incl_intermediate_DL_results2.csv")) %>% data.frame()
data_DL_1$V1 <- NULL
data_DL_2$V1 <- NULL
data_DL_1$Unnamed..0 <- NULL
data_DL_2$Unnamed..0 <- NULL
data_DL_1$...1 <- NULL
data_DL_2$...1 <- NULL
data_DL <- cbind(data_DL_1, data_DL_2[, c(ncol(data_DL_2) - 1, ncol(data_DL_2))])

# combine non-DL and DL data
data_experiments <- cbind(data_non_DL_main, data_DL[, c((ncol(data_DL) - 3):ncol(data_DL))])
data_experiments$V1 <- NULL
data_experiments$Unnamed..0 <- NULL
data_experiments$...1 <- NULL


# _____evaluate instance-level performance -----
performance_data_long <- data_experiments %>% 
  pivot_longer(cols = colnames(data_experiments)[grepl(colnames(data_experiments), pattern = "transformer|symspell|jamspell")], names_to = "spellchecker", values_to = "spc_output")

# indicator for my mitigation/replacement approach
performance_data_long$replacement_approach <- as.numeric(grepl(performance_data_long$spellchecker, pattern = "replacement"))

# indicator for perturbed/unperturbed input
performance_data_long$experiment_perturbed <- as.numeric(!grepl(performance_data_long$spellchecker, pattern = "unperturbed"))

# clean spellchecker var
performance_data_long$spellchecker <- str_replace_all(performance_data_long$spellchecker, pattern = "(transformer|symspell|jamspell).*", replacement = "\\1")

# remove punctuation
performance_data_long$spc_output <- sub(performance_data_long$spc_output, pattern = "[,.!?]", replacement = "")
performance_data_long$unperturbed_version <- sub(performance_data_long$unperturbed_version, pattern = "[,.!?]", replacement = "")

# initialize
performance_data_long$spc_correct <- NA
performance_data_long$spc_correct <- as.numeric(performance_data_long$unperturbed_version == performance_data_long$spc_output)
mean(performance_data_long$spc_correct, na.rm = T)


# +++++++++++++++++++++++++++++++
# (2) Create results tables -----
# +++++++++++++++++++++++++++++++

# convert longer with one observation per race
performance_data_by_race <- performance_data_long %>%
  pivot_longer(!colnames(performance_data_long)[!grepl(colnames(performance_data_long), pattern = "share")], names_to = "race", values_to = "share")
performance_data_by_race$race <- str_replace_all(performance_data_by_race$race, pattern = c("share_" = ""))
performance_data_by_race$share <- as.numeric(performance_data_by_race$share)


# _____name length by race -----
mean(nchar(performance_data_by_race$name))
for(r in c("asian", "black", "hispanic", "white")) {
  tmp <- unique(performance_data_by_race[performance_data_by_race$race == r, c("name", "share", "race", "name_type")])
  print(paste0("Average name length (", r, "): ", sum(nchar(tmp$name) * tmp$share, na.rm = T) / sum(tmp$share, na.rm = T)))
  print(paste0("Average name length first name (", r, "): ", sum(nchar(tmp$name[tmp$name_type == "firstname"]) * tmp$share[tmp$name_type == "firstname"], na.rm = T) / sum(tmp$share[tmp$name_type == "firstname"], na.rm = T)))
  print(paste0("Average name length last name (", r, "): ", sum(nchar(tmp$name[tmp$name_type == "lastname"]) * tmp$share[tmp$name_type == "lastname"], na.rm = T) / sum(tmp$share[tmp$name_type == "lastname"], na.rm = T)))
}

# initialize table
name_length_tbl <- data.frame(
  "Name Type" = c("First Names", "Last Names", "Any Name"), 
  Asian = c(NA, NA, NA),
  Black = c(NA, NA, NA),
  Hispanic = c(NA, NA, NA), 
  White = c(NA, NA, NA)
)

# fill table
for(r in c("asian", "black", "hispanic", "white")) {
  # select only obsrevation for the current race
  tmp <- unique(performance_data_by_race[performance_data_by_race$race == r, c("name", "share", "race", "name_type")])
  
  # generate values
  r <- str_to_title(r)
  name_length_tbl[, r] <- c(
    sum(nchar(tmp$name[tmp$name_type == "firstname"]) * tmp$share[tmp$name_type == "firstname"], na.rm = T) / sum(tmp$share[tmp$name_type == "firstname"], na.rm = T),
    sum(nchar(tmp$name[tmp$name_type == "lastname"]) * tmp$share[tmp$name_type == "lastname"], na.rm = T) / sum(tmp$share[tmp$name_type == "lastname"], na.rm = T),
    sum(nchar(tmp$name) * tmp$share, na.rm = T) / sum(tmp$share, na.rm = T)
  )
}
rm(r, tmp)

# write latex table
x <- name_length_tbl %>%
  kbl(
    caption = "Average Name Length by Race",
    label = "tab:avg_name_length",
    align = c("l", "c", "c", "c", "c"),
    vline = "",
    valign = "ht",
    toprule =  "\\midrule\\midrule",
    midrule = "\\midrule",
    bottomrule = "\\midrule\\midrule",
    linesep = "",
    digits = 3,
    format = "latex"
  ) %>%
  kable_classic(html_font = "Cambria")
write(x, file = here(paste0("tables/name_length.tex")))
rm(x, name_length_tbl)


# _____precise string matching metrics by race -----
# function to generate performance tables for precise string matching
gen_precise_performance_tables <- function(df, exp_type, file_name) {
  
  metrics_df <- df[
    !df$race %in% c("2race", "aian") &
      df$experiment %in% c(exp_type),
  ] %>%
    group_by(spellchecker, replacement_approach, race) %>%
    mutate(
      wtd_accuracy_firstname = sum(spc_correct[name_type == "firstname"] * share[name_type == "firstname"], na.rm = T) / sum(share[name_type == "firstname"], na.rm = T),
      wtd_accuracy_lastname = sum(spc_correct[name_type == "lastname"] * share[name_type == "lastname"], na.rm = T) / sum(share[name_type == "lastname"], na.rm = T),
      wtd_accuracy_comb = sum(spc_correct * share, na.rm = T) / sum(share, na.rm = T),
    ) %>% 
    ungroup() %>%
    distinct(
      spellchecker,
      replacement_approach,
      race,
      wtd_accuracy_comb,
      wtd_accuracy_firstname,
      wtd_accuracy_lastname
    )
  
  # _____get bootstrapped SEs for performance -----#
  boots <- list()
  for (b in 1:500) {
    # prepare boot dataset
    tmp_boot <- df[
      !df$race %in% c("2race", "aian") &
        df$experiment %in% c(exp_type),
    ]
    boot_names <- sample(unique(tmp_boot$name), length(unique(tmp_boot$name)), replace = T)
    tmp_boot <- tmp_boot[tmp_boot$name %in% boot_names, ]
    
    # calculate metrics on bootstrap dataset
    metrics_boot <- tmp_boot %>%
      group_by(spellchecker, replacement_approach, race) %>%
      mutate(
        wtd_accuracy_firstname = sum(spc_correct[name_type == "firstname"] * share[name_type == "firstname"], na.rm = T) / sum(share[name_type == "firstname"], na.rm = T),
        wtd_accuracy_lastname = sum(spc_correct[name_type == "lastname"] * share[name_type == "lastname"], na.rm = T) / sum(share[name_type == "lastname"], na.rm = T),
        wtd_accuracy_comb = sum(spc_correct * share, na.rm = T) / sum(share, na.rm = T),
      ) %>% 
      ungroup() %>%
      distinct(
        spellchecker, 
        replacement_approach, 
        race,
        wtd_accuracy_comb,
        wtd_accuracy_firstname,
        wtd_accuracy_lastname,
      )
    
    # append to list of bootstrapped metrics
    boots <- append(boots, list(metrics_boot))
  }
  rm(b, tmp_boot, boot_names, metrics_boot)
  
  # calculate bootstrapped SE
  boot_se <- boots[[1]]
  boot_se[, 4:ncol(boot_se)] <- NA
  for(c in 4:ncol(boot_se)) {
    for(r in 1:nrow(boot_se)) {
      v <- lapply(boots, FUN = function(x) { x[r, c] }) %>% unlist
      boot_se[r, c] <- sd(v)
    }
  }
  rm(r, c, v)
  
  # transform df into desired table format -----#
  # formatting means
  metrics_df <- metrics_df %>%
    pivot_longer(cols = c("wtd_accuracy_comb", "wtd_accuracy_firstname", "wtd_accuracy_lastname"), names_to = "name_type", values_to = "wtd_accuracy") %>%
    pivot_wider(names_from = race, values_from = wtd_accuracy)
  metrics_df$name_type <- sub(metrics_df$name_type, pattern = "wtd_accuracy_", replacement = "")
  
  # formatting bootstrapped SEs
  boot_se <- boot_se %>%
    pivot_longer(cols = c("wtd_accuracy_comb", "wtd_accuracy_firstname", "wtd_accuracy_lastname"), names_to = "name_type", values_to = "wtd_accuracy") %>%
    pivot_wider(names_from = race, values_from = wtd_accuracy)
  boot_se$name_type <- sub(boot_se$name_type, pattern = "wtd_accuracy_", replacement = "")
  
  # get means +/- se -----#
  mean_CI <- boot_se
  for(c in 4:ncol(mean_CI)) {
    mean_CI[, c] <- as.character(mean_CI[, c])
    for(r in 1:nrow(mean_CI)) {
      mean_CI[r, c] <- paste0("[", round(metrics_df[r, c] - boot_se[r, c], digits = 3), ", ", round(metrics_df[r, c] + boot_se[r, c], digits = 3), "]")
    }
  }
  
  # interleave means and SEs
  metrics_df$var <- 1:nrow(metrics_df)
  metrics_df$row <- 1
  for(v in c("white", "black", "hispanic", "asian")) { metrics_df[, v] <- round(metrics_df[, v], digits = 3) }
  mean_CI$var <- 1:nrow(mean_CI)
  mean_CI$row <- 2
  mean_CI[, 1:2] <- ""
  output_tbl <- rbind(metrics_df, mean_CI)
  output_tbl <- output_tbl[order(output_tbl$var, output_tbl$row, decreasing = F), ]
  
  # prepare column names
  output_tbl <- rename(
    output_tbl,
    "Mitigation Approach" = replacement_approach
  )
  colnames(output_tbl) <- str_to_title(colnames(output_tbl))
  
  # prepare spellchecker names
  output_tbl$Spellchecker <- str_replace_all(output_tbl$Spellchecker, pattern = c(
    "symspell" = "SymSpell",
    "jamspell" = "JamSpell",
    "transformer" = "Transformer"
  ))
  
  # calculate max spread in performance between races by row
  colnames(metrics_df) <- str_to_title(colnames(metrics_df))
  metrics_df <- metrics_df %>%
    rowwise() %>%
    mutate(
      `Spread (%)` = round((max(c(Asian, Black, Hispanic, White)) - min(c(Asian, Black, Hispanic, White))) / max(c(Asian, Black, Hispanic, White)), digits = 3),
    ) %>%
    ungroup()
  
  # add spread to main table
  output_tbl <- left_join(output_tbl, metrics_df[, c("Var", "Row", "Spread (%)")], by = c("Var", "Row"))
  output_tbl$`Spread (%)`[is.na(output_tbl$`Spread (%)`)] <- ""
  output_tbl$var <- NULL
  output_tbl$row <- NULL
  
  # iterate through name types
  for(nt in unique(output_tbl$Name_type)) {
    
    # set strings to build table title
    exp_title1 <- "by Race"
    if(grepl(exp_type[1], pattern = "20")) { exp_title1 <- "with 20% Perturbation" }
    exp_title2 <- "First Names"
    if(grepl(exp_type[1], pattern = "sentence")) { exp_title2 <- "First Name Sentences" }
    if(nt == "lastname") {
      exp_title2 <- "Last Names"
      if(grepl(exp_type[1], pattern = "sentence")) { exp_title2 <- "Last Name Sentences" }
    } else if(nt == "comb") {
      exp_title2 <- "First and Last Names"
      if(grepl(exp_type[1], pattern = "sentence")) { exp_title2 <- "First and Last Name Sentences" }
    }
    
    # generate table
    x <- output_tbl[output_tbl$Name_type == nt, c("Spellchecker", "Mitigation Approach", "Asian", "Black", "Hispanic", "White", "Spread (%)")] %>%
      kbl(
        caption = paste0("Precise Matching Performance ", exp_title1, " - ", exp_title2),
        label = paste0("precise_performance_", file_name, "_", nt),
        align = c("l", "c", "c", "c", "c", "c", "c"),
        vline = "",
        valign = "ht",
        toprule =  "\\midrule\\midrule",
        midrule = "\\midrule",
        bottomrule = "\\midrule\\midrule",
        linesep = "",
        digits = 3,
        format = "latex"
      ) %>%
      kable_classic(html_font = "Cambria")
    write(x, file = here(paste0("tables/precise_performance_by_race_", file_name, "_", nt, ".tex")))
  }
}


# __________name-level experiments -----
# one character perturbation
gen_precise_performance_tables(performance_data_by_race, exp_type = "name_perturbed", file_name = "name_1")

# 20% of characters perturbation
gen_precise_performance_tables(performance_data_by_race, exp_type = "name_perturbed_20", file_name = "name_20")


# __________sentence-level experiments -----
# one character perturbation 
gen_precise_performance_tables(performance_data_by_race, exp_type = paste0("sentence_name_perturbed_", 1:3), file_name = "sentence_1")

# 20% of characters perturbation
gen_precise_performance_tables(performance_data_by_race, exp_type = paste0("sentence_name_perturbed_20_", 1:3), file_name = "sentence_20")


# _____false positives/negatives by race -----
# generate indicators for false positives/negatives (i.e. spellchecked despite correct and vice versa)
performance_data_by_race$false_positive <- as.numeric(
  performance_data_by_race$experiment_perturbed == 0 &
    performance_data_by_race$unperturbed_version != performance_data_by_race$spc_output
)
performance_data_by_race$false_negative <- as.numeric(
  performance_data_by_race$experiment_perturbed == 1 &
    performance_data_by_race$perturbed_version == performance_data_by_race$spc_output
)

# set to NA if "initial condition" not given
performance_data_by_race$false_positive[performance_data_by_race$experiment_perturbed == 1] <- NA
performance_data_by_race$false_negative[performance_data_by_race$experiment_perturbed == 0] <- NA

# function to generate false positives/negatives metrics and corresponding latex table
gen_fp_np_tables <- function(df, exp_type, file_name) {
  
  # calculate average performance by groups -----#
  metrics_df <- df[
    !df$race %in% c("2race", "aian") &
      df$experiment %in% c(exp_type),
  ] %>%
    group_by(spellchecker, replacement_approach, race) %>%
    mutate(
      wtd_fp_firstname = sum(false_positive[name_type == "firstname"] * share[name_type == "firstname"], na.rm = T) / sum(share[name_type == "firstname"], na.rm = T),
      wtd_fn_firstname = sum(false_negative[name_type == "firstname"] * share[name_type == "firstname"], na.rm = T) / sum(share[name_type == "firstname"], na.rm = T),
      wtd_fp_lastname = sum(false_positive[name_type == "lastname"] * share[name_type == "lastname"], na.rm = T) / sum(share[name_type == "lastname"], na.rm = T),
      wtd_fn_lastname = sum(false_negative[name_type == "lastname"] * share[name_type == "lastname"], na.rm = T) / sum(share[name_type == "lastname"], na.rm = T),
      wtd_fp_comb = sum(false_positive * share, na.rm = T) / sum(share, na.rm = T),
      wtd_fn_comb = sum(false_negative * share, na.rm = T) / sum(share, na.rm = T),
    ) %>% 
    ungroup() %>%
    distinct(
      spellchecker,
      replacement_approach,
      race,
      wtd_fp_firstname,
      wtd_fn_firstname,
      wtd_fp_lastname,
      wtd_fn_lastname,
      wtd_fp_comb,
      wtd_fn_comb,
    )
  
  # _____get bootstrapped SEs for performance -----#
  boots <- list()
  for (b in 1:500) {
    # prepare boot dataset
    tmp_boot <- df[
      !df$race %in% c("2race", "aian") &
        df$experiment %in% c(exp_type),
    ]
    boot_names <- sample(unique(tmp_boot$name), length(unique(tmp_boot$name)), replace = T)
    tmp_boot <- tmp_boot[tmp_boot$name %in% boot_names, ]
    
    # calculate metrics on bootstrap dataset
    metrics_boot <- tmp_boot %>%
      group_by(spellchecker, replacement_approach, race) %>%
      mutate(
        wtd_fp_firstname = sum(false_positive[name_type == "firstname"] * share[name_type == "firstname"], na.rm = T) / sum(share[name_type == "firstname"], na.rm = T),
        wtd_fn_firstname = sum(false_negative[name_type == "firstname"] * share[name_type == "firstname"], na.rm = T) / sum(share[name_type == "firstname"], na.rm = T),
        wtd_fp_lastname = sum(false_positive[name_type == "lastname"] * share[name_type == "lastname"], na.rm = T) / sum(share[name_type == "lastname"], na.rm = T),
        wtd_fn_lastname = sum(false_negative[name_type == "lastname"] * share[name_type == "lastname"], na.rm = T) / sum(share[name_type == "lastname"], na.rm = T),
        wtd_fp_comb = sum(false_positive * share, na.rm = T) / sum(share, na.rm = T),
        wtd_fn_comb = sum(false_negative * share, na.rm = T) / sum(share, na.rm = T),
      ) %>% 
      ungroup() %>%
      distinct(
        spellchecker,
        replacement_approach,
        race,
        wtd_fp_firstname,
        wtd_fn_firstname,
        wtd_fp_lastname,
        wtd_fn_lastname,
        wtd_fp_comb,
        wtd_fn_comb,
      )
    
    # append to list of bootstrapped metrics
    boots <- append(boots, list(metrics_boot))
  }
  rm(b, tmp_boot, boot_names, metrics_boot)
  
  # calculate bootstrapped SE
  boot_se <- boots[[1]]
  boot_se[, 4:ncol(boot_se)] <- NA
  for(c in 4:ncol(boot_se)) {
    for(r in 1:nrow(boot_se)) {
      v <- lapply(boots, FUN = function(x) { x[r, c] }) %>% unlist
      boot_se[r, c] <- sd(v)
    }
  }
  rm(r, c, v)
  
  
  # transform df into desired table format -----#
  # formatting means
  metrics_df <- metrics_df %>%
    pivot_longer(cols = c(
      "wtd_fp_firstname",
      "wtd_fn_firstname",
      "wtd_fp_lastname",
      "wtd_fn_lastname",
      "wtd_fp_comb",
      "wtd_fn_comb"
    ), names_to = "name_type", values_to = "wtd_fp_fn") %>%
    pivot_wider(names_from = race, values_from = wtd_fp_fn)
  metrics_df$FP <- grepl(metrics_df$name_type, pattern = "fp") %>% as.numeric()
  metrics_df$FN <- grepl(metrics_df$name_type, pattern = "fn") %>% as.numeric()
  metrics_df$name_type <- sub(metrics_df$name_type, pattern = "wtd_(fp|fn)_", replacement = "")
  
  # formatting bootstrapped SEs
  boot_se <- boot_se %>%
    pivot_longer(cols = c(
      "wtd_fp_firstname",
      "wtd_fn_firstname",
      "wtd_fp_lastname",
      "wtd_fn_lastname",
      "wtd_fp_comb",
      "wtd_fn_comb"
    ), names_to = "name_type", values_to = "wtd_fp_fn") %>%
    pivot_wider(names_from = race, values_from = wtd_fp_fn)
  boot_se$FP <- grepl(boot_se$name_type, pattern = "fp") %>% as.numeric()
  boot_se$FN <- grepl(boot_se$name_type, pattern = "fn") %>% as.numeric()
  boot_se$name_type <- sub(boot_se$name_type, pattern = "wtd_(fp|fn)_", replacement = "")
  
  # get means +/- se -----#
  mean_CI <- boot_se
  for(c in 4:ncol(mean_CI)) {
    mean_CI[, c] <- as.character(mean_CI[, c])
    for(r in 1:nrow(mean_CI)) {
      mean_CI[r, c] <- paste0("[", round(metrics_df[r, c] - boot_se[r, c], digits = 3), ", ", round(metrics_df[r, c] + boot_se[r, c], digits = 3), "]")
    }
  }
  
  # interleave means and SEs
  metrics_df$var <- 1:nrow(metrics_df)
  metrics_df$row <- 1
  for(v in c("white", "black", "hispanic", "asian")) { metrics_df[, v] <- round(metrics_df[, v], digits = 3) }
  mean_CI$var <- 1:nrow(mean_CI)
  mean_CI$row <- 2
  mean_CI[, 1:2] <- ""
  output_tbl <- rbind(metrics_df, mean_CI)
  output_tbl <- output_tbl[order(output_tbl$var, output_tbl$row, decreasing = F), ]
  
  # prepare column names
  output_tbl <- rename(
    output_tbl,
    "Mitigation Approach" = replacement_approach
  )
  colnames(output_tbl) <- str_to_title(colnames(output_tbl))
  
  # prepare spellchecker names
  output_tbl$Spellchecker <- str_replace_all(output_tbl$Spellchecker, pattern = c(
    "symspell" = "SymSpell",
    "jamspell" = "JamSpell",
    "transformer" = "Transformer"
  ))
  
  # calculate max spread in performance between races by row
  colnames(metrics_df) <- str_to_title(colnames(metrics_df))
  metrics_df <- metrics_df %>%
    rowwise() %>%
    mutate(
      `Spread (%)` = round((max(c(Asian, Black, Hispanic, White)) - min(c(Asian, Black, Hispanic, White))) / max(c(Asian, Black, Hispanic, White)), digits = 3),
    ) %>%
    ungroup()
  
  # add spread to main table
  output_tbl <- left_join(output_tbl, metrics_df[, c("Var", "Row", "Spread (%)")], by = c("Var", "Row"))
  output_tbl$`Spread (%)`[is.na(output_tbl$`Spread (%)`)] <- ""
  output_tbl$var <- NULL
  output_tbl$row <- NULL
  
  # iterate through name types
  for(nt in unique(output_tbl$Name_type)) {
    for(fp_fn in c("fp", "fn")) {
      
      if(fp_fn == "fn") {
        tmp <- output_tbl[
          output_tbl$Name_type == nt &
            output_tbl$Fp == 0, 
          c("Spellchecker", "Mitigation Approach", "Asian", "Black", "Hispanic", "White", "Spread (%)")
        ]
      } else {
        tmp <- output_tbl[
          output_tbl$Name_type == nt &
            output_tbl$Fp == 1, 
          c("Spellchecker", "Mitigation Approach", "Asian", "Black", "Hispanic", "White", "Spread (%)")
        ]
      }
      
      # set strings to build table title
      exp_title1 <- "by Race"
      if(grepl(exp_type[1], pattern = "20")) { exp_title1 <- "with 20% Perturbation" }
      exp_title2 <- "First Names"
      if(grepl(exp_type[1], pattern = "sentence")) { exp_title2 <- "First Name Sentences" }
      if(nt == "lastname") {
        exp_title2 <- "Last Names"
        if(grepl(exp_type[1], pattern = "sentence")) { exp_title2 <- "Last Name Sentences" }
      } else if(nt == "comb") {
        exp_title2 <- "First and Last Names"
        if(grepl(exp_type[1], pattern = "sentence")) { exp_title2 <- "First and Last Name Sentences" }
      }
      
      # title blurb
      title_blurb <- ifelse(
        fp_fn == "fp",
        "Over-Spellchecking",
        "Under-Spellchecking"
      )
      
      # generate table
      x <- tmp %>%
        kbl(
          caption = paste0(title_blurb, " ", exp_title1, " - ", exp_title2),
          label = paste0(fp_fn, "_", file_name, "_", nt),
          align = c("l", "c", "c", "c", "c", "c", "c"),
          vline = "",
          valign = "ht",
          toprule =  "\\midrule\\midrule",
          midrule = "\\midrule",
          bottomrule = "\\midrule\\midrule",
          linesep = "",
          digits = 5,
          format = "latex"
        ) %>%
        kable_classic(html_font = "Cambria")
      write(x, file = here(paste0("tables/", fp_fn, "_by_race_", file_name, "_", nt, ".tex")))
    }
  }
}


# __________name-level experiments -----
# one character perturbation
gen_fp_np_tables(performance_data_by_race, exp_type = "name_perturbed", file_name = "name_1")

# 20% of characters perturbation
gen_fp_np_tables(performance_data_by_race, exp_type = "name_perturbed_20", file_name = "name_20")


# __________sentence-level experiments -----
# one character perturbation 
gen_fp_np_tables(performance_data_by_race, exp_type = paste0("sentence_name_perturbed_", 1:3), file_name = "sentence_1")

# 20% of characters perturbation
gen_fp_np_tables(performance_data_by_race, exp_type = paste0("sentence_name_perturbed_20_", 1:3), file_name = "sentence_20")




# +++++++++++++++++++++++++++++++++++++++++
# (3) Performance with fuzzy matching -----
# +++++++++++++++++++++++++++++++++++++++++

# _____"raw" "similar-enough" rate from perturbed to clean ------
performance_data_by_race <- performance_data_by_race %>%
  group_by(name, perturbed_version) %>%
  mutate(
    raw_matched_perturbed_simulated = agrepl(unique(name), unique(perturbed_version)),
  ) %>%
  ungroup()


# _____spellchecked and original "similar-enough"? -----
performance_data_by_race <- performance_data_by_race %>%
  group_by(name, spc_output) %>%
  mutate(
    spc_matched_simulated = agrepl(unique(name), unique(spc_output)),
  ) %>%
  ungroup()


# _____generate jw distance matrix for all names and spellchecker outputs -----
# __________first names -----
jw_matrix_first_names <- stringdistmatrix(
  unique(performance_data_by_race$spc_output[performance_data_by_race$name_type == "firstname"]),
  unique(performance_data_by_race$name[performance_data_by_race$name_type == "firstname"]),
  method = "jw",
)
jw_matrix_first_names <- data.frame(cbind(unique(performance_data_by_race$spc_output[performance_data_by_race$name_type == "firstname"]), jw_matrix_first_names))
colnames(jw_matrix_first_names) <- c("name", unique(performance_data_by_race$name[performance_data_by_race$name_type == "firstname"]))
jw_matrix_first_names[2:ncol(jw_matrix_first_names)] <- sapply(jw_matrix_first_names[2:ncol(jw_matrix_first_names)], as.numeric)
jw_matrix_first_names <- jw_matrix_first_names[!is.na(jw_matrix_first_names$name), ]

# find best matches according to jw distance
max_col <- ncol(jw_matrix_first_names)
jw_matrix_first_names$min_dist <- NA
jw_matrix_first_names$fuzzy_matched_name <- NA
for(row in 1:nrow(jw_matrix_first_names)) {
  if(row %% 500 == 0) print(row)
  a <- jw_matrix_first_names[row, 1:max_col]
  x <- min(c(jw_matrix_first_names[row, 2:max_col] %>% unlist(), 2), na.rm = T)
  jw_matrix_first_names$min_dist[row] <- x
  if(x != 2) {
    jw_matrix_first_names$fuzzy_matched_name[row] <- paste0(colnames(jw_matrix_first_names)[which(a == x)], collapse = ";")
  }
}
rm(row, a, x, max_col)


# __________last names -----
jw_matrix_last_names <- stringdistmatrix(
  unique(performance_data_by_race$spc_output[performance_data_by_race$name_type == "lastname"]),
  unique(performance_data_by_race$name[performance_data_by_race$name_type == "lastname"]),
  method = "jw",
)
jw_matrix_last_names <- data.frame(cbind(unique(performance_data_by_race$spc_output[performance_data_by_race$name_type == "lastname"]), jw_matrix_last_names))
colnames(jw_matrix_last_names) <- c("name", unique(performance_data_by_race$name[performance_data_by_race$name_type == "lastname"]))
jw_matrix_last_names[2:ncol(jw_matrix_last_names)] <- sapply(jw_matrix_last_names[2:ncol(jw_matrix_last_names)], as.numeric)
jw_matrix_last_names <- jw_matrix_last_names[!is.na(jw_matrix_last_names$name), ]

# find best matches according to jw distance
max_col <- ncol(jw_matrix_last_names)
jw_matrix_last_names$min_dist <- NA
jw_matrix_last_names$fuzzy_matched_name <- NA
for(row in 1:nrow(jw_matrix_last_names)) {
  if(row %% 500 == 0) print(row)
  a <- jw_matrix_last_names[row, 1:max_col]
  x <- min(c(jw_matrix_last_names[row, 2:max_col] %>% unlist(), 2), na.rm = T)
  jw_matrix_last_names$min_dist[row] <- x
  if(x != 2) {
    jw_matrix_last_names$fuzzy_matched_name[row] <- paste0(colnames(jw_matrix_last_names)[which(a == x)], collapse = ";")
  }
}
rm(row, a, x, max_col)


# _____generate fuzzy matching performance metrics -----
# create combined jw distance dataset
jw_matrix_first_names$name_type <- "firstname"
jw_matrix_last_names$name_type <- "lastname"
jw_matrix_comb <- rbind(
  jw_matrix_first_names[, c("name", "name_type", "min_dist", "fuzzy_matched_name")],
  jw_matrix_last_names[, c("name", "name_type", "min_dist", "fuzzy_matched_name")]
)

# get experimental results
fuzzy_match_master <- rename(performance_data_by_race[, c("spellchecker", "name_type", "experiment", "name", "spc_output", "race", "share", "replacement_approach")], original_name = name)

# combine experimental results with fuzzy matching results
fuzzy_match_master <- left_join(fuzzy_match_master, jw_matrix_comb, by = c(
  "spc_output" = "name",
  "name_type"
))

# generate indicators for correct fuzzy matches
fuzzy_match_master <- fuzzy_match_master %>%
  group_by(original_name, fuzzy_matched_name) %>%
  mutate(
    correct_match = as.numeric(original_name %in% str_split(fuzzy_matched_name, pattern = ";")[[1]]),
    n_matches = length(str_split(fuzzy_matched_name, pattern = ";")[[1]]),
  ) %>%
  ungroup()
fuzzy_match_master$correct_match_norm <- fuzzy_match_master$correct_match / fuzzy_match_master$n_matches


# _____generate metrics by spellchecker, experiment, and race -----
# function to generate fuzzy tables
gen_fuzzy_performance_tables <- function(df, exp_type, file_name) {
  
  # get performance estimates
  metrics_df <- df[
    !df$race %in% c("2race", "aian") &
      df$experiment %in% c(exp_type),
  ] %>%
    group_by(spellchecker, replacement_approach, race) %>%
    mutate(
      wtd_accuracy_firstname = sum(correct_match_norm[name_type == "firstname"] * share[name_type == "firstname"], na.rm = T) / sum(share[name_type == "firstname"], na.rm = T),
      wtd_accuracy_lastname = sum(correct_match_norm[name_type == "lastname"] * share[name_type == "lastname"], na.rm = T) / sum(share[name_type == "lastname"], na.rm = T),
      wtd_accuracy_comb = sum(correct_match_norm * share, na.rm = T) / sum(share, na.rm = T),
    ) %>% 
    ungroup() %>%
    distinct(
      spellchecker, 
      replacement_approach, 
      race,
      wtd_accuracy_comb,
      wtd_accuracy_firstname,
      wtd_accuracy_lastname,
    )
  
  # _____get bootstrapped SEs for performance -----#
  boots <- list()
  for (b in 1:500) {
    # prepare boot dataset
    tmp_boot <- df[
      !df$race %in% c("2race", "aian") &
        df$experiment %in% c(exp_type),
    ]
    boot_names <- sample(unique(tmp_boot$original_name), length(unique(tmp_boot$original_name)), replace = T)
    tmp_boot <- tmp_boot[tmp_boot$original_name %in% boot_names, ]
    
    # calculate metrics on bootstrap dataset
    metrics_boot <- tmp_boot %>%
      group_by(spellchecker, replacement_approach, race) %>%
      mutate(
        wtd_accuracy_firstname = sum(correct_match_norm[name_type == "firstname"] * share[name_type == "firstname"], na.rm = T) / sum(share[name_type == "firstname"], na.rm = T),
        wtd_accuracy_lastname = sum(correct_match_norm[name_type == "lastname"] * share[name_type == "lastname"], na.rm = T) / sum(share[name_type == "lastname"], na.rm = T),
        wtd_accuracy_comb = sum(correct_match_norm * share, na.rm = T) / sum(share, na.rm = T),
      ) %>% 
      ungroup() %>%
      distinct(
        spellchecker, 
        replacement_approach, 
        race,
        wtd_accuracy_comb,
        wtd_accuracy_firstname,
        wtd_accuracy_lastname,
      )
    
    # append to list of bootstrapped metrics
    boots <- append(boots, list(metrics_boot))
  }
  rm(b, tmp_boot, boot_names, metrics_boot)
  
  # calculate bootstrapped SE
  boot_se <- boots[[1]]
  boot_se[, 4:ncol(boot_se)] <- NA
  for(c in 4:ncol(boot_se)) {
    for(r in 1:nrow(boot_se)) {
      v <- lapply(boots, FUN = function(x) { x[r, c] }) %>% unlist
      boot_se[r, c] <- sd(v)
    }
  }
  rm(r, c, v)
  
  # transform df into desired table format -----#
  # formatting means
  metrics_df <- metrics_df %>%
    pivot_longer(cols = c("wtd_accuracy_comb", "wtd_accuracy_firstname", "wtd_accuracy_lastname"), names_to = "name_type", values_to = "wtd_accuracy") %>%
    pivot_wider(names_from = race, values_from = wtd_accuracy)
  metrics_df$name_type <- sub(metrics_df$name_type, pattern = "wtd_accuracy_", replacement = "")
  
  # formatting bootstrapped SEs
  boot_se <- boot_se %>%
    pivot_longer(cols = c("wtd_accuracy_comb", "wtd_accuracy_firstname", "wtd_accuracy_lastname"), names_to = "name_type", values_to = "wtd_accuracy") %>%
    pivot_wider(names_from = race, values_from = wtd_accuracy)
  boot_se$name_type <- sub(boot_se$name_type, pattern = "wtd_accuracy_", replacement = "")
  
  # get means +/- se -----#
  mean_CI <- boot_se
  for(c in 4:ncol(mean_CI)) {
    mean_CI[, c] <- as.character(mean_CI[, c])
    for(r in 1:nrow(mean_CI)) {
      mean_CI[r, c] <- paste0("[", round(metrics_df[r, c] - boot_se[r, c], digits = 3), ", ", round(metrics_df[r, c] + boot_se[r, c], digits = 3), "]")
    }
  }
  
  # interleave means and SEs
  metrics_df$var <- 1:nrow(metrics_df)
  metrics_df$row <- 1
  for(v in c("white", "black", "hispanic", "asian")) { metrics_df[, v] <- round(metrics_df[, v], digits = 3) }
  mean_CI$var <- 1:nrow(mean_CI)
  mean_CI$row <- 2
  mean_CI[, 1:2] <- ""
  output_tbl <- rbind(metrics_df, mean_CI)
  output_tbl <- output_tbl[order(output_tbl$var, output_tbl$row, decreasing = F), ]
  
  # prepare column names
  output_tbl <- rename(
    output_tbl,
    "Mitigation Approach" = replacement_approach
  )
  colnames(output_tbl) <- str_to_title(colnames(output_tbl))
  
  # prepare spellchecker names
  output_tbl$Spellchecker <- str_replace_all(output_tbl$Spellchecker, pattern = c(
    "symspell" = "SymSpell",
    "jamspell" = "JamSpell",
    "transformer" = "Transformer"
  ))
  
  # calculate max spread in performance between races by row
  colnames(metrics_df) <- str_to_title(colnames(metrics_df))
  metrics_df <- metrics_df %>%
    rowwise() %>%
    mutate(
      `Spread (%)` = round((max(c(Asian, Black, Hispanic, White)) - min(c(Asian, Black, Hispanic, White))) / max(c(Asian, Black, Hispanic, White)), digits = 3),
    ) %>%
    ungroup()
  
  # add spread to main table
  output_tbl <- left_join(output_tbl, metrics_df[, c("Var", "Row", "Spread (%)")], by = c("Var", "Row"))
  output_tbl$`Spread (%)`[is.na(output_tbl$`Spread (%)`)] <- ""
  output_tbl$var <- NULL
  output_tbl$row <- NULL
  
  # generate tables, iterating through name types
  for(nt in unique(output_tbl$Name_type)) {
    
    # set strings to build table title
    exp_title1 <- "by Race"
    if(grepl(exp_type[1], pattern = "20")) { exp_title1 <- "with 20% Perturbation" }
    exp_title2 <- "First Names"
    if(grepl(exp_type[1], pattern = "sentence")) { exp_title2 <- "First Name Sentences" }
    if(nt == "lastname") {
      exp_title2 <- "Last Names"
      if(grepl(exp_type[1], pattern = "sentence")) { exp_title2 <- "Last Name Sentences" }
    } else if(nt == "comb") {
      exp_title2 <- "First and Last Names"
      if(grepl(exp_type[1], pattern = "sentence")) { exp_title2 <- "First and Last Name Sentences" }
    }
    
    # generate table
    x <- output_tbl[output_tbl$Name_type == nt, c("Spellchecker", "Mitigation Approach", "Asian", "Black", "Hispanic", "White", "Spread (%)")] %>%
      kbl(
        caption = paste0("Fuzzy Matching Performance ", exp_title1, " - ", exp_title2),
        label = paste0("fuzzy_performance_", file_name, "_", nt),
        align = c("l", "c", "c", "c", "c", "c", "c"),
        vline = "",
        valign = "ht",
        toprule =  "\\midrule\\midrule",
        midrule = "\\midrule",
        bottomrule = "\\midrule\\midrule",
        linesep = "",
        digits = 3,
        format = "latex"
      ) %>%
      kable_classic(html_font = "Cambria")
    write(x, file = here(paste0("tables/fuzzy_performance_by_race_", file_name, "_", nt, ".tex")))
  }
}

# __________name-level experiments -----
# one character perturbation
gen_fuzzy_performance_tables(fuzzy_match_master, exp_type = "name_perturbed", file_name = "name_1")

# 20% of characters perturbation
gen_fuzzy_performance_tables(fuzzy_match_master, exp_type = "name_perturbed_20", file_name = "name_20")


# __________sentence-level experiments -----
# one character perturbation 
gen_fuzzy_performance_tables(fuzzy_match_master, exp_type = paste0("sentence_name_perturbed_", 1:3), file_name = "sentence_1")

# 20% of characters perturbation
gen_fuzzy_performance_tables(fuzzy_match_master, exp_type = paste0("sentence_name_perturbed_20_", 1:3), file_name = "sentence_20")

# garbage collection
gc()




