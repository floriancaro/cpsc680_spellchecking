# +++++++++++++++++++++++++++++++++++
# ----- GENERATE SYNTHETIC DATA -----
# +++++++++++++++++++++++++++++++++++
# FC20230227

# load libraries
library(tidyverse)
library(openxlsx)
library(stringr)
library(data.table)
library(here)
set.seed(42)


# +++++++++++++++++++
# (1) Load data -----
# +++++++++++++++++++

# _____load prepared NYC data -----
names_all <- fread(file = here("data/nyc_baby_names.csv"))


# __________generate initial dataframe ------
synthetic_data <- by(names_all, seq_len(nrow(names_all)), function(row) {
  list(
    # paste0(example1, str_to_title(row$name), "."), 
    row$name,
    row$count_total_race,
    row$share_white,
    row$share_black,
    row$share_hispanic,
    row$share_asian
  )
})
synthetic_data <- do.call(rbind, synthetic_data) %>% data.frame()
colnames(synthetic_data) <- c("name", "count_total", "share_white", "share_black", "share_hispanic", "share_asian") # "sentence", 
synthetic_data$name <- as.character(synthetic_data$name)
synthetic_data$count_total <- as.numeric(synthetic_data$count_total)
synthetic_data$share_white <- as.numeric(synthetic_data$share_white)
synthetic_data$share_black <- as.numeric(synthetic_data$share_black)
synthetic_data$share_hispanic <- as.numeric(synthetic_data$share_hispanic)
synthetic_data$share_asian <- as.numeric(synthetic_data$share_asian)


# _____load census last names data -----
census_lnames <- fread(file = here("data/census_lastnames_comb.csv"))



# +++++++++++++++++++++++++++++++++++++++++
# (2) Prepare last and full name data -----
# +++++++++++++++++++++++++++++++++++++++++

# _____last name data -----
# take top 500 (as measured by race index) last names by race
tmp_census <- rename(census_lnames[census_lnames$count_agg > 5000, ], lastname = name)
tmp_census$lastname <- str_to_title(tmp_census$lastname)
asian_lastname500 <- slice_max(tmp_census, order_by = tmp_census[, c("api_index", "count_agg")], n = 500)
black_lastname500 <- slice_max(tmp_census, order_by = tmp_census[, c("black_index", "count_agg")], n = 500)
hispanic_lastname500 <- slice_max(tmp_census, order_by = tmp_census[, c("hispanic_index", "count_agg")], n = 500)
white_lastname500 <- slice_max(tmp_census, order_by = tmp_census[, c("white_index", "count_agg")], n = 500)
rm(tmp_census)

# combine into one dataset
synthetic_data_lastnames <- rbind(asian_lastname500, black_lastname500, hispanic_lastname500, white_lastname500)


# _____full name data -----
# take 25 most "distinct" first names by race
tmp_synth <- rename(synthetic_data[synthetic_data$count_total > 25, ], firstname = name)
asian_firstname25 <- slice_max(tmp_synth, order_by = tmp_synth[, c("share_asian", "count_total")], n = 25)
black_firstname25 <- slice_max(tmp_synth, order_by = tmp_synth[, c("share_black", "count_total")], n = 25)
hispanic_firstname25 <- slice_max(tmp_synth, order_by = tmp_synth[, c("share_hispanic", "count_total")], n = 25)
white_firstname25 <- slice_max(tmp_synth, order_by = tmp_synth[, c("share_white", "count_total")], n = 25)
rm(tmp_synth)

# take 25 most "distinct" last names by race
tmp_census <- rename(census_lnames[census_lnames$count_agg > 10000, ], lastname = name)
tmp_census$lastname <- str_to_title(tmp_census$lastname)
asian_lastname25 <- slice_max(tmp_census, order_by = tmp_census[, c("api_index", "count_agg")], n = 25)
black_lastname25 <- slice_max(tmp_census, order_by = tmp_census[, c("black_index", "count_agg")], n = 25)
hispanic_lastname25 <- slice_max(tmp_census, order_by = tmp_census[, c("hispanic_index", "count_agg")], n = 25)
white_lastname25 <- slice_max(tmp_census, order_by = tmp_census[, c("white_index", "count_agg")], n = 25)
rm(tmp_census)

# combine first and last names
asian_last_fullname <- cross_join(asian_firstname25, asian_lastname25)
asian_last_fullname$race_synthetic <- "A"
black_last_fullname <- cross_join(black_firstname25, black_lastname25)
black_last_fullname$race_synthetic <- "B"
hispanic_last_fullname <- cross_join(hispanic_firstname25, hispanic_lastname25)
hispanic_last_fullname$race_synthetic <- "H"
white_last_fullname <- cross_join(white_firstname25, white_lastname25)
white_last_fullname$race_synthetic <- "W"

# combine last and full name data from all races
synthetic_data_fullnames <- rbind(asian_last_fullname, black_last_fullname, hispanic_last_fullname, white_last_fullname)
synthetic_data_fullnames$fullname <- paste(synthetic_data_fullnames$firstname, synthetic_data_fullnames$lastname)


# +++++++++++++++++++++++++++++++++++++++
# (3) Generate random perturbations -----
# +++++++++++++++++++++++++++++++++++++++

# _____perturbation functions -----
perturb_replacement <- function(x, n) {
  if(is_empty(n) | n == 0) {
    return(x)
  } else {
    random_pos <- sample(1:nchar(x), n, replace = F)
    # generate perturbed name
    for(i in 1:n) {
      random_char <- ifelse(
        grepl(str_sub(x, random_pos[i], random_pos[i]), pattern = "[[:upper:]]"),
        sample(LETTERS, 1, replace = T),
        sample(letters, 1, replace = T)
      )
      name_perturbed <- paste0(str_sub(x, 1, (random_pos[i] - 1)), random_char, str_sub(x, random_pos[i] + 1, nchar(x)))
    }
  }
  return(name_perturbed)
}

perturb_insertion <- function(x, n) {
  if(is_empty(n) | n == 0) {
    return(x)
  } else {
    random_pos <- sample(1:nchar(x), n, replace = F)
    # generate perturbed name
    for(i in 1:n) {
      random_char <- ifelse(
        grepl(str_sub(x, random_pos[i], random_pos[i]), pattern = "[[:upper:]]"),
        sample(LETTERS, 1, replace = T),
        sample(letters, 1, replace = T)
      )
      name_perturbed <- paste0(str_sub(x, 1, (random_pos[i])), random_char, str_sub(x, random_pos[i] + 1, nchar(x)))
    }
  }
  return(name_perturbed)
}

perturb_transposition <- function(x, n) {
  if(is_empty(n) | n == 0) {
    return(x)
  } else {
    random_pos <- sample(1:nchar(x), n, replace = F)
    # generate perturbed name
    for(i in 1:n) {
      if(random_pos[i] < nchar(x)) {
        name_perturbed <- paste0(
          str_sub(x, 1, (random_pos[i] - 1)), 
          str_sub(x, random_pos[i] + 1, random_pos[i] + 1),
          str_sub(x, random_pos[i], random_pos[i]),
          str_sub(x, random_pos[i] + 2, nchar(x))
        )
      } else {
        name_perturbed <- paste0(
          str_sub(x, 1, (random_pos[i] - 2)), 
          str_sub(x, random_pos[i], random_pos[i]),
          str_sub(x, random_pos[i] - 1, random_pos[i] - 1),
          str_sub(x, random_pos[i] + 1, nchar(x))
        )
      }
    }
  }
  return(name_perturbed)
}

perturb_deletion <- function(x, n) {
  if(is_empty(n) | n == 0) {
    return(x)
  } else {
    random_pos <- sample(1:nchar(x), n, replace = F)
    # generate perturbed name
    for(i in 1:n) {
      name_perturbed <- paste0(str_sub(x, 1, (random_pos[i] - 1)), str_sub(x, random_pos[i] + 1, nchar(x)))
    }
  }
  return(name_perturbed)
}

# _____perturb names -----
# __________first names -----
# ONE perturbation
synthetic_data[, c("name_perturbed")] <- unlist(sapply(synthetic_data$name, FUN = function(x) { # , "name_perturbed_pos", "name_perturbed_char"
  n_perturb <- 1
  perturbation_type <- sample(1:4, n_perturb, replace = T)
  name_perturbed <- perturb_deletion(x, length(perturbation_type[perturbation_type == 1]))
  name_perturbed <- perturb_insertion(name_perturbed, length(perturbation_type[perturbation_type == 2]))
  name_perturbed <- perturb_transposition(name_perturbed, length(perturbation_type[perturbation_type == 3]))
  name_perturbed <- perturb_replacement(name_perturbed, length(perturbation_type[perturbation_type == 4]))
  return(name_perturbed)
}))

# perturb 20% of name
synthetic_data[, c("name_perturbed_20")] <- unlist(sapply(synthetic_data$name, FUN = function(x) { # , "name_perturbed_pos33", "name_perturbed_char33"
  name_perturbed <- x
  # get number of positions to perturb
  n_perturb <- nchar(x) / 4
  remainder <- n_perturb - floor(n_perturb)
  n_perturb <- floor(n_perturb) + rbinom(n = 1, size = 1, prob = remainder) # throw a coin
  if(n_perturb > 0) {
    perturbation_type <- sample(1:4, n_perturb, replace = T)
    name_perturbed <- perturb_deletion(name_perturbed, length(perturbation_type[perturbation_type == 1]))
    name_perturbed <- perturb_insertion(name_perturbed, length(perturbation_type[perturbation_type == 2]))
    name_perturbed <- perturb_transposition(name_perturbed, length(perturbation_type[perturbation_type == 3]))
    name_perturbed <- perturb_replacement(name_perturbed, length(perturbation_type[perturbation_type == 4]))
  }
  return(name_perturbed) # , random_pos, random_char)
}))

# __________last names -----
# ONE perturbation
synthetic_data_lastnames[, c("lastname_perturbed")] <- unlist(sapply(synthetic_data_lastnames$lastname, FUN = function(x) { # , "name_perturbed_pos", "name_perturbed_char"
  n_perturb <- 1
  perturbation_type <- sample(1:4, n_perturb, replace = T)
  name_perturbed <- perturb_deletion(x, length(perturbation_type[perturbation_type == 1]))
  name_perturbed <- perturb_insertion(name_perturbed, length(perturbation_type[perturbation_type == 2]))
  name_perturbed <- perturb_transposition(name_perturbed, length(perturbation_type[perturbation_type == 3]))
  name_perturbed <- perturb_replacement(name_perturbed, length(perturbation_type[perturbation_type == 4]))
  return(name_perturbed)
}))
# perturb 20% of name
synthetic_data_lastnames[, c("lastname_perturbed_20")] <- unlist(sapply(synthetic_data_lastnames$lastname, FUN = function(x) {
  name_perturbed <- x
  # get number of positions to perturb
  n_perturb <- nchar(x) / 4
  remainder <- n_perturb - floor(n_perturb)
  n_perturb <- floor(n_perturb) + rbinom(n = 1, size = 1, prob = remainder) # throw a coin
  if(n_perturb > 0) {
    perturbation_type <- sample(1:4, n_perturb, replace = T)
    name_perturbed <- perturb_deletion(name_perturbed, length(perturbation_type[perturbation_type == 1]))
    name_perturbed <- perturb_insertion(name_perturbed, length(perturbation_type[perturbation_type == 2]))
    name_perturbed <- perturb_transposition(name_perturbed, length(perturbation_type[perturbation_type == 3]))
    name_perturbed <- perturb_replacement(name_perturbed, length(perturbation_type[perturbation_type == 4]))
  }
  return(name_perturbed) # , random_pos, random_char)
}))


# __________full names -----
# ONE perturbation
synthetic_data_fullnames[, c("firstname_perturbed")] <- unlist(sapply(synthetic_data_fullnames$firstname, FUN = function(x) { # , "name_perturbed_pos", "name_perturbed_char"
  n_perturb <- 1
  perturbation_type <- sample(1:4, n_perturb, replace = T)
  name_perturbed <- perturb_deletion(x, length(perturbation_type[perturbation_type == 1]))
  name_perturbed <- perturb_insertion(name_perturbed, length(perturbation_type[perturbation_type == 2]))
  name_perturbed <- perturb_transposition(name_perturbed, length(perturbation_type[perturbation_type == 3]))
  name_perturbed <- perturb_replacement(name_perturbed, length(perturbation_type[perturbation_type == 4]))
  return(name_perturbed)
}))
# perturb 20% of name
synthetic_data_fullnames[, c("firstname_perturbed_20")] <- unlist(sapply(synthetic_data_fullnames$firstname, FUN = function(x) {
  name_perturbed <- x
  # get number of positions to perturb
  n_perturb <- nchar(x) / 4
  remainder <- n_perturb - floor(n_perturb)
  n_perturb <- floor(n_perturb) + rbinom(n = 1, size = 1, prob = remainder) # throw a coin
  if(n_perturb > 0) {
    perturbation_type <- sample(1:4, n_perturb, replace = T)
    name_perturbed <- perturb_deletion(name_perturbed, length(perturbation_type[perturbation_type == 1]))
    name_perturbed <- perturb_insertion(name_perturbed, length(perturbation_type[perturbation_type == 2]))
    name_perturbed <- perturb_transposition(name_perturbed, length(perturbation_type[perturbation_type == 3]))
    name_perturbed <- perturb_replacement(name_perturbed, length(perturbation_type[perturbation_type == 4]))
  }
  return(name_perturbed) # , random_pos, random_char)
}))

# ONE perturbation
synthetic_data_fullnames[, c("lastname_perturbed")] <- unlist(sapply(synthetic_data_fullnames$lastname, FUN = function(x) { # , "name_perturbed_pos", "name_perturbed_char"
  n_perturb <- 1
  perturbation_type <- sample(1:4, n_perturb, replace = T)
  name_perturbed <- perturb_deletion(x, length(perturbation_type[perturbation_type == 1]))
  name_perturbed <- perturb_insertion(name_perturbed, length(perturbation_type[perturbation_type == 2]))
  name_perturbed <- perturb_transposition(name_perturbed, length(perturbation_type[perturbation_type == 3]))
  name_perturbed <- perturb_replacement(name_perturbed, length(perturbation_type[perturbation_type == 4]))
  return(name_perturbed)
}))
# perturb 20% of name
synthetic_data_fullnames[, c("lastname_perturbed_20")] <- unlist(sapply(synthetic_data_fullnames$lastname, FUN = function(x) {
  name_perturbed <- x
  # get number of positions to perturb
  n_perturb <- nchar(x) / 4
  remainder <- n_perturb - floor(n_perturb)
  n_perturb <- floor(n_perturb) + rbinom(n = 1, size = 1, prob = remainder) # throw a coin
  if(n_perturb > 0) {
    perturbation_type <- sample(1:4, n_perturb, replace = T)
    name_perturbed <- perturb_deletion(name_perturbed, length(perturbation_type[perturbation_type == 1]))
    name_perturbed <- perturb_insertion(name_perturbed, length(perturbation_type[perturbation_type == 2]))
    name_perturbed <- perturb_transposition(name_perturbed, length(perturbation_type[perturbation_type == 3]))
    name_perturbed <- perturb_replacement(name_perturbed, length(perturbation_type[perturbation_type == 4]))
  }
  return(name_perturbed) # , random_pos, random_char)
}))

# __________full names -----
synthetic_data_fullnames$fullname_perturbed <- paste(synthetic_data_fullnames$firstname_perturbed, synthetic_data_fullnames$lastname_perturbed)
synthetic_data_fullnames$fullname_perturbed_20 <- paste(synthetic_data_fullnames$firstname_perturbed_20, synthetic_data_fullnames$lastname_perturbed_20)

# __________prefixes -----
synthetic_data_lastnames$lastname_perturbed_dr <- paste0("Dr ", str_to_title(synthetic_data_lastnames$lastname_perturbed))
synthetic_data_lastnames$lastname_perturbed_mr <- paste0("Mr ", str_to_title(synthetic_data_lastnames$lastname_perturbed))
synthetic_data_lastnames$lastname_perturbed_mrs <- paste0("Mrs ", str_to_title(synthetic_data_lastnames$lastname_perturbed))

synthetic_data_lastnames$lastname_perturbed_20_dr <- paste0("Dr ", str_to_title(synthetic_data_lastnames$lastname_perturbed_20))
synthetic_data_lastnames$lastname_perturbed_20_mr <- paste0("Mr ", str_to_title(synthetic_data_lastnames$lastname_perturbed_20))
synthetic_data_lastnames$lastname_perturbed_20_mrs <- paste0("Mrs ", str_to_title(synthetic_data_lastnames$lastname_perturbed_20))

synthetic_data_fullnames$fullname_perturbed_dr <- paste0("Dr ", str_to_title(synthetic_data_fullnames$fullname_perturbed))
synthetic_data_fullnames$fullname_perturbed_mr <- paste0("Mr ", str_to_title(synthetic_data_fullnames$fullname_perturbed))
synthetic_data_fullnames$fullname_perturbed_mrs <- paste0("Mrs ", str_to_title(synthetic_data_fullnames$fullname_perturbed))

synthetic_data_fullnames$fullname_perturbed_20_dr <- paste0("Dr ", str_to_title(synthetic_data_fullnames$fullname_perturbed_20))
synthetic_data_fullnames$fullname_perturbed_20_mr <- paste0("Mr ", str_to_title(synthetic_data_fullnames$fullname_perturbed_20))
synthetic_data_fullnames$fullname_perturbed_20_mrs <- paste0("Mrs ", str_to_title(synthetic_data_fullnames$fullname_perturbed_20))


# _____perturb text -----
time_machine_perturbed <- c()
for(w in str_split(time_machine, pattern = " ")[[1]]) {
  n_perturb <- 1
  perturbation_type <- sample(1:4, n_perturb, replace = T)
  word_perturbed <- perturb_deletion(w, length(perturbation_type[perturbation_type == 1]))
  word_perturbed <- perturb_insertion(word_perturbed, length(perturbation_type[perturbation_type == 2]))
  word_perturbed <- perturb_transposition(word_perturbed, length(perturbation_type[perturbation_type == 3]))
  word_perturbed <- perturb_replacement(word_perturbed, length(perturbation_type[perturbation_type == 4]))
  
  time_machine_perturbed <- c(time_machine_perturbed, word_perturbed)
}
rm(w)
time_machine_perturbed <- paste(time_machine_perturbed, collapse = " ")

# 20% perturbed
time_machine_perturbed_20 <- c()
for(w in str_split(time_machine, pattern = " ")[[1]]) {
  word_perturbed <- w
  if(runif(1, 0, 1) > .20) {
    perturbation_type <- sample(1:4, n_perturb, replace = T)
    word_perturbed <- perturb_deletion(word_perturbed, length(perturbation_type[perturbation_type == 1]))
    word_perturbed <- perturb_insertion(word_perturbed, length(perturbation_type[perturbation_type == 2]))
    word_perturbed <- perturb_transposition(word_perturbed, length(perturbation_type[perturbation_type == 3]))
    word_perturbed <- perturb_replacement(word_perturbed, length(perturbation_type[perturbation_type == 4]))
  }
  
  time_machine_perturbed_20 <- c(time_machine_perturbed_20, word_perturbed)
}
rm(w)
time_machine_perturbed_20 <- paste(time_machine_perturbed_20, collapse = " ")


# +++++++++++++++++++++++++++++++++++++
# (4) Generate synthetic examples -----
# +++++++++++++++++++++++++++++++++++++

# _____generate synthetic prefixed last names -----
synthetic_data_lastnames$lastname_dr <- paste0("Dr ", str_to_title(synthetic_data_lastnames$lastname))
synthetic_data_lastnames$lastname_mr <- paste0("Mr ", str_to_title(synthetic_data_lastnames$lastname))
synthetic_data_lastnames$lastname_mrs <- paste0("Mrs ", str_to_title(synthetic_data_lastnames$lastname))


# _____generate synthetic sentences with names -----
# prep some example sentences
example1 <- "My name is "
example2 <- " is my name."
example3a <- "The name "
example3b <- " is spelled like this."

# __________first names -----
synthetic_data$sentence_firstname_1 <- paste0(example1, str_to_title(synthetic_data$name), ".")
synthetic_data$sentence_firstname_2 <- paste0(str_to_title(synthetic_data$name), example2)
synthetic_data$sentence_firstname_3 <- paste0(example3a, str_to_title(synthetic_data$name), example3b)

synthetic_data$sentence_firstname_perturbed_1 <- paste0(example1, str_to_title(synthetic_data$name_perturbed), ".")
synthetic_data$sentence_firstname_perturbed_2 <- paste0(str_to_title(synthetic_data$name_perturbed), example2)
synthetic_data$sentence_firstname_perturbed_3 <- paste0(example3a, str_to_title(synthetic_data$name_perturbed), example3b)

synthetic_data$sentence_firstname_perturbed_20_1 <- paste0(example1, str_to_title(synthetic_data$name_perturbed_20), ".")
synthetic_data$sentence_firstname_perturbed_20_2 <- paste0(str_to_title(synthetic_data$name_perturbed_20), example2)
synthetic_data$sentence_firstname_perturbed_20_3 <- paste0(example3a, str_to_title(synthetic_data$name_perturbed_20), example3b)

# __________last names -----
synthetic_data_lastnames$sentence_lastname_1 <- paste0(example1, str_to_title(synthetic_data_lastnames$lastname), ".")
synthetic_data_lastnames$sentence_lastname_2 <- paste0(str_to_title(synthetic_data_lastnames$lastname), example2)
synthetic_data_lastnames$sentence_lastname_3 <- paste0(example3a, str_to_title(synthetic_data_lastnames$lastname), example3b)

synthetic_data_lastnames$sentence_lastname_perturbed_1 <- paste0(example1, str_to_title(synthetic_data_lastnames$lastname_perturbed), ".")
synthetic_data_lastnames$sentence_lastname_perturbed_2 <- paste0(str_to_title(synthetic_data_lastnames$lastname_perturbed), example2)
synthetic_data_lastnames$sentence_lastname_perturbed_3 <- paste0(example3a, str_to_title(synthetic_data_lastnames$lastname_perturbed), example3b)

synthetic_data_lastnames$sentence_lastname_perturbed_20_1 <- paste0(example1, str_to_title(synthetic_data_lastnames$lastname_perturbed_20), ".")
synthetic_data_lastnames$sentence_lastname_perturbed_20_2 <- paste0(str_to_title(synthetic_data_lastnames$lastname_perturbed_20), example2)
synthetic_data_lastnames$sentence_lastname_perturbed_20_3 <- paste0(example3a, str_to_title(synthetic_data_lastnames$lastname_perturbed_20), example3b)

# __________full names -----
synthetic_data_fullnames$sentence_fullname_1 <- paste0(example1, str_to_title(synthetic_data_fullnames$fullname), ".")
synthetic_data_fullnames$sentence_fullname_2 <- paste0(str_to_title(synthetic_data_fullnames$fullname), example2)
synthetic_data_fullnames$sentence_fullname_3 <- paste0(example3a, str_to_title(synthetic_data_fullnames$fullname), example3b)

synthetic_data_fullnames$sentence_fullname_perturbed_1 <- paste0(example1, str_to_title(synthetic_data_fullnames$fullname_perturbed), ".")
synthetic_data_fullnames$sentence_fullname_perturbed_2 <- paste0(str_to_title(synthetic_data_fullnames$fullname_perturbed), example2)
synthetic_data_fullnames$sentence_fullname_perturbed_3 <- paste0(example3a, str_to_title(synthetic_data_fullnames$fullname_perturbed), example3b)

synthetic_data_fullnames$sentence_fullname_perturbed_20_1 <- paste0(example1, str_to_title(synthetic_data_fullnames$fullname_perturbed_20), ".")
synthetic_data_fullnames$sentence_fullname_perturbed_20_2 <- paste0(str_to_title(synthetic_data_fullnames$fullname_perturbed_20), example2)
synthetic_data_fullnames$sentence_fullname_perturbed_20_3 <- paste0(example3a, str_to_title(synthetic_data_fullnames$fullname_perturbed_20), example3b)

# __________prefixes -----
synthetic_data_lastnames$sentence_dr_1 <- paste0(example1, str_to_title(synthetic_data_lastnames$lastname_dr), ".")
synthetic_data_lastnames$sentence_dr_2 <- paste0(str_to_title(synthetic_data_lastnames$lastname_dr), example2)
synthetic_data_lastnames$sentence_dr_3 <- paste0(example3a, str_to_title(synthetic_data_lastnames$lastname_dr), example3b)

synthetic_data_lastnames$sentence_mr_1 <- paste0(example1, str_to_title(synthetic_data_lastnames$lastname_mr), ".")
synthetic_data_lastnames$sentence_mr_2 <- paste0(str_to_title(synthetic_data_lastnames$lastname_mr), example2)
synthetic_data_lastnames$sentence_mr_3 <- paste0(example3a, str_to_title(synthetic_data_lastnames$lastname_mr), example3b)

synthetic_data_lastnames$sentence_mrs_1 <- paste0(example1, str_to_title(synthetic_data_lastnames$lastname_mrs), ".")
synthetic_data_lastnames$sentence_mrs_2 <- paste0(str_to_title(synthetic_data_lastnames$lastname_mrs), example2)
synthetic_data_lastnames$sentence_mrs_3 <- paste0(example3a, str_to_title(synthetic_data_lastnames$lastname_mrs), example3b)

synthetic_data_lastnames$sentence_dr_perturbed_1 <- paste0(example1, str_to_title(synthetic_data_lastnames$lastname_perturbed_dr), ".")
synthetic_data_lastnames$sentence_dr_perturbed_2 <- paste0(str_to_title(synthetic_data_lastnames$lastname_perturbed_dr), example2)
synthetic_data_lastnames$sentence_dr_perturbed_3 <- paste0(example3a, str_to_title(synthetic_data_lastnames$lastname_perturbed_dr), example3b)

synthetic_data_lastnames$sentence_mr_perturbed_1 <- paste0(example1, str_to_title(synthetic_data_lastnames$lastname_perturbed_mr), ".")
synthetic_data_lastnames$sentence_mr_perturbed_2 <- paste0(str_to_title(synthetic_data_lastnames$lastname_perturbed_mr), example2)
synthetic_data_lastnames$sentence_mr_perturbed_3 <- paste0(example3a, str_to_title(synthetic_data_lastnames$lastname_perturbed_mr), example3b)

synthetic_data_lastnames$sentence_mrs_perturbed_1 <- paste0(example1, str_to_title(synthetic_data_lastnames$lastname_perturbed_mrs), ".")
synthetic_data_lastnames$sentence_mrs_perturbed_2 <- paste0(str_to_title(synthetic_data_lastnames$lastname_perturbed_mrs), example2)
synthetic_data_lastnames$sentence_mrs_perturbed_3 <- paste0(example3a, str_to_title(synthetic_data_lastnames$lastname_perturbed_mrs), example3b)

synthetic_data_lastnames$sentence_dr_perturbed_20_1 <- paste0(example1, str_to_title(synthetic_data_lastnames$lastname_perturbed_20_dr), ".")
synthetic_data_lastnames$sentence_dr_perturbed_20_2 <- paste0(str_to_title(synthetic_data_lastnames$lastname_perturbed_20_dr), example2)
synthetic_data_lastnames$sentence_dr_perturbed_20_3 <- paste0(example3a, str_to_title(synthetic_data_lastnames$lastname_perturbed_20_dr), example3b)

synthetic_data_lastnames$sentence_mr_perturbed_20_1 <- paste0(example1, str_to_title(synthetic_data_lastnames$lastname_perturbed_20_mr), ".")
synthetic_data_lastnames$sentence_mr_perturbed_20_2 <- paste0(str_to_title(synthetic_data_lastnames$lastname_perturbed_20_mr), example2)
synthetic_data_lastnames$sentence_mr_perturbed_20_3 <- paste0(example3a, str_to_title(synthetic_data_lastnames$lastname_perturbed_20_mr), example3b)

synthetic_data_lastnames$sentence_mrs_perturbed_20_1 <- paste0(example1, str_to_title(synthetic_data_lastnames$lastname_perturbed_20_mrs), ".")
synthetic_data_lastnames$sentence_mrs_perturbed_20_2 <- paste0(str_to_title(synthetic_data_lastnames$lastname_perturbed_20_mrs), example2)
synthetic_data_lastnames$sentence_mrs_perturbed_20_3 <- paste0(example3a, str_to_title(synthetic_data_lastnames$lastname_perturbed_20_mrs), example3b)


# _____generate combined dataset with all synthetic examples for first, last, and full names -----
# prepare datasets for binding
tmp1 <- synthetic_data
tmp1$name_type <- "firstname"
tmp2 <- synthetic_data_lastnames
tmp2$name_type <- "lastname"
# tmp3 <- synthetic_data_fullnames
# tmp3$name_type <- "fullname"

colnames(tmp1) <- str_replace_all(colnames(tmp1), pattern = c("firstname" = "name"))
colnames(tmp2) <- str_replace_all(colnames(tmp2), pattern = c("lastname" = "name"))
tmp2 <- rename(
  tmp2,
  count_total = count_agg,
  share_black = pctblack_agg,
  share_asian = pctapi_agg,
  share_aian = pctaian_agg,
  share_hispanic = pcthispanic_agg,
  share_white = pctwhite_agg,
  share_2race =  pct2prace_agg
)
tmp2[, c("share_black", "share_asian", "share_aian", "share_hispanic", "share_white", "share_2race")] <- tmp2[, c("share_black", "share_asian", "share_aian", "share_hispanic", "share_white", "share_2race")] / 100

tmp1[, setdiff(colnames(tmp2), colnames(tmp1))] <- NA
# tmp1[, setdiff(colnames(tmp3), colnames(tmp1))] <- NA
# tmp2[, setdiff(colnames(tmp1), colnames(tmp2))] <- NA
# tmp2[, setdiff(colnames(tmp2), colnames(tmp3))] <- NA
# tmp3[, setdiff(colnames(tmp1), colnames(tmp3))] <- NA
# tmp3[, setdiff(colnames(tmp2), colnames(tmp3))] <- NA
synth_data_comb <- rbind(tmp1, tmp2) #, tmp3)
rm(tmp1, tmp2) # , tmp3)
# view(colnames(synth_data_comb))

# turn into long dataset
synth_data_comb_long <- synth_data_comb %>%
  pivot_longer(!colnames(synth_data_comb)[!grepl(colnames(synth_data_comb), pattern = "perturbed")], names_to = "experiment", values_to = "perturbed_version")

# get unperturbed version for experiment in each row
synth_data_comb_long <- synth_data_comb_long %>%
  rowwise() %>%
  mutate(
    unperturbed_version = get(sub(experiment, pattern = "_perturbed(_20){0,1}", replacement = ""))
  ) %>%
  ungroup()

# create some flags for experiment type etc.
synth_data_comb_long$experiment_name_level <- 1
synth_data_comb_long$experiment_name_level[grepl(synth_data_comb_long$experiment, pattern = "sentence")] <- 0

synth_data_comb_long$experiment_perturbation1 <- 1
synth_data_comb_long$experiment_perturbation1[grepl(synth_data_comb_long$experiment, pattern = "20")] <- 0

synth_data_comb_long$experiment_prefix <- 0
synth_data_comb_long$experiment_prefix[grepl(synth_data_comb_long$experiment, pattern = "mr$|mrs$|dr$")] <- 1


# save synthetic data -----
write(time_machine_perturbed, file = here("data/time_machine_1st_chapter_perturbed.txt"))
write(time_machine_perturbed, "/Users/fcaro/Desktop/time_machine_1st_chapter_perturbed.txt")
write(time_machine_perturbed_20, file = here("data/time_machine_1st_chapter_perturbed_20.txt"))
write(time_machine_perturbed_20, "/Users/fcaro/Desktop/time_machine_1st_chapter_perturbed_20.txt")
fwrite(synthetic_data, here("data/synthetic_data_firstnames.csv"))
fwrite(synthetic_data, "/Users/fcaro/Desktop/synthetic_data_firstnames.csv")
fwrite(synthetic_data_lastnames, here("data/synthetic_data_lastnames.csv"))
fwrite(synthetic_data_lastnames, "/Users/fcaro/Desktop/synthetic_data_lastnames.csv")
fwrite(synthetic_data_fullnames, here("data/synthetic_data_fullnames.csv"))
fwrite(synthetic_data_fullnames, "/Users/fcaro/Desktop/synthetic_data_fullnames.csv")
fwrite(synth_data_comb_long, here("data/synthetic_data_comb_long.csv"))
fwrite(synth_data_comb_long, "/Users/fcaro/Desktop/synthetic_data_comb_long.csv")
gc()






