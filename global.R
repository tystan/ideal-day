

# ---- libs ----

library("shiny")
library("shinydashboard")
library("shinyjs")
library("bslib")
library("ggplot2")
library("ggthemes")
library("viridis")
library("plotly")
library("forcats")
library("dplyr")
library("lubridate")
library("readr")
library("tidyr")
library("forcats")
library("purrr")

library("compositions")

library("expm")

# install.packages("mosaic")
project_euclid <- mosaic::project
dotprod_euclid <- mosaic::dot
vlength_euclid <- mosaic::vlength
cosangl_euclid <- function(u, v) {
  dotprod_euclid(u, v) / (vlength_euclid(u) * vlength_euclid(v))
}
get_theta_degrees <- function(u, v) {
  180 * acos(cosangl_euclid(u, v)) / pi
}

# x1 <- c(1,0,0); x2 <- c(1,2,3); y1 <- c(3,4,5);
# vlength_euclid(x1)
# vlength_euclid(x2)
# vlength_euclid(y1)
# dotprod_euclid(x1, x2)
# dotprod_euclid(1 - x1, x2)
# 
# project_euclid(x1, x2, type='length')
# project_euclid(x2, x1, type='length')
# vlength_euclid(project_euclid(x1, x2))
# cosangl_euclid(x1, x2)
# cosangl_euclid(x1, -x1 + c(0, 1, 0))
# cosangl_euclid(x1, x2)
# cosangl_euclid(x2, x1)
# project_euclid(x1, x2)
# project_euclid(x2, x1)

# cosangl_euclid(x1, -x1 - c(0, 0.5, -0.5))
# get_theta_degrees(x1, -x1 - c(0, 0.5, -0.5))
# cosangl_euclid(-x2, x1)
# get_theta_degrees(-x2, x1)
# cosangl_euclid(x2, x1)
# get_theta_degrees(x2, x1)
# cosangl_euclid(x2, -x1)
# get_theta_degrees(x2, -x1)
# cosangl_euclid(x1, x1)
# get_theta_degrees(x1, x1)
# cosangl_euclid(x2, y1)
# get_theta_degrees(x2, y1)

# cosangl_euclid(x1, c(0, 1, 0))

use_plotly <- TRUE

effective_zero <- 1e-12


# ---- data ----




cmp_nms <- c("sleep", "sb", "lpa", "mvpa")
ilr_nms <- paste0("ilr", 1:(length(cmp_nms) - 1))

cmp_default <- 
  as.data.frame(
    1440 * 
      matrix(
        1 / length(cmp_nms), 
        ncol = length(cmp_nms), 
        dimnames = list(NULL, cmp_nms)
      )
  )
ilr_default <- 
  as.data.frame(
    matrix(
      0, 
      ncol = length(ilr_nms), 
      dimnames = list(NULL, ilr_nms)
    )
  )

# sequential binary partition matrix 
sbp4 <- 
  matrix(
    c(
      +1,  0,  0,
      -1, +1,  0,
      -1, -1, +1,
      -1, -1, -1
    ),
    byrow = TRUE, 
    ncol = 3,
    dimnames = list(cmp_nms, ilr_nms)
  )

psi4 <- compositions::gsi.buildilrBase(sbp4)



default_covs_df <- read_rds(file = "dat/default_covs.rds")
# print(default_covs_df)
mod_form_cplx   <- read_rds(file = "dat/mod_form_cplx.rds")
x_dsgn_meta     <- read_rds(file = "dat/x_dsgn_meta.rds")
cols_rm_names   <- x_dsgn_meta$var_and_lvl[x_dsgn_meta$col_inc == 0L]
(cols_rm_names  <- cols_rm_names[!(cols_rm_names %in% "(Intercept)")]) # intercept should stay
xlvl_lst        <- read_rds(file = "dat/xlvl_lst.rds")
best_mod_coefs  <- read_rds(file = "dat/beta_lasso_mat.rds")
in_contour_grid <- read_rds(file = "dat/grid_in_contours.rds")
m_and_v_ilrs    <- read_rds(file = "dat/m_and_v_ilrs.rds")

m_and_v_ilrs <- 
  in_contour_grid %>%
  distinct(strata_id, sex, age_c, bmi_c) %>%
  inner_join(., m_and_v_ilrs, c("sex", "age_c", "bmi_c"))
  
m_and_v_ilrs %>%
  distinct(strata_id, sex, age_c, bmi_c) %>%
  knitr::kable(.)

# |strata_id |sex    |age_c |bmi_c  |
# |:---------|:------|:-----|:------|
# |A         |Female |<65   |not_ow |
# |B         |Female |<65   |ow     |
# |C         |Female |65+   |not_ow |
# |D         |Female |65+   |ow     |
# |E         |Male   |<65   |not_ow |
# |F         |Male   |<65   |ow     |
# |G         |Male   |65+   |not_ow |
# |H         |Male   |65+   |ow     |

m_and_v_ilrs <- 
  m_and_v_ilrs %>%
  select(-sex, -age_c, -bmi_c) 




in_contour_grid <- 
  in_contour_grid %>% 
  select(strata_id, all_of(ilr_nms), all_of(cmp_nms), cont_perc)





get_strata_id <- function(s, a, b) {
  case_when(
    (s == "Female") & (a <  65) & (b <  30) ~ "A",
    (s == "Female") & (a <  65) & (b >= 30) ~ "B",
    (s == "Female") & (a >= 65) & (b <  30) ~ "C",
    (s == "Female") & (a >= 65) & (b >= 30) ~ "D",
    (s ==   "Male") & (a <  65) & (b <  30) ~ "E",
    (s ==   "Male") & (a <  65) & (b >= 30) ~ "F",
    (s ==   "Male") & (a >= 65) & (b <  30) ~ "G",
    (s ==   "Male") & (a >= 65) & (b >= 30) ~ "H",
    # placeholder random choice to cover other sex vals
    TRUE                                    ~ "C" 
  )
}
# get_strata_id(s = "Male", a = 64, b = 21)
# get_strata_id(s = "Female", a = 90, b = 150)
# get_strata_id(s = "Other", a = 30, b = 25)

get_strata_grid <- function(s, a, b) {
  
  wch_strata <- get_strata_id(s = s, a = a, b = b)
  
  special_print_for_console(
    "The corresponding strata ID (A to H) based on the input",
    paste0("(sex, age, bmi) = ", sprintf("(%s, %2.1f, %2.1f)", s, a, b)," is:")
  )
  print(wch_strata)
  
  in_contour_grid %>% 
    dplyr::filter(strata_id == wch_strata)
  
}
# get_strata_grid(s = "Male", a = 64, b = 21)


get_strata_m_and_v <- function(s, a, b) {
  
  wch_strata <- get_strata_id(s = s, a = a, b = b)
  
  m_and_v_ilrs %>% 
    dplyr::filter(strata_id == wch_strata)
  
}
# get_strata_m_and_v(s = "Male", a = 64, b = 21)
# get_strata_m_and_v(s = "Male", a = 64, b = 21)[["m"]][[1]]
# get_strata_m_and_v(s = "Male", a = 64, b = 21)[["v"]][[1]]
# m_and_v_ilrs %>% 
#   dplyr::filter(strata_id == "C") %>%
#   select(-strata_id) 

get_col_types <- function(d) sapply(d, function(x) class(x)[1])

check_colnames_equiv <- function(d1, d2) {
  
  cn1 <- colnames(d1)
  cn2 <- colnames(d2)
  if (!all(cn1 %in% cn2, cn2 %in% cn1)) {
    stop("colnames differ (possibly in length as well)")
  }
  
  return(TRUE)
  
}

force_col_equal <- function(d1, d2) {
  
  stopifnot(check_colnames_equiv(d1, d2))
  
  cn1 <- colnames(d1)
  cn2 <- colnames(d2)
  if (!all(cn1 %in% cn2, cn2 %in% cn1)) {
    stop("colnames differ (possibly in length as well)")
  }
  d2 <- d2[, match(cn1, cn2, nomatch = 0)]
  ct1 <- get_col_types(d1)
  ct2 <- get_col_types(d2)
  
  if (!all(ct1 == ct2)) {
    diffi <- which(ct1 != ct2)
    message("cols with index (in d1) differ from d2: ", paste(diffi, collapse = "|"))
    stop("col-types differ")
  } else {
    return(d2)
  }
  
}


mk_ilr <- function(x) {
  
  x <- force_col_equal(cmp_default, x)
  if (!all(colnames(x) == cmp_nms)) {
    stop("x input (cmps) needs to have only compositional parts columns")
  }
  
  z <- compositions::ilr(x, V = psi4)
  
  if (nrow(x) == 1) { # compositions drops data.frame property... :-(
    # make it a row matrix
    z <- t(unclass(z)) 
  } 
  
  # convert as usual
  z <- as.data.frame(z)
  
  
  return(z)
  
}
# test_cmp <- in_contour_grid[1:20, cmp_nms[4:1]]
# force_col_equal(cmp_default, test_cmp)
# mk_ilr(test_cmp)

mk_comp <- function(z) {
  
  z <- force_col_equal(ilr_default, z)
  if (!all(colnames(z) == ilr_nms)) {
    stop("z input (ilrs) needs to have only ilr columns")
  }
  
  x <- compositions::ilrInv(z, V = psi4)
  
  if (nrow(z) == 1) { # compositions drops data.frame property... :-(
    # make it a row matrix
    x <- t(unclass(x)) 
  } 
  
  x <- 1440 * as.data.frame(x)
  
  return(x)
  
}
# test_comp <- in_contour_grid[1:20, c("ilr2", "ilr3", "ilr1")]
# force_col_equal(ilr_default, test_comp)
# mk_comp(test_comp)

mk_predictor_df <- function(cmp_df, cov_df = default_covs_df) {
  
  cmp_df <- as.data.frame(cmp_df)
  cov_df <- as.data.frame(cov_df)
  n_cmp <- nrow(cmp_df)
  # predictor_df <- map_dfr(seq_len(n_cmp), \(x) cov_df)
  predictor_df <- bind_rows(map(1:n_cmp, \(x) cov_df))
  ilr_df <- mk_ilr(cmp_df)
  predictor_df$ilr <- as.matrix(ilr_df)
  ilr_sq_df <- poly2(ilr_df)
  predictor_df$ilr_sq <- as.matrix(ilr_sq_df)
  
  return(predictor_df)
  
}
# test_cmp <- in_contour_grid[1:20, cmp_nms[4:1]]
# mk_predictor_df(test_cmp)

mk_pred_over_ilrs <- function(predictor_df, which_outc = "globalcog") {
  
  out_pred <-
    get_pred_w_ref_lvls(
      predictor_df, 
      mod_form_cplx, 
      beta_mat = best_mod_coefs,
      lvls = xlvl_lst, 
      frm_rm_terms = cols_rm_names
    )[, which_outc]
  
  
  return(out_pred)
  
  
}
# test_cmp <- in_contour_grid[1:20, cmp_nms[4:1]]
# mk_pred_over_ilrs(mk_predictor_df(test_cmp))

get_opt_cmp_from_preds <- function(predictor_df, propn = 0.05, which_outc = "globalcog") {
  
  y_hat <- mk_pred_over_ilrs(predictor_df, which_outc = which_outc) 
  
  tmp_opt <-
    predictor_df$ilr %>%
    bind_cols(., tibble(y_hat = y_hat)) %>%
    arrange(desc(y_hat)) %>%
    dplyr::filter(row_number() < (propn * n()))
  
  # print(tmp_opt)
  
  y_hat_quant <- 
    tmp_opt %>% 
    pull(y_hat) %>% 
    quantile(., 0.5) %>% 
    unname(.)
  
  perc_str <- sprintf("%2.1f%%", 100 * propn)
  special_print_for_console(
    "'y_hat_quant' below is the _median_ predicted outcome from the top", 
    paste(perc_str, "of predicted values in the strata defined time-use fencing")
  )
  print(c(y_hat_quant = y_hat_quant))
  
  tmp_opt <- 
    tmp_opt %>% 
    summarise(across(everything(), mean))
  
  special_print_for_console(
    "'y_hat_ave' below is the _mean_ predicted outcome from the top", 
    paste(perc_str, "of predicted values in the strata defined time-use fencing")
  )
  print(c(y_hat_ave = tmp_opt[["y_hat"]]))
  
  tmp_opt <- 
    tmp_opt %>%
    select(all_of(ilr_nms)) %>%
    mk_comp(.) 
  
  special_print_for_console(
    "The below is the _compositional mean_ of time-use compositions corresponding to the top",
    paste(perc_str, "of predicted values in the strata defined time-use fencing")
  )
  print(tmp_opt)
  
  tmp_opt <- 
    tmp_opt %>% 
    mutate(across(everything(), \(x) round(x / 60, 1))) %>%
    mutate(y_hat = y_hat_quant)
  
  # %>% arrange(desc(sb), desc(mvpa)) 
  
  # tmp_opt[1, ]
  
  # return(tmp_opt)
  return(list(opt_cmp = tmp_opt, y_dist = y_hat))
  
}





mk_cov_df_from_ui <- function(age, sex, bmi, edu, hear, iso, alc, smk, binj, hbp, dep, t2d, verbose = FALSE) {
  
  out_df <- default_covs_df
  
  out_df[["age"]] <- as.numeric(age)
  out_df[["sex"]] <- as.character(sex)
  out_df[["bmi"]] <- as.numeric(bmi)
  out_df[["highestqual"]] <- as.character(edu)
  out_df[["hearing"]] <- as.character(hear)
  out_df[["isolation"]] <- as.character(iso)
  out_df[["alcohol"]] <- as.character(alc)
  out_df[["smoking"]] <- as.character(smk)
  out_df[["brninj"]] <- as.numeric(binj)
  out_df[["hypertension"]] <- as.character(hbp)
  out_df[["depression"]] <- as.character(dep)
  out_df[["diabetes"]] <- as.character(t2d)
  
  if (verbose) {
    print(as_tibble(out_df))
  }

  return(out_df)
  
}
  
  

mk_cmp_df_from_ui <- function(sleep, sb, lpa, mvpa, strata_id = NULL) {
  
  out_df <- cmp_default
  
  out_df[["sleep"]] <- as.numeric(sleep)
  out_df[["sb"]] <- as.numeric(sb)
  out_df[["lpa"]] <- as.numeric(lpa)
  out_df[["mvpa"]] <- as.numeric(mvpa)
  
  out_df[, cmp_nms] <- 
    1440 * out_df[, cmp_nms] / sum(out_df[, cmp_nms])
  
  # if (!is.null(strata_id)) {
  #   cat("NOTE the mk_cmp_df_from_ui() composition:\n")
  #   print(as_tibble(out_df))
  #   cat("is within the 0.8 percentile?:\n")
  #   mv_lst <- m_and_v_ilrs %>% dplyr::filter(strata_id == strata_id)
  #   print(is_within_constraint(
  #     mk_ilr(out_df[1, ]), 
  #     mv_lst[["m"]][[1]], 
  #     mv_lst[["v"]][[1]], 
  #     max_p = 0.8
  #   ))
  # 
  # }
  ### can't use below as this function is used for other quantities than "current"
  # special_print_for_console(
  #   "This is the current time use combination being used for current day prediction:"
  # )
  # print(out_df)
  
  
  return(out_df)
  
}
# mk_cmp_df_from_ui(sleep = 1, sb = 1, lpa = 1, mvpa = 1)


# ---- consts ----



ideal_day_vec <- c("Sleep" = 8, "Light PA" = 4, "Sit" = 11, "Mod-vig PA" = 1)


daytype_cols <-
  c(
    "My current day" = "#a6a6a6",
    "My 'ideal' day" = "#00b0f0"
  )

get_sgn_hex_col <- function(x) {
  ifelse(x < 0, "#ff3200", ifelse(x > 0, "#00a349", "#000000")) 
}
add_sgn <- function(x) {
  paste0(ifelse(x < 0, "", ifelse(x > 0, "+", "")), sprintf("%2.1f", x))
}


h_to_hm <- function(hours_decimal) {
  h <- floor(hours_decimal)
  m <- round(60 * (hours_decimal - h), 0)
  return(sprintf("(%2.0fh %2.0fm)", h, m))
}


cog_outc_choices <- colnames(best_mod_coefs)
names(cog_outc_choices) <- c(
  "Global cognition (my overall cognitive function)",
  "Memory",
  "Processing speed",
  "Executive function",
  "Reasoning"
)
cog_outc_choices

cog_outc_choices_alt <- cog_outc_choices
names(cog_outc_choices_alt) <- gsub(" \\(.*\\)", "", names(cog_outc_choices_alt))
cog_outc_choices_alt


# demos_age_choices <- paste(c(
#   # "<=45",
#   # "46-55",
#   # "56-65",
#   # ">=66"
#   "<65",
#   "≥65"
# ), "years")

demos_sex_choices <- c(
  "Female" = "Female",
  "Male" = "Male",
  "Other" = "Female"
  # "Prefer not to say"
)

demos_edu_choices <- c(
  "University/college degree" = "college/university",
  "Other professional qualification\n(e.g., nursing or teachers’ college)" = "other professional qual",
  "Certificate III, IV or diploma" = "cert III/diploma",
  "High school (≥ Year 10)" = "high school",
  "Other/prefer not to say" = "unknown"
)


demos_hyp_choices <- c(
  "Yes" = "yes",
  "No" = "no",
  "Unknown" = "unknown"
)

demos_t2d_choices <- c(
  "Yes" = "yes",
  "No" = "no",
  "Unknown" = "unknown"
)

demos_dep_choices <- c(
  "Yes" = "yes",
  "No" = "no",
  "Unknown" = "unknown"
)

demos_binj_choices <- c(
  "Yes" = 1,
  "No" = 0,
  "Unknown" = 0
)

demos_hear_choices <- c(
  "Yes" = "yes",
  "No" = "no",
  "Unknown" = "unknown"
)

demos_lone_choices <- c(
  "Yes" = "yes",
  "No" = "no",
  "Unknown" = "unknown"
)

demos_alc_choices <- c(
  "3 or more times per week" = "often/very often",
  "2 or less times per week\n(includes no alcohol)" = "sometimes/never"
)

demos_smok_choices <- c(
  "I currently smoke" = "current",
  "I previously smoked, but not anymore" = "previous",
  "I have never smoked" = "never"
)







# ---- funcs ----


special_print_for_console <- function(...) {
  cat("\n\n#####################################\n")
  cat(paste("###", paste(c(...), collapse = "\n### ")), sep = "")
  cat("\n#####################################\n\n")
}
# special_print_for_console("dfgth", "dfghsfgdh", "Ddfgdfg")


get_countour_propn <- function(z, m_z, v_z) {
  pchisq(q = mahalanobis(z, m_z, v_z), df = length(z))
}

is_within_constraint <- function(z, m_z, v_z, max_p = 0.8) {
  get_countour_propn(z, m_z, v_z) <= max_p
}
# is_within_constraint(rep(0, 3), rep(0, 3), diag(3))
# is_within_constraint(rep(1, 3), rep(0, 3), diag(3))
# is_within_constraint(rep(2, 3), rep(0, 3), diag(3))



check_good_delta <- function(x_new, x_cur, x_opt, p = 1 / 3) {
  x_new >= (p * x_opt + (1 - p) * x_cur) 
}
### testing
# check_good_delta(1.4, 1, 2)
# check_good_delta(1.2, 1, 2)
# check_good_delta(-1.6, -2, -1)
# check_good_delta(-1.7, -2, -1)
# check_good_delta(-99, -200, 100)
# check_good_delta(-100, -200, 100)
# check_good_delta(-101, -200, 100)

check_bad_delta <- function(x_new, x_cur, x_opt, p = 1 / 3) {
  x_new <= ((1 + p) * x_cur - p * x_opt ) 
}
### testing
check_bad_delta(0.6, 1, 2)
check_bad_delta(0.7, 1, 2)
check_bad_delta(-1.2, 1, 2)
check_bad_delta(-2.6, -2, -1)
check_bad_delta(-2.3, -2, -1)
check_bad_delta(-299, -200, 100)
check_bad_delta(-300, -200, 100)
check_bad_delta(-301, -200, 100)

calc_bmi <- function(w_kg, h_cm) {
  as.numeric(w_kg) / (as.numeric(h_cm) / 100)^2
}
# calc_bmi("90", "190")

is_atom_fct <- function(x) {
  x_class <- class(x)
  return(is.atomic(x) & ("factor" %in% x_class))
}
is_atom_chr <- function(x) {
  x_class <- class(x)
  return(is.atomic(x) & ("character" %in% x_class))
}
is_atom_fct_or_char <- function(x) { 
  return(is_atom_fct(x) | is_atom_chr(x)) 
}


get_lvls <- function(x) {
  x_lvls <- NULL
  if (is_atom_chr(x)) {
    x_lvls <- levels(factor(x)) # is char so convert to factor
  } else if (is_atom_fct(x)) {
    x_lvls <- levels(x)         # is already factor, extract lvls
  }
  return(x_lvls)
}


sanitise_ilrs <- function(x) {
  
  if ("rmult" %in% class(x)) {
    class(x) <- NULL # remove "rcomp" class, will either result in numeric vector or matrix
    attr(x, "orig") <- NULL # remove original composition info (issue with indexes)
    if ("numeric" %in% class(x)) { # if vector turn into 1 row matrix
      x <- matrix(x, nrow = 1, dimnames = list(NULL, names(x)))
    }
  }
  return(x)
  
}

poly2 <- function(x, just_names = FALSE) {
  
  # make sure is matrix
  x <- sanitise_ilrs(x)
  
  n <- ncol(x) 
  cnames <- colnames(x)
  
  if (is.null(cnames)) {
    cnames <- paste0("c", 1L:n)
  }
  
  # get all tuples of (j,k) where j <= k
  tups <- subset(expand.grid(j = 1:n, k = 1:n), j <= k)
  tups <- tups[order(tups$j, tups$k), ] # make sure consistent ordering
  j <- tups$j
  k <- tups$k
  
  # drop = FALSE is to make sure 1 row matrices don't become vectors
  sq_out <- x[, j, drop = FALSE] * x[, k, drop = FALSE] 
  colnames(sq_out) <- paste0(cnames[j], ":", cnames[k])
  
  if (just_names) {
    return(colnames(sq_out))
  } else {
    return(sq_out)
  }
  
}


get_linearalg_pred_dat <- function(df_dat, frm, beta_mat = beta_lasso_mat, frm_rm_terms = NULL) {
  
  x_0 <- model.matrix(delete.response(terms(frm)), data = df_dat)
  
  if (!is.null(frm_rm_terms)) {
    x_0 <- x_0[, !(colnames(x_0) %in% frm_rm_terms), drop = FALSE]
  }
  
  # frm_rm_terms_beta <- frm_rm_terms[!(frm_rm_terms == "(Intercept)")]
  # beta_extras <- rownames(beta_mat) %in% frm_rm_terms_beta
  # if (sum(beta_extras) > 0) {
  #   beta_mat <- beta_mat[!beta_extras, , drop = FALSE]
  # }
  
  # Add back intercept term if deleted above
  if (sum(colnames(x_0) %in% "(Intercept)") < effective_zero) { # more general
    x_0 <- cbind("(Intercept)" = 1, x_0)
  }
  
  if (ncol(x_0) != nrow(beta_mat)) {
    message("Number of columns in design matrix is not the same as the number of rows in beta")
    print(paste(ncol(x_0), "!=", nrow(beta_mat)))
    stop("exiting calc because of incombatable X and beta matrices")
  } else if (any(colnames(x_0) != rownames(beta_mat))) {
    message("columns names of design matrix and model betas do not have the same names")
    print(kable(cbind.data.frame(
      column_no = 1:ncol(x_0),
      designmat_cns = colnames(x_0), 
      beta_rms = rownames(beta_mat)
    )[colnames(x_0) != rownames(beta_mat), , drop = FALSE])) 
    stop("exiting calc because of incombatable coefficient names")
  }
  
  pfit_linearalg <- x_0 %*% beta_mat
  ### keep as matrix
  # pfit_linearalg <- pfit_linearalg[1:nrow(df_dat), ]
  
  return(pfit_linearalg) # note this is a matrix now, not a numeric vector
  
}



incorperate_xlev_in_df <- function(df_dat, lvls) {
  col_indx <- match(names(lvls), colnames(df_dat), nomatch = 0)
  if (!all(col_indx > 0)) {
    stop("Input data.frame does not have all the columns listed in the lvls input")
  }
  df_dat_fctised <- df_dat[, col_indx]
  df_cnames <- colnames(df_dat_fctised)
  if (any(df_cnames != names(lvls))) { ## triple check
    stop("Ty's column matching logic has failed")
  }
  for (j in 1:ncol(df_dat_fctised)) {
    col_j <- df_dat_fctised[[j]]
    if (is_atom_chr(col_j)) {
      df_dat_fctised[[j]] <- factor(col_j, levels = lvls[[j]])
      if (!all(col_j == as.character(df_dat_fctised[[j]]))) { ## triple check
        stop("conversion of character column ", df_cnames[j], " to factor has failed")
      }
    } else if (is_atom_fct(col_j)) {
      if (!all(levels(col_j) == lvls[[j]])) { ## triple check
        stop("discrepancy between provided levels in ", df_cnames[j], " and lvls input")
      }
      df_dat_fctised[[j]] <- factor(col_j, levels = lvls[[j]])
      if (!all(as.character(col_j) == as.character(df_dat_fctised[[j]]))) { ## triple check
        stop("conversion of factor column ", df_cnames[j], " to factor (with reordered labels) has failed")
      }
    } else if (!(is.null(get_lvls(col_j)) & is.null(lvls[[j]]))) { ## triple check
      stop("non-(chr|fct) column ", df_cnames[j], " has corresponding non-null levels in lvls input (func failed)")
    }
  }
  return(df_dat_fctised)
}


get_pred_w_ref_lvls <- function(df_dat, frm, beta_mat = beta_lasso_mat, frm_rm_terms = NULL, lvls = xlvl_lst) {
  df_dat <- incorperate_xlev_in_df(df_dat, lvls)
  return(get_linearalg_pred_dat(df_dat, frm, beta_mat = beta_mat, frm_rm_terms = frm_rm_terms))
}










# ---- ui_elements ----

# success_cards <- 
#   list(
#     
#   )


cards <- list(
  card(
    full_screen = TRUE,
    fill = FALSE,
    card_header("Choose which cognitive outcome you’d like to focus on:"),
    radioButtons(inputId = "cog_outc", label = "", choices = cog_outc_choices, inline = FALSE, width = "100%")
  ),
  card(
    full_screen = TRUE,
    fill = FALSE,
    card_header("Tell us about yourself:"),
    layout_columns(
      col_widths = c(3, 3, 6),
      # radioButtons(inputId = "demos_age", label = "What is your current age?", choices = demos_age_choices),
      numericInput(inputId = "demos_age", label = "What is your current age (years)?", 
                   width = "100%", 
                   value = 70, step = 5, min = 30, max = 105),
      radioButtons(inputId = "demos_sex", label = "What is your sex?", 
                   choices = demos_sex_choices, 
                   selected = demos_sex_choices[names(demos_sex_choices) == "Female"]),
      radioButtons(inputId = "demos_edu", label = "What is your highest qualification?", 
                   choices = demos_edu_choices, 
                   selected = demos_edu_choices[demos_edu_choices == "high school"]),
    ),
    layout_columns(
      col_widths = c(3, 3, 6),
      numericInput(inputId = "demos_wei", label = "What is your current weight (kg)?", 
                   width = "100%", 
                   value = 80, step = 5, min = 30, max = 200),
      numericInput(inputId = "demos_hei", label = "What is your height (cm)?", 
                   width = "100%", 
                   value = 170, step = 5, min = 30, max = 230),
      p()
    ),
  ),
  card(
    full_screen = TRUE,
    card_header("Tell us about your health:"),
    layout_columns(
      radioButtons(
        inputId = "demos_hyp", label = "Have you been diagnosed with high blood pressure?", 
        choices = demos_hyp_choices, selected = demos_hyp_choices[demos_hyp_choices == "no"]
      ),
      radioButtons(
        inputId = "demos_t2d", label = "Have you been diagnosed with Type 2 Diabetes?", 
        choices = demos_t2d_choices, selected = demos_t2d_choices[demos_t2d_choices == "no"]
      ),
      radioButtons(
        inputId = "demos_dep", label = "Have you ever had a time where you felt depressed or down for at least one week?", 
        choices = demos_dep_choices, selected = demos_dep_choices[demos_dep_choices == "no"]
      )
    ),
    p(),
    layout_columns(
      radioButtons(
        inputId = "demos_cran", label = "Have you been diagnosed with a concussion or other traumatic brain injury?", 
        choices = demos_binj_choices, selected = demos_binj_choices[names(demos_binj_choices) == "No"]
      ),
      radioButtons(
        inputId = "demos_hear", label = "Do you have any difficulty with your hearing?", 
        choices = demos_hear_choices, selected = demos_hear_choices[demos_hear_choices == "no"]
      ),
      radioButtons(
        inputId = "demos_lone", label = "In your life currently, do you often feel lonely or isolated?", 
        choices = demos_lone_choices, selected = demos_lone_choices[demos_lone_choices == "no"])
      
    ),
    p(),
    layout_columns(
      radioButtons(
        inputId = "demos_alc", label = "How often do you consume alcohol?", 
        choices = demos_alc_choices, selected = demos_alc_choices[length(demos_alc_choices)]
      ),
      radioButtons(
        inputId = "demos_smok", label = "What is your history of tobacco smoking?", 
        choices = demos_smok_choices, selected = demos_smok_choices[length(demos_smok_choices)]
      )
    )
  ),
  card(
    full_screen = TRUE,
    card_header("On an average day, how much time do you spend in the following behaviours?"),
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      p("Sleep (hrs):"),
      p("Sitting (hrs):"),
      p("Light physical activity (hrs):"),
      p("Moderate-vigorous physical activity (hrs):"),
      numericInput(inputId = "tu_slp", label = "", 
                       width = "50%", 
                       value = 7.5, step = 0.5, min = 1, max = 23),
      numericInput(inputId = "tu_sit", label = "", 
                       width = "50%", 
                   value = 13, step = 0.5, min = 1, max = 23),
      numericInput(inputId = "tu_lpa", label = "", 
                       width = "50%", 
                       value = 2.8, step = 0.2, min = 1, max = 23),
      numericInput(inputId = "tu_vpa", label = "", 
                       width = "50%", 
                   value = 0.2, step = 0.2, min = 0, max = 12),
      uiOutput(outputId = "tu_slp_out"),
      uiOutput(outputId = "tu_sit_out"),
      uiOutput(outputId = "tu_lpa_out"),
      uiOutput(outputId = "tu_vpa_out")
    )
  ),
  card(
    full_screen = TRUE,
    card_header("Using the interactive sliders below, visualise what happens to your cognitive function when you make small changes in your day"),
    layout_columns(
      col_widths  = c(7, 5),
      box(
        width = 12,
        sliderInput(
          "slide_slp", "Sleep (mins)", 
          value = 0, min = -60, max = 60, ticks = FALSE,
          width = "100%"
        ), # sliderInput.Sleep
        sliderInput(
          "slide_sed", "Sedentary behaviour (mins)", 
          value = 0, min = -60, max = 60, ticks = FALSE,
          width = "100%"
        ), # sliderInput.DomSoc
        sliderInput(
          "slide_lpa", "Light physical activity (mins)", 
          value = 0, min = -60, max = 60, ticks = FALSE,
          width = "100%"
        ), # sliderInput.PA
        sliderInput(
          "slide_vpa", "Moderate-vigorous physical activity (mins)", 
          value = 0, min = -60, max = 60, ticks = FALSE,
          width = "100%"
        )
      ),
      
      
      uiOutput("ui1")
      
    )
  )
)

