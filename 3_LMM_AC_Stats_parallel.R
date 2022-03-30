## ------------------------------------------------------------------------------------------------------------------------------------
# Setup ---------------------------------------------------------------------------------
library(reshape2)
library(lme4)

library(parallel)
library(doParallel)
library(foreach)

# Directories
dir_project <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1-9kU5O4-bU0AeEZKukSvz4jbvolppJGG/ddpham/Prewhitening-Paper" # Mac Pro
# dir_project <- "G:/.shortcut-targets-by-id/1-9kU5O4-bU0AeEZKukSvz4jbvolppJGG/ddpham/Prewhitening-Paper" # Damon personal

# Where to save the first part of this analysis (this script)
dir_agg <- file.path(dir_project, "AC1_agg")
dir_lmm <- file.path(dir_project, "AC1_lmm")

# 45 HCP retest subjects
subjects <- c(
  103818, 187547,
  105923, 192439,
  111312, 194140,
  114823, 195041,
  115320, 200109,
  122317, 200614,
  125525, 204521,
  130518, 250427,
  135528, 287248,
  137128, 341834,
  139839, 433839,
  143325, 562345,
  144226, 599671,
  146129, 601127,
  149337, 627549,
  149741, 660951,
  151526, 662551,
  158035, 783462,
  169343, 859671,
  172332, 861456,
  175439, 877168,
  177746, 917255,
  185442
)

# Remove subjects excluded from Fatma's original analysis.
subjects_rm <- c(
  144226, 627549,
  660951, 169343,
  177746
)
# 877168 has missing data for Language; see 0_Check
subjects <- setdiff(subjects, subjects_rm)

# tasks <- c("emotion", "gambling", "language", "motor", "relational", "social")
tasks <- c("emotion", "gambling", "motor", "relational")

missing <- readRDS("0_MissingFiles.rds")
missing[128,"emotion"] <- 3 #Truncated CIFTI

nVox <- 10578

## ------------------------------------------------------------------------------------------------------------------------------------
iters <- expand.grid(
  pw_FWHM=c(5, Inf),
  pw_order=c(0, 1, 3, 6),
  meas=c( "aci", "ar6c", "var", "acf1", "ar6v")
)

time <- Sys.time()

#for (ii in rev(seq(nrow(iters)))) {
for (ii in seq(8, 1)) {

  # Prep ------------------------------------------------------------------------------
  # Get iteration info.
  pw_FWHM <- iters[ii, "pw_FWHM"]
  pw_order <- iters[ii, "pw_order"]
  meas <- iters[ii, "meas"]

  out_fname <- file.path(dir_lmm, paste0(meas, "_pwFWHM-", pw_FWHM, "_pwO-", pw_order, ".rds"))
  if (file.exists(out_fname)) { next }
  cat(basename(out_fname))

  dat <- readRDS(file.path(dir_agg, paste0(meas, ".rds")))
  dat <- dat[,,,,as.character(pw_FWHM),as.character(pw_order),,]

  # 9 rows for string the Fixed Effects (E, G, M, R, RL, E:dHRF, G:dHRF, M:dHRF, R:dHRF)
  fixed_fx <- array(NA, dim= c(nVox, 9))
  # 37 rows for storing Random Effect's std. dev.
    # RE of dHRF: task & task = 36 [(8*9)/2 for diaogonal & its lower triangular]
    # RE's residual = 1
  var_cor <- array(NA, dim = c(nVox, 37))

  cores <- parallel::detectCores()
  nCores <- cores[1] - 8
  cluster <- parallel::makeCluster(nCores, outfile="")
  doParallel::registerDoParallel(cluster)

  vv <- 1
  dat_vv <- dat[,,,,,vv]
  df_vv <- reshape2::melt(dat_vv, varnames=names(dimnames(dat_vv)))
  df_vv$dHRF <- (df_vv$HRF=="dHRF")
  q <- lme4::lmer(value ~ -1 + task + acquisition + dHRF:task  + (-1 + dHRF:task | subject) + (-1 + task | subject), data = df_vv)
  fixed_fx[vv,] <- c(lme4::fixef(q))
  var_cor[vv,] <- c((as.data.frame(lme4::VarCorr(q))$sdcor))
  colnames(fixed_fx) <- names(lme4::fixef(q))
  colnames(var_cor) <- apply(as.data.frame(lme4::VarCorr(q))[,seq(3)], 1, paste, collapse="-")

  merge_vox <- function(x){list(fixed_fx=do.call(rbind, lapply(x, `[[`, "fixed_fx")), var_cor=rbind(lapply(x, `[[`, "var_cor")))}
  q <- foreach::foreach(vv = seq(2, nVox)) %dopar% {
    if (vv %% 100 == 0) { print(vv) }
    dat_vv <- dat[,,,,,vv]
    df_vv <- reshape2::melt(dat_vv, varnames=names(dimnames(dat_vv)))
    df_vv$dHRF <- (df_vv$HRF=="dHRF")
    q <- lme4::lmer(value ~ -1 + task + acquisition + dHRF:task  + (-1 + dHRF:task | subject) + (-1 + task | subject), data = df_vv)
    list(
      fixed_fx=(lme4::fixef(q)),
      var_cor=c((as.data.frame(lme4::VarCorr(q))$sdcor))
    )
  }
  fixed_fx[seq(2, nVox),] <- do.call(rbind, lapply(q, `[[`, "fixed_fx"))
  var_cor[seq(2, nVox),] <- do.call(rbind, lapply(q, `[[`, "var_cor"))
  saveRDS(list(fixed_fx = fixed_fx, var_cor = var_cor), out_fname)

  stopCluster(cluster)

  cat("\n")
  print(Sys.time() - time); time <- Sys.time()
}

