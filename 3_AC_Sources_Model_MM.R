library(reshape2)

library(lme4)
stopifnot(packageVersion("lme4") >= "1.1.26")

# Directories
#dir_data <- "/Users/Shared/Lab_Data"
#dir_project <- "/Users/ddpham/Desktop/HCP-AC"
dir_project <- "~/Google Drive/My Drive/MEJIA_LAB-Damon/Prewhitening-Paper/"

# Where the AC results were saved
# dir_ac1 <- file.path(dir_project, "AC1")
dir_agg <- file.path(dir_project, "AC1_agg")
dir_lm <- file.path(dir_project, "AC1_lm2")

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

tasks <- c("emotion", "gambling", "language", "motor", "relational", "social")

nVox <- 59412

time <- Sys.time()
for (meas in c("var", "acf1", "ar6c", "ar6v", "aic")) {
  cat(meas, "\t")

  fixed_fx <- matrix(nrow=nVox, ncol=19) #6 tasks + 6 for dHRF + 6 for motion + 1 for acquisition
  var_cor <- matrix(nrow=nVox, ncol=22) #6 for diagonal + 6*5/2 for off-diagonals + 1 for residual
  
  dat <- readRDS(file.path(dir_agg, paste0(meas, ".rds")))
  df <- do.call(expand.grid, dimnames(dat)[rev(seq(length(dim(dat))-1))])
  df$motion <- grepl("rp", df$nreg)
  df$hrf_dt <- grepl("hrfdt", df$nreg)
  for (vv in seq(nVox)) {
    if (vv %% 10000 == 0) { cat(vv, "\t") }
    df$meas <- as.vector(dat[,,,,,vv])
    dat_vv <- dat[,,,,,vv]
    df_vv <- melt(dat_vv, varnames=names(dimnames(dat_vv)))
    df_vv$dHRF <- 1 - grepl('hrfdt', df_vv$nreg) #0 = dHRF included (baseline), 1 = dHRF not included
    df_vv$motion <- 1 - grepl('rp', df_vv$nreg) #0 = motion regression (baseline), 1 = no motion regression
    #table(df_vv$nreg, df_vv$dHRF)
    #table(df_vv$nreg, df_vv$motion)
    
    q <- lme4::lmer(value ~ -1 + task + task:(dHRF + motion) + acquisition + ((-1 + task)|subject), data=df_vv) # lm2
    #q2 <- lme4::lmer(value ~ -1 + task + task:(dHRF + motion) + acquisition + ((task*(dHRF + motion))|subject), data=df_vv) # lm2
    # q <- lme4::lmer(meas ~ 1 + task + (1+task|subject)+ acquisition + nreg, data=df) # lm
    fixed_fx[vv,] <- fixef(q)
    var_cor[vv,] <- length(as.data.frame(VarCorr(q))$sdcor)
    if (vv == 1) { 
      colnames(fixed_fx) <- names(fixef(q))
      colnames(var_cor) <- apply(as.data.frame(VarCorr(q))[,seq(3)], 1, paste, collapse="-") 
    }
  }
  #consider save(fixed_fx, var_cor, file='myfile.RData') if you're saving multiple objects
  saveRDS(list(fixed_fx=fixed_fx, var_cor=var_cor), file.path(dir_lm, paste0(meas, ".rds")))
  
  cat("\n")
  print(Sys.time() - time); time <- Sys.time()
}

