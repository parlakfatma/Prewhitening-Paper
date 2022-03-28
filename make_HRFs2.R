
make_HRFs2 <- function (onsets, TR, duration, downsample = 100) {
  K <- length(onsets)
  if (is.null(names(onsets))) 
    task_names <- paste0("task", 1:K)
  else task_names <- names(onsets)
  nsec <- duration * TR
  stimulus <- rep(0, nsec * downsample)
  HRF <- neuRosim::canonicalHRF(seq(0, 30, by = 1/downsample))[-1]
  inds <- seq(TR * downsample, nsec * downsample, by = TR * 
                downsample)
  design <- matrix(NA, nrow = duration, ncol = K)
  colnames(design) <- task_names
  for (k in 1:K) {
    onsets_k <- onsets[[k]][, 1]
    durations_k <- onsets[[k]][, 2]
    stimulus_k <- stimulus
    for (ii in 1:length(onsets_k)) {
      start_ii <- round(onsets_k[ii] * downsample)
      end_ii <- round(onsets_k[ii] * downsample + durations_k[ii] * 
                        downsample)
      if (end_ii > length(stimulus_k)) { 
        warning("Truncating stimulus for an event ending after the scan.")
        end_ii <- length(stimulus_k)
      }
      stimulus_k[start_ii:end_ii] <- 1
    }
    HRF_k <- stats::convolve(stimulus_k, rev(HRF), type = "open")
    design[, k] <- HRF_k[inds]
  }
  return(design)
}