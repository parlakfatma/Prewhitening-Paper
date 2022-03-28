
main_dir <- '~/Google Drive/My Drive/MEJIA_LAB-Damon/Prewhitening-Paper/code_MM/'
setwd(main_dir)

tasks <- c("SOCIAL","RELATIONAL","MOTOR","LANGUAGE","GAMBLING","EMOTION")
subjects <- read.table('subjects.txt')[,1]

###############################################################
## RETEST DATA
###############################################################

data_dir <- '/N/project/BayesGLM_Validation/HCP'
data_dir_local <- '/Users/Shared/Lab_Data/'

#RE-transfer first subject for social
#run for other tasks

for(t in tasks){

  print(t)
  setwd(data_dir_local)
  dir.create(paste0('HCP_',t))
  setwd(paste0('HCP_',t))
  dir.create('retest')
  setwd('retest')

  from <- file.path(data_dir, paste0('HCP_',t,'_tmp'), '*')
  to <- '.'
  cmd <- paste0('scp -r afmejia@carbonate.uits.iu.edu:',from,' ',to)
  system(cmd)

}

###############################################################
## ORIGINAL HCP DATA (RETEST SUBJECTS ONLY)
###############################################################

#TEST AND RUN

data_dir <- '/N/dcwan/projects/hcp'

for(t in tasks){
  print(t)
  setwd(file.path(data_dir_local,paste0('HCP_',t)))
  for(s in subjects){

    print(s)
    dir.create(as.character(s))
    setwd(as.character(s))
    dir.create(paste0('MNINonLinear/Results/tfMRI_',t,'_LR/EVs'), recursive=TRUE)
    dir.create(paste0('MNINonLinear/Results/tfMRI_',t,'_RL/EVs'), recursive=TRUE)

    #LR acquisition
    dir_st <- file.path(data_dir,s,'/MNINonLinear/Results', paste0('tfMRI_',t,'_LR'))
    file1 <- file.path(dir_st, paste0('tfMRI_',t,'_LR_Atlas.dtseries.nii'))
    file2 <- file.path(dir_st, 'Movement_Regressors.txt')
    file3 <- file.path(dir_st, 'EVs/*.txt')
    dir_to <- paste0('MNINonLinear/Results/tfMRI_',t,'_LR/')
    system(paste0('scp afmejia@carbonate.uits.iu.edu:',file1, ' ', dir_to))
    system(paste0('scp afmejia@carbonate.uits.iu.edu:',file2, ' ', dir_to))
    system(paste0('scp afmejia@carbonate.uits.iu.edu:',file3, ' ', dir_to, '/EVs/'))

    #RL acquisition
    dir_st <- file.path(data_dir,s,'/MNINonLinear/Results', paste0('tfMRI_',t,'_RL'))
    file1 <- file.path(dir_st, paste0('tfMRI_',t,'_RL_Atlas.dtseries.nii'))
    file2 <- file.path(dir_st, 'Movement_Regressors.txt')
    file3 <- file.path(dir_st, 'EVs/*.txt')
    dir_to <- paste0('MNINonLinear/Results/tfMRI_',t,'_RL/')
    system(paste0('scp afmejia@carbonate.uits.iu.edu:',file1, ' ', dir_to))
    system(paste0('scp afmejia@carbonate.uits.iu.edu:',file2, ' ', dir_to))
    system(paste0('scp afmejia@carbonate.uits.iu.edu:',file3, ' ', dir_to, '/EVs/'))
    setwd('..')
  }
  setwd('..')
}

