

loadDat <- function(datfile, datpath, sheetname = NA, skiprows = 0){

  if(grepl('[.]xlsx',datfile)){
    dat <- as.data.frame(readxl::read_excel(file.path(datpath, datfile), sheet = sheetname,col_names = TRUE, skip = skiprows))
    colnames(dat) <- unlist(lapply(colnames(dat), function(x){
                      a <- gsub('[[]','.',x)
                      a <- gsub('[]]','.',a)}
                      ))

  } else {
    dat <- read.delim(file.path(datpath, datfile), skip = skiprows)

  }


  starttimerows <- which(!is.na(dat$PrepTime.FinishTime))
  dat$Start <- NA
  for(i in 1:length(starttimerows)){
    stime <- dat[starttimerows[i],'PrepTime.FinishTime']
    startrow <- starttimerows[i]
    if(i == length(starttimerows)){
      finalrow <- dim(dat)[1]
    } else{
      finalrow <- starttimerows[i+1]-1
    }
    dat$Start[startrow:finalrow] <- stime
  }


  dat$CueStart.sec <- (dat$Cue.StartTime - dat$Start)/1000
  dat$FdbkStart.sec <- (dat$Feedback.StartTime - dat$Start)/1000
  dat$CondAnt <- 'none'
  dat$CondFeedback <- 'none'

  dat$CondAnt[dat$Condition == 'Triangle'] <- 'No'
  dat$CondAnt[dat$Condition == 'SmallReward'] <- 'Reward'
  dat$CondAnt[dat$Condition == 'LgReward'] <- 'Reward'
  dat$CondAnt[dat$Condition == 'SmallPun'] <- 'Loss'
  dat$CondAnt[dat$Condition == 'LgPun'] <- 'Loss'

  dat$CondFeedback[dat$CondAnt == 'Loss' & dat$prbacc == 1] <- 'LossPos'
  dat$CondFeedback[dat$CondAnt == 'Loss' & dat$prbacc == 0] <- 'LossNeg'
  dat$CondFeedback[dat$CondAnt == 'Reward' & dat$prbacc == 1] <- 'RewardPos'
  dat$CondFeedback[dat$CondAnt == 'Reward' & dat$prbacc == 0] <- 'RewardNeg'

  trialrows <- dat$Procedure.Trial. == 'RunProc'
  dat <- dat[trialrows,]
  dat$Run <- dat$RunList.Cycle


  return(dat)


}


writeTimingfiles <- function(dat, prefix, pid, session, timingfile_format, condition_labels, savedir){
  antcond <- unique(dat$CondAnt)
  antcond <- antcond[!is.na(antcond)]
  antcond <- antcond[!(antcond %in% 'none')]

  for(cc in antcond){
    condition <- condition_labels[[cc]]
    fname <- eval(parse(text = timingfile_format))
    fid <- file(file.path(savedir, fname ),'w+')
    myruns <- unique(dat$Run[dat$CondAnt == cc])
    myruns <- myruns[!is.na(myruns)]
    for(rr in myruns){
      times <- dat$CueStart.sec[dat$CondAnt == cc & dat$Run == rr]
      writeLines(paste(times, collapse = ' '),fid)

    }
    close(fid)
  }

  fdbcond <- unique(dat$CondFeedback)
  fdbcond <- fdbcond[!is.na(fdbcond)]
  fdbcond <- fdbcond[!(fdbcond %in% 'none')]
  for(cc in fdbcond){
    condition <- condition_labels[[cc]]
    fname <- eval(parse(text = timingfile_format))
    fid <- file(file.path(savedir, fname ),'w+')
    myruns <- unique(dat$Run[dat$CondFeedback == cc])
    myruns[!is.na(myruns)]
    for(rr in myruns){
      times <- dat$FdbkStart.sec[dat$CondFeedback == cc & dat$Run == rr]
      writeLines(paste(times, collapse = ' '),fid)

    }
    close(fid)
  }
}
