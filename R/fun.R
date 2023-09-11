

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

  dat$CondAnt[dat$Condition == 'Triangle'] <- 'Neutral'
  dat$CondAnt[dat$Condition == 'SmallReward'] <- 'Reward'
  dat$CondAnt[dat$Condition == 'LgReward'] <- 'Reward'
  dat$CondAnt[dat$Condition == 'SmallPun'] <- 'Loss'
  dat$CondAnt[dat$Condition == 'LgPun'] <- 'Loss'

  dat$CondFeedback[dat$CondAnt == 'Loss' & dat$prbacc == 1] <- 'LossPos'
  dat$CondFeedback[dat$CondAnt == 'Loss' & dat$prbacc == 0] <- 'LossNeg'
  dat$CondFeedback[dat$CondAnt == 'Reward' & dat$prbacc == 1] <- 'RewardPos'
  dat$CondFeedback[dat$CondAnt == 'Reward' & dat$prbacc == 0] <- 'RewardNeg'
  dat$CondFeedback[dat$CondAnt == 'Neutral'] <- 'NeutralFdbk'

  trialrows <- dat$Procedure.Trial. == 'RunProc'
  dat <- dat[trialrows,]
  dat$Run <- dat$RunList.Cycle


  return(dat)


}


alltimes <- writeTimingfiles <- function(dat, prefix, pid, session, timingfile_format, condition_labels, savedir, figurename){
  times <- list()

  antcond <- unique(dat$CondAnt)
  antcond <- antcond[!is.na(antcond)]
  antcond <- antcond[!(antcond %in% 'none')]

  for(cc in antcond){
    runtimes <- list()
    condition <- condition_labels[[cc]]
    fname <- eval(parse(text = timingfile_format))
    fid <- file(file.path(savedir, fname ),'w+')
    myruns <- unique(dat$Run[dat$CondAnt == cc])
    myruns <- myruns[!is.na(myruns)]
    for(rr in myruns){
      tt <- dat$CueStart.sec[dat$CondAnt == cc & dat$Run == rr]
      writeLines(paste(tt, collapse = ' '),fid)
      runtimes[[rr]] <- data.frame(cond = cc, run = rr, time = tt, stringsAsFactors = FALSE)

    }
    times[[cc]] <- do.call('rbind',runtimes)
    close(fid)
  }

  fdbcond <- unique(dat$CondFeedback)
  fdbcond <- fdbcond[!is.na(fdbcond)]
  fdbcond <- fdbcond[!(fdbcond %in% 'none')]
  for(cc in fdbcond){
    runtimes <- list()
    condition <- condition_labels[[cc]]
    fname <- eval(parse(text = timingfile_format))
    fid <- file(file.path(savedir, fname ),'w+')
    myruns <- unique(dat$Run[dat$CondFeedback == cc])
    myruns[!is.na(myruns)]
    for(rr in myruns){
      tt <- dat$FdbkStart.sec[dat$CondFeedback == cc & dat$Run == rr]
      writeLines(paste(tt, collapse = ' '),fid)
      runtimes[[rr]] <- data.frame(cond = cc, run = rr, time = tt, stringsAsFactors = FALSE)

    }
    times[[cc]] <- do.call('rbind',runtimes)
    close(fid)
  }
  alltimes <- do.call('rbind',times)
  if(!is.null(figurename)){
    hlinedat <- data.frame(
      time = rep(c(min(alltimes$time),max(alltimes$time)),by = length(unique(alltimes$cond)) * length(unique(alltimes$run))),
      cond = rep(unique(alltimes$cond), each = 2, 2),
      run = rep(unique(alltimes$run), each = 2*length(unique(alltimes$cond))))
    ggplot(alltimes, aes(x = time, y = cond)) + theme_bw()  + geom_line(data = hlinedat, aes(color = cond), alpha = .1) + geom_line(aes(group = 1)) + geom_point(aes(color = cond)) + facet_wrap(. ~ run, ncol = 1) + labs(x = '', y = '')
    ggsave(figurename, height = 3, width = 12)
  }

  return(alltimes)
}
