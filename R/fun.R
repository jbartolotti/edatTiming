

loadDat <- function(datfile, datpath, sheetname = NA, skiprows = 'auto'){

  procedure_trial_col <- 'Procedure.Trial.'
  procedure_trial_value <- 'RunProc'

  if(grepl('[.]xlsx',datfile)){
    dat <- as.data.frame(readxl::read_excel(file.path(datpath, datfile), sheet = sheetname,col_names = TRUE, skip = skiprows))
    colnames(dat) <- unlist(lapply(colnames(dat), function(x){
                      a <- gsub('[[]','.',x)
                      a <- gsub('[]]','.',a)}
                      ))

  } else {

    # Read the file with the default encoding
    fcon <- file(file.path(datpath,datfile))
    mylines <- try(readLines(fcon))


    myencoding <- 'default'
    # Check if the first line is \xff\xfe*
    if (mylines[1] == "\xff\xfe*" || mylines[1] == 'ÿþC') {
      # Re-import the file with encoding = "UTF-16LE"
      myencoding <- "UTF-16LE"
      fcon <- file(file.path(datpath,datfile), encoding = myencoding)
      mylines <- readLines(fcon, encoding = myencoding)
    }

    # If auto-check skiprows, check whether the first line is a filename header
    if(skiprows == 'auto'){
      if(grepl('[.]edat3',mylines[1])){
        skiprows = 1
      } else{
        skiprows = 0
      }
    }

    if(trimws(mylines[1]) == '*** Header Start ***'){
      dat <- edatTxt2mat(mylines)
      procedure_trial_col <- 'Procedure'
      procedure_trial_value <- 'RewardProc'

    } else{
      if(myencoding == 'default'){
        dat <- read.delim(file.path(datpath, datfile), skip = skiprows)
      } else{
        dat <- read.delim(file.path(datpath, datfile), skip = skiprows, fileEncoding = myencoding)

      }
    }
  }
  out <- list(dat = dat, config = list(procedure_trial_col = procedure_trial_col, procedure_trial_value = procedure_trial_val))
  return(out)
}

procDat <- function(dat, proc_method, config = NULL){
  if(is.null(config)){
    procedure_trial_col <- 'Procedure.Trial.'
    procedure_trial_value <- 'RunProc'
  } else{
    procedure_trial_col <- config$procedure_trial_col
    procedure_trial_value <- config$procedure_trial_val
  }

  proc_method <- tolower(proc_method)
  if(proc_method == 'mid'){

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
    dat <- dat[!is.na(dat$Start) & !is.na(dat$Cue.StartTime) & !is.na(dat$Feedback.StartTime),]
    dat$Start <- as.numeric(dat$Start)
    dat$Cue.StartTime <- as.numeric(dat$Cue.StartTime)
    dat$Feedback.StartTime <- as.numeric(dat$Feedback.StartTime)



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

    trialrows <- dat[,procedure_trial_col] == procedure_trial_value
    dat <- dat[trialrows,]
    dat$Run <- dat$RunList.Cycle
  } else if(proc_method == 'nback'){
    exp_start_time <- as.numeric(unique(na.omit(dat$Goodluck.OffsetTime))[1])

    if(any('Trial' %in% colnames(dat))){
      trials <- dat$Trial
    } else {
      trial_onestim <- dat$singletriallist
      trial_twostim <- dat$TrialList
      trials <- ifelse(is.na(trial_onestim), trial_twostim, trial_onestim)
    }
    block_start_rows <- which(trials == 1)

    if(!any('Procedure.Block.' %in% colnames(dat)))
    {
      dat[,'Procedure.Block.'] <- NA
      dat[!is.na(dat$Procedure) & dat$Procedure == 'singletrial1','Procedure.Block.'] <- 'OneBackSeq'
      dat[!is.na(dat$Procedure) & dat$Procedure == 'trial1','Procedure.Block.'] <- 'TwoBackSeq'
    }
      conditions <- dat[block_start_rows,'Procedure.Block.']

    # Get the onsets for trial 1 in each condition, then merge them
    time_onestim <- dat[block_start_rows,'onestim.OnsetTime']
    time_twostim <- dat[block_start_rows,'twostim.OnsetTime']
    times <- as.numeric(ifelse(is.na(time_onestim), time_twostim, time_onestim))
    onsets <- times - exp_start_time

    dd <- data.frame(onset = onsets/1000, duration = 64, trial_type = conditions, block = 1:length(block_start_rows))

    return(dd)

  }






  return(dat)


}



edatTxt2mat <- function(mylines) { #rawfile, rawpath) {


# NOTE: this is now handled by loadDat above, which sends mylines here.
#  # Read the file with the default encoding
#  fcon <- file(file.path(rawpath,rawfile))
#  mylines <- try(readLines(fcon))
#
#  # Check if the first line is \xff\xfe*
#  if (mylines[1] == "\xff\xfe*") {
#    # Re-import the file with encoding = "UTF-16LE"
#    fcon <- file(file.path(rawpath,rawfile), encoding = "UTF-16LE")
#    mylines <- readLines(fcon, encoding = "UTF-16LE")
#  }

  mylines <- trimws(mylines)
  starts <- which(mylines == '*** LogFrame Start ***')
  ends <- which(mylines == '*** LogFrame End ***')
  mydats <- list()
  for(i in 1:length(starts)){
    dd <- mylines[(starts[i]+1):(ends[i]-1)]

    # Split the strings by ":"
    mat <- do.call(rbind, strsplit(dd, ":"))
    mat[,2] <- trimws(mat[,2])

    mydats[[i]] <- mat
    #    # Create a data frame with the first row as names
    #    df <- as.data.frame(t(mat[2, ]), stringsAsFactors = FALSE)
    #    names(df) <- mat[1, ]


  }
  ldf <- lapply(1:length(mydats), function (x){
    y = as.data.frame(t(mydats[[x]][,2 ]), stringsAsFactors = FALSE)
    names(y) <- t(mydats[[x]][,1])
    return(y)
  })

  # Combine the list of data frames by rows and fill in the missing values
  df <- plyr::rbind.fill(ldf)

  return(df)
}

writeTimingfiles <- function(dat, prefix, pid, session, timingfile_format, condition_labels, savedir, figurename, proc_method, format){
  format <- tolower(format)
  if(format == 'fmriprep'){
    fname <- eval(parse(text = timingfile_format))
    write.table(dat, file.path(savedir,fname), sep = '\t', row.names = FALSE, quote = FALSE)
    out <- dat

  } else if(format == 'afni'){

    proc_method <- tolower(proc_method)
    if(proc_method == 'nback'){
      for(cc in unique(dat$trial_type)){
        thiscond <- subset(dat, trial_type == cc)
        condition <- cc
        fname <- eval(parse(text = timingfile_format))
        fid <- file(file.path(savedir, fname ),'w')
        writeLines(paste(thiscond$onset, collapse = ' '),fid)
        close(fid)
        out <- dat
      }
    } else if(proc_method == 'mid'){
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
        ggplot2::ggplot(alltimes, ggplot2::aes(x = time, y = cond)) +
          ggplot2::theme_bw()  +
          ggplot2::geom_line(data = hlinedat, ggplot2::aes(color = cond), alpha = .1) +
          ggplot2::geom_line(ggplot2::aes(group = 1)) +
          ggplot2::geom_point(ggplot2::aes(color = cond)) +
          ggplot2::facet_wrap(. ~ run, ncol = 1) + labs(x = '', y = '')
        ggplot2::ggsave(figurename, height = 3, width = 12)
      }
      out <- alltimes
    }
  }
  return(out)

}
