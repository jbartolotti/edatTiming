



#' edatTimingFiles
#'
#' Read an edat text file and create timing files based on condition names
#' for use with afni preprocessing. Returns a data frame of onset times for condition and run.
#'
#' Usage:
#' edatTimingFiles('MID_NN-RCT-2002_T1.txt','data','output', 1001, 1)
#'
#' @param datfile Name of the edat text file
#' @param datpath path to the datfile
#' @param savedir path to the folder where timing files will be saved
#' @param pid Participant ID
#' @param session Scan Session ID
#' @param skiprows (optional) number of rows to skip when importing the edatfile. Default is '1', meaning that column names start on row 2.
#' @param sheetname (optional) If datfile is an excel sheet instead of a text file, the name of the Sheet to import
#' @param timingfile_format (optional) A string containing R code to evaluate to create the filenames of the timing files. Default is:
#'      "sprintf('%s%s_%.2d_%s.txt', prefix, pid, session, condition)"
#' @param prefix (optional) a parameter that can be used in 'timingfile_format'. Defaults to 'NN'
#' @param condition_labels (optional) A list of condition-label pairs. The keys are the conditions in the edat file. The values are the strings that get used to create the timingfile filenames.
#' @param figure (optional) A full path+filename to save a timing file figure. Defaults to NULL (i.e., do not make a figure)
#' @param noreturn (optional) if TRUE, the function will return NULL instead of the timing data frame
#' #'
#' @export
edatTimingFiles <- function(datfile, datpath, savedir,
                            pid, session,
                            skiprows = 1, sheetname = NA,
                            prefix = 'NN',
                            timingfile_format = 'default',
                            condition_labels = 'default',
                            figure = NULL,
                            noreturn = TRUE
                            ){
  if(timingfile_format == 'default'){
    timingfile_format <- "sprintf('%s%s_%.2d_%s.txt', prefix, pid, session, condition)"
  }
  if(condition_labels == 'default'){
    condition_labels <- list(
      Loss = 'Loss',
      Reward = 'Reward',
      Neutral = 'Neutral',
      NeutralFdbk = 'NeutralFdbk',
      LossNeg = 'PunNeg',
      RewardNeg = 'RewNeg',
      LossPos = 'PunPos',
      RewardPos = 'RewPos'
    )
  }

  dat <- loadDat(datfile, datpath, sheetname = sheetname, skiprows = skiprows)

  times <- writeTimingfiles(dat, prefix, pid, session, timingfile_format, condition_labels, savedir, figure)
  if(noreturn){times <- NULL}
  return(times)
}


