



#' edatTimingFiles
#'
#' Read an edat text file and create timing files based on condition names
#' for use with afni preprocessing. Returns a data frame of onset times for condition and run.
#' Input can either be a raw eprime text file (i.e., first line "*** Header Start ***"), or an exported eprime text file (i.e., a tab delimited table -- a processed version of the raw eprime text file).
#'
#' Example function calls:
#'
#' edatTimingFiles('MID_NN-RCT-2002_T1.txt','data','output', 1001, 1)
#'
#' edatTimingFiles('NBACK.txt','data','output','001','BL', timingfile_format = "sprintf('sub-%s_ses-%s_task-NBack_events.tsv', pid, session)", proc_method = 'nback', export_format = 'fmriprep')
#'
#' @param datfile Name of the edat text file
#' @param datpath path to the datfile
#' @param savedir path to the folder where timing files will be saved
#' @param pid Participant ID
#' @param session Scan Session ID
#' @param skiprows (optional) number of rows to skip when importing the edatfile. Default is 'auto': If the first line contains '.edat3' it will skip that line and start on 2, otherwise it will start on the first line.
#' @param sheetname (optional) If datfile is an excel sheet instead of a text file, the name of the Sheet to import
#' @param timingfile_format (optional) A string containing R code to evaluate to create the filenames of the timing files. Default is:
#'      "sprintf('%s%s_%s_%s.txt', prefix, pid, session, condition)"
#' @param prefix (optional) a parameter that can be used in 'timingfile_format'. Defaults to 'NN'
#' @param condition_labels (optional) A list of condition-label pairs. The keys are the conditions in the edat file. The values are the strings that get used to create the timingfile filenames.
#' @param figure (optional) A full path+filename to save a timing file figure. Defaults to NULL (i.e., do not make a figure)
#' @param noreturn (optional) if TRUE (default), the function will return NULL instead of the timing data frame. To return the timing data, set to FALSE.
#' @param proc_method (optional) Determines which internal function is used to detect block/trial onsets and conditions. Defaults to 'MID', monetary incentive delay. Also supports 'nback'.
#' @param export_format (optional) Defaults to 'AFNI', and creates a separate timing file for each condition containing event onsets. Also supports 'fmriprep' which outputs a BIDS-compliant events.tsv file.
#'
#'
#' @export
edatTimingFiles <- function(datfile, datpath, savedir,
                            pid, session,
                            skiprows = 'auto', sheetname = NA,
                            prefix = 'NN',
                            timingfile_format = 'default',
                            condition_labels = 'default',
                            figure = NULL,
                            noreturn = TRUE,
                            proc_method = 'MID',
                            export_format = 'afni'
                            ){
  if(timingfile_format == 'default'){
    timingfile_format <- "sprintf('%s%s_%s_%s.txt', prefix, pid, session, condition)"
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

  loaded_dat <- loadDat(datfile, datpath, sheetname = sheetname, skiprows = skiprows)
  dat <- procDat(loaded_dat$dat, proc_method, config = loaded_dat$config)

  times <- writeTimingfiles(dat, prefix, pid, session, timingfile_format, condition_labels, savedir, figure, proc_method, export_format)
  if(noreturn){times <- NULL}
  return(times)
}

#rawpath <- '~/R-Drive/Stancil_S/CMH00002228_NN-RCT/MR_Data/Edats/2_MID'
#rawfile <- 'MID_Behavioral_20161218_LM_Trigger_v1-2002-1.txt'
#savedir <- '~/R-Drive/Bartolotti_J'

## write file if name provided
#if(!is.null(savefile) && !is.null(savepath)){
#  write.table(df, file.path(savepath, savefile), sep = '\t', row.names = FALSE, col.names = TRUE)
#}


