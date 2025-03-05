if(FALSE){
fcon <- file(file.path(datpath,datfile))
mylines <- try(readLines(fcon))
if(mylines[1] == 'ÿþC')

fcon <- file(file.path(datpath,datfile), encoding = "UTF-16LE")
mylines <- readLines(fcon, encoding = "UTF-16LE")

if(skiprows == 'auto'){
  if(grepl('[.]edat3',mylines[1])){
    skiprows = 1
  } else{
    skiprows = 0
  }
}
dat <- read.delim(file.path(datpath, datfile), skip = skiprows, fileEncoding = 'UTF-16LE')


pid = '003'
session = 'BL'
proc_method <- 'nback'
prefix=''
timingfile_format <- "sprintf('sub-%s_ses-%s_task-NBack_events.tsv', pid, session)"
mysave <- 'ignore'
skiprows = 'auto'
datpath <- file.path('ignore')
datfile <- 'nback_excel.txt'
export_format <- 'fmriprep'

edatTimingFiles(datfile,datpath, mysave, pid, session, prefix = prefix, timingfile_format = timingfile_format, proc_method = proc_method, export_format = export_format)


}
