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

basedir <- '~/R-Drive/Hoglund/Vidoni_E/146904/COMET_MATLAB_Scripts/fMRI_Scripts'

edatTimingFiles('Nback_MRI_IGNITE-69-1.txt',
                file.path(basedir,'Edats/69-1-Nback'),
                file.path(basedir,'Timing'),
                69,
                1,
                prefix = '',
                timingfile_format = "sprintf('sub-%s_ses-%s_task-NBack_events.tsv', pid, session)",
                proc_method = 'nback',
                export_format = 'fmriprep')

basedir <- '~/R-Drive/Hoglund/Vidoni_E/146904/COMET_MATLAB_Scripts/fMRI_Scripts'

edatTimingFiles('Nback_MRI_IGNITE-69-1.txt',
                file.path(basedir,'Edats/69-1-Nback'),
                file.path(basedir,'Timing'),
                69,
                1,
                prefix = '',
                timingfile_format = "sprintf('%s_%s_NBack_%s.txt', pid, session, condition)",
                proc_method = 'nback',
                export_format = 'afni')



#dat <- data.frame(onset = c(7,98,189,280), duration = c(64,64,64,64), trial_type = c('one','two','one','two'), block = c(1,2,3,4))


basedir <- '~/R-Drive/Vidoni_E/146904/COMET_MATLAB_Scripts/fMRI_Scripts'

edatTimingFiles('Nback_MRI_IGNITE-69-1.txt',
                file.path(basedir,'Edats/69-1-Nback'),
                file.path('~','Timing'),
                69,
                1,
                prefix = '',
                timingfile_format = "sprintf('%s_%s_NBack_%s.txt', pid, session, condition)",
                proc_method = 'nback',
                export_format = 'afni')



}





##############

file8 <- 'test_utf8.txt'
file16 <- 'test_utf16le.txt'

fcon <- file(file8, encoding = 'UTF-8')
read8_8 <- readLines(fcon, encoding = "UTF-8")
close(fcon)

fcon <- file(file8, encoding = 'UTF-16LE')
read8_16 <- readLines(fcon, encoding = "UTF-16LE")
close(fcon)

fcon <- file(file8)
read8_def <- readLines(fcon)
close(fcon)

fcon <- file(file16, encoding = 'UTF-8')
read16_8 <- readLines(fcon, encoding = "UTF-8")
close(fcon)

fcon <- file(file16, encoding = 'UTF-16LE')
read16_16 <- readLines(fcon, encoding = "UTF-16LE")
close(fcon)

fcon <- file(file16)
read16_def <- readLines(fcon)
close(fcon)

stringi::stri_enc_detect(read8_8[[1]])
stringi::stri_enc_detect(read8_16[[1]])
stringi::stri_enc_detect(read16_8[[1]])
stringi::stri_enc_detect(read16_16[[1]])

nnr <- file.path(datpath,datfile)
fcon <- file(nnr,  encoding = "UTF-8")
readnnr_8 <- readLines(fcon, encoding = "UTF-8")
close(fcon)

fcon <- file(nnr)
readnnr_def <- readLines(fcon)
close(fcon)


fcon <- file(nnr,  encoding = "UTF-16LE")
readnnr_16 <- readLines(fcon, encoding = "UTF-16LE")
close(fcon)

a <- stri_enc_detect(read16_def[1])[[1]]
b <- stri_enc_detect(read8_def[1])[[1]]

which(a$Encoding == "UTF-16LE")
which(a$Encoding == "UTF-8")

which(b$Encoding == "UTF-16LE")
which(b$Encoding == "UTF-8")


encodingType <- function(filename, use_default_utf8 = TRUE){
  fcon <- file(filename)
  read_default <- try(readLines(fcon))
  close(fcon)
  enc <- stringi::stri_enc_detect(read_default[1])[[1]]
  rank8 <- which(enc$Encoding == "UTF-8")
  rank16 <- which(enc$Encoding == "UTF-16LE")
  if(length(rank8) == 0){rank8 <- Inf}
  if(length(rank16) == 0){rank16 <- Inf}

  if(rank16 < rank8){
    this_encoding <- 'UTF-16LE'
  } else{
    if(use_default_utf8){
      this_encoding <- 'default'
    } else{
      this_encoding <- 'UTF-8'
    }
  }
  return(this_encoding)
}



