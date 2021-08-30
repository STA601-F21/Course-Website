#remotes::install_github("jhelvy/xaringanBuilder")
#remotes::install_github('rstudio/chromote')
#install.packages('pdftools')

library(xaringanBuilder)

basedir = "/Users/clyde/Dropbox/sta601/Course-Website"

convert = function(file) {
  input=paste0(file, ".Rmd")
  handout = paste0(file, "-handout.pdf")
  slides = paste0(file, ".pdf")
  build_pdf(input, output = handout,complex_slides=TRUE)
  build_pdf(input, output=slides, complex_slides=TRUE, partial_slides = TRUE)
}

setwd("/Users/clyde/Dropbox/sta601/Course-Website/static/slides")

# Lectures

#convert("00-course-overview")
convert("01-basics-of-bayes")

setwd(basedir)
