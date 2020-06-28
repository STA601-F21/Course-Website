

#remotes::install_github("rstudio/webshot2")
#library(webshot2)

library(webshot)
#install_phantomjs(force=T)


webshot("/Users/oma9/Google Drive/GitHub/Courses/2020/Summer/STA 360:602L/Course-Website/static/slides/01-course-overview.html",
        "01-course-overview.pdf",delay=10)

rmdshot("/Users/oma9/Google Drive/GitHub/Courses/2020/Summer/STA 360:602L/Course-Website/static/slides/01-course-overview.rmd",
        "01-course-overview.pdf",delay=10)

webshot("https://sta-602l-s20.github.io/Course-Website/slides/lec-slides/02-one-parameter-models-I.html",
        "02-one-parameter-models-I.pdf",delay=10)
webshot("/Users/oma9/Google Drive/GitHub/Courses/2020/Spring/STA 602L/Course-Website/static/slides/lec-slides/03-one-parameter-models-II.html",
        "03-one-parameter-models-II.pdf",delay=10)


#pagedown::chrome_print("/Users/oma9/Google Drive/GitHub/Courses/2020/Spring/STA 602L/Course-Website/static/slides/lec-slides/02-one-parameter-models-I.html",
#                       wait=10)


