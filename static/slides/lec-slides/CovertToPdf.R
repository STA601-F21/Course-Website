


library(webshot)
#install_phantomjs(force=T)
webshot("/Users/oma9/Google Drive/GitHub/Courses/2020/Spring/STA 602L/Course-Website/static/slides/lec-slides/01-intro.html",
        "01-intro.pdf",delay=10)

webshot("https://sta-602l-s20.github.io/Course-Website/slides/lec-slides/02-one-parameter-models-I.html",
        "02-one-parameter-models-I.pdf",delay=10)
webshot("/Users/oma9/Google Drive/GitHub/Courses/2020/Spring/STA 602L/Course-Website/static/slides/lec-slides/03-one-parameter-models-II.html",
        "03-one-parameter-models-II.pdf",delay=10)


#pagedown::chrome_print("/Users/oma9/Google Drive/GitHub/Courses/2020/Spring/STA 602L/Course-Website/static/slides/lec-slides/02-one-parameter-models-I.html",
#                       wait=10)
