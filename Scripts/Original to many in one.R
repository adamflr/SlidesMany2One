# Stepwise changes to original Rmd to form many slides on one slide
composition <- data.frame(component = slidesFromRmd("Test/test1.Rmd"),
                          main_slide = 1,
                          row = c(1,1,1,2,2,2),
                          col = c(1,2,3,1,2,3))

mainslides <- data.frame(name = "first test slide")

rmd_org <- rmd_new <- readLines("Test/test1.Rmd")

composition$first_line <- firstinstance(composition$component, rmd_org)
composition$last_line <- c(composition$first_line[-1], length(rmd_org))
