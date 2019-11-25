# Stepwise changes to original Rmd to form many slides on one slide
composition <- data.frame(component = slidesFromRmd("Test/test1.Rmd"),
                          main_slide = 1,
                          row = c(1,1,1,2,2,2),
                          col = c(1,2,3,1,2,3))

mainslides <- data.frame(name = "first test slide")

rmd_org <- readLines("Test/test1.Rmd")

composition$first_line <- firstinstance(composition$component, rmd_org)
composition$last_line <- c(composition$first_line[-1], length(rmd_org))

library(dplyr)
composition %>%
  group_by(main_slide) %>% 
  mutate(x1 = round(100 * (row - 1) / max(row), 2),
         y1 = round(100 * (col - 1) / max(col), 2),
         height = round(100 * 1 / max(row), 2),
         width = round(100 * 1 / max(col), 2)) %>% 
  ungroup() -> composition

composition %>% 
  group_by(main_slide) %>% 
  summarise(components = n(),
            rows = max(row),
            cols = max(col),
            first_line = min(first_line),
            last_line = max(last_line)) -> composition_mains

rmd_new <- rmd_org[1:min(composition$first_line - 1)]

added_main <- c("##", 
                "", 
                "<div style='position:absolute;height:700px;width:1300px;top:-20%;left:-20%;background-color:#f0f;font-size:1vw;'>", 
                "")

for(i in 1:dim(composition_mains)[1]){
  components <- composition %>% filter(main_slide == i)
  for(j in 1:dim(components)[1]){
    comp <- paste0("<div class='fragment fade-in fade-out' style='position:absolute;height:",
                   components$height[j], "%;width:",
                   components$width[j], "%;top:",
                   components$y1[j], "%;left:",
                   components$x1[j], "%;'>")
    title <- paste0("<h2> ", rev(rev(components$component[j])[-c(1:3)]), " </h2>") # DEPENDS ON ALL TITLES HAVING THREE LEADING SYMBOLS
    added_main <- c(added_main)
  }
  
}