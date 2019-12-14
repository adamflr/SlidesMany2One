# Stepwise changes to original Rmd to form many slides on one slide
composition <- data.frame(component = extract_slides("Test/test1.Rmd")[,1],
                          first_line = extract_slides("Test/test1.Rmd")[,2],
                          last_line = extract_slides("Test/test1.Rmd")[,3],
                          main_slide = 1,
                          row = c(1,1,1,2,2,2),
                          col = c(1,2,3,1,2,3),
                          stringsAsFactors = F)

rmd_org <- readLines("Test/test1.Rmd")

library(dplyr)
composition %>%
  group_by(main_slide) %>% 
  mutate(y1 = round(100 * (row - 1) / max(row), 2),
         x1 = round(100 * (col - 1) / max(col), 2),
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

for(i in 1:dim(composition_mains)[1]){
  components <- composition %>% filter(main_slide == i)
  
  added_main <- c("##", 
                  "", 
                  "<div style='position:absolute;height:700px;width:1300px;top:-20%;left:-20%;font-size:1vw;'>", 
                  "")
  
  for(j in 1:dim(components)[1]){
    comp <- paste0("<div class='fragment fade-in' style='position:absolute;height:",
                   components$height[j], "%;width:",
                   components$width[j], "%;top:",
                   components$y1[j], "%;left:",
                   components$x1[j], "%;'>")
    
    title <- paste0("<h2> ", paste(strsplit(components$component[j], " ")[[1]][-1], collapse = " "), " </h2>")
    
    slide <- rmd_org[(components$first_line[j] + 1) : components$last_line[j]]
    
    added_main <- c(added_main, comp, title, slide, "</div>")
  }
  
  added_main <- c(added_main, "</div>")
  
  rmd_new <- c(rmd_new, added_main)
}

library(rmarkdown)
writeLines(rmd_new, con = "Test/rmd_new2.Rmd")
setwd("Test/")
render(input = "rmd_new.Rmd", output_file = "rmd_new.html")
setwd("..")
