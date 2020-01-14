# Stepwise changes to original Rmd to form many slides on one slide
file_path <- "Test/01 Introduktion.Rmd"
composition <- data.frame(component = extract_slides(file_path)[,1],
                          first_line = extract_slides(file_path)[,2],
                          last_line = extract_slides(file_path)[,3],
                          stringsAsFactors = F)
composition <- data.frame(composition,
                          main_slide = rep(1:10, each = 6)[1:length(composition$component)],
                          row = rep(c(1,1,1,2,2,2), 10)[1:length(composition$component)],
                          col = rep(c(1,2,3,1,2,3), 10)[1:length(composition$component)],
                          stringsAsFactors = F)

rmd_org <- readLines(file_path, encoding = "UTF-8")

library(dplyr)
total_slide_size <- 75
composition %>%
  group_by(main_slide) %>% 
  mutate(y1 = round(100 * (row - 1) / max(row), 2),
         x1 = round(100 * (col - 1) / max(col), 2),
         height = round(total_slide_size * 1 / max(row), 2),
         width = round(total_slide_size * 1 / max(col), 2)) %>% 
  ungroup() -> composition

composition$additional_css <- ""
composition$additional_css <- "background-color:#0f0;border:solid;border-width:1px;border-radius:5px;padding:10px;"

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
                  "<div style='position:absolute;height:800px;width:1200px;top:0px;left:-10%;font-size:0.75vw;'>", 
                  "")
  
  for(j in 1:dim(components)[1]){
    comp <- paste0("<div class='fragment fade-in' style='position:absolute;height:",
                   components$height[j], "%;width:",
                   components$width[j], "%;top:",
                   components$y1[j], "%;left:",
                   components$x1[j], "%;",
                   components$additional_css[j],
                   "'>")
    
    title <- paste0("<h2> ", paste(strsplit(components$component[j], " ")[[1]][-1], collapse = " "), " </h2>")
    
    slide <- rmd_org[(components$first_line[j] + 1) : components$last_line[j]]
    
    added_main <- c(added_main, comp, title, slide, "</div>")
  }
  
  added_main <- c(added_main, "</div>")
  
  rmd_new <- c(rmd_new, added_main)
}

library(rmarkdown)
#writeLines(rmd_new, con = "Test/rmd_new2.Rmd") # Encoding?
write.table(rmd_new, "Test/rmd_new_table.Rmd", row.names = F, col.names = F, 
            fileEncoding = "UTF-8", quote = F)

setwd("Test/")
render(input = "rmd_new_table.Rmd", output_file = "rmd_new.html", encoding = "UTF-8")
setwd("..")
