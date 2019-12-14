# Present the headers from the markdown file
extract_slides <- function(file){
  a <- readLines(file, encoding = "UTF-8")
  
  outside_chunk <- !(cumsum(substr(a, 1, 3) == "```") %% 2)
  hashtagged <- substr(a, 1, 1) == "#"
  
  slides <- data.frame(headers = a[hashtagged & outside_chunk],
                       first_line = which(hashtagged & outside_chunk),
                       stringsAsFactors = F)
  slides$last_line <- c(slides$first_line[-1] - 1, length(a))
  
  slides
}
