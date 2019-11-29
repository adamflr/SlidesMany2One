# Present the headers from the markdown file
extract_slides <- function(file){
  a <- readLines(file, encoding = "UTF-8")
  
  outside_chunk <- !(cumsum(substr(a, 1, 3) == "```") %% 2)
  a <- a[outside_chunk]
  
  a[substr(a, 1, 1) == "#"]
}

locate_first_instance <- function(x, y){
  foo <- function(a, b){
    min(which(a == b))
  }
  purrr::map_dbl(x, function(x) foo(x, y))
}

