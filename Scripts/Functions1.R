# Present the headers from the markdown file
# Will in current state also give any comments in R code
slidesFromRmd <- function(file){
  a <- readLines(file, encoding = "UTF-8")
  a[substr(a, 1, 1) == "#"]
}

firstinstance <- function(x, y){
  foo <- function(a, b){
    min(which(a == b))
  }
  purrr::map_dbl(x, function(x) foo(x, y))
}

