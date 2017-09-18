# in R download The Lancet CONCORD-2 paper 
# system("wget 'http://www.thelancet.com/pdfs/journals/lancet/PIIS0140-6736(14)62038-9.pdf'")
# download and unpack tabula.jar from http://tabula.technology/ 
# in folder containing tabula.jar run: java -Dfile.encoding=utf-8 -Xms256M -Xmx1024M -jar tabula.jar
# Open pdf and extract tables by manually selecting subtable for each region. (Asia, Europe etc.). 
# Don't export table headers. Reconstruct headers later separately.
# This tabula part can be scripted if you manage to install tabula-extractor etc. (https://github.com/tabulapdf/tabula-extractor)

# In text editor (RStudio) clean extracted tables from symbols. Replace centered dot symbols with 
# dots and replace double dots (..) with NA. Replace dash symbols with hyphens.


library(magrittr)
library(plyr);
library(dplyr)

munge <- function(x) {
  sites <- c("Stomach","Colon","Rectum","Liver","Lung",
             "Breast","Cervix","Ovary","Prostate","Leukaemia","ALL")
  x %<>% "["(apply(., 2, function(y) !all(is.na(y))))
  colnames(x) <- c("Period", sites)
  newx <- x[!x$Period %in% "", ]
  newx$Period %>% 
    as.character %>%
    .[grepl("[A-z]", .)] %>%
    rep(., each = 4) %>%
    data.frame(Country = ., newx) %>% 
    .[!apply(., 1, function(x) x[1]==x[2]), ] %>% {
      values <- "["(., , 3:ncol(.)) %>% 
        apply(., 2, . %>% as.character %>% as.numeric)
      "["(., , 1:2) %>% 
        cbind(values)
    }
}

stringr::str_split("(24.0-25.6)", pattern = "-") %>% 
  lapply(readr::parse_number)

readr::parse_number("(25.6)")

concord <- list.files(path = "data/", pattern = ".csv", full.names = TRUE) %>%
  sapply(read.csv, header = FALSE) %>%
  lapply(munge) %>% 
  bind_rows(.id = ".id") %>% 
  mutate(Region = sub("data//([A-z-]+).csv", "\\1", .id),
         Period = sub("([0-9-]+) ","\\1", Period)) %>% 
  select(-.id)
write.table(concord, "output/concord2.csv", sep = ",", row.names = FALSE)

library(purrr)
sites <- c("Stomach","Colon","Rectum","Liver",
           "Lung","Breast","Cervix","Ovary",
           "Prostate","Leukaemia","ALL")
concord <- data_frame(path = list.files(path = "data/", pattern = ".csv", full.names = TRUE))
concord <- mutate(concord, dat = map(path, readr::read_csv, col_names = FALSE))


tab <- concord$dat[[3]]

extract_country <- function(X1){
  countries <- grep("^[[:alpha:]]", X1)
  rep(X1[countries], diff(c(countries, length(X1) + 1)))
}

extract_period <- function(X1){
  periods <- which(!is.na(X1))
  times <- diff(c(periods, length(X1)+1))
  rep(X1[periods], diff(c(periods, length(X1) + 1)))
}

## Add countries and periods
tab <- mutate(tab, country = extract_country(X1)) %>% 
  filter(!grepl("^[[:alpha:]]", X1)) %>% 
  mutate(periods = extract_period(X1)) %>% 
  select(country, periods, X2:X22)

## munge survival and confidence intervals
## exclude cols with all NAs
tab <- select(tab, which(!vapply(tab, function(x) all(is.na(x)), logical(1L))))
(colnames(tab))
tab <- mutate_at(tab, vars(X2:X22), function(x) grepl("^\\(", x)) %>% 
  mutate(ci = pmap_lgl(list(X2,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22), any)) %>% 
  select(ci) %>% 
  bind_cols(tab, .)

var <- tab$X22
extract_numbers <- function(var){
  
  if(grepl("^\\(", var)){
    numstr <- strsplit(var, "-")
    dfs <- c(vapply(numstr, readr::parse_number, numeric(2)))
    names(dfs) <- c("lower", "upper")
    return(dfs)
  }
  
  readr::parse_number(var)
}

library(tidyr)
mutate_at(tab, vars(X2:X13,X15:X22), ~map(.x, extract_numbers)) %>% 
  gather("site", "survival", -country, -periods, -ci) %>% 
  filter(!map_lgl(survival, ~any(is.na(.x)))) %>% 
  .[-c(1015, 1017),] %>% 
  spread(ci, survival) %>% 



