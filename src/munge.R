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
library(plyr);library(dplyr)

munge <- function(x) {
  sites <- c("Stomach","Colon","Rectum","Liver","Lung",
             "Breast","Cervix","Ovary","Prostate","Leukaemia","ALL")
  x %<>% "["(apply(.,2, function(y) !all(is.na(y))))
  colnames(x) <- c("Period",sites)
  newx <- x[!x$Period%in%"",]
  newx$Period %>% 
    as.character %>%
    .[grepl("[A-z]",.)] %>%
    rep(.,each = 4) %>%
    data.frame(Country=.,newx) %>% 
    .[!apply(., 1, function(x) x[1]==x[2]),] %>% {
      values <- "["(.,,3:ncol(.)) %>% 
        apply(.,2, .%>%as.character%>%as.numeric)
      "["(.,,1:2) %>% cbind(values)}
}

concord <- list.files(path = "data/", pattern = ".csv", full.names = TRUE) %>%
  sapply(read.csv,header = FALSE) %>%
  lapply(munge) %>%
  ldply %>%
  mutate(Region=gsub("data/([A-z-]+).csv","\\1",.id),
         Period=sub("([0-9-]+) ","\\1",Period))
