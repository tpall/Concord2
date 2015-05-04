# Concord 2

Global surveillance of cancer survival 1995–2009 (CONCORD-2) study 5-year survival data from Table 4

Data was extracted from The Lancet [CONCORD-2 paper](http://www.thelancet.com/pdfs/journals/lancet/PIIS0140-6736(14)62038-9.pdf)  pdf using tabula http://tabula.technology/

Table was extracted by manually selecting subtable for each continent/region (Asia, Europe etc.). 
Table header was not exported and reconstructed later separately.
Tabula part can be scripted if you manage to install [tabula-extractor](https://github.com/tabulapdf/tabula-extractor)

In text editor (I used RStudio) clean extracted tables from symbols. Replace centered dot symbols with dots and replace double dots (..) with NA. Replace dash symbols with hyphens.
