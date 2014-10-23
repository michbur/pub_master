library(XML)

getJournalTitles <- function() {
  journals <- readHTMLTable("http://www.bioinformatics.org/wiki/Journals", 
                            stringsAsFactors = FALSE)
  journals <- journals[[2]]
  journals[, 2] <- as.numeric(journals[, 2])
  journals <- journals[order(journals[, 2], decreasing = TRUE), ]
  titles <- head(journals[, 1], 20)
  titles
}

getJournalXML <- function(title) {
  term <- paste(title, "[JOUR]", sep = "")
  e <- entrez_search("pubmed", term, usehistory = "y")
  f <- entrez_fetch("pubmed", WebEnv = e$WebEnv, query_key = e$QueryKey, 
                    rettype = "xml", retmax = e$count)
  d <- xmlTreeParse(f, useInternalNodes = TRUE)
  outfile <- paste(gsub(" ", "_", title), "xml", sep = ".")
  saveXML(xmlRoot(d), outfile)
}

titles[6] <- gsub("&", "and", titles[6])
titles[11] <- "Proteins"

# saves XML files in current working directory
pbsapply(titles, function(x) 
  getJournalXML(x))


titles <- getJournalTitles()
