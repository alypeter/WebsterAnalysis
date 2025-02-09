library(tidyverse)
library(magick)

magick::magick_config()

# attempt to read in the PDF of my text and convert to text and or OCR

library(pdftools)
#to find all the PDFs in the source file (the 385P file on desktop)

files <- list.files(pattern = "pdf$")

#To read in the file and rename it

Webster <- lapply(files, pdf_text)

#checking it read in correctly
length(Webster)
lapply(Webster, length)

# now on to text mining
# this comes from University of Virginia Library page, "Reading PDF Files into R for Text Mining"
library(tm)
library(SnowballC)
library(tesseract)

corp <- Corpus(URISource(files),
               readerControl = list(reader = readPDF))
# Need to remove the Unicode symbols
corp <- tm_map(corp, removePunctuation, ucp = TRUE)

#create a Term Document Matrix to start

Webster.tdm <- TermDocumentMatrix(corp,
                                  control =
                                    list(stopwords = TRUE,
                                         stemming = TRUE,
                                         removeNumbers = TRUE,
                                         wordLengths = c(3, Inf)))
inspect(Webster.tdm[1:10,])                                        
# It still has quite a bit of random symbols, probably because the OCR on it is not amazing?

# Interesting analysis of the frequent terms, saved for looking again
Frequent_terms <- findFreqTerms(Webster.tdm, lowfreq = 100, highfreq = Inf)

# This is now on my own, trying to find the best way to extract the place names and the words
# associated with it, like earthquakes and epidemics, and the years, if possible

pdf_ocr_text(
  "Original Webster Work_OCR.pdf",
  dpi = 600,
  language = "eng")

#making a link to the new png files in the 385P folder
files2 <- list.files(pattern = "png$")

#having the tesseract package OCR the png files (and trying to save it as its own element)
#then I can try to save it somehow, like the write lines function I tried below
Webster_text <- tesseract::ocr(files2)

save(Webster_text, file = "Webster Text.RData")

class(Webster_text) # says its a character

#trying to save the output as something so I don't have to run the OCR every single time
# running the OCR takes some time

write_lines("webster_text", file = "Webster Lines", sep = "\\r\\n")
Webster_Lines <- capture.output("Webster_text")

# I keep getting the error message "Error in as.character(x) : cannot coerce type 
#'closure' to vector of type 'character' and after looking at definitions and Stack Overflow
#I think the problem is that it's trying to convert a png to txt and won't do it (for obvious reasons)
# I'm going to try to create a new file of the OCR text instead (I think?)

#Great explanation from StackOverflow: "The use of "closure" in this reference means that 
#something that should be a character is actually a function. For instance, often people use 
#data as a variable name, storing a frame within it; but if they restart R and forget to 
#redefine the variable, they are instead referencing the function named data, 
#returning some error like object of type 'closure' ...."
                       
#Trying something new to get the data saved, I "imported" the data from the Webster text output
# that running the OCR gave me.

Webster.tmp <- VCorpus(VectorSource(Webster_text),
                       readerControl = list(language = "eng"))
save(Webster.tmp, file = "Webster_by_file.RData")
save(Webster.tmp, file = "Webster_files.rds")

#This at least gave me a file that has the results broken out by each image and the ocr results

# saving everything 
save(corp, file = "corpdata.RData")
save(Webster.tdm, file = "Webster_tdm.RData")
save(Frequent_terms, file = "frequent_terms.RData")
save(Webster_Lines, File = "Webster_Lines.RData")

# I found a copy of the Webster book that was already translated into plain text, so I want to
# read that in so I have a cleaner copy to work with.


