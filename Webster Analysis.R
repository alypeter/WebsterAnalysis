library(tidyverse)
library(tm)
library(SnowballC)
library(tesseract)
library(stringr)
library(koRpus)
library(spacyr)
library(tidytext)

# I'm going to read in the cleaner text version of Webster's book that I found on the UMichigan website
#This gave me a data set, but I'm not sure how to work with it since it isn't the text itself
Brief_History <- VCorpus(VectorSource(x = "A_Brief_History.txt"), 
        readerControl = list(language = "eng"))
save(Brief_History, file = "Brief_History.RData")

#This gave me the text as a character, which won't work for some of the functions I want to do
Webster_book <- "A_Brief_History.txt"
Webster_content <- readLines(Webster_book)
save(Webster_content, file = "Webster_content.RData")
class(Webster_content) # character

#This finally gave me the data in a 1 column table, which I can search, with every line from
# the text document as a line in the column (with empty lines for spaces)
Webster_as_list = 
  tibble(file = Webster_content) 
save(Webster_as_list, file = "Webster_as_list.RData")

count_sentences(Webster_as_list) #says the input must be a character vector
count_sentences(Webster_content) # gave me a readout


# going to try to tokenize the text, which means making the 'tokens' sentences instead of words
# or paragraphs, but I'm not quite sure which of the above data or values will work

#First I had to find a way to take out all the empty columns from the text
# But this only applies to the list version of the text, not the sentences
Webster_as_list <- Webster_as_list[!apply(Webster_as_list == "", 1, all),]
class(Webster_as_list) #tbl_df - table data frame

#Now I can try to tokenize it but only the characters (not the data frame)
Sentences <- tokenize_sentences(Webster_content)
Webster_sentences <- Sentences
save(Webster_sentences, File = "Webster_sentences.RData")

#The sentences are close-ish to being sentences. So now I should be able to detect and search for
#different words to pull out the entire sentence around it

# Need to create a list of keywords to search for

disease_keywords <- c("epidemic","epidemics","miasma", "disease","diseases","pestilence","fever","fevers","mortality")
Earth_keywords <- c("earthquake","earthquakes","tempest","tempests","eruption")
air_keywords <- c("air", "vapor", "vapour")
save(disease_keywords, file = "disease_keywords.RData")
save(Earth_keywords, file = "Earth_keywords.RData")
save(air_keywords, file = "air_keywords.RData")

#going to try to find the keywords in the text, just to see if it works

str_locate(Webster_content, air_keywords) #this one didn't work

#I had to flatten the string into a single string to use the locator
 # Then I ran that result through the string locate, and got results!
Air_words <- Webster_content %>% str_flatten(collapse = "|") %>% str_locate_all(air_keywords)
save(Air_words, file = "Air_words.RData")

# I should be able to do this with the other keyword groups
disease_words <- Webster_content %>% 
  str_flatten(collapse = "|") %>% 
  str_locate_all((disease_keywords))
save(disease_words, file = "disease_words.RData")

earth_words <- Webster_content %>%
  str_flatten(collapse = "|") %>%
  str_locate_all((Earth_keywords))
save(earth_words, file = "earth_words.RData")

# Next I should try pulling the words from each side of each of the words/results
# found this function to pull whole sentences with the specific keywords
# and it seemed to work? But also not really, since it just resulted in the entire Webster_content
# appearing in the result, not parsed by keywords 
# nevermind, I forgot to adjust the function name after editing it. Now it works!

extract_sentences_with_keywords <- function(text, keywords){
  sentences <- unlist(str_split(text, "(?<![A-Z][a-z]\\.)(?<=\\.)\\s+"))
  pattern <- str_c(keywords, collapse = "|")
  sentences_with_keywords <- sentences[str_detect(sentences, regex(pattern, ignore_case = TRUE))]
  return(sentences_with_keywords)}

air_sentences <- extract_sentences_with_keywords(Webster_content,air_keywords)
print(air_sentences)  
air_sentences
save(air_sentences, file = "air_sentences.RData")
view(air_sentences, title = "air_sentences")

#It WORKED!!! I was able to parse out just the sentences with my keywords in it! 
# Now to try it on the other keywords, which have more words in the keyword list

#going to try it for disease words

earth_sentences <- extract_sentences_with_keywords(Webster_content, Earth_keywords)
save(earth_sentences, file = "earth_sentences.RData")
view(earth_sentences, title = "earth_sentences")

#and now for disease words

disease_sentences <- extract_sentences_with_keywords(Webster_content, disease_keywords)
save(disease_sentences, file = "disease_sentences.RData")
view(disease_sentences, title = "disease_sentences")

#they all worked!! 
# Now, I just need to figure what the next step should be