library(dplyr)
library(stringr)

# 1. Read in
beyonce <- read.csv("beyonce.csv", header = T, stringsAsFactors = F)
stoplist <- scan("stoplist_HW.txt", what = character(), sep = "\n")

#2. Frequency list (most frequent words at top)
beyonce_freq <- beyonce %>% count(Word, name = "Freq") %>% arrange(desc(Freq))

#3. Type Token Ration
types <- length(beyonce_freq$Word)
tokens <- length(beyonce)
types/tokens

#4. Filter out stop list
stoplist <- scan("stoplist_HW.txt", what = character(), sep = "\n")

beyonce_freq_clean <- beyonce_freq %>% filter(!(Word %in% stoplist))
head(beyonce_freq, 20)

# 5. Bigram Frequency
beyonce_bigram <- beyonce_freq_clean %>% mutate(Bigram = str_c(lag(Word), Word, sep = " "))

beyonce_bigram %>% 
  count(Bigram, name = "Freq") %>% 
  filter(!is.na(Bigram)) %>% 
  arrange(desc(Freq))

head(beyonce_bigram)

#        Word   Bigram
# 1st    Oh     NA
# 2nd    baby   oh baby
# 3rd    like   baby like
# 4th    got    like got
# 5th    can    got can

# 6. Concordance lines - "love"

beyonce_conc <- beyonce %>% 
  mutate(Left = str_c(lag(Word, n = 2),
                      lag(Word, n = 1),
                      sep = " "),
         Right = str_c(lead(Word, n = 1),
                       lead(Word, n = 2),
                       sep = " ")) %>%
  filter(str_detect(Word, pattern = "love")) %>% 
  select(Left, Word, Right)

head(beyonce_conc)


#Part 2: 
# 1. Create sub corpora

beyonce_solo <- beyonce %>% filter(is.na(Featuring))
beyonce_collab <- beyonce %>% filter(!is.na(Featuring))

# 2. Frequency of "oh" and "baby" in each corpora 

beyonce_solo_oh <- beyonce_solo %>% filter(Word == "oh") %>% count(Word, name = "oh")
beyonce_solo_baby <- beyonce_solo %>% filter(Word == "baby") %>% count(Word, name = "baby")                 
beyonce_collab_oh <- beyonce_collab %>% filter(Word == "oh") %>% count(Word, name = "oh")
beyonce_collab_baby <- beyonce_collab %>% filter(Word == "baby") %>% count(Word, name = "baby")


#3. Matrix
bmat <- matrix(c(126, 72,
                 36, 56), 2, 2,
               dimnames = list(c("Solo", "Collab"), c("oh", "baby")))

# 4.
chisq.test(bmat, correct = FALSE)

# 5.
num <- chisq.test(bmat, correct = FALSE)$statistic
denom <- sum(bmat)*(min(dim(bmat))-1)
phi <- sqrt(num/denom)
phi

chisq.test(bmat, correct = FALSE)$residual

assocplot(bmat)

#This means that "oh" used more than expected in Solo songs from Beyonce, while "baby" is used 
# less than expected and for Collab songs the reverse is true.

# 6.

str(beyonce_collab)
str(beyonce_solo)

bsolo_me <- beyonce_solo[beyonce_solo$Word=="me",]
bcollab_me <- beyonce_collab[beyonce_collab$Word=="me",]

bsolo_me_total <- length(beyonce_solo$Word)
View(bsolo_me_total)
bcollab_me_total <- length(beyonce_collab$Word)
View(bcollab_me_total)

collab_total <- 721
solo_total <- 750
collab_me <- 106
solo_me <- 103

solo_expected <- solo_total*(solo_me+collab_me) / (bsolo_me_total+bcollab_me_total)
collab_expected <- collab_total*(solo_me+collab_me) / (bsolo_me_total+bcollab_me_total)

G2 <- 2*((solo_me*log(solo_me/solo_expected)) + (collab_me*log(collab_me/collab_expected)))
G2

# The result is 784.2921 meaning the word "me" is very key to Beyonce Solo

# 7. Calculate the mutual information of ‘got me'
w1 <- beyonce_freq[beyonce_freq$Word=="got",]
w2 <- beyonce_freq[beyonce_freq$Word=="me",]

#using beyonce_freq and beyonce_bigram 

# And beyonce size word count
b_total <- sum(beyonce$Word)

# We need to get the bigram frequency of 'mr_darcy'
wdpr <- beyonce_bigram[beyonce_bigram$Bigram=="got_me",]

# Calculate mutual information
pmi_got_me <- log2((wdpr/b_total)/((w1/b_total)*(w2/b_total)))
pmi_got_me

# This means

# 8.
ranking <- beyonce %>% group_by(Rank, Song) %>% 
  count(Word, name = "Freq") %>% 
  filter(Word == "me") %>% arrange(Rank)
