# load in packages

library(dplyr)
library(stringr)

# 1. Read in and rename
Meta <- read.csv("FakeNewsMeta.csv", header = T, stringsAsFactors = FALSE)

# 2. Create new column...

View(Meta$published) 
Meta <- Meta %>% mutate(date_published = str_extract(published, "[:digit:]{4}\\-[:digit:]{2}\\-[:digit:]{2}"))

# 3. Find the average number of shares
Sum1 <- Meta %>% group_by(type) %>% 
  summarise(AverageShares = mean(shares))

View(Sum1)
  # the highest was Bias

# 4. Find number of articles per author

Meta %>% count(author) %>% arrange(desc(n))

#   1 ""(no author)    1943
#   2 admin            247
#   3 Alex Ansary      100
#   4 Eddy Lavine      100
#   5 Editor           100
#   6 Gillian          100
#   7 Pakalert         100s
#   8 Starkman         100
#   9 BareNakedIslam   99
#   10 Dave Hodges     99

# 5. Find each author's number of posts per day...


Meta <- Meta %>% 
  mutate(PPD = str_extract(author, "^[:alpha:]+|[:digit:]+$[:alpha:]+|[:digit:]+")) %/%

  count(PPD, date_published) %>% 
  arrange(desc(n))
  
# 6. Eddy Lavine.. frequency over time...

# 7. Using th einformation in Meta find the country Eddy was posting from

# 8.KNowing what we know about Eddy's posting frequency... 



