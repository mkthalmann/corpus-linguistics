## ----setup, echo = FALSE, warning = FALSE, message = FALSE--------------------
library(here)
library(knitr)

knitr::opts_chunk$set(
  echo = TRUE,
  warning = FALSE,
  message = FALSE,
  comment = NA,
  collapse = TRUE,
  cache = TRUE,
  rownames.print = FALSE,
  cols.print = 7
)

knitr::opts_chunk$set(cache.path = here("cache/"))

knitr::opts_chunk$set(
  fig.path = here("figs/"),
  fig.show = "hold",
  fig.align = "center",
  out.width = "100%"
)

knitr::opts_chunk$set(
  tidy = "styler",
  tidy.opts = list(
    scope = "tokens",
    strict = TRUE,
    indent_by = 4
  )
)

# compress png images
knit_hooks$set(optipng = hook_optipng)


## ----loadpack, results='hide'-------------------------------------------------
# load packages
packages <- c(
  # general
  "tidyverse", "forcats", "here", "psych", "janitor",
  "lubridate", "syn", "broom", "stringr",
  # rmarkdown related
  "kableExtra", "styler", "knitr",
  # plotting
  "gganimate", "scales", "patchwork",
  "scico", "hrbrthemes",
  # corpus and text processing
  "wordcloud", "tm", "syuzhet", "ngram"
)
xfun::pkg_attach(packages, install = TRUE)
# allow duplicate labels for chunks
options(
  scipen = 999, width = 100, max.print = 999
)

# seed as random number generation tasks
set.seed(1234)


## ----theming, echo=FALSE------------------------------------------------------
add <- 0
theme_maik <- function() {
  theme_ipsum_rc() %+replace%
    theme(
      legend.key = element_rect(colour = NA),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.key.size = unit(0.3, "cm"),
      legend.margin = margin(0, 0, 0, 0, "cm"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_line(colour = "gray"),
      panel.grid.minor.y = element_line(colour = "gray"),
      panel.spacing = unit(.5, "lines"),
      plot.margin = unit(c(.1, .1, .1, .1), "cm"),
      axis.line = element_line(colour = "gray", size = .5),
      strip.text.x = element_text(size = 9),
    ) +
    theme(
      legend.text = element_text(size = 7 + add),
      legend.title = element_text(size = 8 + add),
      axis.text.x = element_text(size = 7 + add),
      axis.text.y = element_text(size = 7 + add),
      axis.title.x = element_text(size = 7 + add),
      axis.title.y = element_text(size = 7 + add),
      plot.title = element_text(size = 13),
      plot.subtitle = element_text(size = 10 + add),
      plot.caption = element_text(size = 7 + add),
      strip.text.x = element_text(size = 7 + add),
      strip.text.y = element_text(size = 7 + add)
    )
}

theme_set(theme_maik())


## import pandas as pd

## import re

##
## # empty lists that we will populate by finding regular expression patterns

## # in the raw data. In the final step, we will combine the lists into a data

## # frame which will be our final data for use in R

## msg_date = []

## msg_time = []

## msg_sender = []

## msg = []

##
## # open the chat log in UTF-8 format

## with open('data_raw/_chat.txt', 'r', encoding='utf-8') as f:

##     string = f.readlines()

##     # iterate over the entire log to identify different regex patterns

##     for row in range(1, len(string)):

##

##         # the date pattern we want to look for

##         date_pattern = '(\d+.\d+.\d+)'

##         # if the date is found, add it to the list, if not add NA

##         try:

##             date = re.search(date_pattern, string[row]).group(0)

##         except AttributeError:

##             date = "NA"

##         msg_date.append(date)

##

##         # same process for time stamps

##         time_pattern = '\d+:\d+:\d+'

##         try:

##             time = re.search(time_pattern, string[row]).group(0)

##         except AttributeError:

##             time = "NA"

##         msg_time.append(time)

##

##         # now find the senders of the individual messages

##         person_pattern = '[\]]\s\w+'

##         try:

##             # use the entire match but delete the closing square bracket

##             person = re.search(person_pattern, string[row]).group(0).replace("] ", "")

##         except AttributeError:

##             person = "NA"

##         msg_sender.append(person)

##

##         # and, finally, the messages themselves

##         msg_pattern = '(:\s).*'

##         try:

##             # delete the colon and the space that follows it

##             message = re.search(msg_pattern, string[row]).group(0).replace(": ", "")

##         except AttributeError:

##             message = "NA"

##         msg.append(message)

##
## # combine the lists into a data frame and add a row that contains the column names

## df = pd.DataFrame(list(zip(msg_date, msg_time, msg_sender, msg)),

##                   columns=['date', 'time', 'sender', 'message'])

##
## # export the data frame as a csv file

## df.to_csv("data/messages_cleaned.csv", index=False)


## ----import-------------------------------------------------------------------
# import data from python script output
d <- read.csv(here("data", "messages_cleaned.csv"), as.is = TRUE)
data.frame(
  variable = names(d),
  class = sapply(d, typeof),
  first_values = sapply(d, function(x) {
    paste0(head(x, 3),
      collapse = ", "
    )
  }),
  row.names = NULL
)
head(d)


## ----emptycol-----------------------------------------------------------------
# remove empty rows
d <- d[complete.cases(d), ]
anyNA(d)


## ----freqsender---------------------------------------------------------------
# sender summary
senders <- tabyl(d$sender)
adorn_pct_formatting(senders)


## ----convert------------------------------------------------------------------
# data format conversions
# adding date, time, month and week day information
d <- d %>%
  mutate(
    date = dmy(date),
    time = hour(hms(time)),
    month = months(date, abbreviate = TRUE),
    month = factor(month,
      levels = c(
        "Sep", "Oct", "Nov", "Dec",
        "Jan", "Feb", "Mar", "Apr",
        "May", "Jun", "Jul", "Aug"
      )
    ),
    day = wday(date, label = TRUE),
    day = factor(day,
      levels = c(
        "Mon", "Tue", "Wed",
        "Thu", "Fri", "Sat", "Sun"
      )
    )
  )


## ----afterdate----------------------------------------------------------------
head(d, 13)


## ----length-and-count---------------------------------------------------------
# length of messages and message count per sender
d <- d %>%
  mutate(length = nchar(message)) %>%
  group_by(sender) %>%
  mutate(count = length(message)) %>%
  ungroup()


## ----countwords---------------------------------------------------------------
# count all the words
d$word <- str_count(d$message, boundary("word"))
# amount of words in the corpus
sum(d$word)


## ----freqword, fig.height = 3-------------------------------------------------
# word summary
words <- tabyl(d$word) %>%
  adorn_pct_formatting() %>%
  head(15)

ggplot(words, aes(x = `d$word`, y = n)) +
  geom_line(color = "#98D4D4FF") +
  scale_x_continuous(breaks = pretty_breaks(n = 15)) +
  ylim(0, max(words$n)) +
  labs(y = "Occurrences", x = "Message Length in Words")


## ----desclen------------------------------------------------------------------
# descriptives for message length
describeBy(d$length, d$sender, mat = TRUE, digits = 2)


## ----descword-----------------------------------------------------------------
# descriptives for word count
describeBy(d$word, d$sender, mat = TRUE, digits = 2)


## ----msgcounts, optipng = '-o7'-----------------------------------------------
msgct1 <- ggplot(d, aes(x = date, color = sender)) +
  stat_bin(geom = "line") +
  labs(
    x = "Time of Year",
    y = "Message Count",
    color = "Sender"
  ) +
  theme(
    legend.position = c(0.85, 0.95),
    axis.text.x = element_text(angle = 60, hjust = 1)
  ) +
  scale_x_date(breaks = pretty_breaks(n = 12)) +
  scale_color_manual(values = c("#98D4D4FF", "#FF929AFF"))

msgct2 <- ggplot(d, aes(x = time, fill = sender)) +
  geom_histogram(position = "dodge") +
  labs(
    x = "Time of Day",
    y = "Message Count",
    fill = "Sender"
  ) +
  theme(
    legend.position = c(0.3, 0.95),
    legend.direction = "horizontal"
  ) +
  scale_fill_manual(values = c("#98D4D4FF", "#FF929AFF"))

msgct3 <- ggplot(d, aes(x = day, fill = sender)) +
  geom_bar(position = "dodge") +
  labs(
    x = "Weekday",
    y = "Message Count",
    fill = "Sender"
  ) +
  theme(legend.position = c(0.75, 0.95)) +
  scale_fill_manual(values = c("#98D4D4FF", "#FF929AFF"))

msgct1 /
  (msgct2 | msgct3)


## ----freqweek-----------------------------------------------------------------
# weekday summary
days <- tabyl(d$day)
adorn_pct_formatting(days)


## ----messagelength, optipng = '-o7'-------------------------------------------
msgln1 <- ggplot(d, aes(x = date, y = length, color = sender)) +
  stat_summary(fun = mean, geom = "line") +
  labs(
    x = "Time of Year",
    y = "Message Length",
    color = "Sender"
  ) +
  theme(
    legend.position = c(0.3, 0.95),
    legend.direction = "horizontal",
    axis.text.x = element_text(angle = 60, hjust = 1)
  ) +
  scale_x_date(breaks = pretty_breaks(n = 12)) +
  scale_color_manual(values = c("#98D4D4FF", "#FF929AFF"))

msgln2 <- ggplot(d, aes(x = time, y = length, color = sender)) +
  stat_summary(fun = mean, geom = "line") +
  labs(
    x = "Time of Day",
    y = "Mean Character and Word Amount Per Message",
    color = "Sender"
  ) +
  stat_summary(aes(y = word),
    fun = mean, geom = "line",
    linetype = "dashed"
  ) +
  theme(
    legend.position = c(0.5, 0.95),
    legend.direction = "horizontal"
  ) +
  scale_color_manual(values = c("#98D4D4FF", "#FF929AFF"))

msgln1 /
  msgln2


## ----sentencetypes------------------------------------------------------------
# analyse sentence types including proportions
sentence_types <- paste(tolower(d$message), collapse = " ")
# add space before sentence type punctuation
sentence_types <- gsub("\\!", " !", sentence_types)
sentence_types <- gsub("\\?", " ?", sentence_types)
sentence_types <- gsub("\\.", " .", sentence_types)
# split at space to generate individual words
sentence_types <- data.frame(strsplit(sentence_types, " "))[, 1]
# subset out only ?, ! and .
sentence_types <- factor(sentence_types[sentence_types %in%
  c("?", "!", ".")])


## ----sentencepercent----------------------------------------------------------
# compute the overall occurrences as well as their ratio
sentence_types <- tabyl(sentence_types)
names(sentence_types) <- c("sentence", "occurrences", "percent")
sentence_types$percent <- percent(sentence_types$percent)

sentence_types


## ----newcols------------------------------------------------------------------
# function to read in the patterns from the text files with regex patterns
# and count the occurrences in the message column in form of a vector
# with each value representing count per row (message)
input_pattern <- function(file) {
  fullfile <- here("assets", paste0(file, ".txt"))
  pattern <- readChar(fullfile, file.info(fullfile)$size)
  str_count(d$message, regex(pattern, ignore_case = TRUE))
}

# look for specific message type and create a counting column
# for all eight patterns
d <- d %>%
  mutate(
    ily = input_pattern("ily"),
    miss = input_pattern("miss"),
    baby = input_pattern("pet"),
    sex = input_pattern("sex"),
    sorry = input_pattern("sorry"),
    insult = input_pattern("insult"),
    tired = input_pattern("tired"),
    drag = input_pattern("drag")
  )


## ----freqmesstype-------------------------------------------------------------
# check the overall amounts of the new columns
d %>%
  gather(
    "type", "count", ily, miss, baby, sex,
    insult, sorry, tired, drag
  ) %>%
  group_by(type) %>%
  summarise(count = sum(count)) %>%
  arrange(desc(count)) %>%
  mutate(prop = percent(count / sum(count))) -> dlong

dlong


## ----freqmesstypesender-------------------------------------------------------
d %>%
  gather(
    "type", "count", ily, miss, baby, sex,
    insult, sorry, tired, drag
  ) %>%
  group_by(type, sender) %>%
  summarise(count = sum(count)) %>%
  arrange(desc(count))


## ----convertofactor, echo=FALSE-----------------------------------------------
# convert columns to factors
d <- mutate_at(d, c("sender"), factor)


## ----lovemsgplt, optipng = '-o7'----------------------------------------------
ilyplt1 <- ggplot(d, aes(x = date, y = ily, color = sender)) +
  stat_summary(fun = mean, geom = "line") +
  labs(
    x = "Time of Year",
    y = "I-Love-You Type Messages",
    color = "Sender"
  ) +
  scale_color_manual(values = c("#98D4D4FF", "#FF929AFF")) +
  coord_cartesian(ylim = c(0, 2)) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  scale_x_date(breaks = pretty_breaks(n = 12))

ilyplt2 <- ggplot(d, aes(x = month, y = ily, color = sender, group = sender)) +
  stat_summary(fun = mean, geom = "line") +
  stat_summary(fun = mean, geom = "point") +
  scale_color_manual(values = c("#98D4D4FF", "#FF929AFF")) +
  labs(
    x = "Month",
    y = "I-Love-You Type Messages"
  ) +
  guides(color = FALSE)

ilyplt3 <- ggplot(d, aes(x = day, y = ily, color = sender, group = sender)) +
  stat_summary(fun = mean, geom = "line") +
  stat_summary(fun = mean, geom = "point") +
  scale_color_manual(values = c("#98D4D4FF", "#FF929AFF")) +
  stat_summary(
    fun.data = mean_se, geom = "errorbar",
    width = 0.25, alpha = .5, linetype = 1,
    color = "#98D4D4FF"
  ) +
  labs(
    x = "Weekday",
    y = "I-Love-You Type Messages"
  ) +
  guides(color = FALSE)

ilyplt1 /
  (ilyplt2 | ilyplt3)


## ----typeplt, optipng = '-o7', fig.height = 3---------------------------------
d %>%
  gather("type", "count", ily, miss, baby, sex) %>%
  ggplot(aes(x = month, y = count, color = type, group = type)) +
  stat_summary(fun = mean, geom = "line") +
  labs(
    x = "Month",
    y = "Message Count",
    color = "Type",
    group = "Type"
  ) +
  scale_color_manual(values = scico(4, palette = "batlow")) +
  theme(
    legend.position = c(0.52, 0.95),
    legend.direction = "horizontal",
    axis.text.x = element_text(angle = 60, hjust = 1)
  ) +
  facet_wrap(~sender)


## ----typegif, fig.height = 3, gganimate = list(fps = 5, nframes = 50)---------
d %>%
  gather(
    "type", "count", ily, baby, miss, sex,
    insult, sorry, tired, drag
  ) %>%
  mutate(month = factor(month, levels = month.abb)) %>%
  count(type, date, month, wt = count) %>%
  ggplot(aes(reorder(type, -n), n, color = type)) +
  geom_line(
    mapping = aes(group = 1),
    stat = "summary", fun = mean
  ) +
  geom_jitter(alpha = .3) +
  geom_point(stat = "summary", fun = mean, size = 3) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.35) +
  labs(
    title = "Mean of Message Type Occurrence in: {closest_state}",
    x = "Message Type",
    y = "Mean Occurrence"
  ) +
  guides(color = FALSE) +
  coord_cartesian(ylim = c(0, 15)) +
  scale_color_scico_d(palette = "batlow") +
  transition_states(month,
    state_length = 3,
    transition_length = 5
  )


## ----messages-----------------------------------------------------------------
# create one long string with all the messages
messages_all <- paste(tolower(d$message), collapse = " ")

# remove some strings that would hinder further analyses
messages_all <- gsub(",|\\(|)|\\.|:|;|\\!|\\?", "", messages_all)
messages_all <- gsub("[[:punct:]]", "", messages_all)

# create a long string for each of us
messages_all_a <- paste(tolower(d$message[d$sender == "Andrew"]),
  collapse = " "
)
messages_all_m <- paste(tolower(d$message[d$sender == "Maik"]),
  collapse = " "
)


## ----assemblecorps------------------------------------------------------------
# assemble the corpora
corpus <- VCorpus(VectorSource(messages_all))
corpus_andrew <- VCorpus(VectorSource(messages_all_a))
corpus_maik <- VCorpus(VectorSource(messages_all_m))

# generate list of words to be removed from the corpora
mystopwords <- c(
  stopwords("english"), "'re", "'ll", "omit", "didn'",
  "’ve", "’ll", "didn’", "didn’", "don’", "’re",
  "image", "video", "doesn’", "omit", "omitted", "can’t",
  "isn’t", "wasn’t", "let’s", "haven’t", "won’t", "couldn’t"
)
mystopwords <- setdiff(mystopwords, c("love"))

# function to perform some cleanup operations
prep_corpus <- function(c) {
  c <- tm_map(c, content_transformer(tolower))
  c <- tm_map(c, removeWords, mystopwords)
  c <- tm_map(c, removeNumbers)
  c <- tm_map(c, removePunctuation)
  c <- tm_map(c, stripWhitespace)
  # seems to mess everything up, so I do not stem this corpus
  # c <- tm_map(c, stemDocument)
  c
}

# use function to prep all three corpora
corpus <- prep_corpus(corpus)
corpus_andrew <- prep_corpus(corpus_andrew)
corpus_maik <- prep_corpus(corpus_maik)


## ----dtmall-------------------------------------------------------------------
# generate a document-term matrix
dtm_all <- DocumentTermMatrix(corpus)
# length should be total number of terms (i.e., unique words)
corpfreq <- colSums(as.matrix(dtm_all))
length(corpfreq)


## ----clouds, optipng = '-o7', fig.height = 4----------------------------------
cols <- c(
  "#1F7A80FF", "#79C4B2FF", "#98D4D4FF",
  "#FF929AFF", "#FF6359FF", "#9C5568FF"
)

wordcloud(corpus,
  max.words = 150, random.order = FALSE,
  min.freq = 5, rot.per = 0, use.r.layout = TRUE,
  colors = cols, scale = c(3, .7)
)


## ----dtma---------------------------------------------------------------------
dtm_a <- DocumentTermMatrix(corpus_andrew)
# length should be total number of terms
corpfreq <- colSums(as.matrix(dtm_a))
length(corpfreq)


## ----cloudandrew, optipng = '-o7', fig.height = 4-----------------------------
wordcloud(corpus_andrew,
  max.words = 150, random.order = FALSE,
  min.freq = 5, rot.per = 0, use.r.layout = TRUE,
  colors = cols, scale = c(3, .7)
)


## ----dtmm---------------------------------------------------------------------
dtm_m <- DocumentTermMatrix(corpus_maik)
# length should be total number of terms
corpfreq <- colSums(as.matrix(dtm_m))
length(corpfreq)


## ----cloudmaik, optipng = '-o7', fig.height = 4-------------------------------
wordcloud(corpus_maik,
  max.words = 150, random.order = FALSE,
  min.freq = 5, rot.per = 0, use.r.layout = TRUE,
  colors = cols, scale = c(3, .7)
)


## ----freqterm-----------------------------------------------------------------
# list words that occur at least 150 times for each corpus
findFreqTerms(dtm_a, lowfreq = 150)
findFreqTerms(dtm_m, lowfreq = 150)


## ----worddiffa----------------------------------------------------------------
andrew_unique <- setdiff(
  findFreqTerms(dtm_a, lowfreq = 5),
  findFreqTerms(dtm_m, lowfreq = 5)
)


tibble(
  first15 = head(andrew_unique, 15),
  last15 = tail(andrew_unique, 15)
)


## ----worddiffm----------------------------------------------------------------
maik_unique <- setdiff(
  findFreqTerms(dtm_m, lowfreq = 5),
  findFreqTerms(dtm_a, lowfreq = 5)
)

tibble(
  first15 = head(maik_unique, 15),
  last15 = tail(maik_unique, 15)
)


## ----preprocess---------------------------------------------------------------
# function to preprocess the string
prep <- function(x) {
  gramstring <- preprocess(x,
    case = "lower",
    remove.punct = TRUE
  )
  gramstring <- gsub(
    "omit|omitted|image", "",
    gramstring
  )
  gramstring
}

# preprocessing for overall and per sender strings
gramstring <- prep(messages_all)
gramstring_a <- prep(messages_all_a)
gramstring_m <- prep(messages_all_m)


## ----gramsummary--------------------------------------------------------------
# generate a summary
# for all messages combined
a <- string.summary(gramstring)
data.frame(a[1], a[2], a[6], av_word_length = a[[2]] / a[[6]])


## ----gramsummary_a------------------------------------------------------------
# for andrew's
a <- string.summary(gramstring_a)
data.frame(a[1], a[2], a[6], av_word_length = a[[2]] / a[[6]])


## ----gramsummary_m------------------------------------------------------------
# for maik's
a <- string.summary(gramstring_m)
data.frame(a[1], a[2], a[6], av_word_length = a[[2]] / a[[6]])


## ----trigrams-----------------------------------------------------------------
# process trigrams
tri <- ngram(gramstring, n = 3)
tri_a <- ngram(gramstring_a, n = 3)
tri_m <- ngram(gramstring_m, n = 3)


## ----extracttop---------------------------------------------------------------
# extract top trigrams
tridat <- head(get.phrasetable(tri), 15)
tridat_a <- head(get.phrasetable(tri_a), 15)
tridat_m <- head(get.phrasetable(tri_m), 15)


## ----triplts, optipng = '-o7'-------------------------------------------------
tri_all <- ggplot(tridat, aes(x = reorder(ngrams, freq), y = freq)) +
  geom_bar(stat = "identity", fill = "#98D4D4FF") +
  coord_flip() +
  labs(x = "Trigrams", y = "Count")

tri_a <- ggplot(tridat_a, aes(x = reorder(ngrams, freq), y = freq)) +
  geom_bar(stat = "identity", fill = "#98D4D4FF") +
  coord_flip() +
  labs(x = "Trigrams", y = "Count", title = "Andrew")

tri_m <- ggplot(tridat_m, aes(x = reorder(ngrams, freq), y = freq)) +
  geom_bar(stat = "identity", fill = "#98D4D4FF") +
  coord_flip() +
  labs(x = "Trigrams", y = "Count", title = "Maik")

tri_all /
  (tri_a | tri_m)


## ----babble_a-----------------------------------------------------------------
# for andrew's messages
tibble(
  message = replicate(
    10,
    babble(ngram(gramstring_a, n = 3), genlen = 5)
  )
)


## ----babble_m-----------------------------------------------------------------
# for maiks's messages
tibble(
  message = replicate(
    10,
    babble(ngram(gramstring_m, n = 3), genlen = 5)
  )
)


## ----sentiments---------------------------------------------------------------
# do the sentiment analysis for Andrew's messages
d_andrew <- filter(d, sender == "Andrew")
sent_andrew <- get_nrc_sentiment(d_andrew$message)

# sentiment analysis for Maik's messages
d_maik <- filter(d, sender == "Maik")
sent_maik <- get_nrc_sentiment(d_maik$message)

# combine the two datasets into a complete sentiment representation
sent <- sent_andrew %>%
  mutate(sender = "Andrew") %>%
  bind_rows(sent_maik %>%
    mutate(sender = "Maik"))

# and save it
write.csv(
  sent, here("data", "sentiments.csv"),
  row.names = FALSE, quote = FALSE
)


## ----sentcolsums--------------------------------------------------------------
# check the overall amounts of the new columns
sent_andrew %>%
  bind_rows(sent_maik) %>%
  summarise_all(list(sum))


## ----sentcolsums_a------------------------------------------------------------
sent_andrew %>%
  summarise_all(list(sum))


## ----sentcolsums_m------------------------------------------------------------
sent_maik %>%
  summarise_all(list(sum))


## ----sentpltsbar, optipng = '-o7'---------------------------------------------
plot_sent_all <- sent %>%
  gather("type", "count", everything()) %>%
  group_by(type) %>%
  summarise(count = sum(as.numeric(count))) %>%
  arrange(desc(count)) %>%
  ggplot(aes(x = reorder(type, count), y = count)) +
  geom_bar(stat = "identity", fill = "#98D4D4FF") +
  coord_flip() +
  labs(x = "Sentiment", y = "Count") +
  scale_y_continuous(breaks = pretty_breaks(n = 12))

plot_sent_a <- sent_andrew %>%
  gather("type", "count", everything()) %>%
  group_by(type) %>%
  summarise(count = sum(as.numeric(count))) %>%
  arrange(desc(count)) %>%
  ggplot(aes(x = reorder(type, count), y = count)) +
  geom_bar(stat = "identity", fill = "#98D4D4FF") +
  coord_flip() +
  labs(title = "Andrew", x = "Sentiment", y = "Count") +
  scale_y_continuous(
    breaks = pretty_breaks(n = 12),
    limits = c(0, 6000)
  )

plot_sent_m <- sent_maik %>%
  gather("type", "count", everything()) %>%
  group_by(type) %>%
  summarise(count = sum(as.numeric(count))) %>%
  arrange(desc(count)) %>%
  ggplot(aes(x = reorder(type, count), y = count)) +
  geom_bar(stat = "identity", fill = "#98D4D4FF") +
  coord_flip() +
  labs(title = "Maik", x = "Sentiment", y = "Count") +
  scale_y_continuous(
    breaks = pretty_breaks(n = 12),
    limits = c(0, 6000)
  )

plot_sent_all /
  (plot_sent_a | plot_sent_m)


## ----sentlists----------------------------------------------------------------
# read in the word lists to base the sentiment analysis on
neg_words <- scan(here("assets", "negative.txt"),
  sep = "\n", what = "char"
)
pos_words <- scan(here("assets", "positive.txt"),
  sep = "\n", what = "char"
)
head(pos_words, 15)
head(neg_words, 15)


## ----posnegscores-------------------------------------------------------------
# compute the positive and negative scores based on the word list
d$n_pos <- sapply(d$message, USE.NAMES = FALSE, function(x) {
  length(x[x %in% pos_words])
})
d$n_neg <- sapply(d$message, USE.NAMES = FALSE, function(x) {
  length(x[x %in% neg_words])
})


## ----sentsub------------------------------------------------------------------
# create new data set that contains a subset
# where each message is at least one word long
dsent <- subset(d, word > 0)
# compute some simple stats
dsent$pos_ratio <- dsent$n_pos / dsent$word
dsent$neg_ratio <- dsent$n_neg / dsent$word
dsent$sent_val <- dsent$pos_ratio - dsent$neg_ratio

# aggregate the data for plotting
info_df <- aggregate(word ~ month, data = dsent, mean)
info_df <- merge(info_df,
  aggregate(pos_ratio ~ month, data = dsent, mean),
  by = "month"
)
info_df <- merge(info_df,
  aggregate(neg_ratio ~ month, data = dsent, mean),
  by = "month"
)
info_df <- merge(info_df,
  aggregate(pos_ratio ~ month, data = dsent, sciplot::se),
  by = "month"
)
info_df <- merge(info_df, aggregate(
  neg_ratio ~ month,
  data = dsent, sciplot::se
),
by = "month"
)
info_df <- merge(info_df, aggregate(sent_val ~ month, data = dsent, mean),
  by = "month"
)
info_df <- merge(info_df, aggregate(sent_val ~ month,
  data = dsent,
  sciplot::se
), by = "month")
names(info_df)[3:8] <- c(
  "mean_pos_ratio", "mean_neg_ratio",
  "se.pos_ratio", "se.neg_ratio",
  "mean.sent_val", "se.sent_val"
)
info_df$sent_pol <- ifelse(info_df$mean.sent_val > 0, "positive", "negative")


## ----infodfhead---------------------------------------------------------------
head(info_df)


## ----meansentplt, optipng = '-o7'---------------------------------------------
# add sentiment analysis to main data set
d <- cbind(d, sent[1:(length(sent) - 1)])

sent_month <- d %>%
  gather(
    "sentiment", "count", positive, negative, trust, surprise,
    sadness, joy, fear, disgust, anticipation, anger
  ) %>%
  arrange(desc(count)) %>%
  ggplot(aes(
    x = month, y = count, color = reorder(sentiment, -count),
    group = reorder(sentiment, count)
  )) +
  geom_line(stat = "summary", fun = mean) +
  theme(legend.position = "bottom") +
  labs(
    x = "Month", y = "Mean Sentiment Count",
    color = "Sentiment", group = "Sentiment"
  ) +
  scale_color_manual(values = scico(10, palette = "batlow")) +
  guides(
    color = guide_legend(ncol = 6),
    group = guide_legend(ncol = 6)
  ) +
  facet_wrap(~sender, ncol = 1)

sent_month


## ----polplts, optipng = '-o7'-------------------------------------------------
# separate polarity based on the month
sentiment_df <- data.frame(
  month = rep(info_df$month, 2),
  value = c(
    info_df$mean_pos_ratio,
    info_df$mean_neg_ratio * -1
  ),
  errors = c(
    info_df$se.pos_ratio,
    info_df$se.neg_ratio
  ),
  polarity = rep(c(
    "positive", "negative"
  ),
  each = nrow(info_df)
  )
)
sentiment_df$polarity <- factor(sentiment_df$polarity, levels = c(
  "positive", "negative"
))

pol_both <- ggplot(data = sentiment_df, aes(
  x = month, y = value, fill = polarity
)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = value - errors, ymax = value + errors),
    width = 0.25, alpha = .5
  ) +
  labs(
    y = "Mean sentiment value", x = "Month", fill = "Polarity"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("#98D4D4FF", "#FF929AFF"))

pol_overall <- ggplot(data = info_df) +
  aes(
    x = reorder(month, mean.sent_val),
    y = mean.sent_val,
    fill = sent_pol
  ) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(
    ymin = mean.sent_val - se.sent_val,
    ymax = mean.sent_val + se.sent_val
  ),
  width = 0.25, alpha = .5
  ) +
  labs(y = "Mean sentiment value", x = "Month") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("#FF929AFF", "#98D4D4FF")) +
  guides(fill = FALSE) +
  coord_flip()

pol_both /
  pol_overall


## ----exportfiles--------------------------------------------------------------
# export final dataset exlcuding the message texts
d %>%
  select(-message) %>%
  write.table(here("data", "messages_full.csv"),
    row.names = FALSE, quote = FALSE
  )

# unique words
write.table(
  maik_unique, here("data", "unique_maik.txt"),
  row.names = FALSE, quote = FALSE
)
write.table(
  andrew_unique, here("data", "unique_andrew.txt"),
  row.names = FALSE, quote = FALSE
)

# sentence types
write.table(sentence_types, here("data", "sentence_types.csv"),
  row.names = FALSE, quote = FALSE
)

# message types
write.table(dlong, here("data", "message_types.csv"),
  row.names = FALSE, quote = FALSE
)

# sentiment analysis
write.table(info_df, here("data", "sentiment_stats.csv"),
  row.names = FALSE, quote = FALSE
)


## ----session, cache=FALSE-----------------------------------------------------
xfun::session_info(dependencies = FALSE)
