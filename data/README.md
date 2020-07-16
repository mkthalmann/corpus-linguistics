# About the files in this subdirectory

- `messages_full.csv` is the full data set at the end of all analyses (minus the message texts)
- `message_types.csv` contains the results of message types using regular regular expressions. The
regular expression patterns are found in the `assets` directory
- `sentence_types.csv` is the breakdown of exclamative/imperative/interrogative sentences as
identified per message-terminating punctuation
- `sentiment_stats.csv` holds a detailed overview of the sentiment analysis, separated the months in
which the messages were sent
- `sentiments.csv` contains the raw sentiment scores per message and sender
- `unique_andrew.txt` and `unique_maik.txt` are word lists of those whitespace-delimited character
sequences that were unique to each speaker
