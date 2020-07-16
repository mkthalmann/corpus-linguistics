import pandas as pd
import re

# empty lists that we will populate by finding regular expression patterns
# in the raw data. In the final step, we will combine the lists into a data 
# frame which will be our final data for use in R
msg_date = []
msg_time = []
msg_sender = []
msg = []

# open the chat log in UTF-8 format
with open('data_raw/_chat.txt', 'r', encoding='utf-8') as f:
    string = f.readlines()
    # iterate over the entire log to identify different regex patterns
    for row in range(1, len(string)):
 
        # the date pattern we want to look for
        date_pattern = '(\d+.\d+.\d+)'
        # if the date is found, add it to the list, if not add NA
        try:
            date = re.search(date_pattern, string[row]).group(0)
        except AttributeError:
            date = "NA"
        msg_date.append(date)
 
        # same process for time stamps
        time_pattern = '\d+:\d+:\d+'
        try:
            time = re.search(time_pattern, string[row]).group(0)
        except AttributeError:
            time = "NA"
        msg_time.append(time)
 
        # now find the senders of the individual messages
        person_pattern = '[\]]\s\w+'
        try:
            # use the entire match but delete the closing square bracket
            person = re.search(person_pattern, string[row]).group(0).replace("] ", "")
        except AttributeError:
            person = "NA"
        msg_sender.append(person)
 
        # and, finally, the messages themselves
        msg_pattern = '(:\s).*'    
        try:
            # delete the colon and the space that follows it
            message = re.search(msg_pattern, string[row]).group(0).replace(": ", "")
        except AttributeError:
            message = "NA"
        msg.append(message)

# combine the lists into a data frame and add a row that contains the column names
df = pd.DataFrame(list(zip(msg_date, msg_time, msg_sender, msg)),
                  columns=['date', 'time', 'sender', 'message'])

# export the data frame as a csv file
df.to_csv("data/messages_cleaned.csv", index=False)
