rm(list=ls())
setwd("~/Documents/project/viber-logs/")

library(stringr)
library(reshape2)
library(dplyr)
library(lubridate)
library(ggplot2)
source("plt.R")

# Trick to read lines that only contains ","
# Force the csv to have 5 columns, with the 4th ending with either 6 or 9
# (which are the last digits of our phone numbers

str <- readLines("Sara-all.csv", n=-1)
matches <- str_match(str, pattern = "\\s*(.+),\\s*(.+),\\s*(.+),\\s*(.+)[6,9],\\s*(.+)")
df <- data.frame(matches[, -1], stringsAsFactors=F)
colnames(df) <- c("date","time","sender","receiver","msg")

# Format the date and create a row with the number of characters of the messages
df <- df %>%
  mutate(posix.date=parse_date_time(paste0(date,time),"%d%m%y%H%M%S")) %>%           
  mutate(nb.char = nchar(msg)) %>%
  select(posix.date, sender, msg, nb.char) %>%
  arrange(as.numeric(posix.date))


# Change the senders' names
df <- df %>%
  mutate(sender = replace(sender, sender == "Sara", "Her")) %>%
  mutate(sender = replace(sender, sender == "Moi", "Him"))

## Bar charts number of message per sender
df.senders <- df %>%
  group_by(sender) %>%
  summarise(count = n(), n.char = sum(nchar(msg)))

ggplot(data=df.senders, aes(x=sender,y=count, fill=sender))+geom_bar(stat="identity")+
  theme_mini(scale_text = 1.2)+ylab("number of msgs")+ggtitle("Number of messages per sender")+
  scale_fill_brewer(palette = "Set1")+scale_color_brewer(palette = "Set1")
#ggsave("figs/nb_msg.png", width = 5, height = 3) 


## Time needed to for a message to be answered

df.diff <- df %>%
  mutate( is.diff.sender = (sender != lag(sender))) %>%
  filter( is.diff.sender ) %>%
  select( posix.date, sender ) %>%
  mutate( diff.time = as.numeric(posix.date - lag(posix.date) ) ) %>%
  filter(!is.na(diff.time))

# Median and mean time to answer
response.delay <- df.diff %>%
  group_by(sender) %>%
  summarise(mean = mean(diff.time)/60, median = median(diff.time)/60)

# Group for specific interval of time
answ.delays <- df.diff %>%
  group_by(sender) %>%
  summarise(c = n(), 
            a1 = sum(between(diff.time, 0, (2*60))),
            a2 = sum(between(diff.time, (2*60+1),(30*60))),
            a3 = sum(between(diff.time, 30*60+1, 60*60)),
            a4 = sum(between(diff.time, 60*60+1, 3600*4)),
            a5 = sum(between(diff.time, 3600*4+1, 3600*8)),
            a7 = sum(diff.time > (3600*8 + 1)))

# Compute the pourcentage instead of the total number of messages
answ.delays[, -(1:2)] <- answ.delays[, -(1:2)]/answ.delays$c
answ.delays <- answ.delays[, -2]

colnames(answ.delays) <- c("sender", "<2 min","2-30 min","30-60 min","1-4 hours", "4-8 hours", ">8 hours")

# Let's tidy the data.frame
answ.delays.tidy <- melt(
  data=answ.delays,
  id = "sender",
  variable.name = "time",
  value.name = "value")

ggplot(data=answ.delays.tidy, aes(x=time, y=value*100, fill=sender))+
  geom_bar(stat="identity", position="dodge")+  theme_mini(scale_text = 1.2) +
  scale_fill_brewer(name="answerer",type = "qual", palette = "Set1")+xlab("time to respond")+ylab("%")+
  ggtitle("Time to respond to a previous message")
#ggsave("figs/time_to_answer.png", width = 5, height = 3) 


## Calendar Heat plot !
#df.round <- df %>%
#  mutate(day = round_date(posix.date, unit=c("day")))


df.msg.day <- df %>%
  mutate(day = round_date(posix.date, unit=c("day"))) %>%
  group_by(day) %>%
  summarise(count = n(),
            n.char = sum(nb.char)) %>%
  filter(count > 10)

source("calendarHeat.R")

png(filename="figs/calendar-day.png", 
    type="cairo",
    units="in", 
    width=7,
    height=5, 
    pointsize=2.5, 
    res=96*2)
calendarHeat(df.msg.day$day, df.msg.day$count,ncolors = 4, color = "y2r", varname = "")
dev.off()


df.msg.day.user <- df.round %>%
  group_by(sender, day) %>% 
  summarise(count = n(), n.char = sum(nb.char))

ggplot(data=df.msg.day.user, aes(x=day, y=count, color=sender))+geom_point() + 
  scale_color_brewer(palette="Set1") + geom_smooth() + theme_mini(1.2) +
  coord_cartesian(ylim=c(0,100))

## Histogram number of character

# ggplot(data=df, aes(x=nb.char, color=sender))+
#   geom_density(adjust=.5)+  theme_mini(scale_text = 1.5) +
#   scale_fill_brewer(type = "qual", palette = "Set1")+xlab("n.char")+ylab("%") + xlim(0, 150)
# 
# 
# ggplot(data=df, aes(x=as.numeric(nb.char), color=sender, fill=sender))+
#   geom_histogram(bin=10,aes(y= ..density..), position="dodge") +  theme_mini(scale_text = 1.5) +
#   scale_fill_brewer(type = "qual", palette = "Set1")+xlab("n.char")+ylab("%") + xlim(1, 150)
# 
# 
# ggplot(data=filter(df, nb.char < 150), aes(x=as.numeric(nb.char)))+
#   geom_histogram(aes(y = (..count..)/sum(..count..)*100), binwidth = 3) + 
#   theme_mini(scale_text = 1.5) + xlim(1,150)+xlab("n.char")+ylab("%")

# Character filtering photos, and stickers!
df.character <- df %>%
  filter(msg != "Message photo") %>%
  filter(msg != "Autocollant")

ggplot(data=df.character, aes(x=nb.char))+
  geom_histogram(aes(y = (..count..)/sum(..count..)*100), binwidth = 3) + 
  theme_mini(scale_text = 1.5) + xlab("n.char")+ylab("%")+
  xlab("number of characters")+
  ggtitle("Histogram of the number of characters\n per messages")+xlim(1, 120)
#ggsave(filename = "figs/n_char.png", width = 5, height = 3)

# Also mutate with day of the week (monday, tuesday....),
# year, month, dateobj

# number of message per day (monday - ... - sunday), and by hours
msgperday <- df %>%
  group_by(week.day = wday(posix.date, label=TRUE, abbr=FALSE)) %>%
  summarise(msg.count = n())
msgperhour <- df %>%
  group_by(hours = hour(posix.date)) %>%
  summarise(msg.count = n())

# Histogram number of messages per hour
brs <- c(1, seq(4, 24, by = 4))
labs <- c("1:00am", "4:00am", "8:00am", "12:00pm", "4:00pm", "8:00pm", "12:00am")
ggplot(data=msgperhour, aes(x=hours, y=msg.count))+geom_histogram(stat="identity")+
  theme_mini(1.2) + ylab("number of the msgs") + xlab("hour of the day") +
  ggtitle("When the messages are sent during a day")+
  scale_x_continuous(breaks = brs, labels=labs)
ggsave("figs/hist-hours.png", width=5, height=3)



## Word cloud for both

library(tm)

word.toremove <- c(stopwords("en"), stopwords("fr"), 
                   "cest","message","jai","dont","plus", "photo", "sticker", "autocollant")

library(SnowballC)
library(RColorBrewer)
library(wordcloud)

corpus <- Corpus(DataframeSource(data.frame(tolower(df$msg))))
corpus <- corpus %>%
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(stripWhitespace) %>%
  tm_map(removeWords, word.toremove)%>%
  # tm_map(stemDocument) %>%
  TermDocumentMatrix

v <- sort(rowSums(as.matrix(corpus)), decreasing=TRUE)
df.word <- data.frame(classement=1:length(v), word=names(v), freq=v)
pal <- brewer.pal(9, "Reds")
pal <- pal[-1] 
pal <- brewer.pal(6,"Dark2")
pal <- pal[-(1)]
pal <- brewer.pal(4, name = "Set1")

png("wordcloudtt.png", width=1280, height=800)

wordcloud(words = df.word$word, freq = df.word$freq, min.freq = 1,
          max.words=50, random.order=FALSE, rot.per=0.15, scale=c(12,.3),
          colors=pal, vfont=c("sans serif","bold"))
dev.off()



## Wordcloud for her

df.her <- filter(df, sender == "Her")
corpus <- Corpus(DataframeSource(data.frame(tolower(df.her$msg))))
corpus <- corpus %>%
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(stripWhitespace) %>%
  tm_map(removeWords, word.toremove)%>%
  # tm_map(stemDocument) %>%
  TermDocumentMatrix 

v <- sort(rowSums(as.matrix(corpus)), decreasing=TRUE)
df.word <- data.frame(classement=1:length(v), word=names(v), freq=v)

png("wordcloud-her.png", width=1280, height=800)

wordcloud(words = df.word$word, freq = df.word$freq, min.freq = 1,
          max.words=40, random.order=FALSE, rot.per=0.15, scale=c(12,.3),
          colors=pal, vfont=c("sans serif","bold"))
dev.off()



## Word cloud for him
df.him <- filter(df, sender == "Him")
corpus <- Corpus(DataframeSource(data.frame(tolower(df.him$msg))))
corpus <- corpus %>%
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(stripWhitespace) %>%
  tm_map(removeWords, word.toremove)%>%
  # tm_map(stemDocument) %>%
  TermDocumentMatrix 

v <- sort(rowSums(as.matrix(corpus)), decreasing=TRUE)
df.word <- data.frame(classement=1:length(v), word=names(v), freq=v)

png("wordcloud-him.png", width=1280, height=800)
wordcloud(words = df.word$word, freq = df.word$freq, min.freq = 1,
          max.words=40, random.order=FALSE, rot.per=0.15, scale=c(12,.3),
          colors=pal, vfont=c("sans serif","bold"))
dev.off()




## Analysis for how we e-laugh and e-love

word.toremove <- c(stopwords("en"), stopwords("fr"), 
                   "!")

df.lol <- df
df.lol$msg <- gsub('[?]+','',df.lol$msg)
df.lol$msg <- gsub('[!]+','',df.lol$msg)
df.lol$msg <- gsub('[.]+','',df.lol$msg)
corpus <- Corpus(DataframeSource(data.frame(tolower(df.lol$msg))))
corpus <- corpus %>%
  tm_map(removeNumbers) %>%
  tm_map(stripWhitespace) %>%
  tm_map(removeWords, word.toremove)%>%
  # tm_map(stemDocument) %>%
  TermDocumentMatrix


v <- sort(rowSums(as.matrix(corpus)), decreasing=TRUE)
df.word <- data.frame(classement=1:length(v), word=names(v), freq=v)


# Some regex for e-laugh

idx.lol <- grep(pattern = "(l+o+l+z*)+", df.word$word)
idx.hehe <- grep(pattern = "(h+e)(h+e)+", df.word$word)
idx.haha <- grep(pattern = "([abw]?h+a)(h+a)+", df.word$word)
df.smiley <- df.word %>%
  filter(word %in% c(":-)",";-)",":-p",":-*", "^^", "autocollant"))


df.lulz <- data.frame(x=c("lol","hehe","smiley","haha"), 
                      y = c(sum(df.word$freq[idx.lol]),
                            sum(df.word$freq[idx.hehe]),
                            sum(df.smiley$freq),
                            sum(df.word$freq[idx.haha])))


# Some regex for e-love

idx.bis <- grep(pattern = "bis", x = df.word$word)
idx.kiss <- grep(pattern = "kiss", x = df.word$word)
idx.love <- grep(pattern = "love|luv", x = df.word$word)
idx.smack <- grep(pattern = "smack", x = df.word$word)
idx.cuddle <- grep(pattern = "cudd|c.lin", x = df.word$word)

df.luve <- data.frame(x=c("bisou","kiss","love","smack","cuddle"), 
                      y = c(sum(df.word$freq[idx.bis]),
                            sum(df.word$freq[idx.kiss]),
                            sum(df.word$freq[idx.love]),
                            sum(df.word$freq[idx.smack]),
                            sum(df.word$freq[idx.cuddle])))


ggpie(df.lulz, by='x', total='y')+theme_mini(1.8)+ 
  theme(axis.ticks=element_blank(),
  axis.text.y=element_blank(),
  axis.text.x=element_text(colour='black'),
  axis.title=element_blank(),
  panel.grid.major=element_blank(),
  legend.position="none") +
  ggtitle("How we e-laught!")

#ggsave("figs/e-laught.png", width = 5, height = 5)


ggpie(df.luve, by='x', total='y')+theme_mini(1.8)+ 
  theme(axis.ticks=element_blank(),
        axis.text.y=element_blank(),
        axis.text.x=element_text(colour='black'),
        axis.title=element_blank(),
        panel.grid.major=element_blank(),
        legend.position="none") +
  ggtitle("How we e-love!")
#ggsave("figs/e-love.png", width = 5, height = 5)
