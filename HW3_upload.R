library(devtools)
library(Rfacebook)
library(httr)
library(wordcloud2)
library("jiebaR")
library(magrittr)
library(dplyr)
Sys.setlocale(category = "LC_ALL", locale = "cht")
data.path<-"c:/R/R_facebook/193424980763650/" #防汛抗旱粉絲團
setwd(data.path)

#setp 1 get fb token
#https://developers.facebook.com/tools/explorer/145634995501895/
#copy token to token
#press 提交
token <- "EAACEdEose0cBAHdYVNZCpZCaN3wzXZCFYH6VT25SKgvTmTIpnnJwFdSdJ4EIZAwepN1IljHCvNUdbGjJo0PXnIidkCMouLuBK76SZCfC6h7WPvFRRveLNnOTbLMFI8jZB9TwrVEQqjZCZAYcQiM8j6uR6jGBh14WZAPsO0M8waeCaB9ZCBzPJsbD8nZCwmEj1Aj1SCsWS2HRpxpbAZDZD"
me <- getUsers("me", token, private_info = TRUE)
me$name


#require("Rfacebook")
#fb.oauth <- fbOAuth(
#  app_id="122224441788562",
#  app_secret="4624c2c943181ad09cfc60e80110e7df",
#  extended_permissions = TRUE)

#me <- getUsers("me",token=fb.oauth)
#me$name

page.id <- "193424980763650" #粉絲專頁防汛抗旱粉絲團-193424980763650
page <- getPage(page.id, token, n = 600)
str(page)
names( page )
page$message[ 1:5 ]


## convert Facebook date format to R date format
format.facebook.date <- function(datestring) {
  date <- as.POSIXct(datestring, format = "%Y-%m-%dT%H:%M:%S+0000", tz = "GMT")
}
# aggregate metric counts over month
aggregate.metric <- function(metric) {
  m <- aggregate(page[[paste0(metric, "_count")]], list(month = page$month),
                 mean)
  m$month <- as.Date(paste0(m$month, "-15"))
  m$metric <- metric
  return(m)
}
# create data frame with average metric counts per month
page$datetime <- format.facebook.date(page$created_time)
page$month <- format(page$datetime, "%Y-%m")
df.list <- lapply(c("likes", "comments", "shares"), aggregate.metric)
df <- do.call(rbind, df.list)
# visualize evolution in metric

library(ggplot2)
library(scales)
ggplot(df, aes(x = month, y = x, group = metric)) +
  geom_line(aes(color = metric)) +
  scale_y_log10("Average count per post",
                breaks = c(2, 10, 50, 100)) +
  theme_bw() +
  theme(axis.title.x = element_blank())

page$message1<-gsub(pattern = "http(s)?://[a-zA-Z0-9\\./_]+", page$message, replacement = "")
page$message1<-gsub(pattern = "[0-9]+\\.[0-9]+", page$message1, replacement = "")
page$message1<-gsub(pattern = "[0-9]+", page$message1, replacement = "")
page$message1<-gsub(pattern = " ", replacement = "", page$message1)
page$message1<-gsub(pattern = " ", replacement = "", page$message1)
page$message1<-gsub(pattern = "【", page$message1, replacement = "")
page$message1<-gsub(pattern = "】", page$message1, replacement = "")
page$message1<-gsub(pattern = "，", page$message1, replacement = "")
page$message1<-gsub(pattern = "；", page$message1, replacement = "")
page$message1<-gsub(pattern = "：", page$message1, replacement = "")
page$message1<-gsub(pattern = "？", page$message1, replacement = "")
page$message1<-gsub(pattern = "／", page$message1, replacement = "")
page$message1<-gsub(pattern = "。", page$message1, replacement = "")
page$message1<-gsub(pattern = "「", page$message1, replacement = "")
page$message1<-gsub(pattern = "」", page$message1, replacement = "")
page$message1<-gsub(pattern = "◎", page$message1, replacement = "")
page$message1<-gsub(pattern = "…", page$message1, replacement = "")
page$message1<-gsub(pattern = "！", page$message1, replacement = "")
page$message1<-gsub(pattern = "（", page$message1, replacement = "")
page$message1<-gsub(pattern = "）", page$message1, replacement = "")
page$message1<-gsub(pattern = "、", page$message1, replacement = "")
page$message1<-gsub(pattern = "》", page$message1, replacement = "")
page$message1<-gsub(pattern = "《", page$message1, replacement = "")
page$message1<-gsub(pattern = "/", page$message1, replacement = "")
page$message1<-gsub(pattern = "\n", page$message1, replacement = "")
page$message1<-gsub(pattern = ")", page$message1, replacement = "")
page$message1<-gsub(pattern = "(", page$message1, replacement = "")

page$message1<-gsub(pattern = "攼", page$message1, replacement = "")
page$message1<-gsub(pattern = "㹤", page$message1, replacement = "")
page$message1<-gsub(pattern = "愼", page$message1, replacement = "")
page$message1<-gsub(pattern = "㸰", page$message1, replacement = "")
page$message1<-gsub(pattern = "戼", page$message1, replacement = "")
page$message1<-gsub(pattern = "㹣", page$message1, replacement = "")
page$message1<-gsub(pattern = "攼", page$message1, replacement = "")
page$message1<-gsub(pattern = "㹤", page$message1, replacement = "")

page$message1<-gsub(pattern = "㹥", page$message1, replacement = "")
page$message1<-gsub(pattern = "㠼", page$message1, replacement = "")
page$message1<-gsub(pattern = "㸱", page$message1, replacement = "")
page$message1<-gsub(pattern = "㸹", page$message1, replacement = "")
page$message1<-gsub(pattern = "㸴", page$message1, replacement = "")

page$message1<-gsub(pattern = "攼", page$message1, replacement = "")
page$message1<-gsub(pattern = "㠼", page$message1, replacement = "")
page$message1<-gsub(pattern = "㸱", page$message1, replacement = "")
page$message1<-gsub(pattern = "㸹", page$message1, replacement = "")
page$message1<-gsub(pattern = "㸴", page$message1, replacement = "")

page$message1<-gsub(pattern = "㤼", page$message1, replacement = "")
page$message1<-gsub(pattern = "㸲", page$message1, replacement = "")
page$message1<-gsub(pattern = "㸳", page$message1, replacement = "")
page$message1<-gsub(pattern = "㸵", page$message1, replacement = "")
page$message1<-gsub(pattern = "㸶", page$message1, replacement = "")
page$message1<-gsub(pattern = "㸷", page$message1, replacement = "")
page$message1<-gsub(pattern = "㸳", page$message1, replacement = "")
page$message1<-gsub(pattern = "㸸", page$message1, replacement = "")
page$message1<-gsub(pattern = "㸶", page$message1, replacement = "")
page$message1<-gsub(pattern = "㹡", page$message1, replacement = "")
page$message1<-gsub(pattern = "㹢", page$message1, replacement = "")
page$message1<-gsub(pattern = "㹦", page$message1, replacement = "")
#㸲㹡㹦㹢㹦㸸
#㸹
#㸷
#㸶㹦㸲㹡㹦㹢
#攼㹤愼㸰戼㹣攼㹤戼㹥㠼㸱
#小編將會把禮品攼㹤愼㸰戼㹣攼㹤戼㹥㠼㸱寄給您!"
#"㤼"     "㸲"     "㸳"     "㸵"     "㸶"     "㸷"     "㸸"     "㹡"    
#[10] "㹢"     "㹦" 
cc = worker()

  
wordcloud_table<-data.frame(table(cc[page$message1]))


wordcloud_table<-wordcloud_table%>%
                 filter(substr(wordcloud_table$Var1, start =1, stop =1)!="U")
wordcloud2(wordcloud_table)
wordcloud(wordcloud_table$Var1, wordcloud_table$Freq, min.freq = 40, random.order = F, ordered.colors = F ,colors = rainbow(50))








