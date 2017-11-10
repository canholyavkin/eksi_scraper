# Paket Kurulumu -------------------------------------------------------------------------------------------------------
library("tidyverse")
library("rvest")
library("stringr")

# Veri Çekilmesi -------------------------------------------------------------------------------------------------------
# Verilerin alınacağı başlık bağlantısı başlık id'si ile bitmelidir. Bağlantı içinde sayfa sayısını belirten 
# parametreler ("?p=1" gibi) olmamaldır.

url            <- "https://eksisozluk.com/teyitorg--5292793"

last.page      <- read_html(url) %>% html_node(".pager") %>% html_attr("data-pagecount") %>% as.numeric()
title          <- read_html(url) %>% html_nodes("h1")  %>% html_attr("data-title")

user.list      <- vector()
time.list      <- vector()
entry.list     <- vector()

for (i in 1:last.page) {
  current.page <- paste0(url,"?p=",i)
  user.list    <- append(user.list, read_html(current.page) %>% html_nodes(".entry-author")  %>% html_text())
  time.list    <- append(time.list, read_html(current.page) %>% html_nodes(".permalink")  %>% html_text())
  entry.list   <- append(entry.list, read_html(current.page) %>% 
                           html_nodes(".content") %>% 
                           html_text() %>% 
                           str_trim() %>%
                           str_split(" ") %>% 
                           unlist())
  # Yüklenme Sayacı
  if(i %% 10==0) {
    cat("\014")
    print(paste0("Çekilen Veri: % ",format(round(i/last.page*100, 2), nsmall = 2)))
  }
}


# En Çok Giriye Sahip Kullanıcılar -------------------------------------------------------------------------------------
top_users      <- as.data.frame(table(user.list)) %>% arrange(-Freq) %>% head(10)

ggplot(data = top_users, aes(x=reorder(user.list,-Freq), y=Freq, fill=Freq)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  labs(
    title = paste0("ekşi sözlük - ",title),
    subtitle = paste0("Başlık içinde en çok entry giren 10 kullanıcı listelenmiştir."),
    caption = paste0("Kaynak Bağlantı: ",url),
    x= "Kullanıcı adı",
    y= "Entry Sayısı"
  )


# Tarihe Bağlı Histogram-----------------------------------------------------------------------------------------------

timeline       <- as.data.frame(matrix(ncol=1, nrow = length(time.list)))
timeline$V1    <- as.Date(gsub( " .*$", "", time.list ), format = "%d.%m.%Y")
# timeline$V1    <- as.POSIXct(gsub( " ~.*$", "", time.list ), format = "%d.%m.%Y %H:%M")
# 
events   <- as.data.frame(table(timeline), stringsAsFactors = F) %>% 
  filter(Freq>length(time.list)/25)

events$timeline <- as.Date(events$timeline, format = "%Y-%m-%d")

ggplot(data = timeline) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_histogram(aes(x=V1, fill=..density..), bins = length(time.list)/25) +
  geom_label(data = events, aes(x=timeline, y=Freq, label=timeline), 
            position = position_dodge(width = 1), size = 3, fontface = "bold") +
  labs(
    title = paste0("ekşi sözlük - ",title),
    subtitle = paste0("Başlık içindeki ",last.page," sayfadan ",length(time.list), " entry çekilmiştir."),
    caption = paste0("Kaynak Bağlantı: ",url),
    x= "",
    y= "Entry Sayısı"
  )


# Timeline Graph 2 ----
timeline2       <- as.data.frame(matrix(ncol=1, nrow = length(time.list)))
timeline2$V1    <- str_split(time.list," ") %>%
  lapply("[[", 2) %>%
  unlist() %>%
  strptime(format="%H:%M")

ggplot(data = timeline2) +
  scale_x_datetime(date_labels = "%H", date_breaks = "1 hour") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_histogram(aes(x=V1, fill=..density..), bins = 720)


# Kelime Yoğunluğu -----------------------------------------------------------------------------------------------------

removed.words  <- c("da","ve","de","bu","gibi","ise","ya",";",",","",
                    "artık","diye","\r\n    ","\r\n  ","---","daha","ama",
                    "mi","ne","kadar","bir","o","için","ile","hem","her","ki",
                    "bile","olan","böyle","olarak","çok","(bkz:","en","sonra",
                    "var","şu","önce","değil","bi")

top_words <- entry.list %>% 
  table() %>%
  as.data.frame() %>%
  arrange(-Freq) %>%
  filter(!(. %in% removed.words)) %>%
  head(20)

ggplot(data = top_words, aes(x=reorder(.,-Freq), y=Freq, fill=Freq)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  labs(
    title = paste0("ekşi sözlük - ",title),
    subtitle = paste0("Başlık içinde kullanılan ",length(entry.list)," kelimeden en çok kullanılan 20'si listelenmiştir. "),
    caption = paste0("Kaynak Bağlantı: ",url),
    x= "",
    y= "Kullanım Sayısı"
  )