#####라이브러리_실행#####

library(RSelenium)
library(rvest)
library(httr)
library(stringr)
library(wdman)
library(tibble)
library(dplyr)
library(ggvis)
library(ggplot2)
Sys.setenv(JAVA_HOME = "C:/Program Files/Java/jdk1.8.0_144")
library(rJava) 
library(KoNLP) 
useNIADic()
library(tm)
library(wordcloud2)

#####


#####전체_소스_가져오기#####

  #4567번_포트에_크롬_드라이버_배정#
ch=wdman::chrome(port = 4567L)
  #remort설정#
remDr=remoteDriver(port=4567L, browserName='chrome')
  #크롬드라이버_실행#
remDr$open()
  #설정_URL로_이동#
remDr$navigate("https://play.google.com/store/apps/details?id=kr.co.smartstudy.pinkfongtv.ko_android_googlemarket&showAllReviews=true")
  #css의_body를_element로_찾아_지정#
webElem <- remDr$findElement("css", "body") 
  #해당_element(화면)의_끝(end)으로_이동#
webElem$sendKeysToElement(list(key = "end"))
  #while문_종료_플래그#
flag <- TRUE 
  #시간_측정_변수#
endCnt <- 0 

while (flag) {
  Sys.sleep(50) #50초 대기
  webElemButton <- remDr$findElements(using = 'css selector',value = '.ZFr60d.CeoRYc') #'더보기' 버튼 element 찾아 지정
  
  if(length(webElemButton)==1){ #버튼이 나타난 경우 진입
    endCnt <- 0 #시간 측정 초기화
    webElem$sendKeysToElement(list(key = "home")) #화면의 처음(home)으로 이동
    webElemButton <- remDr$findElements(using = 'css selector',value = '.ZFr60d.CeoRYc')
    remDr$mouseMoveToLocation(webElement = webElemButton[[1]]) #해당 버튼으로 포인터 이동
    remDr$click() #마우스 클릭 액션
    webElem$sendKeysToElement(list(key = "end")) #해당 화면의 끝(end)으로 이동
  }else{
    if(endCnt>5){ #50초 이상 대기한 경우 진입
      flag <- FALSE #while문 종료
    }else{
      endCnt <- endCnt + 1 #대기 시간 증가
    }
  }
}
  #페이지_전체_가져오기#
frontPage <- remDr$getPageSource() 

#####


#####부분_추출#####

  #리뷰_게시자#
reviewNames <- read_html(frontPage[[1]]) %>% 
                            html_nodes('.bAhLNe.kx8XBd') %>% 
                                        html_nodes('.X43Kjb') %>% html_text()
  #리뷰_게시_일자_및_시간#
reviewDates <- read_html(frontPage[[1]]) %>% 
                            html_nodes('.bAhLNe.kx8XBd') %>% 
                                        html_nodes('.p2TkOb') %>% html_text()
  #리뷰_내용#
reviewComments <- read_html(frontPage[[1]]) %>% 
                              html_nodes('.UD7Dzf') %>%  html_text() 
  #리뷰_반응수#
reviewLikeCounts <- read_html(frontPage[[1]]) %>% 
                                html_nodes('.jUL89d.y92BAb') %>% html_text() %>% as.numeric()  
  #리뷰_별점#
reviewRatings <- read_html(frontPage[[1]]) %>% 
                            html_nodes(xpath = '//*[@class="pf5lIe"]') %>%
                                            gsub(pattern = "^.*?Rated | stars.*?$", replacement = ""); head(reviewRatings,10)
reviewRatings <- reviewRatings[-c(1,1082:1086)]
reviewRatings <- str_sub(unlist(reviewRatings), 48, 48); head(reviewRatings, 15)

#####


#####데이터_통합_및_RSelenium으로_열었던_크롬_창_닫음#####

  #수집한_데이터_통합#
reviewData <- tibble(name=reviewNames, 
                     date=reviewDates, 
                     comment=reviewComments, 
                     like=reviewLikeCounts, 
                     rating=reviewRatings)
  #데이터_CSV_저장#
write.csv(reviewData, "C:/Users/Minseung Chang/Downloads/reviewData.csv")
  #크롬_창_닫음#
remDr$close() 

#####


#####데이터_전처리#####
  
  #Date_형식_변환#
reviewData$date <- as.Date(reviewData$date, "%Y년%m월%d일"); str(reviewData$date)
  #월_기준_컬럼_생성#
reviewData$months <- cut(reviewData$date, breaks = "month", start.on.monday = F)

#####


#####별점-공감_시계열_그래프#####
reviewData %>% 
  ggvis( ~date, ~like, fill= ~rating, opacity := 0.5) %>% 
                                              layer_points() %>% 
                                                    layer_smooths(opacity := 0.8) 

#####


#####공감을_가장_많이_받은_리뷰_Top_10&그래프#####

  #날짜_전체_기준#
sub_1 <- arrange(reviewData, desc(like)) %>% head(10) %>% as.tibble()
#별점-공감_그래프
ggplot(sub_1, aes(x = like, y = rating)) + 
  geom_segment(aes(yend = rating), xend = 0, color = "grey50") + 
  geom_point(size = 5, aes(color = rating)) + 
  theme(panel.grid.major.y = element_blank(), legend.position = c(0.95, 0.15), legend.justification = c(1, 0.5)) +
  ggplot2::annotate("text", x = 59, y = 3.8, label = "2018-06-20") +
  ggplot2::annotate("text", x = 146, y = 2.8, label = "2017-01-18") +
  ggplot2::annotate("text", x = 100, y = 1.8, label = "2017-06-03") +
  ggplot2::annotate("text", x = 80, y = 0.8, label = "2018-04-02")

  #날짜_최근3개월_기준#
sub_2 <- arrange(reviewData, desc(months)) %>% head(35) %>% as.tibble()
sub_2 <- arrange(sub_2, desc(like)) %>% head(10) %>% as.tibble()
#별점-공감_그래프
ggplot(sub_2, aes(x = like, y = rating)) + 
  geom_segment(aes(yend = rating), xend = 0, color = "grey50") + 
  geom_point(size = 5, aes(color = rating)) + 
  theme(panel.grid.major.y = element_blank(), legend.position = c(0.95, 0.75), legend.justification = c(1, 0.5)) +
  ggplot2::annotate("text", x = 23, y = 2.8, label = "2019-01-30") +
  ggplot2::annotate("text", x = 20, y = 1.8, label = "2019-01-31") +
  ggplot2::annotate("text", x = 37, y = 0.8, label = "2018-12-19") 

  #별점_5_기준#
sub_3 <- subset(reviewData,
                select = c(name, date, comment, like, rating, months),
                subset = rating == 5)
sub_3 <- arrange(sub_3, desc(like)) %>% head(10) %>% as.tibble()
#워드_클라우드_그래프
sub_3_word <- unlist(sapply(sub_3$comment, extractNoun, USE.NAMES = F))
#write.csv(sub_3_word, "C:/Users/Minseung Chang/Downloads/sub_3_word.csv")
sub_3_word_table <- table(sub_3_word)
wordcloud2(sub_3_word_table,
           fontFamily = "Doutm",
           fontWeight = "bold",
           color = "skyblue",
           backgroundColor = "#003366",
           size = 1.2,
           rotateRatio = 3,
           shape = "triangle")

  #별점_4_기준#
sub_4 <- subset(reviewData,
                select = c(name, date, comment, like, rating, months),
                subset = rating == 4)
sub_4 <- arrange(sub_4, desc(like)) %>% head(10) %>% as.tibble()
#워드_클라우드_그래프
sub_4_word <- unlist(sapply(sub_4$comment, extractNoun, USE.NAMES = F))
#write.csv(sub_4_word, "C:/Users/Minseung Chang/Downloads/sub_4_word.csv")
sub_4_word_table <- table(sub_4_word)
wordcloud2(sub_4_word_table,
           fontFamily = "Doutm",
           fontWeight = "bold",
           color = "skyblue",
           backgroundColor = "#003366",
           size = 1.2,
           rotateRatio = 3,
           shape = "triangle")

  #별점_3_기준#
sub_5 <- subset(reviewData,
                select = c(name, date, comment, like, rating, months),
                subset = rating == 3)
sub_5 <- arrange(sub_5, desc(like)) %>% head(10) %>% as.tibble()
#워드_클라우드_그래프
sub_5_word <- unlist(sapply(sub_5$comment, extractNoun, USE.NAMES = F))
#write.csv(sub_5_word, "C:/Users/Minseung Chang/Downloads/sub_5_word.csv")
sub_5_word_table <- table(sub_5_word)
wordcloud2(sub_5_word_table,
           fontFamily = "Doutm",
           fontWeight = "bold",
           color = "skyblue",
           backgroundColor = "#003366",
           size = 1.2,
           rotateRatio = 3,
           shape = "triangle")

  #별점_2_기준#
sub_6 <- subset(reviewData,
                select = c(name, date, comment, like, rating, months),
                subset = rating == 2)
sub_6 <- arrange(sub_6, desc(like)) %>% head(10) %>% as.tibble()
#워드_클라우드_그래프
sub_6_word <- unlist(sapply(sub_6$comment, extractNoun, USE.NAMES = F))
#write.csv(sub_6_word, "C:/Users/Minseung Chang/Downloads/sub_6_word.csv")
sub_6_word_table <- table(sub_6_word)
wordcloud2(sub_6_word_table,
           fontFamily = "Doutm",
           fontWeight = "bold",
           color = "skyblue",
           backgroundColor = "#003366",
           size = 1.2,
           rotateRatio = 3,
           shape = "triangle")

  #별점_1_기준#
sub_7 <- subset(reviewData,
                select = c(name, date, comment, like, rating, months),
                subset = rating == 1)
sub_7 <- arrange(sub_7, desc(like)) %>% head(10) %>% as.tibble()
#워드_클라우드_그래프
sub_7_word <- unlist(sapply(sub_7$comment, extractNoun, USE.NAMES = F))
#write.csv(sub_7_word, "C:/Users/Minseung Chang/Downloads/sub_7_word.csv")
sub_7_word_table <- table(sub_7_word)
wordcloud2(sub_7_word_table,
           fontFamily = "Doutm",
           fontWeight = "bold",
           color = "skyblue",
           backgroundColor = "#003366",
           size = 1.2,
           rotateRatio = 3,
           shape = "triangle")

#####
