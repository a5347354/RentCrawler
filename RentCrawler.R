library(rvest)
#宣告一個function
grabDatas = function(pattern,keyword){
  url = paste0('https://rent.591.com.tw/new/?kind=1&region=1&pattern=',pattern,'&keywords=',keyword,'&section=12')
  #url = paste0('https://rent.591.com.tw/new/?kind=1&region=1&pattern=3&keywords=景美&section=12')
  page_url = read_html(url) 
  title = page_url %>% html_nodes(".infoContent") %>% html_nodes("h3") %>%  html_nodes("a") %>% html_text() %>% .[. != ""]
  rent_urls = page_url %>% html_nodes(".infoContent") %>% html_nodes("h3") %>%  html_nodes("a") %>%  html_attr("href") %>% .[. != "javascript:;"]
  ## 字串處理-網址
  library(stringr)
  rent_urls = paste0("https:",str_trim(str_match(rent_urls,'\n(.+)\n')[,2]))
  
  ##個別處理url
  df = data.frame()
  for (rent_url in rent_urls) {
    ##正規表達法 . = 萬用字元
    url = read_html(rent_url)
    title = url %>% html_nodes(".houseInfoTitle") %>% html_text()
    now = url %>% html_nodes(".attr") %>% html_text() %>% str_match(.,'現況.:.(.+)') %>% .[,2] %>% str_trim()
    if(grepl("權狀坪數", url)){
      model = url %>% html_nodes(".attr") %>% html_text() %>% str_match(.,'格局.:.(.+).坪數.') %>% .[,2] %>% str_match(.,'(.+)坪數.')  %>% .[,2] %>% str_trim()
    }else{
      model = url %>% html_nodes(".attr") %>% html_text() %>% str_match(.,'格局.:.(.+)坪數.') %>% .[,2] %>% str_trim()
    }
    price = url %>% html_nodes(".price") %>% html_nodes("i") %>% html_text() %>% str_trim()
    address = url %>% html_nodes(".addr") %>% html_text() 
    landlord = url %>% html_nodes(".avatarRight") %>% html_nodes("i") %>%html_text()
    ##利用正規表達法取出電話
    mobile = str_match(url,'<span class="num">(.+)</span>')[,2]
    
    tem = data.frame(新上架 = "",價錢 = price, 格局 = model, 房東 = landlord, 電話 = mobile, 地址 = address, 網址 = rent_url,標題 = title)
    df = rbind(df,tem)
  }
  return(df)
}


#patterns =>幾房
patterns = c(3,4)
keywords = c("景美","萬隆")
df = data.frame()
for (keyword in keywords) {
  for (pattern in patterns) {
    df = rbind(df,grabDatas(pattern,keyword))
    Sys.sleep(1)
  }
}

datas = read.csv("拜託給我房子_R.csv", header = TRUE, sep = ",")[,-1]
datas$新上架 = "" 
df$新上架 = "新"
total = rbind(datas, df)
total = total[!duplicated(total[,"標題"]),]

write.csv(total,file = "拜託給我房子_R.csv")

