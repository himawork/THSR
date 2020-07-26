# update info of sting stocks ----
ScodeNums = names(readRDS('e:/tess/SpySto.RDS')[['sting']])
remDr <- remoteDriver(browserName = "chrome", remoteServerAddr = "localhost", port = 4444)
shell('java -Dwebdriver.chrome.driver="E:/500px/Selenium/chromedriver.exe" -jar "E:/500px/Selenium/selenium-server-standalone-3.141.59.jar" -port 4444', wait = F)
Sys.sleep(10)
tryCatch({
  remDr$open();remDr$setWindowPosition(-1980,-890);remDr$setWindowSize(2000,1400) 
  Sys.sleep(10)
  SpySto(type = 'sting',update = T, StingStoNum = ScodeNums)
},error = function(err){
  print('reopen with errors...')
  shell('java -Dwebdriver.chrome.driver="E:/500px/Selenium/chromedriver.exe" -jar "E:/500px/Selenium/selenium-server-standalone-3.141.59.jar" -port 4444', wait = F)
  Sys.sleep(10)
  remDr$open();remDr$setWindowPosition(-1980,-890);remDr$setWindowSize(2000,1400)
  Sys.sleep(10)
  SpySto(type = 'sting',update = T, StingStoNum = ScodeNums)
})
Hsto.info = readRDS('e:/tess/Hsto.info.RDS')
Spycode = Hsto.info$Code
SpyAdd = Spycode[which(!Spycode %in% ScodeNums)]
if (length(SpyAdd) > 0) {SpySto(type = 'sting', new = T, StingStoNum = SpyAdd)}
# # get index of golbal
shell('taskkill /F /IM chromedriver.exe')
shell('taskkill /F /IM chrome.exe')
## remove NA datas
SpySto.db = readRDS('e:/tess/SpySto.RDS')
ScodeNums = names(SpySto.db[['sting']])
for (Scode in ScodeNums) {
  SpySto.db[['sting']][[Scode]][['dailyK']] = 
    na.omit(SpySto.db[['sting']][[Scode]][['dailyK']])
}
saveRDS(SpySto.db,'e:/tess/SpySto.RDS')
