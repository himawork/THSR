# ...load functions----
source('e:/Scientist career/R_files/201911/THS/Functions.R',echo = F,encoding = 'UTF-8')  
source('e:/Scientist career/R_files/201911/THS/FunctionsHead.R',echo = F,encoding = 'UTF-8')  
logger <- log4r::create.logger(logfile = "e:/tess/trade.log", level = "INFO")
### ...prepare software running environment...----
if (format(Sys.time(),'%H:%M') <= '09:30') {Xiadan(THS = T)}
softFSC()
Xiadan()
# (before-09:30)----
### ...update info of holding stocks----
if (format(Sys.time(),'%H:%M') <= '09:30') {
  if (format(Sys.time(),'%H:%M') <= '09:00') {
    # Start trade software
    testCopy = HstoInfo();
    if (testCopy == 'clearClipBoard') {Xiadan(restart = T); HstoInfo()}
    exchangeTime = read.table('e:/tess/backupDB/exchange.info')[1,1]
    if (exchangeTime != format.Date(Sys.Date(), "%Y-%m")) {dydata(DBclass = 'SpySto')}
    # refresh DailyRealPrice and DailyTrading
    if (as.Date(file.mtime('e:/tess/DailyRealPrice.RDS')) != Sys.Date() | !file.exists('e:/tess/DailyRealPrice.RDS')) {
      DailyRealPrice = NULL; 
      saveRDS(DailyRealPrice,'e:/tess/DailyRealPrice.RDS')}
    if (as.Date(file.mtime('e:/tess/DailyTrading.RDS')) != Sys.Date() | !file.exists('e:/tess/DailyTrading.RDS')) {
      dydata(DBclass = 'DailyTrading')}
    # TSS = readRDS('e:/tess/TSS.RDS')
    # TotalSpycode = unique(unlist(readRDS('e:/tess/TSS.RDS')))
    # StockCodes = GetXxtp(); StockCodes = StockCodes[which(!StockCodes %in% TotalSpycode)]; 
    # SpyAdd = StockCodes[1:10]
    # SpySto(type = 'sting', new = T, StingStoNum = SpyAdd)
    # TSS[['MSI']] = names(readRDS('e:/tess/SpySto.RDS')[['sting']])
    # saveRDS(TSS, 'e:/tess/TSS.RDS')
    # update info of sting stocks
    tryCatch({source('e:/NewPC/THS/UpdateStoInfo.R', local = T, echo = F, encoding = 'UTF-8')}, 
             error = function(err) {
               log4r::error(logger, paste0(err, '\tRe-Source updateStoinfo.R...'))
               source('e:/NewPC/THS/UpdateStoInfo.R', local = T, echo = F, encoding = 'UTF-8')
             })
  }
  # ...test the time left to 09:30 ----
  while (TRUE) {
    if (format(Sys.time(), "%H:%M") >= '09:30') {
      print('Get out loop and goto buy...'); break()  
    } else if (format(Sys.time(), "%H:%M") %in% c("09:20")) {
      K.old = tryCatch({readRDS('e:/tess/SpySto.RDS')}, 
                       error = function(err) {
                         log4r::error(logger, paste0(err, '\tUse backuped SpySto.RDS...'))
                         file.copy('e:/tess/backupDB/SpySto.RDS', 'e:/tess/SpySto.RDS', overwrite = T);
                         readRDS('e:/tess/backupDB/SpySto.RDS')
                       })
      # test exclude stocks with Sword band less profit
      ScodeNums = names(K.old[['sting']])
      exclude = NULL
      for (Scode in ScodeNums) {
        # swordTest = Sword(Scode, days = 10, ratio = 0.8) == 'DOWN'
        swordTest = Sword(Scode, days = 10, ratio = 5, testType = "PushPull") == 'positive'
        if (!swordTest) {exclude = c(exclude, Scode)}
      }
      write.table(exclude,'e:/tess/exclude.info',row.names = F, col.names = F)
      # test exclude stocks with sdJ
      test.exclude = ScodeNums[which(!ScodeNums %in% exclude)]; test.res = NULL
      for (Scode in test.exclude) {
        dailyK.old = K.old[['sting']][[Scode]][['dailyK']]
        sdJ1 = dailyK.old[1,'J'] - dailyK.old[2,'J']; sdJ2 = dailyK.old[2,'J'] - dailyK.old[3,'J']
        sdJ1 = ifelse(!anyNA(sdJ1),sdJ1,0); sdJ2 = ifelse(!anyNA(sdJ2),sdJ2,0); 
        sdJ.test <- (sdJ1 > sdJ2 & dailyK.old[1,'J'] < 90)
        if (!(sdJ.test)) {test.res = c(test.res, Scode)}
      }
      write.table(test.res,'e:/tess/exclude.info',row.names = F, col.names = F, append = T)
      #  test exclude stocks with less profit
      less.profit = ScodeNums[which(!ScodeNums %in% c(exclude, test.res))]; less.res = NULL
      for (Scode in less.profit) {
        bandTest = bandSword(Scode, bandFrom = 30, days = 10, ratio = 0.8)
        if (length(bandTest) < 1) {
          print(paste0('No profit band of stock ',Scode))
          less.res = c(less.res, Scode)
        } else {
          Profit = SwordProfit(Scode, bandRes = bandTest, MaxHoldDays = 30, Proless = 200)
          if (Profit < 500) {less.res = c(less.res, Scode)}
        }
      }
      write.table(less.res,'e:/tess/exclude.info',row.names = F, col.names = F, append = T)
      # Add shitSto to exclude.info
      shitSto.info = read.table('e:/tess/shitSto.info',colClasses = 'character')[,1]
      write.table(shitSto.info, 'e:/tess/exclude.info',row.names = F, col.names = F, append = T)
      # Record testStoDB everday
      weekDay = timeDate::dayOfWeek(timeDate::Sys.timeDate('Asia/Shanghai'))[[1]]
      if (file.exists('e:/tess/testStoDB.RDS')) {
        testStoDB = readRDS('e:/tess/testStoDB.RDS')
      } else {testStoDB = NULL}
      exclude = read.table('e:/tess/exclude.info',colClasses = 'character')[,1]
      testSto = ScodeNums[which(!ScodeNums %in% exclude)]
      testStoDB[[weekDay]] = testSto
      saveRDS(testStoDB, 'e:/tess/testStoDB.RDS')
    } else {
      print('Keep rest before market opening...')
      Sys.sleep(30)
    }
  }
}
# Sting when open morning market
# (09:30-15:00)----
if (format(Sys.time(), "%H:%M") >= '09:30' & format(Sys.time(), "%H:%M") <= '14:57') {
  shell('title Wait buy...')
  # get holding stocks
  tryCatch({HstoInfo()},error = function(err){log4r::error(logger, paste0(err, '\tCannot get info of holding stocks ...'))})
  # prepare parameters for trading
  K.old = tryCatch({readRDS('e:/tess/SpySto.RDS')}, 
                   error = function(err) {
                     log4r::error(logger, paste0(err, '\tUse backuped SpySto.RDS...'))
                     file.copy('e:/tess/backupDB/SpySto.RDS', 'e:/tess/SpySto.RDS', overwrite = T);
                     readRDS('e:/tess/backupDB/SpySto.RDS')
                   })
  DailyTrade = tryCatch({readRDS('e:/tess/DailyTrading.RDS')}, 
                        error = function(err) {
                          log4r::error(logger, paste0(err, '\tUse backuped DailyTrading.RDS ...'))
                          file.copy('e:/tess/DailyTrading.RDS.old', 'e:/tess/DailyTrading.RDS', overwrite = T)
                          readRDS('e:/tess/DailyTrading.RDS')
                        })
  DailyRealPrice = tryCatch({readRDS('e:/tess/DailyRealPrice.RDS')}, 
                            error = function(err) {
                              log4r::error(logger, paste0(err, '\tUse backuped DailyRealPrice.RDS ...'))
                              file.copy('e:/tess/DailyRealPrice.RDS.old', 'e:/tess/DailyRealPrice.RDS', overwrite = T)
                              readRDS('e:/tess/DailyRealPrice.RDS')
                            })
  # loop trading ...
  while (TRUE) {
    tryCatch({HstoInfo()},error = function(err){log4r::error(logger, paste0(err, '\tCannot get info of holding stocks ...'))})
    Hsto.info = readRDS('e:/tess/Hsto.info.RDS')
    cat('\t\nstart trading...\n')
    # save realtime price 
    Spycode = Hsto.info$Code;
    for (Scode in Spycode) {
      Current = Hsto.info$Price[which(Hsto.info$Code == Scode)] %>% as.numeric() 
      if (Scode %in% names(DailyRealPrice)) {
        DailyRealPrice[[Scode]] = list(c(Current,DailyRealPrice[[Scode]][[1]]))
      } else {
        DailyRealPrice[[Scode]] = list(c(Current,Current,Current))
      }
    }
    saveRDS(DailyRealPrice,'e:/tess/DailyRealPrice.RDS')
    # test buyORsell
    for (Scode in Spycode) {
      # dailyK of history 
      dailyK.old = K.old[['sting']][[Scode]][['dailyK']]
      meanVib = mean(K.old[['sting']][[Scode]][['CStoDetail']][1:5,8], na.rm = T)
      meanVib = ifelse(anyNA(meanVib), 2, meanVib)
      Close = dailyK.old$CLOSE[1]; 
      MA10 = mean(colMeans(dailyK.old[1:10,c('CLOSE','OPEN')]), na.rm = T)
      Profit <- Hsto.info$Profit[which(Hsto.info$Code == Scode)] %>% as.numeric()
      Invest <- Hsto.info$Value[which(Hsto.info$Code == Scode)] %>% as.numeric()
      Hdays <-  Hsto.info$Hdays[which(Hsto.info$Code == Scode)] %>% as.numeric()
      ProLess <- (Hdays %/% 20 + 1) * 300 
      Countsell = Hsto.info$LiveN[which(Hsto.info$Code == Scode)] %>% as.numeric()
      TotalN = Hsto.info$TotalN[which(Hsto.info$Code == Scode)] %>% as.numeric()
      Current = Hsto.info$Price[which(Hsto.info$Code == Scode)] %>% as.numeric() 
      Open = tail(DailyRealPrice[[Scode]][[1]],1); meanP = mean(DailyRealPrice[[Scode]][[1]])
      # calculate dynamic ChangeRate...
      ChangeRate = (Current - Close)/Close * 100
      DailyChangeRate = (Current - Open)/Open * 100
      absChangeR = abs(ChangeRate - DailyChangeRate)
      absChangeR = ifelse(length(absChangeR) == 0, 0, absChangeR)
      VibCR = (ChangeRate > meanVib/2)
      print(paste0('ChangeRate of ',Scode,' is: ',ChangeRate))
      ChangeRateP2 = (mean(DailyRealPrice[[Scode]][[1]][1:2]) - Close)/Close * 100
      DailyChangeRateP2 = (mean(DailyRealPrice[[Scode]][[1]][1:3]) - Open)/Open * 100
      sdJ1 = dailyK.old[1,'J'] - dailyK.old[2,'J'];sdJ2 = dailyK.old[2,'J'] - dailyK.old[3,'J']
      sdJ1 = ifelse(!anyNA(sdJ1),sdJ1,0); sdJ2 = ifelse(!anyNA(sdJ2),sdJ2,0); 
      swordNP = Sword(Scode, days = 10, testType = 'PushPull')
      # ...sword trade ----
      if (VibCR & ChangeRate <= ChangeRateP2) {
        if (Profit > ProLess) {
          GoSell(Stock = Scode, minusN = Countsell); Countsell = 0; TotalN = 0
        } else if (swordNP == 'negative') {
          minusCount = ifelse(Profit > ProLess, Countsell, 
                              ifelse(Countsell > 1000, 1000, Countsell - 100))
          GoSell(Stock = Scode, minusN = minusCount); 
          Countsell = Countsell - minusCount; TotalN = TotalN - minusCount
          DailyTrade[['DSsto']][Scode] = c(Current, DailyTrade[['DSsto']][Scode])
          DailyTrade[['DSellN']][Scode] = c(minusCount, DailyTrade[['DSellN']][Scode])
        }
      }
      # ...band trade----
      if (sdJ1 > 0 & VibCR) {
        tradeRe =  tryCatch({readRDS('e:/tess/tradeRe.RDS')}, 
                            error = function(err) {
                              log4r::error(logger, paste0(err, "\tUse backuped tradeRe.RDS ..."))
                              file.copy('e:/tess/backupDB/tradeRe.RDS', 'e:/tess/tradeRe.RDS', overwrite = T);
                              readRDS('e:/tess/backupDB/tradeRe.RDS')
                            })
        if (Scode %in% names(tradeRe)) {
          trade.dat = tradeRe[[Scode]]
          posBuy = which(!is.na(trade.dat$tagBuy))
          if (length(posBuy) > 0) {
            trade.buy.dat = trade.dat[posBuy,]
            for (t in 1:nrow(trade.buy.dat)) {
              buyN = trade.buy.dat$buyN[t]
              buyP = ifelse(trade.buy.dat$buyP[t] == 0, trade.buy.dat$price[t], trade.buy.dat$buyP[t])
              if ((Current - buyP) > (Current * meanVib/100)) {
                minusCount = ifelse(Countsell > buyN, buyN, Countsell - 100)
                GoSell(Stock = Scode, minusN = minusCount); 
                Countsell = Countsell - minusCount; TotalN = TotalN - minusCount
                if (minusCount == buyN) {
                  tradeRe[[Scode]]$tagBuy[posBuy[t]] = NA
                } else {
                  tradeRe[[Scode]]$buyN[posBuy[t]] = (buyN - minusCount)
                }
                saveRDS(tradeRe,'e:/tess/tradeRe.RDS')
              }
            }
          }
        }
      }
      # ...ChangeRate to buy or sell----
      DBsto.test = tryCatch({(!Scode %in% names(DailyTrade[['DBsto']]) & ChangeRate >= ChangeRateP2 & Invest < 30000 & absChangeR > 2)}, error = function(err) {
        log4r::error(logger, paste0(err, "\tDBsto.test encounter error, use default parameters."))
        return(FALSE)
      })
      DSsto.test = tryCatch({(!Scode %in% names(DailyTrade[['DSsto']]) & ChangeRate <= ChangeRateP2 & absChangeR > 2)}, error = function(err) {
        log4r::error(logger, paste0(err, "\tDSsto.test encounter error, use default parameters."))
        return(FALSE)
      })
      if (DBsto.test) {
        plusNum = 0
        if (ChangeRate > -8 & ChangeRate <= -6) {
          plusNum = 700
        } else if (ChangeRate > -6 & ChangeRate <= -4) {
          plusNum = 600
        } else if (ChangeRate > -4 & ChangeRate <= -2 & Profit >= -1000) {
          plusNum = 500
        }
        if (plusNum > 0 & TotalN != 0) {
          GoBuy(Stock = Scode, plusN = plusNum);
          DailyTrade[['DBsto']][Scode] = c(Current, DailyTrade[['DBsto']][Scode])
          DailyTrade[['DBuyN']][Scode] = c(plusNum, DailyTrade[['DBuyN']][Scode])
        }
      }
      #
      if (DSsto.test) {
        minusCount = 0
        if (ChangeRate >= 2 & ChangeRate <= 4) {
          minusCount = ifelse(Profit > ProLess, Countsell, 
                              ifelse(Countsell > 500, 500, Countsell - 100))
        } else if (ChangeRate > 4 & ChangeRate <= 6) {
          minusCount = ifelse(Profit > ProLess, Countsell, 
                              ifelse(Countsell > 600, 600, Countsell - 100))
        } else if (ChangeRate > 6 & ChangeRate <= 8) {
          minusCount = ifelse(Profit > ProLess, Countsell, 
                              ifelse(Countsell > 700, 700, Countsell - 100))
        } else if (ChangeRate > 8) {
          minusCount = ifelse(Profit > ProLess, Countsell, 
                              ifelse(Countsell > 1000, 1000, Countsell - 100))
        }
        if (minusCount > 0) {
          GoSell(Stock = Scode, minusN = minusCount); 
          Countsell = Countsell - minusCount; TotalN = TotalN - minusCount
          DailyTrade[['DSsto']][Scode] = c(Current, DailyTrade[['DSsto']][Scode])
          DailyTrade[['DSellN']][Scode] = c(minusCount, DailyTrade[['DSellN']][Scode])
        }
      }
      # ...DailyChangeRate to buy or sell----
      TSsto.test = tryCatch({(Current > meanP & DailyChangeRate > meanVib/3 & DailyChangeRate > DailyChangeRateP2)}, 
                            error = function(err) {
                              log4r::error(logger, paste0(err, "\tTSsto.test encounter error, use default parameters."))
                              return(FALSE)
                            })
      TBsto.test = tryCatch({(Current < meanP & DailyChangeRate < -meanVib/3 & DailyChangeRate < DailyChangeRateP2 & 
                                Invest < 30000)}, 
                            error = function(err) {
                              log4r::error(logger, paste0(err, "\tTBsto.test encounter error, use default parameters."))
                              return(FALSE)
                            })
      if (TSsto.test) {
        if (!Scode %in% names(DailyTrade[['TSsto']])) {
          minusCount = 0
          if (DailyChangeRate >= 1 & DailyChangeRate < 3) {
            minusCount = ifelse(Profit > ProLess, Countsell, 
                                ifelse(Countsell > 500, 500, Countsell - 100))
          } else if (DailyChangeRate >= 3 & DailyChangeRate < 5) {
            minusCount = ifelse(Profit > ProLess, Countsell, 
                                ifelse(Countsell > 600, 600, Countsell - 100))
          } else if (DailyChangeRate >= 5 & DailyChangeRate < 7) {
            minusCount = ifelse(Profit > ProLess, Countsell, 
                                ifelse(Countsell > 700, 700, Countsell - 100))
          } else if (DailyChangeRate >= 7) {
            minusCount = ifelse(Profit > ProLess, Countsell, 
                                ifelse(Countsell > 1000, 1000, Countsell - 100))
          }
          if (minusCount > 0) {
            GoSell(Stock = Scode, minusN = minusCount); 
            Countsell = Countsell - minusCount; TotalN = TotalN - minusCount
            DailyTrade[['TSsto']][Scode] = c(Current, DailyTrade[['TSsto']][Scode])
            DailyTrade[['TSellN']][Scode] = c(minusCount, DailyTrade[['TSellN']][Scode])
          }
          # test to T-sell
          if (Scode %in% names(DailyTrade[['TBsto']]) | Scode %in% names(DailyTrade[['DBsto']])) {
            BuyP = ifelse(Scode %in% names(DailyTrade[['TBsto']]), 
                          DailyTrade[['TBsto']][[Scode]], DailyTrade[['DBsto']][[Scode]])
            if (TotalN > Countsell & Current > BuyP) {
              BuyN = TotalN - Countsell
              TradeN = ifelse(Countsell > BuyN, BuyN, ifelse(Profit > ProLess, Countsell, Countsell - 100))
              Tpro = (Current * TradeN - BuyP * BuyN) > (Current * BuyN * meanVib/100)
              if (Tpro) {
                GoSell(Stock = Scode, minusN = TradeN); 
                Countsell = Countsell - TradeN; TotalN = TotalN - TradeN
                DailyTrade[['TSsto']][Scode] = c(Current, DailyTrade[['TSsto']][Scode])
                DailyTrade[['TSellN']][Scode] = c(TradeN, DailyTrade[['TSellN']][Scode])
              }
            }
          }
        }
      }
      #
      if (TBsto.test) {
        if (!Scode %in% names(DailyTrade[['TBsto']])) {
          plusNum = 0
          if (DailyChangeRate <= -5 & DailyChangeRate > -8) {
            plusNum = 700
          } else if (DailyChangeRate <= -3 & DailyChangeRate > -5) {
            plusNum = 600
          } else if (DailyChangeRate <= -1 & DailyChangeRate > -3 & Profit >= -500) {
            plusNum = 500
          }
          if (plusNum > 0 & TotalN != 0) {
            GoBuy(Stock = Scode, plusN = plusNum)
            DailyTrade[['TBsto']][Scode] = c(Current, DailyTrade[['TBsto']][Scode])
            DailyTrade[['TBuyN']][Scode] = c(plusNum, DailyTrade[['TBuyN']][Scode])
          }
          # test to T-buy
          if (Scode %in% names(DailyTrade[['TSsto']]) | Scode %in% names(DailyTrade[['DSsto']])) {
            SellP = ifelse(Scode %in% names(DailyTrade[['TSsto']]), 
                           DailyTrade[['TSsto']][[Scode]], DailyTrade[['DSsto']][[Scode]])
            if (TotalN != 0 & Current < SellP) {
              SellN = ifelse(Scode %in% names(DailyTrade[['TSsto']]), 
                             DailyTrade[['TSellN']][[Scode]], DailyTrade[['DSellN']][[Scode]])
              Tpro = (SellP * SellN - Current * 500) > (Current * 500 * meanVib/100)
              if (Tpro) {
                GoBuy(Stock = Scode, plusN = 500)
                DailyTrade[['TBsto']][Scode] = c(Current, DailyTrade[['TBsto']][Scode])
                DailyTrade[['TBuyN']][Scode] = c(500, DailyTrade[['TBuyN']][Scode])
              }
            }
          }
        }
      }
      # ...Unknown place----
      NTBsto.test = tryCatch({(!Scode %in% names(DailyTrade[['DBsto']]) & Invest < 30000 & ChangeRate < ChangeRateP2)}, 
                            error = function(err) {
                              log4r::error(logger, paste0(err, "\tNTBsto.test encounter error, use default parameters."))
                              return(FALSE)
                            })
      if (NTBsto.test) {
        if (sdJ1 > sdJ2 & VibCR & swordNP == 'positive') {
          if (Current <= MA10) {
            plusNum = 0
            if (ChangeRate <= -5 & ChangeRate > -8) {
              plusNum = 700
            } else if (ChangeRate <= -2 & ChangeRate > -5) {
              plusNum = 600
            } else if (ChangeRate > -2 & ChangeRate < 1) {
              plusNum = 500
            }
            if (plusNum > 0 & TotalN != 0) {
              GoBuy(Stock = Scode, plusN = plusNum)
              DailyTrade[['TBsto']][Scode] = c(Current, DailyTrade[['TBsto']][Scode])
              DailyTrade[['TBuyN']][Scode] = c(plusNum, DailyTrade[['TBuyN']][Scode])
            }
          }
        }
      }
    }
    saveRDS(DailyTrade, 'e:/tess/DailyTrading.RDS')
    # ...two hours rest----
    # (11:30-13:00)----
    if (format(Sys.time(), "%H:%M") >= '11:30' && format(Sys.time(), "%H:%M") <= '13:00') {
      print('test recall stocks...')
      HstoInfo(testRecall = T)
      shell('title Mid rest...')
      Sys.sleep(30); shell('taskkill /F /IM xiadan.exe')
      # ...update info of mid-sting stocks
      tryCatch({source('e:/NewPC/THS/MidUpdateStoInfo.R', local = T, echo = F, encoding = 'UTF-8')}, 
               error = function(err) {
                 log4r::error(logger, paste0(err, '\tRe-Source MidUpdateStoinfo.R ...'))
                 source('e:/NewPC/THS/MidUpdateStoInfo.R', local = T, echo = F, encoding = 'UTF-8')
               })
      # loop test in mid rest
      while (TRUE) {
        if (format(Sys.time(),'%H:%M') %in% c('12:20','12:57')) {
          # restart Xiadan
          keybd.press('esc'); Sys.sleep(30)
          Xiadan(restart = T)
        } else if (format(Sys.time(),'%H:%M') >= '13:00')  {
          # get out of rest ...
          print('get out of rest...')
          break()
        }
        # else
        print('keep rest in middle time...')
        Sys.sleep(30)
      } }
    # ...creat new stocks when left too much money----
    if (as.numeric(format(Sys.time(), "%M")) %% 15 == 5) {
      tryCatch({dydata(DBclass = "BackUpQuaterly")}, error = function(err) {
        log4r::error(logger, paste0(err, '\tBackup data quaterly failed ...'))
      })
      # get money left
      money = getnewimg(sub.win = 'money');
      Tmoney = tryCatch({sum(as.numeric(Hsto.info$Price)) * 500}, error = function(err) {
        log4r::error(logger, paste0(err, '\tSum T-totaly money failed ...'))
        return(30000)
      })
      if (money > Tmoney) {
        moneyTF = TRUE
        ScodeNums = names(K.old[['sting']])
        testCode = ScodeNums[which(!ScodeNums %in% Spycode)]
        exclude = read.table('e:/tess/exclude.info',colClasses = 'character')[,1]
        testCode1 = testCode[which(!testCode %in% exclude)]
        if (length(testCode1) > 0) {
          for (Scode in testCode1) {
            swordNP = Sword(Scode, days = 10, testType = 'PushPull', ratio = 5) == 'positive'
            if (moneyTF & swordNP) {
              cat(paste0('Test buy condition of ',Scode,'...\n'))
              dailyK.old = K.old[['sting']][[Scode]][['dailyK']]
              Close = dailyK.old$CLOSE[1]; MA10 = mean(colMeans(dailyK.old[1:10,c('CLOSE','OPEN')]), na.rm = T)
              Current = getPrice(Scode = Scode); ChangeRate = (Current - Close)/Close * 100
              sdJ1 = dailyK.old[1,'J'] - dailyK.old[2,'J'];sdJ2 = dailyK.old[2,'J'] - dailyK.old[3,'J']
              sdJ1 = ifelse(!anyNA(sdJ1),sdJ1,0); sdJ2 = ifelse(!anyNA(sdJ2),sdJ2,0); 
              meanVib = mean(K.old[['sting']][[Scode]][['CStoDetail']][1:5,8], na.rm = T)
              meanVib = ifelse(anyNA(meanVib), 2, meanVib)
              VibCR = (ChangeRate < -meanVib/3)
              # test buy condition...
              if (sdJ1 > sdJ2 & Current > 10 & Current < 30 & Current <= MA10 & dailyK.old[1,'J'] < 90 & VibCR) {
                if (ChangeRate <= -5 & ChangeRate > -8) {
                  cat('Gobuy.700 with ChangeRate <= -5\n')
                  GoBuy(Stock = Scode, plusN = 700); 
                  DailyTrade[['DBsto']][Scode] = Current
                  DailyTrade[['DBuyN']][Scode] = 700
                  money = money - Current * 700
                  if (money < Tmoney) {moneyTF = F; break()}
                } else if (ChangeRate <= -2 & ChangeRate > -5) {
                  cat('Gobuy.600 with ChangeRate <= -2\n')
                  GoBuy(Stock = Scode, plusN = 600); 
                  DailyTrade[['DBsto']][Scode] = Current
                  DailyTrade[['DBuyN']][Scode] = 600
                  money = money - Current * 600
                  if (money < Tmoney) {moneyTF = F; break()}
                } else if (ChangeRate <= 1 & ChangeRate > -2) {
                  cat('Gobuy.500 with ChangeRate <= -2\n')
                  GoBuy(Stock = Scode, plusN = 500); 
                  DailyTrade[['DBsto']][Scode] = Current
                  DailyTrade[['DBuyN']][Scode] = 500
                  money = money - Current * 500
                  if (money < Tmoney) {moneyTF = F; break()}
                }
              }
            } }
          saveRDS(DailyTrade, 'e:/tess/DailyTrading.RDS')  }  }
    }
    # update index of stock market
    if (as.numeric(format(Sys.time(), "%M")) %% 5 == 0) {
      tryCatch(getindex(type = "Fmins"), error = function(err) {
        log4r::error(logger, paste0(err, '\tGet Fmins index of Sto-market failed ...'))
      })
    }
    # time to closing market
    if (format(Sys.time(), "%H:%M") >= '14:57' ) {
      print('test recall stocks...')
      HstoInfo(testRecall = T)
      print('Get out of selling loop...')
      break()
    } else {
      cat('Keep selling check...\n\n')
      Sys.sleep(50)
    }
  } 
}
# (after-15:00)----
if (format(Sys.time(),'%H:%M') >= '14:57') {
  Sys.sleep(runif(1, 30, 50)); 
  money = getnewimg(sub.win = 'money')
  getnewimg(refresh = T)
  Sys.sleep(runif(1, 150, 180))
  HstoInfo(); Sys.sleep(runif(1, 10, 20)); shitSto(); 
  shell('taskkill /F /IM xiadan.exe')
  # ...update tradeReport----
  if (file.exists('e:/tess/tradeRe.RDS')) {
    tradeRe = tryCatch({readRDS('e:/tess/tradeRe.RDS')}, 
                       error = function(err) {
                         log4r::error(logger, paste0(err, "\tUse backuped tradeRe.RDS ..."))
                         file.copy('e:/tess/backupDB/tradeRe.RDS', 'e:/tess/tradeRe.RDS', overwrite = T);
                         readRDS('e:/tess/backupDB/tradeRe.RDS')
                       })
    if (tradeRe[[1]]$date[1] != Sys.Date()) {
      tradeReport(update = T)
    }
  } else {tradeReport(update = F)}
  # update index of Sto-market
  tryCatch(getindex(type = "Daily"), error = function(err) {
    log4r::error(logger, paste0(err, '\tGet Daily index of Sto-market failed ...'))
  })
  # ...update info of sting stocks after market closing...----
  tryCatch({source('e:/NewPC/THS/UpdateStoInfo.R', local = T, echo = F, encoding = 'UTF-8')}, 
           error = function(err) {
             log4r::error(logger, paste0(err, '\tRe-Source updateStoinfo.R ...'))
             source('e:/NewPC/THS/UpdateStoInfo.R', local = T, echo = F, encoding = 'UTF-8')
           })
  Sys.sleep(runif(1, 10, 20));
  # ...send trading messages----
  # update info
  K.old = readRDS('e:/tess/SpySto.RDS')
  UpdateK = tapply(K.old[['sting']], names(K.old[['sting']]), FUN = function(dat){
    dat[[1]][['dailyK']][1, c("Date")] == Sys.Date()})
  UpdateR = round(length(which(UpdateK == T)) / length(UpdateK) * 100, 2)
  meanPri.RES = tapply(K.old[['sting']], names(K.old[['sting']]), FUN = function(dat){
    mean(colMeans(dat[[1]][['dailyK']][1:10, c("CLOSE", "OPEN")], na.rm = T)) > 10})
  meanPriR = round(length(which(meanPri.RES == T)) / length(meanPri.RES) * 100, 2)
  # trade info
  sys.user = Sys.info()[['nodename']]
  Money.rest = ifelse(anyNA(money),'NULL', money)
  T.invest = sum(as.numeric(readRDS('e:/tess/Hsto.info.RDS')$Value))
  T.profit = sum(as.numeric(readRDS('e:/tess/Hsto.info.RDS')$Profit))
  T.profit.old = sum(as.numeric(readRDS('e:/tess/backupDB/Hsto.info.RDS')$Profit))
  Daily.profit = round(T.profit - T.profit.old, 2)
  errlog = grep('INFO', readLines('e:/tess/trade.log'), value = T, invert = T)
  text = paste0('From: ',sys.user,': \nM_rest: ', Money.rest, '; \nT_invest: ', T.invest, 
                '; \nT_pro: ',T.profit, '; \nDay_pro: ', Daily.profit, 
                '\nUpdatedK_Rate: ', UpdateR,'%; \nminMeanPri_Rate: ', meanPriR, '%.\n\n====== errlog:\n', 
                paste0(errlog, collapse = '\n'))
  SendTrading(to = 'helongw313@outlook.com', message = text)
  Sys.sleep(runif(1, 10, 20))
  # re-check Midupdate info of mid-sting stocks and replace NAs with final info
  tryCatch({source('e:/NewPC/THS/MidUpdateStoInfo.R', local = T, echo = F, encoding = 'UTF-8')}, 
           error = function(err) {
             log4r::error(logger, paste0(err, '\tRe-Source MidUpdateStoinfo.R ...'))
             source('e:/NewPC/THS/MidUpdateStoInfo.R', local = T, echo = F, encoding = 'UTF-8')
           })
  # BackUpDaily and pushing updated trading info
  dydata(DBclass = 'BackUpDaily')
  GitPP(weekday = "Fri", push = T) 
}
