# Read historical dataset of stage levels
h <- read.csv("https://raw.githubusercontent.com/thspires/catalao/master/Cotas.csv")
# # # #
# Begin data scraping from portodemanaus
temp <- read_html(html_session("https://www.portodemanaus.com.br/?pagina=nivel-do-rio-negro-hoje"))
temp <- html_table(temp,fill=T)
ano <- gsub(".+de ","",temp[[2]][2,9])
temp <- data.frame(temp[[8]])
temp <- temp[,grep("Cota",temp[2,])]
names(temp) <- temp[1,]
temp <- temp[-c(1,2,34:38),]
temp$dia <- c(1:31)
temp <- melt(temp,id=c("dia"))
temp$value <- as.numeric(gsub(",","\\.",temp$value))
temp <- temp[!is.na(temp$value),]
temp <- temp[!is.na(temp$value),]
temp <- subset(temp,value!=0.00)
head(temp)
names(temp) <- c("Dia","Mes","Cota")
temp$Ano <- ano
temp <- temp[,c(2,1,4,3)]
temp$Estacao <- NA
temp$Estacao[is.na(temp$Estacao) & temp$Cota>26] <- "Cheia"
temp$Estacao[is.na(temp$Estacao) & temp$Cota<20] <- "Seca"
temp$Estacao[is.na(temp$Estacao) & temp$Cota>20 & temp$Cota < 26 & temp$Dia2 <150] <- "Enchente"
temp$Estacao[is.na(temp$Estacao)] <- "Cheia"
temp$Dia2 <- (h$Dia2[nrow(h)]+1) : (h$Dia2[nrow(h)]+nrow(temp))
temp$Anohidro <- h$Anohidro[nrow(h)]
h <- rbind(h,temp)
h$Dia3 <- NA; for(i in 1:max(h$Anohidro)){h$Dia3[which(h$Anohidro==i)] <- 1:length(h$Dia3[which(h$Anohidro==i)])}
h$Mes <- factor(h$Mes,levels=c("Janeiro","Fevereiro","Marco","Abril","Maio","Junho","Julho","Agosto","Setembro","Outubro","Novembro","Dezembro"))
h$Ano <- as.numeric(h$Ano)
h$Anohidro <- as.numeric(h$Anohidro)

# Calcular medias para fazer hidrografa da serie historica
tmp <- tapply(h$Cota,h$Dia2,mean)
tmp2 <- tapply(h$Cota,h$Dia3,mean)
# Cota de hoje e diferenca com ontem
cotahj <- h$Cota[nrow(h)]
dif <- round(cotahj - tmp[h$Dia2[nrow(h)]],digits=2)
icone <- if (dif>0) "arrow-up" else "arrow-down"
sd <- if (dif>0) "subiu" else "desceu"

# Diferenca com o mesmo dia do ano paassado
anopassado <- h$Ano[nrow(h)]-1
hj_dia <- h$Dia2[nrow(h)]
cotaanopassado <- h[which(h$Ano==anopassado & h$Dia2==hj_dia),"Cota"]
dif2 <- round(cotahj - cotaanopassado,digits=2)
icone2 <- if (dif2>0) "arrow-up" else "arrow-down"
sd2 <- if (dif2>0) "mais alto" else "mais baixo"
# save this as .Rdata and push to github?

