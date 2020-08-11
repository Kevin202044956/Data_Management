par(mfrow=c(1,1))
digit = matrix(scan('HandWritten.txt'), 16, byrow = TRUE)
image(t(digit[16:1, ]))


df = read.csv('mixture.csv', row.names = 1)
col_list<-ifelse(df$y==1,'blue','red')
plot(x2 ~ x1, df, cex.lab = 1.5, xlim = c(-3.5, 5.5), ylim = c(-3.5, 4.5), bty = 'L',
     col=col_list)
lines(df$x1, (0.978 + 0.134*df$x1)/1.398, lwd = 2)
x1 = sort(df$x1)
x = c(min(x1)-0.5, x1, max(x1)+0.5)
polygon(c(x[1], x, x[length(x)]),
        c(min(df$x2)-0.5, (0.978 + 0.134*x)/1.398, min(df$x2)-0.5), col = 2)
polygon(c(x[1], x, x[length(x)]),
        c(max(df$x2)+0.5, (0.978 + 0.134*x)/1.398, max(df$x2)+0.5), col = 4)
pchs = ifelse(df$y == 0, 2, 8)
points(df$x1, df$x2, pch = pchs,col=col_list)
legend('topright', legend = c(0, 1), pch = c(2, 8), cex = 1.5, bty = 'n', inset = 0.01
       ,col=c('red','blue'))




#Pie plot and bar plot
df = read.csv('Q4_data.csv', stringsAsFactors = FALSE)
par(mfrow = c(1,2))
tb = table(df$Location)
pie(table(df$Location), col = colours()[3:7], main = 'Pie Chart of Location',
    radius = 0.9, cex = 1, border = FALSE, cex.main = 1.5,
    labels = paste0(names(table(df$Location)), ': ', table(df$Location)))
df$Winner = gsub('#[0-9]+\\s+', '', df$Winner)
my_winner = c(table(df$Winner)[1],table(df$Winner)[3],table(df$Winner)[2])
bp <- barplot(my_winner,col = colours()[5:7], ylim = c(0, 80),
        cex.names = 1, ylab = 'Count', main = 'Barplot of Winner', cex.main = 1.5
      )
text(bp,c(64.5, 56.5, 4.5), my_winner,cex=1.5,pos=3)

#The score difference
score = as.numeric(unlist(strsplit(df$Score, 'C')))
score1 = score[seq(1, length(score), 2)]
score2 = score[seq(2, length(score), 2)]
score_Vir = ifelse(df$Winner == 'Virginia', score1, score2)
score_Nor = ifelse(df$Winner == 'Virginia', score2, score1)
df['VA_score']<-score_Vir
df['Nor_score']<- score_Nor
df['diff']<- df$VA_score-df$Nor_score
par(mfrow = c(1,1))
plot(df$diff, type = 'o', col='lightblue', xlab = "Each Competition", 
     ylab = "Score", main = "Score Winning", yaxt="n",
     pch = 19,cex = 0.7)
axis(side = 2,at=c(-60,-40,-20,0,20,40,60), labels = c(60,40,20,0,20,40,60))
axis(side = 4, at = c(-30,30), labels = c("NC", "VA"))
abline(h=0, col="pink",lwd=2,lty = 2)

#The average score winning for VA and NC
NC_win_score<- df[which(df$diff < 0),]
VA_win_score<- df[which(df$diff > 0),]
data<-c(mean(abs(NC_win_score$diff)), mean(VA_win_score$diff))
bp2<-barplot(data, col=colours()[6:7],
        ylab = "Score wins", names.arg = c("North Carolina", "Virginia"), main = "Average scores winning for all games"
        ,ylim = c(0,25))
text(bp,c(18.71875, 15.5), round(data,1), cex = 1.2, pos=3)


#lucky location
df = read.csv('Q4_data.csv', stringsAsFactors = FALSE)
wining_loc_VA<- df[which(df$Winner=='Virginia'),"Location"]
wining_loc_NC<- df[which(df$Winner=='North Carolina'), "Location"]
par(mfrow=c(1,2))
tb1<-table(wining_loc_VA)
tb2<-table(wining_loc_NC)

pie(tb1,col = colours()[4:7], main="Wining place for VA",
    radius = 0.9, cex=1, border=FALSE, cex.main=1.5,
    labels = paste0(names(tb1), ': ', tb1))

pie(tb2,col = colours()[2:6], main="Wining place for NC",
    radius = 0.9, cex=1, border=FALSE, cex.main=1.5,
    labels = paste0(names(tb2), ': ', tb2))





df = read.csv('TeacherHires.csv', stringsAsFactors = FALSE, na.strings = 'N/A')
df$Calculated.Age = as.numeric(df$Calculated.Age)
df = df[!is.na(df$Calculated.Age), ]
df$Age = ifelse(df$Calculated.Age < 40, 'young', 'old')
df$Interview = ifelse(df$Interview == 'no', 'no', 'yes')
df$Hired = ifelse(df$Hired == 'no', 'no', 'yes')
par(mfrow = c(1, 2), mar = c(3, 3, 2, 3))
tb1 = round(prop.table(table(df$Interview, df$Age), 2), 4)
barplot(tb1, beside = TRUE, legend.text = TRUE, ylim  = c(0, 1), xlab = 'Age',
        ylab = 'Percentage', main = 'Interviewed or not vs Age',
        args.legend = list(bty = 'n'))
text(c(1.5, 2.5), tb1[, 1]+0.05, paste0(tb1[, 1]*100, '%'))
text(c(4.5, 5.5), tb1[, 2]+0.05, paste0(tb1[, 2]*100, '%'))
tb2 = round(prop.table(table(df$Hired, df$Age), 2), 4)
barplot(tb2, beside = TRUE, legend.text = TRUE, ylim  = c(0, 1), xlab = 'Age',
        ylab = 'Percentage', main = 'Hired or not vs Age',
        args.legend = list(bty = 'n'))

text(c(1.5, 2.5), tb2[, 1]+0.05, paste0(tb2[, 1]*100, '%'))
text(c(4.5, 5.5), tb2[, 2]+0.05, paste0(tb2[, 2]*100, '%'))

df$GPA.undergrad = as.numeric(df$GPA.undergrad)
df$GPA.GRAD = as.numeric(df$GPA.GRAD)
df = df[!((is.na(df$GPA.GRAD)) | (is.na(df$GPA.undergrad))), ]
par(mfrow = c(2, 2), mar = c(3, 3, 3, 2))
boxplot(GPA.undergrad ~ Hired, df, main = 'Hired or not vs GPA Undergrad')
boxplot(GPA.GRAD ~ Hired, df, main = 'Hired or not vs GPA Grad')
ind = grep('months', df$Work.Experience )
df$Work.Experience = as.numeric(gsub('\\s+.*s', '', df$Work.Experience))
df$Work.Experience[ind] = df$Work.Experience[ind] / 12
boxplot(Work.Experience ~ Interview, df, main = 'Interviewed or not vs Work Experience')
boxplot(Work.Experience ~ Hired, df, main = 'Hired or not vs Work.Experience')

updateR()

phi<-c(1,3/2,-1)
ployroot(phi)