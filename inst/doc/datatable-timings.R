### R code from vignette source 'datatable-timings.Rnw'

###################################################
### code chunk number 1: datatable-timings.Rnw:15-18
###################################################
if (!exists("data.table",.GlobalEnv)) library(data.table)  # see Intro.Rnw for comments on these two lines
rm(list=as.character(tables()$NAME),envir=.GlobalEnv)
options(width=70)  # so lines wrap round


###################################################
### code chunk number 2: datatable-timings.Rnw:52-68
###################################################
n = ceiling(1e7/26^2)   # 10 million rows
DF = data.frame(x=rep(LETTERS,each=26*n),
                y=rep(letters,each=n),
                v=rnorm(n*26^2),
                stringsAsFactors=FALSE)
DT = as.data.table(DF)
system.time(setkey(DT,x,y))  # one-off cost, usually
tables()

tt=system.time(ans1 <- DF[DF$x=="R" & DF$y=="h",]); tt
head(ans1)
dim(ans1)
ss=system.time(ans2 <- DT[J("R","h")]); ss
head(ans2)
dim(ans2)
identical(ans1$v,ans2$v)


###################################################
### code chunk number 3: datatable-timings.Rnw:70-71
###################################################
if(!identical(ans1$v,ans2$v)) stop("Test 1 not identical")


###################################################
### code chunk number 4: datatable-timings.Rnw:78-83
###################################################
ttt=system.time(ans1 <- tapply(DF$v,DF$x,sum)); ttt
head(ans1)
sss=system.time(ans2 <- DT[,sum(v),by=x]); sss
head(ans2)
identical(as.vector(ans1), ans2$V1)


###################################################
### code chunk number 5: datatable-timings.Rnw:85-86
###################################################
if(!identical(as.vector(ans1), ans2$V1)) stop("Test 2 not identical")


###################################################
### code chunk number 6: datatable-timings.Rnw:97-102
###################################################
ans = matrix(c(tt[3],ss[3],ttt[3],sss[3]),byrow=TRUE,ncol=2)
rownames(ans)=c("==","tapply")
colnames(ans)=c("base","data.table")
ans = cbind(ans,"times faster"=as.integer(ans[,1]/ans[,2]))



###################################################
### code chunk number 7: datatable-timings.Rnw:103-104
###################################################
ans


###################################################
### code chunk number 8: datatable-timings.Rnw:107-108
###################################################
toLatex(sessionInfo())


