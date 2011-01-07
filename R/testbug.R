

testbug = function() {
   cat("hello\n")
   DT = data.table(a=letters[c(1,1,1,2,2)],6:10)
   stop("my error")
   browser()
   cat("mate\n")
}


