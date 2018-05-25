##### I currently plan to create a bank of 5000 random questions per test,      #####
##### which requires 25 distinct generators per test as they appear below. So,  #####
##### 75 total genrators for a semester of three tests. Once finished with      #####
##### individual generators, I will begin function wrapping, creating either a  #####
##### single function with many adaptive arguments, or a suite of functions.    #####

##### These will help streamline test writing, requiring only that one runs the #####
##### code and occasionally re-uploads the test banks. The longest part of the  #####
##### process is waiting on the e-learning uploads of the CSVs so-created.      #####                                                               #####

##### 7/25 Generators for test 1 completed  #####
##### 0/25 Generators for test 2 completed  #####
##### 0/25 Generators for test 3 completed  #####

#### 0/75 Generators wrapped into functions #####

##### n is the number of questions desired, answers is the number of answers    #####
##### per question, points.per.q are the points assigned to each question,      #####
##### difficulty is the difficulty assigned to each question, and type is the   #####
##### question's category (all as listed on e-learning. I've labeled the        #####
##### questions by concept, e-learning type, and creation of the concept/type   #####
##### pair. For example, MeanMC1 is a generator for my first very simple        #####
##### multiple choice question about a mean.                                    #####

##### MeanMC1 #####
ID <- "MeanMC1"
n = 200
type <- "MC"
answers <- 4
points.per.q <- 4
difficulty <- 1
quest.txt <- "What is the mean of the following dataset?"
dat.size = 5
digits = 1
questions <- data.frame()
for(i in 1:n)
{
points <- sample(c(rep(0,answers-1),100),replace=F)
corr.ind <- 6 + which.max(points)
data <- round(rnorm(dat.size,mean=rnorm(1,mean=900,sd=400),sd=200) + (0.5*rt(dat.size,df=30)), digits = digits)
corr.ans <- round(mean(data), digits = digits)
ans.txt <- round(rnorm(answers,mean=rnorm(1,mean=900,sd=400),sd=200) + (0.75*rt(answers,df=30)), digits = digits)
hint <- "Watch out for negatives!"
feedback <- "Did you sum the numbers (subtracting any negatives) and divide by the sample size?"
param <- c("NewQuestion","ID","Title","QuestionText","Points","Difficulty",
           rep("Option", answers),"Hint","Feedback")
content <- c(type, ID, "Find the Mean", paste(quest.txt,
                                                     paste(as.character(data),
                                                           collapse=", ",sep=""),
                                                     collapse = ""),
             points.per.q, difficulty, points, hint, feedback)
options <- c(rep("",6), ans.txt, rep("",2))
options[corr.ind] <- corr.ans
questions[(1+(8+answers)*i):((8+answers)*(i+1)),1] <- param
questions[(1+(8+answers)*i):((8+answers)*(i+1)),2] <- content
questions[(1+(8+answers)*i):((8+answers)*(i+1)),3] <- options
}
questions <- questions[(9+answers):((8+answers)*(n+1)),]
write.table(questions, sep=",", file=paste(ID, ".csv", sep = ""),
            row.names=F, col.names=F)

##### MedianMC1 #####
ID <- "MedianMC1"
n = 200
type <- "MC"
answers <- 4
points.per.q <- 4
difficulty <- 1
quest.txt <- "What is the median of the following dataset?"
digits = 1
questions <- data.frame()
for(i in 1:n)
{
  dat.size = sample(c(5,6), size = 1)
  points <- sample(c(rep(0,answers-1),100),replace=F)
  corr.ind <- 6 + which.max(points)
  data <- round(rnorm(dat.size,mean=rnorm(1,mean=900,sd=400),sd=200) + (0.5*rt(dat.size,df=30)), digits = digits)
  corr.ans <- round(median(data), digits = digits)
  ans.txt <- round(sample(c(median(data)-15.2, sd(data),
                       rnorm(answers-2,mean=rnorm(1,mean=900,sd=400),sd=200) + (0.5*rt(answers-2,df=30))),
                     replace = F),
                   digits = digits)
  hint <- "Sort the data first."
  feedback <- "Sort and find the middle number, or take the average of the two middle numbers."
  param <- c("NewQuestion","ID","Title","QuestionText","Points","Difficulty",
             rep("Option", answers),"Hint","Feedback")
  content <- c(type, ID, "Find the Median", paste(quest.txt,
                                                           paste(as.character(data),
                                                                 collapse=", ",sep=""),
                                                           collapse = ""),
               points.per.q, difficulty, points, hint, feedback)
  options <- c(rep("",6), ans.txt, rep("",2))
  options[corr.ind] <- corr.ans
  questions[(1+(8+answers)*i):((8+answers)*(i+1)),1] <- param
  questions[(1+(8+answers)*i):((8+answers)*(i+1)),2] <- content
  questions[(1+(8+answers)*i):((8+answers)*(i+1)),3] <- options
}
questions <- questions[(9+answers):((8+answers)*(n+1)),]
write.table(questions, sep=",", file=paste(ID, ".csv", sep = ""),
            row.names=F, col.names=F)

##### SDMC1 #####
ID <- "SDMC1"
n = 200
type <- "MC"
answers <- 4
points.per.q <- 4
difficulty <- 1
quest.txt <- "What is the SD of the following dataset?"
dat.size = 5
digits = 1
questions <- data.frame()
for(i in 1:n)
{
  points <- sample(c(rep(0,answers-1),100),replace=F)
  corr.ind <- 6 + which.max(points)
  data <- round(rnorm(dat.size,mean=rnorm(1,mean=900,sd=400),sd=200) + (0.5*rt(dat.size,df=30)), digits = 1)
  corr.ans <- round(sd(data), digits = digits)
  ans.txt <- round(sample(c(sd(data)^2, sqrt(sd(data)^2*(dat.size-1)),
                       rnorm(answers-2,mean=rnorm(1,mean=900,sd=400),sd=200) + (0.5*rt(answers-2,df=30))),
                     replace = F), digits = digits)
  hint <- "Don't forget to take a square root at the end."
  feedback <- "1: Mean. 2: Squared differences. 3: Sum. 4: Divide. 5. Square Root. "
  param <- c("NewQuestion","ID","Title","QuestionText","Points","Difficulty",
             rep("Option", answers),"Hint","Feedback")
  content <- c(type, ID, "Find the SD", paste(quest.txt,
                                                       paste(as.character(data),
                                                             collapse=", ",sep=""),
                                                       collapse = ""),
               points.per.q, difficulty, points, hint, feedback)
  options <- c(rep("",6), round(ans.txt, digits = digits), rep("",2))
  options[corr.ind] <- corr.ans
  questions[(1+(8+answers)*i):((8+answers)*(i+1)),1] <- param
  questions[(1+(8+answers)*i):((8+answers)*(i+1)),2] <- content
  questions[(1+(8+answers)*i):((8+answers)*(i+1)),3] <- options
}
questions <- questions[(9+answers):((8+answers)*(n+1)),]
write.table(questions, sep=",", file=paste(ID, ".csv", sep = ""),
            row.names=F, col.names=F)

##### NormDistMC1 #####
ID <- "NormDistMC1"
n = 200
type <- "MC"
answers <- 4
points.per.q <- 4
difficulty <- 1
quest.txt1 <- "Given that some normally distributed data has a mean of "
quest.txt2 <- " and a standard deviation of "
quest.txt3 <- ". What is the probability that a randomly sampled datapoint will be less than "
quest.txt4 <- "?"
dat.size = 1
digits = 3
questions <- data.frame()
for(i in 1:n)
{
  points <- sample(c(rep(0,answers-1),100),replace=F)
  corr.ind <- 6 + which.max(points)
  data1 <- rnorm(dat.size,mean=rnorm(1,mean=900,sd=300),sd=100) + (0.25*rt(dat.size,df=30))
  data2 <- runif(dat.size, 65, 150)
  data3 <- sample(c(runif(dat.size, data1 - 180, data1 - 5),
                  runif(dat.size, data1 + 5, data1 + 180)), size = dat.size)
  corr.ans <- pnorm(round(((data3 - data1)/data2), digits = 2))
  up.min <- corr.ans + .05
  down.max <- corr.ans - .05
  ans.txt <- sample(if(corr.ans < .025){runif(answers, up.min, 1.05)}
                    else{if(corr.ans > .975){runif(answers, -0.05, down.max)}
                      else{c(runif(ceiling(answers/2), -0.05, down.max),
                             runif(ceiling(answers/2), up.min, 1.05))}},
                    size = answers, replace = F)
  hint <- "You'll need your Z-table for this question."
  feedback <- "1: Calculate Z. 2: Find area below on the Z-table."
  param <- c("NewQuestion","ID","Title","QuestionText","Points","Difficulty",
             rep("Option", answers),"Hint","Feedback")
  content <- c(type, ID, "Normal Probability", paste(quest.txt1, round(data1, digits=3), quest.txt2,
                                                                round(data2, digits=3), quest.txt3, 
                                                                round(data3, digits = 3), quest.txt4,
                                                                collapse = "", sep= ""),
               points.per.q, difficulty, points, hint, feedback)
  options <- c(rep("",6), round(ans.txt, digits = digits), rep("",2))
  options[corr.ind] <- round(corr.ans,digits = digits)
  questions[(1+(8+answers)*i):((8+answers)*(i+1)),1] <- param
  questions[(1+(8+answers)*i):((8+answers)*(i+1)),2] <- content
  questions[(1+(8+answers)*i):((8+answers)*(i+1)),3] <- options
}
questions <- questions[(9+answers):((8+answers)*(n+1)),]
write.table(questions, sep=",", file=paste(ID, ".csv", sep = ""),
            row.names=F, col.names=F)

##### NormDistMC2 #####
ID <- "NormDistMC2"
n = 200
type <- "MC"
answers <- 4
points.per.q <- 4
difficulty <- 1
quest.txt1 <- "Given that some normally distributed data has a mean of "
quest.txt2 <- " and a standard deviation of "
quest.txt3 <- ". What is the probability that a randomly sampled datapoint will be greater than "
quest.txt4 <- "?"
dat.size = 1
digits = 3
questions <- data.frame()
for(i in 1:n)
{
  points <- sample(c(rep(0,answers-1),100),replace=F)
  corr.ind <- 6 + which.max(points)
  data1 <- rnorm(dat.size,mean=rnorm(1,mean=900,sd=300),sd=100) + (0.25*rt(dat.size,df=30))
  data2 <- runif(dat.size, 65, 150)
  data3 <- sample(c(runif(dat.size, data1 - 180, data1 - 5),
                    runif(dat.size, data1 + 5, data1 + 180)), size = dat.size)
  corr.ans <- 1 - pnorm(round(((data3-data1)/data2), digits = 2))
  up.min <- corr.ans + .05
  down.max <- corr.ans - .05
  ans.txt <- sample(if(corr.ans < .025){runif(answers, up.min, 1.05)}
                    else{if(corr.ans > .975){runif(answers, -0.05, down.max)}
                         else{c(runif(ceiling(answers/2), -0.05, down.max),
                                runif(ceiling(answers/2), up.min, 1.05))}},
                    size = answers, replace = F)
  hint <- "You'll need your Z-table for this question."
  feedback <- "1: Calculate Z. 2: Find area below on the Z-table. 3: Take 1 - area below."
  param <- c("NewQuestion","ID","Title","QuestionText","Points","Difficulty",
             rep("Option", answers),"Hint","Feedback")
  content <- c(type, ID, "Normal Probability", paste(quest.txt1, round(data1, digits=3), quest.txt2,
                                                                round(data2, digits=3), quest.txt3, 
                                                                round(data3, digits = 3), quest.txt4,
                                                                collapse = "", sep= ""),
               points.per.q, difficulty, points, hint, feedback)
  options <- c(rep("",6), round(ans.txt, digits = digits), rep("",2))
  options[corr.ind] <- round(corr.ans,digits = digits)
  questions[(1+(8+answers)*i):((8+answers)*(i+1)),1] <- param
  questions[(1+(8+answers)*i):((8+answers)*(i+1)),2] <- content
  questions[(1+(8+answers)*i):((8+answers)*(i+1)),3] <- options
}
questions <- questions[(9+answers):((8+answers)*(n+1)),]
write.table(questions, sep=",", file=paste(ID, ".csv", sep = ""),
            row.names=F, col.names=F)

##### InvNormMC1 #####
ID <- "InvNormMC1"
n = 200
type <- "MC"
answers <- 4
points.per.q <- 4
difficulty <- 1
quest.txt1 <- "Given that some normally distributed data has a mean of "
quest.txt2 <- " and a standard deviation of "
quest.txt3 <- ". What is the value x of this dataset where "
quest.txt4 <- "% of all other data values are "
dat.size = 1
digits = 1
questions <- data.frame()
for(i in 1:n)
{
  points <- sample(c(rep(0,answers-1),100), replace=F)
  corr.ind <- 6 + which.max(points)
  data1 <- round((rnorm(dat.size,mean=rnorm(1,mean=900,sd=300),sd=100) + (0.25*rt(dat.size,df=30))), digits = digits)
  data2 <- round(runif(dat.size, 65, 150), digits = digits) 
  data3 <- round(runif(dat.size, 3, 97), digits = 0)
  data4 <- sample(c("less?", "greater?"), size = 1)
  corr.ans <- ifelse(data4 == "less?", (data2*round(qnorm(data3/100), digits = 2) + data1),
                     (data2*round(qnorm(data3/100, lower.tail = F), digits = 2) + data1))
  up.min <- corr.ans + data2/3
  down.max <- corr.ans - data2/3
  #ans.text <- sample(c(runif(ceiling(answers/2), (data1 - 5*data2), down.max),
  #                     runif(ceiling(answers/2), up.min, (data1 + 5*data2))),
  #                   size = answers, replace = F)
  ans.txt <- sample(if(corr.ans < (data1 - 2*data2)){runif(answers, up.min, (data1 + 4*data2))}
                       else{if(corr.ans > (data1 + 2*data2)){runif(answers, (data1 - 4*data2), down.max)}
                            else{c(runif(ceiling(answers/2), (data1 - 4*data2), down.max),
                                   runif(ceiling(answers/2), up.min, (data1 + 4*data2)))}},
                    size = answers, replace = F)
  hint <- "This is an inverted or 'backward' Z-table question. Pick the closest answer."
  feedback <- "1: Find the closest probability on the Z-table. 2: Find the Z value. 3: Calculate the value x."
  param <- c("NewQuestion","ID","Title","QuestionText","Points","Difficulty",
             rep("Option", answers),"Hint","Feedback")
  content <- c(type, ID, "Inverse Normal Probability", paste(quest.txt1, data1, quest.txt2,
                                                                data2, quest.txt3, data3, quest.txt4,
                                                                data4, collapse = "", sep= ""),
               points.per.q, difficulty, points, hint, feedback)
  options <- c(rep("",6), round(ans.txt, digits = digits), rep("",2))
  options[corr.ind] <- round(corr.ans, digits = digits)
  questions[(1+(8+answers)*i):((8+answers)*(i+1)),1] <- param
  questions[(1+(8+answers)*i):((8+answers)*(i+1)),2] <- content
  questions[(1+(8+answers)*i):((8+answers)*(i+1)),3] <- options
}
questions <- questions[(9+answers):((8+answers)*(n+1)),]
write.table(questions, sep=",", file=paste(ID, ".csv", sep = ""),
            row.names=F, col.names=F)

##### RobustMC1 #####
ID <- "RobustMC1"
n = 200
type <- "MC"
answers <- 5
points.per.q <- 4
difficulty <- 1
quest.txt1 <- "A researcher wants to estimate the center of her data's distribution with a statistic that is "
quest.txt2 <- " Which statistic should she select for this purpose?"
dat.size = 
digits = 
questions <- data.frame()
for(i in 1:n)
{
  points <- sample(c(rep(0,answers-1),100),replace=F)
  corr.ind <- 6 + which.max(points)
  data <- sample(c("the least robust.", "the most robust."), size = 1)
  corr.ans <- ifelse(data == "the least robust.", "The mean", "The median")
  ans.txt <- sample(c(ifelse(data == "the least robust", "The median", "The mean"), "The 10% trimmed mean",
                       "The first quartile", "The third quartile", "The full range (max - min)", "The middle 50% range (Q3 - Q1)",
                       "The standard deviation", "The maximum", "The minimum", "A relative frequency"),
                     size = answers)
  hint <- "Remember that 'robust' means insensitive to outliers."
  feedback <- "The mean is least robust, and the median is most robust."
  param <- c("NewQuestion","ID","Title","QuestionText","Points","Difficulty",
             rep("Option", answers),"Hint","Feedback")
  content <- c(type, ID, "Robust Concept", paste(quest.txt1, data, quest.txt2,
                                                          collapse = "", sep = ""),
               points.per.q, difficulty, points, hint, feedback)
  options <- c(rep("",6), ans.txt, rep("",2))
  options[corr.ind] <- corr.ans
  questions[(1+(8+answers)*i):((8+answers)*(i+1)),1] <- param
  questions[(1+(8+answers)*i):((8+answers)*(i+1)),2] <- content
  questions[(1+(8+answers)*i):((8+answers)*(i+1)),3] <- options
}
questions <- questions[((9+answers)):((8+answers)*(n+1)),]
write.table(questions, sep=",", file=paste(ID, ".csv", sep = ""),
            row.names=F, col.names=F)

##### EmpRuleMC1 #####



##### VarTypeMC1 #####



##### VarTypeMC2 #####

