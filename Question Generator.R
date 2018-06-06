##### I currently plan to create a bank of 5000 random questions per test,      #####
##### which requires 25 distinct generators per test as they appear below. So,  #####
##### 75 total genrators for a semester of three tests. Once finished with      #####
##### individual generators, I will begin function wrapping, creating either a  #####
##### single function with many adaptive arguments, or a suite of functions.    #####

##### These will help streamline test writing, requiring only that one runs the #####
##### code and occasionally re-uploads the test banks. The longest part of the  #####
##### process is waiting on the e-learning uploads of the images and CSVs.      #####                                                               #####

##### 17/25 Generators for test 1 completed  #####
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
title <- "MeanMC1"
n = 200
type <- "MC"
answers <- 4
points.per.q <- 4
difficulty <- 1
quest.txt <- "What is the mean of the following dataset?"
dat.size = 5
digits = 1
hint <- "Take note that this question is asking for the mean, not the median."
feedback <- "Did you sum the numbers (subtracting any negatives) and divide by the sample size?"
param <- c("NewQuestion","ID","Title","QuestionText","Points","Difficulty",
           rep("Option", answers),"Hint","Feedback")
questions <- data.frame()
for(i in 1:n)
{
ID <- paste(title, i, sep = "-")
points <- sample(c(rep(0,answers-1),100),replace=F)
corr.ind <- 6 + which.max(points)
data <- round(rnorm(dat.size,mean=rnorm(1,mean=900,sd=400),sd=200) + (0.5*rt(dat.size,df=30)), digits = digits)
corr.ans <- round(mean(data), digits = digits)
up.min <- corr.ans + sd(data)/4
down.max <- corr.ans - sd(data)/4
ans.txt <- round(sample(c(sum(data), sum(data)/(dat.size-1),
                          runif(ceiling(3*answers/5), corr.ans - 4*sd(data), down.max),
                          runif(ceiling(3*answers/5), up.min, corr.ans + 4*sd(data))),
                        size = answers),
                 digits = digits)
content <- c(type, ID, ID, paste(quest.txt, paste(as.character(data),
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
write.table(questions, sep=",", file=paste(title, ".csv", sep = ""),
            row.names=F, col.names=F)

##### MedianMC1 #####
title <- "MedianMC1"
n = 200
type <- "MC"
answers <- 4
points.per.q <- 4
difficulty <- 1
quest.txt <- "What is the median of the following dataset?"
digits = 1
dat.size <- c(5,6)
hint <- "Sort the data first."
feedback <- "Sort and find the middle number, or take the average of the two middle numbers."
param <- c("NewQuestion","ID","Title","QuestionText","Points","Difficulty",
           rep("Option", answers),"Hint","Feedback")
questions <- data.frame()
for(i in 1:n)
{
  ID <- paste(title, i, sep = "-")
  dat.size1 = sample(dat.size, size = 1)
  points <- sample(c(rep(0,answers-1),100),replace=F)
  corr.ind <- 6 + which.max(points)
  data <- round(rnorm(dat.size1,mean=rnorm(1,mean=900,sd=400),sd=200) + (0.5*rt(dat.size1,df=30)), digits = digits)
  corr.ans <- round(median(data), digits = digits)
  up.min <- corr.ans + sd(data)/4
  down.max <- corr.ans - sd(data)/4
  ans.txt <- round(sample(c(if((dat.size1 %% 2) == 0){sort(data)[ceiling((dat.size1+1)/2)]}else{sort(data)[1+(dat.size1+1)/2]},
                            if((dat.size1 %% 2) == 0){sort(data)[floor((dat.size1+1)/2)]}else{sort(data)[(dat.size1+1)/2-1]},
                            runif(ceiling(answers/2), corr.ans - 4*sd(data), down.max),
                            runif(ceiling(answers/2), up.min, corr.ans + 4*sd(data))),
                          size = answers),
                   digits = digits)
  content <- c(type, ID, ID, paste(quest.txt, paste(as.character(data),
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
write.table(questions, sep=",", file=paste(title, ".csv", sep = ""),
            row.names=F, col.names=F)

##### SDMC1 #####
title <- "SDMC1"
n = 200
type <- "MC"
answers <- 4
points.per.q <- 4
difficulty <- 1
quest.txt <- "What is the standard deviation of the following dataset?"
dat.size = 5
digits = 1
hint <- "Don't forget to take a square root at the end."
feedback <- "1: Mean. 2: Squared differences. 3: Sum. 4: Divide. 5. Square Root. "
param <- c("NewQuestion","ID","Title","QuestionText","Points","Difficulty",
           rep("Option", answers),"Hint","Feedback")
questions <- data.frame()
for(i in 1:n)
{
  ID <- paste(title, i, sep = "-")
  points <- sample(c(rep(0,answers-1),100),replace=F)
  corr.ind <- 6 + which.max(points)
  data <- round(rnorm(dat.size,mean=rnorm(1,mean=900,sd=400),sd=200) + (0.5*rt(dat.size,df=30)), digits = 1)
  corr.ans <- round(sd(data), digits = digits)
  up.min <- corr.ans + sd(data)/4
  down.max <- corr.ans - sd(data)/4
  ans.txt <- round(sample(c(sd(data)^2, sqrt(sd(data)^2*(dat.size-1)), sqrt((data-mean(data))^2/dat.size),
                            runif(ceiling(answers/2), corr.ans - 4*sd(data), down.max),
                            runif(ceiling(answers/2), up.min, corr.ans + 4*sd(data))),
                          size = answers),
                   digits = digits)
  content <- c(type, ID, ID, paste(quest.txt, paste(as.character(data),
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
write.table(questions, sep=",", file=paste(title, ".csv", sep = ""),
            row.names=F, col.names=F)

##### NormDistMC1 #####
title <- "NormDistMC1"
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
hint <- "You'll need your Z-table for this question."
feedback <- "1: Calculate Z. 2: Find area below on the Z-table."
param <- c("NewQuestion","ID","Title","QuestionText","Points","Difficulty",
           rep("Option", answers),"Hint","Feedback")
questions <- data.frame()
for(i in 1:n)
{
  ID <- paste(title, i, sep = "-")
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
  content <- c(type, ID, ID, paste(quest.txt1, round(data1, digits=3), quest.txt2,
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
write.table(questions, sep=",", file=paste(title, ".csv", sep = ""),
            row.names=F, col.names=F)

##### NormDistMC2 #####
title <- "NormDistMC2"
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
hint <- "You'll need your Z-table for this question."
feedback <- "1: Calculate Z. 2: Find area below on the Z-table. 3: Take 1 - area below."
param <- c("NewQuestion","ID","Title","QuestionText","Points","Difficulty",
           rep("Option", answers),"Hint","Feedback")
questions <- data.frame()
for(i in 1:n)
{
  ID <- paste(title, i, sep = "-")
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
  content <- c(type, ID, ID, paste(quest.txt1, round(data1, digits=3), quest.txt2,
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
write.table(questions, sep=",", file=paste(title, ".csv", sep = ""),
            row.names=F, col.names=F)

##### InvNormMC1 #####
title <- "InvNormMC1"
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
hint <- "This is an inverted or 'backward' Z-table question. Pick the closest answer."
feedback <- "1: Find the closest probability on the Z-table. 2: Find the Z value. 3: Calculate the value x."
param <- c("NewQuestion","ID","Title","QuestionText","Points","Difficulty",
           rep("Option", answers),"Hint","Feedback")
questions <- data.frame()
for(i in 1:n)
{
  ID <- paste(title, i, sep = "-")
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
  ans.txt <- sample(if(corr.ans < (data1 - 2*data2)){runif(answers, up.min, (data1 + 4*data2))}
                       else{if(corr.ans > (data1 + 2*data2)){runif(answers, (data1 - 4*data2), down.max)}
                            else{c(runif(ceiling(answers/2), (data1 - 4*data2), down.max),
                                   runif(ceiling(answers/2), up.min, (data1 + 4*data2)))}},
                    size = answers, replace = F)
  content <- c(type, ID, ID, paste(quest.txt1, data1, quest.txt2,
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
write.table(questions, sep=",", file=paste(title, ".csv", sep = ""),
            row.names=F, col.names=F)

##### RobustMC1 #####
title <- "RobustMC1"
n = 200
type <- "MC"
answers <- 5
points.per.q <- 4
difficulty <- 1
quest.txt1 <- "A researcher wants to estimate the center of her data's distribution with a statistic that is "
quest.txt2 <- " Which statistic should she select for this purpose?"
dat.size = 
digits = 
hint <- "Remember that 'robust' means insensitive to outliers."
feedback <- "The mean is least robust, and the median is most robust."
param <- c("NewQuestion","ID","Title","QuestionText","Points","Difficulty",
           rep("Option", answers),"Hint","Feedback")
questions <- data.frame()
for(i in 1:n)
{
  ID <- paste(title, i, sep = "-")
  points <- sample(c(rep(0,answers-1),100),replace=F)
  corr.ind <- 6 + which.max(points)
  data <- sample(c("the least robust.", "the most robust."), size = 1)
  corr.ans <- ifelse(data == "the least robust.", "The mean", "The median")
  ans.txt <- sample(c(ifelse(data == "the least robust", "The median", "The mean"), "The 10% trimmed mean",
                       "The first quartile", "The third quartile", "The full range (max - min)", "The middle 50% range (Q3 - Q1)",
                       "The standard deviation", "The maximum", "The minimum", "A relative frequency"),
                     size = answers)
  content <- c(type, ID, ID, paste(quest.txt1, data, quest.txt2,
                                                          collapse = "", sep = ""),
               points.per.q, difficulty, points, hint, feedback)
  options <- c(rep("",6), ans.txt, rep("",2))
  options[corr.ind] <- corr.ans
  questions[(1+(8+answers)*i):((8+answers)*(i+1)),1] <- param
  questions[(1+(8+answers)*i):((8+answers)*(i+1)),2] <- content
  questions[(1+(8+answers)*i):((8+answers)*(i+1)),3] <- options
}
questions <- questions[((9+answers)):((8+answers)*(n+1)),]
write.table(questions, sep=",", file=paste(title, ".csv", sep = ""),
            row.names=F, col.names=F)

##### EmpRuleMC1 #####
title <- "EmpRuleMC1"
n = 200
type <- "MC"
answers <- 4
points.per.q <- 4
difficulty <- 1
quest.txt1 <- "Given that some normally distributed data has a mean of "
quest.txt2 <- " and a standard deviation of "
quest.txt3 <- ". What is the "
quest.txt4 <- " endpoint of the interval that is centered on the mean and includes "
quest.txt5 <- "% of all the data?"
dat.size = 1
digits = 2
hint <- "This is an empirical rule question."
feedback <- "1: Determine if the endpoint is lower or upper. 2: Find the probability centered on the mean (68, 95, or 99.7%). 3: Subtract or add 1, 2, or 3 SDs from the mean depending on steps 1 and 2."
param <- c("NewQuestion","ID","Title","QuestionText","Points","Difficulty",
           rep("Option", answers),"Hint","Feedback")
questions <- data.frame()
for(i in 1:n)
{
  ID <- paste(title, i, sep = "-")
  points <- sample(c(rep(0,answers-1),100), replace=F)
  corr.ind <- 6 + which.max(points)
  data1 <- round((rnorm(dat.size,mean=rnorm(1,mean=900,sd=300),sd=100) + (0.25*rt(dat.size,df=30))), digits = digits)
  data2 <- round(runif(dat.size, 65, 150), digits = digits)
  data3 <- sample(c("lower", "upper"), size = 1)
  data4 <- sample(c(68, 95, 99.7), size = 1)
  corr.ans <- ifelse((data3 == "lower"), (data1 - data2*if(data4 == 68){1}
                                                       else{if(data4 == 95){2}
                                                            else{3}}),
                     (data1 + data2*if(data4 == 68){1}
                                    else{if(data4 == 95){2}
                                         else{3}}))
  up.min <- corr.ans + data2/3
  down.max <- corr.ans - data2/3
  ans.txt <- sample(if(corr.ans == data1 - 3*data2){runif(answers, up.min, (data1 + 4*data2))}
                    else{if(corr.ans == (data1 + 3*data2)){runif(answers, (data1 - 4*data2), down.max)}
                      else{c(runif(ceiling(answers/2), (data1 - 4*data2), down.max),
                             runif(ceiling(answers/2), up.min, (data1 + 4*data2)))}},
                    size = answers, replace = F)
  content <- c(type, ID, ID, paste(quest.txt1, data1, quest.txt2,
                                   data2, quest.txt3, data3, quest.txt4,
                                   data4, quest.txt5, collapse = "", sep= ""),
               points.per.q, difficulty, points, hint, feedback)
  options <- c(rep("",6), round(ans.txt, digits = digits), rep("",2))
  options[corr.ind] <- round(corr.ans, digits = digits)
  questions[(1+(8+answers)*i):((8+answers)*(i+1)),1] <- param
  questions[(1+(8+answers)*i):((8+answers)*(i+1)),2] <- content
  questions[(1+(8+answers)*i):((8+answers)*(i+1)),3] <- options
}
questions <- questions[(9+answers):((8+answers)*(n+1)),]
write.table(questions, sep=",", file=paste(title, ".csv", sep = ""),
            row.names=F, col.names=F)

##### VarTypeMC1 #####
title <- "VarTypeMC1"
n = 200
type <- "MC"
answers <- 4
points.per.q <- 4
difficulty <- 1
quest.txt1 <- "A student wants to visualize data that is "
quest.txt2 <- ". What kind of graph should she select for this purpose?"
dat.size = 
digits = 
hint <- "Focus on the variable type: categorical vs. numeric."
feedback <- "Categorical: Bar or Pie. Numeric: Stem & Leaf, Histogram, Dot, or Box."
param <- c("NewQuestion","ID","Title","QuestionText","Points","Difficulty",
           rep("Option", answers),"Hint","Feedback")
questions <- data.frame()
for(i in 1:n)
{
  ID <- paste(title, i, sep = "-")
  points <- sample(c(rep(0,answers-1),100),replace=F)
  corr.ind <- 6 + which.max(points)
  data <- sample(c("categorical", "numerical"), size = 1)
  cat.ans <- c("Bar Graph", "Pie Graph")
  cat.supp <- c("None of These", "All of These")
  num.ans <- c("Stem & Leaf Plot", "Histogram", "Dot Plot", "Box Plot")
  corr.ans <- ifelse(data == "categorical", sample(cat.ans, size = 1), sample(num.ans, size = 1))
  ans.txt <- if(data == "categorical"){sample(c(num.ans, "Ordinal", "Nominal"), size = answers)}
             else{c(cat.ans, sample(c("Interval", "Ratio"), size = answers - 3), 
                    sample(cat.supp, size = 1))}
  content <- c(type, ID, ID, paste(quest.txt1, data, quest.txt2,
                                   collapse = "", sep = ""),
               points.per.q, difficulty, points, hint, feedback)
  options <- c(rep("",6), ans.txt, rep("",2))
  options[corr.ind] <- corr.ans
  questions[(1+(8+answers)*i):((8+answers)*(i+1)),1] <- param
  questions[(1+(8+answers)*i):((8+answers)*(i+1)),2] <- content
  questions[(1+(8+answers)*i):((8+answers)*(i+1)),3] <- options
}
questions <- questions[((9+answers)):((8+answers)*(n+1)),]
write.table(questions, sep=",", file=paste(title, ".csv", sep = ""),
            row.names=F, col.names=F)

##### VarTypeMC2 #####
title <- "VarTypeMC2"
n = 200
type <- "MC"
answers <- 2
points.per.q <- 4
difficulty <- 1
quest.txt1 <- "A student decides to take a survey for her class project. She randomly surveys other students and records their "
quest.txt2 <- ". She decides to use a "
quest.txt3 <- " to visualize this data. Did she choose a correct graphic?"
dat.size = 
digits = 
hint <- "What is the variable's type: categorical or numeric?"
feedback <- "Categorical: Bar, RF table, or Pie. Numeric: Stem & Leaf, RF table, Histogram, Dot, or Box."
param <- c("NewQuestion","ID","Title","QuestionText","Points","Difficulty",
           rep("Option", answers),"Hint","Feedback")
questions <- data.frame()
for(i in 1:n)
{
  ID <- paste(title, i, sep = "-")
  points <- sample(c(rep(0,answers-1),100),replace=F)
  corr.ind <- 6 + which.max(points)
  num.data1 <- c("heights in inches", "weights in lbs", "GPAs in raw points",
                "hours spent sleeping per night", "hours spent studying per day",
                "vertical leap in inches", "age in years", "calories consumed per day",
                "current number of completed STEM courses", "years left before their anticipated graduation-date")
  cat.data1 <- c("heights in ranges (5.0-5.5 ft, 5.5-6 ft, etc)", "weights in ranges (100-125 lbs, 125-150 lbs, etc.)",
                "racial demographics", "countries of origin", "emotional states (sad, happy, etc.)",
                "tax income brackets", "marital statuses", "living arrangements (dorm, off-campus apartment, etc.)",
                "nationalities", "native language (English, Spanish, etc.)")
  data1 <- sample(c(num.data1, cat.data1), size = 1)
  num.data2 <- c("relative frequency table", "stem & leaf plot", "histogram",
                       "dotplot", "boxplot")
  cat.data2 <- c("relative frequency table","bar graph", "pie graph")
  data2 <- sample(c(num.data2, cat.data2), size = 1)
  corr.ans <- if(((data1 %in% cat.data1) & (data2 %in% cat.data2)) |
                 ((data1 %in% num.data1) & (data2 %in% num.data2))){"Yes"}
              else{"No"}
  ans.txt <- rep(if(corr.ans == "Yes"){"No"}else{"Yes"}, 2)
  content <- c(type, ID, ID, paste(quest.txt1, data1, quest.txt2, data2, quest.txt3,
                                   collapse = "", sep = ""),
               points.per.q, difficulty, points, hint, feedback)
  options <- c(rep("",6), ans.txt, rep("",2))
  options[corr.ind] <- corr.ans
  questions[(1+(8+answers)*i):((8+answers)*(i+1)),1] <- param
  questions[(1+(8+answers)*i):((8+answers)*(i+1)),2] <- content
  questions[(1+(8+answers)*i):((8+answers)*(i+1)),3] <- options
}
questions <- questions[((9+answers)):((8+answers)*(n+1)),]
write.table(questions, sep=",", file=paste(title, ".csv", sep = ""),
            row.names=F, col.names=F)

set.seed(11452)
##### SkewGraphMC1 #####
title <- "SkewGraphMC1"
n = 200
type <- "MC"
answers <- 4
points.per.q <- 4
difficulty <- 1
quest.txt <- "While analyzing a dataset, a researcher plots a histogram of one of her variables. The histogram she makes is depicted. What type of skewness, if any, is present in the variable's distribution?"
dat.size = 10000
digits = 3
hint <- "Focus on the tails."
feedback <- "Left Skew: Left tail. Right Skew: Right tail. No Skew: Symmetric tails."
param <- c("NewQuestion","ID","Title","QuestionText","Points","Difficulty", "Image",
           rep("Option", answers),"Hint","Feedback")
questions <- data.frame()
for(i in 1:n)
{
  ID <- paste(title, i, sep = "-")
  points <- sample(c(rep(0,answers-1),100),replace=F)
  corr.ind <- 7 + which.max(points)
  left.shape1 <- runif(1,40,50)
  left.shape2 <- runif(1,1,10)
  left.data <- rbeta(dat.size, left.shape1, left.shape2)
  right.shape1 <- runif(1,1,10)
  right.shape2 <- runif(1,40,50)
  right.data <- rbeta(dat.size, right.shape1, right.shape2)
  sym.data <- rnorm(dat.size, 0, 1)
  data.dec <- sample(c(1,2,3), size = 1)
  data <- if(data.dec == 1){sym.data}else{if(data.dec == 2){left.data}else{right.data}}
  corr.ans <- if(identical(data, sym.data)){"No Skew"}else{if(identical(data, right.data)){"Right Skew"}else{"Left Skew"}}
  ans.txt <- if(corr.ans == "No Skew"){c(sample(c("Right Skew", "Left Skew", "Curve Skew", "Histographic Skew"), size = answers-1),
                                              sample(c("All of These", "None of These"), size = 1))}
             else{if(corr.ans == "Left Skew"){c(sample(c("Right Skew", "No Skew", "Curve Skew", "Histographic Skew"), size = answers-1),
                                                sample(c("All of These", "None of These"), size = 1))}
                  else{c(sample(c("No Skew", "Left Skew", "Curve Skew", "Histographic Skew"), size = answers-1),
                         sample(c("All of These", "None of These"), size = 1))}}
  content <- c(type, ID, ID, quest.txt, points.per.q, difficulty,
               paste("Images/", paste(title, i, sep = "-"), ".jpeg", sep = ""),
               points, hint, feedback)
  options <- c(rep("",7), ans.txt, rep("",2))
  options[corr.ind] <- corr.ans
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),1] <- param
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),2] <- content
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),3] <- options
  jpeg(filename=paste(paste(title, i, sep = "-"), ".jpeg", sep = ""))
  hist(data, xlim=c(min(data),max(data)), probability=T, 
       col='lightblue', xlab=' ', ylab=' ', axes=F,
       main = "Researcher's Histogram")
  dev.off()
}
questions <- questions[((10+answers)):((9+answers)*(n+1)),]
write.table(questions, sep=",", file=paste(title, ".csv", sep = ""),
            row.names=F, col.names=F)

##### StemGraphMC1 #####
library(graphics)
library(fmsb)
title <- "StemGraphMC1"
n = 200
type <- "MC"
answers <- 5
points.per.q <- 4
difficulty <- 1
quest.txt1 <- "While analyzing a dataset, a researcher makes a stem and leaf plot of one of her variables. The stem and leaf plot she makes is depicted. What is the "
quest.txt2 <- " value?"
dat.size = 41
digits = 0
hint <- "Pay close attention to the key given for the plot."
feedback <- "Stem and leaf plots are like histograms flipped over. The min is at the top, the middle halfway down, and the max is at the bottom."
param <- c("NewQuestion","ID","Title","QuestionText","Points","Difficulty", "Image",
           rep("Option", answers),"Hint","Feedback")
questions <- data.frame()
for(i in 1:n)
{
  ID <- paste(title, i, sep = "-")
  points <- sample(c(rep(0,answers-1),100),replace=F)
  corr.ind <- 7 + which.max(points)
  left.data <- c(sample(seq(100, 199, by = 10^(-digits)), size = ceiling(dat.size/5)),
                 sample(seq(200, 299, by = 10^(-digits)), size = ceiling(2*dat.size/5)),
                 sample(seq(300, 400, by = 10^(-digits)), size = ceiling(3*dat.size/5)))
  right.data <- c(sample(seq(300, 400, by = 10^(-digits)), size = ceiling(dat.size/5)),
                  sample(seq(200, 299, by = 10^(-digits)), size = ceiling(2*dat.size/5)),
                  sample(seq(100, 199, by = 10^(-digits)), size = ceiling(3*dat.size/5)))
  sym.data <- c(sample(seq(300, 400, by = 10^(-digits)), size = ceiling(dat.size/5)),
                sample(seq(200, 299, by = 10^(-digits)), size = ceiling(3*dat.size/5)),
                sample(seq(100, 199, by = 10^(-digits)), size = ceiling(dat.size/5)))
  data.dec <- sample(c(1,2,3), size = 1)
  data <- sample(round(if(data.dec == 1){sym.data}
                          else{if(data.dec == 2){left.data}
                               else{right.data}},
                       digits = digits),
                 size = dat.size)
  data1 <- sample(c("smallest", "second smallest", "third smallest", "median",
                    "third largest", "second largest", "largest"), size = 1)
  corr.ans <- if(data1 == "smallest"){min(data)}
              else{if(data1 == "second smallest"){sort(data)[2]}
                   else{if(data1 == "third smallest"){sort(data)[3]}
                        else{if(data1 == "median"){sort(data)[(dat.size+1)/2]}
                             else{if(data1 == "third largest"){sort(data)[dat.size-2]}
                                  else{if(data1 == "second largest"){sort(data)[dat.size-1]}
                                       else{if(data1 == "largest"){max(data)}}}}}}}
  ans.txt <- if(corr.ans == min(data)){sample(sort(data)[-1], size = answers)}
             else{if(corr.ans == sort(data)[2]){sample(sort(data)[-2], size = answers)}
                  else{if(corr.ans == sort(data)[3]){sample(sort(data)[-3], size = answers)}
                       else{if(corr.ans == sort(data)[(dat.size+1)/2]){sample(sort(data)[-((dat.size+1)/2)], size = answers)}
                            else{if(corr.ans == sort(data)[dat.size-2]){sample(sort(data)[-(dat.size-2)], size = answers)}
                                 else{if(corr.ans == sort(data)[dat.size-1]){sample(sort(data)[-(dat.size-1)], size = answers)}
                                      else{if(corr.ans == max(data)){sample(sort(data)[-dat.size], size = answers)}}}}}}}
  content <- c(type, ID, ID, paste(quest.txt1, data1, quest.txt2, sep = ""),
               points.per.q, difficulty, paste("Images/", paste(title, i, sep = "-"), ".jpeg", sep = ""),
               points, hint, feedback)
  options <- c(rep("",7), ans.txt, rep("",2))
  options[corr.ind] <- corr.ans
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),1] <- param
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),2] <- content
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),3] <- options
  jpeg(filename=paste(paste(title, i, sep = "-"), ".jpeg", sep = ""))
  gstem(data, 3)
  dev.off()
}
questions <- questions[((10+answers)):((9+answers)*(n+1)),]
write.table(questions, sep=",", file=paste(title, ".csv", sep = ""),
            row.names=F, col.names=F)

##### RFTableMC1 #####
library(graphics)
library(gridExtra)
title <- "RFTableMC1"
n = 200
type <- "MC"
answers <- 5
points.per.q <- 4
difficulty <- 1
quest.txt1 <- "For her class project in statistics, a student researcher decides to survey other students and track the number of ramen noodle eaters she can find in various majors. She decides to summarize her findings in the depicted (incomplete) relative frequency table. What is the relative frequency of ramen noodle eaters who major in "
quest.txt2 <- "?"
majors <- c("Physics", "Statistics", "Speech Pathology", "Psychology", "Music", "Philosophy",
            "Mathematics", "Fine Arts", "Chemistry", "Sociology", "Education", "Business",
            "Accounting", "Geology", "Occupational Therapy", "Nursing", "Medicine",
            "Electrical Engineering", "Mechanical Engineering", "Civil Engineering",
            "English", "Spanish", "French", "Arabic", "Economics", "Applied Mathematics")
dat.size = 5:15
digits = 2
hint <- "Read the Data Presentation chapter of your coursepack."
feedback <- "For the specified category, calculate frequency over total and multiply by 100%."
param <- c("NewQuestion","ID","Title","QuestionText","Points","Difficulty", "Image",
           rep("Option", answers),"Hint","Feedback")
questions <- data.frame()
for(i in 1:n)
{
  ID <- paste(title, i, sep = "-")
  points <- sample(c(rep(0,answers-1),100),replace=F)
  corr.ind <- 7 + which.max(points)
  dat.size1 <- sample(dat.size, size = 1)
  Majors <- sample(majors, size = dat.size1)
  data1 <- sample(Majors, size = 1)
  RamenEaters <- sample(seq(100, 400, by = 3), size = dat.size1)
  RF.calc <- round(RamenEaters/sum(RamenEaters)*100, digits = digits)
  RF <- paste(RF.calc, "%", sep = "")
  data.calc <- data.frame(Majors, RF.calc)
  data <- data.frame(Majors, RamenEaters, RF, stringsAsFactors = FALSE)
  corr.ans <- RF[Majors == data1]
  data[Majors == data1, 3] <- NA
  data <- rbind(data, c("Total", sum(RamenEaters), "100%"))
  ans.txt <- c(sample(c(paste(seq((RF.calc[Majors == data1] + 1), 40, by = 10^-digits), "%", sep = ""),
                        paste(seq(0, (RF.calc[Majors == data1] - 1), by = 10^-digits), "%", sep = "")),
                      size = answers-1),
               sample(c("None of These", "All of These"), size = 1))
  content <- c(type, ID, ID, paste(quest.txt1, data1, quest.txt2, sep = ""),
               points.per.q, difficulty, paste("Images/", paste(title, i, sep = "-"), ".jpeg", sep = ""),
               points, hint, feedback)
  options <- c(rep("",7), ans.txt, rep("",2))
  options[corr.ind] <- corr.ans
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),1] <- param
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),2] <- content
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),3] <- options
  jpeg(filename=paste(paste(title, i, sep = "-"), ".jpeg", sep = ""),
       height = 25*nrow(data), width = 120*ncol(data))
  p <- tableGrob(data)
  grid.arrange(p)
  dev.off()
}
questions <- questions[((10+answers)):((9+answers)*(n+1)),]
write.table(questions, sep=",", file=paste(title, ".csv", sep = ""),
            row.names=F, col.names=F)

##### BoxMC1 #####
library(graphics)
title <- "BoxMC1"
n = 200
type <- "MC"
answers <- 5
points.per.q <- 4
difficulty <- 1
quest.txt1 <- "While analyzing a dataset, a researcher makes a boxplot of one of her variables. The boxplot is depicted above. What is the value of the "
quest.txt2 <- "?"
dat.size = 41
digits = 0
hint <- "You can read about boxplots in the data presentation chapter of your coursepack."
feedback <- "From lowest to highest, boxplots have lines at the minimum, Q1, Q2 (median), Q3, and the max."
param <- c("NewQuestion","ID","Title","QuestionText","Points","Difficulty", "Image",
           rep("Option", answers),"Hint","Feedback")
questions <- data.frame()
for(i in 1:n)
{
  ID <- paste(title, i, sep = "-")
  points <- sample(c(rep(0,answers-1),100),replace=F)
  corr.ind <- 7 + which.max(points)
  left.data <- c(sample(seq(100, 199, by = 10^(-digits)), size = ceiling(dat.size/6)),
                 sample(seq(200, 299, by = 10^(-digits)), size = ceiling(2*dat.size/6)),
                 sample(seq(300, 400, by = 10^(-digits)), size = ceiling(3*dat.size/6)))
  right.data <- c(sample(seq(300, 400, by = 10^(-digits)), size = ceiling(dat.size/6)),
                  sample(seq(200, 299, by = 10^(-digits)), size = ceiling(2*dat.size/6)),
                  sample(seq(100, 199, by = 10^(-digits)), size = ceiling(3*dat.size/6)))
  sym.data <- c(sample(seq(300, 400, by = 10^(-digits)), size = ceiling(1.5*dat.size/6)),
                sample(seq(200, 299, by = 10^(-digits)), size = ceiling(3*dat.size/6)),
                sample(seq(100, 199, by = 10^(-digits)), size = ceiling(1.5*dat.size/6)))
  data.dec <- sample(c(1,2,3), size = 1)
  data <- sample(round(if(data.dec == 1){sym.data}
                       else{if(data.dec == 2){left.data}
                         else{right.data}},
                       digits = digits),
                 size = dat.size)
  data1 <- sample(c("minimum", "first quartile", "second quartile", "third quartile",
                    "maximum"), size = 1)
  corr.ans <- if(data1 == "minimum"){min(data)}
  else{if(data1 == "first quartile"){fivenum(data)[2]}
    else{if(data1 == "second quartile"){fivenum(data)[3]}
      else{if(data1 == "third quartile"){fivenum(data)[4]}
        else{if(data1 == "maximum"){max(data)}}}}}
  ans.txt <- if(corr.ans == min(data)){sample(sort(data)[4:dat.size], size = answers)}
  else{if(corr.ans == fivenum(data)[2]){sample(c(sort(data)[1:floor(dat.size/8)], sort(data)[ceiling(3*dat.size/8):dat.size]), size = answers)}
    else{if(corr.ans == fivenum(data)[3]){sample(c(sort(data)[1:floor(3*dat.size/8)], sort(data)[ceiling(5*dat.size/8):dat.size]), size = answers)}
      else{if(corr.ans == fivenum(data)[4]){sample(c(sort(data)[1:floor(5*dat.size/8)], sort(data)[ceiling(7*dat.size/8):dat.size]), size = answers)}
        else{if(corr.ans == max(data)){sample(sort(data)[1:(dat.size-3)], size = answers)}}}}}
  content <- c(type, ID, ID, paste(quest.txt1, data1, quest.txt2, sep = ""),
               points.per.q, difficulty, paste("Images/", paste(title, i, sep = "-"), ".jpeg", sep = ""),
               points, hint, feedback)
  options <- c(rep("",7), ans.txt, rep("",2))
  options[corr.ind] <- corr.ans
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),1] <- param
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),2] <- content
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),3] <- options
  jpeg(filename=paste(paste(title, i, sep = "-"), ".jpeg", sep = ""))
  boxplot(data, horizontal = T, col = 'lightblue')
  title("Researcher's Boxplot")
  dev.off()
}
questions <- questions[((10+answers)):((9+answers)*(n+1)),]
write.table(questions, sep=",", file=paste(title, ".csv", sep = ""),
            row.names=F, col.names=F)

##### QuartMC1 #####
title <- "QuartMC1"
n = 200
type <- "MC"
answers <- 4
points.per.q <- 4
difficulty <- 1
quest.txt1 <- "What is the "
quest.txt2 <- " of the following dataset?    "
digits = 1
dat.size <- 11:15
hint <- "Sort the data first."
feedback <- "Sort the data, then find either the .25(n + 1)st number (Q1) or the .75(n + 1)st number (Q3)."
param <- c("NewQuestion","ID","Title","QuestionText","Points","Difficulty",
           rep("Option", answers),"Hint","Feedback")
questions <- data.frame()
for(i in 1:n)
{
  ID <- paste(title, i, sep = "-")
  dat.size1 = sample(dat.size, size = 1)
  points <- sample(c(rep(0,answers-1),100),replace=F)
  corr.ind <- 6 + which.max(points)
  data1 <- sample(c("first quartile", "third quartile"), size = 1)
  data <- round(rnorm(dat.size1,mean=rnorm(1,mean=900,sd=400),sd=200) + (0.5*rt(dat.size1,df=30)), digits = digits)
  corr.ans <- if(data1 == "first quartile"){round(mean(c(sort(data)[ceiling((dat.size1+1)/4)],
                                                         sort(data)[floor((dat.size1+1)/4)])),
                                   digits = digits)}
              else{round(mean(c(sort(data)[ceiling(3*(dat.size1+1)/4)],
                                sort(data)[floor(3*(dat.size1+1)/4)])),
                         digits = digits)}
  up.min <- corr.ans + sd(data)/8
  down.max <- corr.ans - sd(data)/8
  ans.txt <- round(sample(c(median(data), mean(data),
                            if(data1 == "first quartile")
                              {if((dat.size1 %% 2) == 0){c(sort(data)[ceiling((dat.size1+1)/4)],
                                                           sort(data)[floor((dat.size1+1)/4)])}
                               else{c(sort(data)[1+(dat.size1+1)/4], sort(data)[(dat.size1+1)/4-1])}},
                            if(data1 == "third quartile")
                              {if((dat.size1 %% 2) == 0){c(sort(data)[ceiling(3*(dat.size1+1)/4)],
                                                           sort(data)[floor(3*(dat.size1+1)/4)])}
                               else{c(sort(data)[1+3*(dat.size1+1)/4], sort(data)[3*(dat.size1+1)/4-1])}},
                            runif(ceiling(answers/4), corr.ans - 3*sd(data), down.max),
                            runif(ceiling(answers/4), up.min, corr.ans + 3*sd(data))),
                          size = answers),
                   digits = digits)
  content <- c(type, ID, ID, paste(quest.txt1, data1, quest.txt2,
                                   paste(as.character(data), collapse=",  ",sep=""),
                                   collapse = "", sep = ""),
               points.per.q, difficulty, points, hint, feedback)
  options <- c(rep("",6), ans.txt, rep("",2))
  options[corr.ind] <- corr.ans
  questions[(1+(8+answers)*i):((8+answers)*(i+1)),1] <- param
  questions[(1+(8+answers)*i):((8+answers)*(i+1)),2] <- content
  questions[(1+(8+answers)*i):((8+answers)*(i+1)),3] <- options
}
questions <- questions[(9+answers):((8+answers)*(n+1)),]
write.table(questions, sep=",", file=paste(title, ".csv", sep = ""),
            row.names=F, col.names=F)

##### SDMC2 #####
library(graphics)
library(gridExtra)
title <- "SDMC2"
n = 200
type <- "MC"
answers <- 2
points.per.q <- 4
difficulty <- 1
quest.txt1 <- "Ian and his friend Neil like playing the video game Mario Kart together. After racing together many times, they hypothesize that Neil "
quest.txt2 <- " race times than Ian. To test their hypothesis, they monitor 50 consecutive races and record the above means and standard deviations of their race times (in minutes). Do these summary statistics confirm their hypothesis?"
Names <- c("Ian", "Neil")
digits = 2
hint <- "Remember that the mean and SD each measure something very different."
feedback <- "Means measure center, and SDs measure spread. So, a higher mean = longer race times, a higher SD = less consistent or more variable race times, and vice versa for both mean and SD."
param <- c("NewQuestion","ID","Title","QuestionText","Points","Difficulty", "Image",
           rep("Option", answers),"Hint","Feedback")
questions <- data.frame()
for(i in 1:n)
{
  ID <- paste(title, i, sep = "-")
  points <- sample(c(rep(0,answers-1),100),replace=F)
  corr.ind <- 7 + which.max(points)
  data1 <- sample(c("has more consistent (less variable)", "has less consistent (more variable)"), size = 1)
  Means <- sample(seq(1, 5, by = .01), size = 2)
  SDs <- sample(seq(.5, 2, by = .01), size = 2)
  corr.ans <- if((data1 == "has more consistent (less variable)"))
                {
                if(SDs[1] > SDs[2]){"Yes"}else{"No"}
                }
              else
                {
                if(data1 == "has less consistent (more variable)")
                  {
                  if(SDs[2] > SDs[1]){"Yes"}else{"No"}
                  }
                }
  data <- data.frame(Names, Means, SDs, stringsAsFactors = FALSE)
  ans.txt <- if(corr.ans == "Yes"){rep("No", 2)}else{rep("Yes", 2)}
  content <- c(type, ID, ID, paste(quest.txt1, data1, quest.txt2, sep = ""),
               points.per.q, difficulty, paste("Images/", paste(title, i, sep = "-"), ".jpeg", sep = ""),
               points, hint, feedback)
  options <- c(rep("",7), ans.txt, rep("",2))
  options[corr.ind] <- corr.ans
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),1] <- param
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),2] <- content
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),3] <- options
  jpeg(filename=paste(paste(title, i, sep = "-"), ".jpeg", sep = ""),
       height = 30*nrow(data), width = 55*ncol(data))
  p <- tableGrob(data)
  grid.arrange(p)
  dev.off()
}
questions <- questions[((10+answers)):((9+answers)*(n+1)),]
write.table(questions, sep=",", file=paste(title, ".csv", sep = ""),
            row.names=F, col.names=F)

##### MeanMC2 #####
library(graphics)
library(gridExtra)
title <- "MeanMC2"
n = 200
type <- "MC"
answers <- 2
points.per.q <- 4
difficulty <- 1
quest.txt1 <- "Ian and his friend Neil like playing the video game Mario Kart together. After racing together many times, they hypothesize that Neil averages "
quest.txt2 <- " race times than Ian. To test their hypothesis, they monitor 50 consecutive races and record the above means and standard deviations of their race times (in minutes). Do these summary statistics help affirm their hypothesis?"
Names <- c("Ian", "Neil")
digits = 2
hint <- "Remember that the mean and SD each measure something very different."
feedback <- "Means measure center, and SDs measure spread. So, a higher mean = longer race times, a higher SD = less consistent or more variable race times, and vice versa for both mean and SD."
param <- c("NewQuestion","ID","Title","QuestionText","Points","Difficulty", "Image",
           rep("Option", answers),"Hint","Feedback")
questions <- data.frame()
for(i in 1:n)
{
  ID <- paste(title, i, sep = "-")
  points <- sample(c(rep(0,answers-1),100),replace=F)
  corr.ind <- 7 + which.max(points)
  data1 <- sample(c("faster", "slower"), size = 1)
  Means <- sample(seq(1, 5, by = .01), size = 2)
  SDs <- sample(seq(.5, 2, by = .01), size = 2)
  corr.ans <- if((data1 == "faster"))
  {
    if(Means[1] > Means[2]){"Yes"}else{"No"}
  }
  else
  {
    if(data1 == "slower")
    {
      if(Means[2] > Means[1]){"Yes"}else{"No"}
    }
  }
  data <- data.frame(Names, Means, SDs, stringsAsFactors = FALSE)
  ans.txt <- if(corr.ans == "Yes"){rep("No", 2)}else{rep("Yes", 2)}
  content <- c(type, ID, ID, paste(quest.txt1, data1, quest.txt2, sep = ""),
               points.per.q, difficulty, paste("Images/", paste(title, i, sep = "-"), ".jpeg", sep = ""),
               points, hint, feedback)
  options <- c(rep("",7), ans.txt, rep("",2))
  options[corr.ind] <- corr.ans
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),1] <- param
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),2] <- content
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),3] <- options
  jpeg(filename=paste(paste(title, i, sep = "-"), ".jpeg", sep = ""),
       height = 30*nrow(data), width = 55*ncol(data))
  p <- tableGrob(data)
  grid.arrange(p)
  dev.off()
}
questions <- questions[((10+answers)):((9+answers)*(n+1)),]
write.table(questions, sep=",", file=paste(title, ".csv", sep = ""),
            row.names=F, col.names=F)