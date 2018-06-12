##### I currently plan to create a bank of 5000 random questions per test,      #####
##### which requires 25 distinct generators per test as they appear below. So,  #####
##### 75 total genrators for a semester of three tests. Once finished with      #####
##### individual generators, I will begin function wrapping, creating either a  #####
##### single function with many adaptive arguments, or a suite of functions.    #####

##### These will help streamline test writing, requiring only that one runs the #####
##### code and occasionally re-uploads the test banks. The longest part of the  #####
##### process is waiting on the e-learning uploads of the images and CSVs.      #####                                                               #####

##### 25/25 Generators for test 1 completed  #####
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
quest.txt <- "What is the mean of the following dataset?  Data: "
dat.size = 5
digits = 1
loc.path <- 
e.path <- 
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
up.min <- round(corr.ans + sd(data)/8, digits = digits)
down.max <- round(corr.ans - sd(data)/8, digits = digits)
ans.txt <- round(sample(c(sum(data), sum(data)/(dat.size-1),
                          seq(corr.ans - 3*sd(data), down.max, 10^-digits),
                          seq(up.min, corr.ans + 3*sd(data), 10^-digits)),
                        size = answers),
                 digits = digits)
content <- c(type, ID, ID, paste(quest.txt, paste(as.character(data),
                                                  collapse=",  ",sep=""),
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

##### MeanMC2 #####
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
loc.path <- "Images/"
e.path <- "Images/"
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
               points.per.q, difficulty, paste(e.path, paste(title, i, sep = "-"), ".jpeg", sep = ""),
               points, hint, feedback)
  options <- c(rep("",7), ans.txt, rep("",2))
  options[corr.ind] <- corr.ans
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),1] <- param
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),2] <- content
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),3] <- options
  jpeg(filename=paste(loc.path, paste(title, i, sep = "-"), ".jpeg", sep = ""),
       height = 30*nrow(data), width = 55*ncol(data))
  p <- tableGrob(data)
  grid.arrange(p)
  dev.off()
}
questions <- questions[((10+answers)):((9+answers)*(n+1)),]
write.table(questions, sep=",", file=paste(title, ".csv", sep = ""),
            row.names=F, col.names=F)

##### MeanMC3 #####
title <- "MeanMC3"
n = 200
type <- "MC"
answers <- 4
points.per.q <- 4
difficulty <- 1
quest.txt1 <- "While doing some data entry, you realize that you've made a big mistake. Every data value in your mistaken dataset is "
quest.txt2 <- " than it should have been. The mean of the mistaken dataset was "
quest.txt3 <- ". You correct the error and recalculate the mean. What is the new corrected mean?  Mistaken Data:  "
dat.size = c(10:20)
digits = 1
loc.path <- 
  e.path <- 
  hint <- "There is a shortcut here."
feedback <- "In the case of multiplication, the new mean will be x times the old mean, where x changes depending on the context of the question. In the case of addition or subtraction, the new mean will be the old mean plus or minus x."
param <- c("NewQuestion","ID","Title","QuestionText","Points","Difficulty",
           rep("Option", answers),"Hint","Feedback")
questions <- data.frame()
for(i in 1:n)
{
  ID <- paste(title, i, sep = "-")
  points <- sample(c(rep(0,answers-1),100),replace=F)
  corr.ind <- 6 + which.max(points)
  dat.size1 <- sample(dat.size, size = 1)
  data <- round(rnorm(dat.size1,mean=rnorm(1,mean=900,sd=400),sd=200) + (0.5*rt(dat.size1,df=30)), digits = digits)
  data1 <- sample(c("ten times smaller", "ten times larger", "five times smaller", "five times larger",
                    "ten points less", "ten points greater", "five points less", "five points greater"),
                  size = 1)
  data2 <- round(mean(data), digits = digits)
  scale <- if(data1 == "ten times smaller"){10}
  else{if(data1 == "ten times larger"){.1}
    else{if(data1 == "five times smaller"){5}
      else{{.2}}}}
  constant <- if(data1 == "ten points less"){10}
  else{if(data1 == "ten points greater"){-10}
    else{if(data1 == "five points less"){5}
      else{if(data1 == "five points greater"){-5}}}}
  corr.ans <- if(data1 == "ten times smaller" | data1 == "ten times larger" | 
                 data1 == "five times smaller" | data1 == "five times larger")
  {round(scale*data2, digits = digits)}
  else{round(data2 + constant, digits = digits)}
  up.min <- round(corr.ans + sd(data)/8, digits = digits)
  down.max <- round(corr.ans - sd(data)/8, digits = digits)
  ans.txt <- round(sample(c(if(data1 == "ten times smaller" | data1 == "ten times larger" |
                               data1 == "five times smaller" | data1 == "five times larger")
  {c(sum(scale*data), sum(scale*data)/(dat.size1-1), data2)}
  else{c(sum(data + constant), sum(data + constant)/(dat.size1-1), data2)},
  seq(corr.ans - 2*sd(data), down.max, 10^-digits),
  seq(up.min, corr.ans + 2*sd(data), 10^-digits)),
  size = answers),
  digits = digits)
  content <- c(type, ID, ID, paste(quest.txt1, data1, quest.txt2, data2, quest.txt3,
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

##### MedianMC1 #####
title <- "MedianMC1"
n = 200
type <- "MC"
answers <- 4
points.per.q <- 4
difficulty <- 1
quest.txt <- "What is the median of the following dataset?  Data: "
digits = 1
dat.size <- c(5,6)
loc.path <- 
e.path <- 
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
  up.min <- round(corr.ans + sd(data)/8, digits = digits)
  down.max <- round(corr.ans - sd(data)/8, digits = digits)
  ans.txt <- round(sample(c(if((dat.size1 %% 2) == 0){sort(data)[ceiling((dat.size1+1)/2)]}else{sort(data)[1+(dat.size1+1)/2]},
                            if((dat.size1 %% 2) == 0){sort(data)[floor((dat.size1+1)/2)]}else{sort(data)[(dat.size1+1)/2-1]},
                            seq(corr.ans - 2*sd(data), down.max, 10^-digits),
                            seq(up.min, corr.ans + 2*sd(data), 10^-digits)),
                          size = answers),
                   digits = digits)
  content <- c(type, ID, ID, paste(quest.txt, paste(as.character(data),
                                                    collapse=",  ",sep=""),
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
quest.txt <- "What is the standard deviation of the following dataset?  Data: "
dat.size = 5
digits = 1
loc.path <- 
e.path <- 
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
  up.min <- round(corr.ans + sd(data)/8, digits = digits)
  down.max <- round(corr.ans - sd(data)/8, digits = digits)
  ans.txt <- round(sample(c(sd(data)^2, sqrt(sd(data)^2*(dat.size-1)), sqrt((data-mean(data))^2/dat.size),
                            seq(corr.ans - 3*sd(data), down.max, 10^-digits),
                            seq(up.min, corr.ans + 3*sd(data), 10^-digits)),
                          size = answers),
                   digits = digits)
  content <- c(type, ID, ID, paste(quest.txt, paste(as.character(data),
                                                    collapse=",  ",sep=""),
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

##### SDMC2 #####
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
loc.path <- "Images/"
e.path <- "Images/"
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
               points.per.q, difficulty, paste(e.path, paste(title, i, sep = "-"), ".jpeg", sep = ""),
               points, hint, feedback)
  options <- c(rep("",7), ans.txt, rep("",2))
  options[corr.ind] <- corr.ans
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),1] <- param
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),2] <- content
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),3] <- options
  jpeg(filename=paste(loc.path, paste(title, i, sep = "-"), ".jpeg", sep = ""),
       height = 30*nrow(data), width = 55*ncol(data))
  p <- tableGrob(data)
  grid.arrange(p)
  dev.off()
}
questions <- questions[((10+answers)):((9+answers)*(n+1)),]
write.table(questions, sep=",", file=paste(title, ".csv", sep = ""),
            row.names=F, col.names=F)

##### SDMC3 #####
title <- "SDMC3"
n = 200
type <- "MC"
answers <- 4
points.per.q <- 4
difficulty <- 1
quest.txt1 <- "While doing some data entry, you realize that you've made a big mistake. Every data value in your mistaken dataset is "
quest.txt2 <- " than it should have been. The standard deviation of the mistaken dataset was "
quest.txt3 <- ". You correct the error and recalculate the standard deviation. What is the new corrected standard deviation?  Mistaken Data:  "
dat.size = c(10:20)
digits = 1
loc.path <- 
  e.path <- 
  hint <- "There are shortcuts here."
feedback <- "In the case of multiplication, the new SD will be x times the old SD, where x changes depending on the context of the question. In the case of addition or substraction, the SD will not change."
param <- c("NewQuestion","ID","Title","QuestionText","Points","Difficulty",
           rep("Option", answers),"Hint","Feedback")
questions <- data.frame()
for(i in 1:n)
{
  ID <- paste(title, i, sep = "-")
  points <- sample(c(rep(0,answers-1),100),replace=F)
  corr.ind <- 6 + which.max(points)
  dat.size1 <- sample(dat.size, size = 1)
  data <- round(rnorm(dat.size1,mean=rnorm(1,mean=900,sd=400),sd=200) + (0.5*rt(dat.size1,df=30)), digits = digits)
  data1 <- sample(c("ten times smaller", "ten times larger", "five times smaller", "five times larger",
                    "ten points less", "ten points greater", "five points less", "five points greater"),
                  size = 1)
  data2 <- round(sd(data), digits = digits)
  scale <- if(data1 == "ten times smaller"){10}
  else{if(data1 == "ten times larger"){.1}
    else{if(data1 == "five times smaller"){5}
      else{if(data1 == "five times larger"){.2}}}}
  corr.ans <- if(data1 == "ten times smaller" | data1 == "ten times larger" |
                 data1 == "five times smaller" | data1 == "five times larger")
  {round(scale*data2, digits = digits)}
  else{data2}
  up.min <- round(corr.ans + sd(data)/8, digits = digits)
  down.max <- round(corr.ans - sd(data)/8, digits = digits)
  ans.txt <- round(sample(c(if(corr.ans == data2){c(sd(data)^2, sqrt(sd(data)^2*(dat.size1-1)),
                                                    -data2, sqrt((data-mean(data))^2/dat.size1))}
                            else{c(sd(scale*data)^2, sqrt(sd(scale*data)^2*(dat.size1-1)),
                                   -sd(scale*data), sqrt((scale*data-mean(scale*data))^2/dat.size1))},
                            seq(corr.ans - 2*sd(data), down.max, 10^-digits),
                            seq(up.min, corr.ans + 2*sd(data), 10^-digits)),
                          size = answers),
                   digits = digits)
  content <- c(type, ID, ID, paste(quest.txt1, data1, quest.txt2, data2, quest.txt3,
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
loc.path <- 
e.path <- 
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
  up.min <- round(corr.ans + .05, digits)
  down.max <- round(corr.ans - .05, digits)
  ans.txt <- sample(if(corr.ans <= .05){seq(up.min, 1.05, 10^-digits)}
                    else{if(corr.ans >= .95){seq(-0.05, down.max, 10^-digits)}
                      else{c(seq(-0.05, down.max, 10^-digits),
                             seq(up.min, 1.05, 10^-digits))}},
                    size = answers)
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
loc.path <- 
e.path <- 
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
  up.min <- round(corr.ans + .05, digits = digits)
  down.max <- round(corr.ans - .05, digits = digits)
  ans.txt <- sample(if(corr.ans < .05){seq(up.min, 1.05, 10^-digits)}
                    else{if(corr.ans > .975){seq(-0.05, down.max, 10^-digits)}
                         else{c(seq(-0.05, down.max, 10^-digits),
                                seq(up.min, 1.05, 10^-digits))}},
                    size = answers)
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
loc.path <- 
e.path <- 
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
  up.min <- round(corr.ans + data2/3, digits = digits)
  down.max <- round(corr.ans - data2/3, digits = digits)
  ans.txt <- sample(if(corr.ans < (data1 - 2*data2)){seq(up.min, (data1 + 4*data2), 10^-digits)}
                       else{if(corr.ans > (data1 + 2*data2)){seq((data1 - 4*data2), down.max, 10^-digits)}
                            else{c(seq((data1 - 4*data2), down.max, 10^-digits),
                                   seq(up.min, (data1 + 4*data2), 10^-digits))}},
                    size = answers)
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
loc.path <- 
e.path <- 
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
loc.path <- 
e.path <- 
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
  up.min <- round(corr.ans + data2/3, digits = digits)
  down.max <- round(corr.ans - data2/3, digits = digits)
  ans.txt <- sample(if(corr.ans == data1 - 3*data2){seq(up.min, (data1 + 3*data2), 10^-digits)}
                    else{if(corr.ans == (data1 + 3*data2)){seq((data1 - 3*data2), down.max, 10^-digits)}
                      else{c(seq((data1 - 3*data2), down.max, 10^-digits),
                             seq(up.min, (data1 + 3*data2), 10^-digits))}},
                    size = answers)
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
loc.path <- 
e.path <- 
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
num.data1 <- c("heights in inches", "weights in lbs", "GPAs in raw points",
               "hours spent sleeping per night", "hours spent studying per day",
               "vertical leap in inches", "age in years", "calories consumed per day",
               "current number of completed STEM courses", "years left before their anticipated graduation-date")
cat.data1 <- c("heights in ranges (5.0-5.5 ft, 5.5-6 ft, etc)", "weights in ranges (100-125 lbs, 125-150 lbs, etc.)",
               "racial demographics", "countries of origin", "emotional states (sad, happy, etc.)",
               "tax income brackets", "marital statuses", "living arrangements (dorm, off-campus apartment, etc.)",
               "nationalities", "native language (English, Spanish, etc.)")
dat.size = 
digits = 
loc.path <- 
e.path <- 
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

##### VarTypeMC3 #####
title <- "VarTypeMC3"
n = 200
type <- "MC"
answers <- 4
points.per.q <- 4
difficulty <- 1
quest.txt1 <- "A study's dataset includes the following variable: "
quest.txt2 <- ". What is this variable's type and level of measurement?"
rat.data1 <- c("subjects' heights in inches", "subjects' hours spent sleeping per night", "subjects' ages in years",
               "subjects' current number of completed STEM courses", "subjects' years since university admission (2, 3, etc.)")
ord.data1 <- c("subjects' heights in ranges (5.0-5.5 ft, 5.5-6 ft, etc)",
               "subjects' weights in ranges (100-125 lbs, 125-150 lbs, etc.)",
               "subjects' tax income brackets", "subjects' military ranks",
               "customers' satisfaction (not satisfied, satisfied, etc.)")
int.data1 <- c("subjects' GPAs in raw points", "subjects' shoe sizes (8, 9, etc.)", "temperatures in celsius",
               "subjects' point grades on statistics tests from different classes (71, 72, etc.)",
               "consumers' review scores of a product (0 to 100)")
nom.data1 <- c("subjects' racial demographics", "subjects' countries of origin",
               "subjects' marital statuses", "subjects' nationalities",
               "subjects' native language (English, Spanish, etc.)")
dat.size <- 
digits <- 
loc.path <- 
e.path <- 
hint <- "Try drawing out a tree of all the possible answers. You might be able to eliminate some."
feedback <- "If it's just a label, it's categorical and nominal. If it's a label that can be ordered, it's categorical and ordinal. If it has a number-based measurement scale where just differences are meaningful, it's numeric and interval. If the scale has a true zero and ratios are meaningful, it's numeric and ratio."
param <- c("NewQuestion","ID","Title","QuestionText","Points","Difficulty",
           rep("Option", answers),"Hint","Feedback")
questions <- data.frame()
for(i in 1:n)
{
  ID <- paste(title, i, sep = "-")
  points <- sample(c(rep(0,answers-1),100),replace=F)
  corr.ind <- 6 + which.max(points)
  data1 <- sample(c(rat.data1, ord.data1, int.data1, nom.data1), size = 1)
  corr.ans <- if(data1 %in% rat.data1){"Numeric, Ratio"}
              else{if(data1 %in% ord.data1){"Categorical, Ordinal"}
                   else{if(data1 %in% int.data1){"Numeric, Interval"}
                        else{"Categorical, Nominal"}}}
  ans.bad <- c("Numeric, Ordinal", "Categorical, Interval", "Categorical, Ratio")
  ans.cat <- c("Categorical, Ordinal", "Categorical, Nominal")
  ans.num <- c("Numeric, Interval", "Numeric, Ratio")
  ans.txt <- sample(if(data1 %in% rat.data1){c(ans.bad, ans.cat, "Numeric, Interval")}
                    else{if(data1 %in% ord.data1){c(ans.bad, ans.num, "Categorical, Nominal")}
                         else{if(data1 %in% int.data1){c(ans.bad, ans.cat, "Numeric, Ratio")}
                              else{c(ans.bad, ans.num, "Categorical, Ordinal")}}},
                    size = answers)
  content <- c(type, ID, ID, paste(quest.txt1, data1, quest.txt2,
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
loc.path <- "Images/"
e.path <- "Images/"
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
               paste(e.path, paste(title, i, sep = "-"), ".jpeg", sep = ""),
               points, hint, feedback)
  options <- c(rep("",7), ans.txt, rep("",2))
  options[corr.ind] <- corr.ans
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),1] <- param
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),2] <- content
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),3] <- options
  jpeg(filename=paste(loc.path, paste(title, i, sep = "-"), ".jpeg", sep = ""))
  hist(data, xlim=c(min(data),max(data)), probability=T, 
       col='lightblue', xlab=' ', ylab=' ', axes=F,
       main = "Researcher's Histogram")
  dev.off()
}
questions <- questions[((10+answers)):((9+answers)*(n+1)),]
write.table(questions, sep=",", file=paste(title, ".csv", sep = ""),
            row.names=F, col.names=F)

##### StemGraphMC1 #####
library(fmsb)
title <- "StemGraphMC1"
n = 200
type <- "MC"
answers <- 5
points.per.q <- 4
difficulty <- 1
quest.txt1 <- "While analyzing a dataset, a researcher makes a stem and leaf plot of one of her variables. The stem and leaf plot she makes is depicted. What is the "
quest.txt2 <- " value?"
dat.size = seq(21, 41, by = 2)
digits = -1
scale = 1
loc.path <- "Images/"
e.path <- "Images/"
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
  dat.size1 <- sample(dat.size, size = 1)
  left.data <- c(sample(seq(100, 350, by = 10^(-digits)), size = ceiling(dat.size1/6)),
                 sample(seq(360, 610, by = 10^(-digits)), size = ceiling(2*dat.size1/6)),
                 sample(seq(620, 870, by = 10^(-digits)), size = ceiling(3*dat.size1/6)))
  right.data <- c(sample(seq(620, 870, by = 10^(-digits)), size = ceiling(dat.size1/6)),
                  sample(seq(360, 610, by = 10^(-digits)), size = ceiling(2*dat.size1/6)),
                  sample(seq(100, 350, by = 10^(-digits)), size = ceiling(3*dat.size1/6)))
  sym.data <- c(sample(seq(620, 870, by = 10^(-digits)), size = ceiling(1.5*dat.size1/6)),
                sample(seq(360, 610, by = 10^(-digits)), size = ceiling(3*dat.size1/6)),
                sample(seq(100, 350, by = 10^(-digits)), size = ceiling(1.5*dat.size1/6)))
  data.dec <- sample(c(1,2,3), size = 1)
  data <- sample(if(data.dec == 1){sym.data}
                   else{if(data.dec == 2){left.data}
                        else{right.data}},
                 size = dat.size1)
  data1 <- sample(c("smallest", "second smallest", "third smallest", "median",
                    "third largest", "second largest", "largest"), size = 1)
  corr.ans <- if(data1 == "smallest"){min(data)}
              else{if(data1 == "second smallest"){sort(data)[2]}
                   else{if(data1 == "third smallest"){sort(data)[3]}
                        else{if(data1 == "median"){median(data)}
                             else{if(data1 == "third largest"){sort(data)[dat.size1-2]}
                                  else{if(data1 == "second largest"){sort(data)[dat.size1-1]}
                                       else{if(data1 == "largest"){max(data)}}}}}}}
  ans.txt <- if(corr.ans == min(data)){c(sample(apply(expand.grid(sort(data)[-1], c(.1, 1, 10)), 1, prod),
                                                size = answers-1),
                                         sample(c(10*corr.ans, corr.ans/10), size = 1))}
             else{if(corr.ans == sort(data)[2]){c(sample(apply(expand.grid(sort(data)[-2], c(.1, 1, 10)), 1, prod),
                                                         size = answers-1),
                                                  sample(c(10*corr.ans, corr.ans/10), size = 1))}
                  else{if(corr.ans == sort(data)[3]){c(sample(apply(expand.grid(sort(data)[-3], c(.1, 1, 10)), 1, prod),
                                                              size = answers-1),
                                                       sample(c(10*corr.ans, corr.ans/10), size = 1))}
                       else{if(corr.ans == median(data)){c(sample(apply(expand.grid(sort(data)[-round((dat.size1+1)/2, digits = 0)], c(.1, 1, 10)), 1, prod),
                                                                  size = answers-1),
                                                           sample(c(10*corr.ans, corr.ans/10), size = 1))}
                            else{if(corr.ans == sort(data)[dat.size1-2]){c(sample(apply(expand.grid(sort(data)[-(dat.size1-2)], c(.1, 1, 10)), 1, prod),
                                                                                  size = answers-1),
                                                                           sample(c(10*corr.ans, corr.ans/10), size = 1))}
                                 else{if(corr.ans == sort(data)[dat.size1-1]){c(sample(apply(expand.grid(sort(data)[-(dat.size1-1)], c(.1, 1, 10)), 1, prod),
                                                                                       size = answers-1),
                                                                                sample(c(10*corr.ans, corr.ans/10), size = 1))}
                                      else{if(corr.ans == max(data)){c(sample(apply(expand.grid(sort(data)[-dat.size1], c(.1, 1, 10)), 1, prod),
                                                                              size = answers-1),
                                                                       sample(c(10*corr.ans, corr.ans/10), size = 1))}}}}}}}
  content <- c(type, ID, ID, paste(quest.txt1, data1, quest.txt2, sep = ""),
               points.per.q, difficulty, paste(e.path, paste(title, i, sep = "-"), ".jpeg", sep = ""),
               points, hint, feedback)
  options <- c(rep("",7), ans.txt, rep("",2))
  options[corr.ind] <- corr.ans
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),1] <- param
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),2] <- content
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),3] <- options
  jpeg(filename=paste(loc.path, paste(title, i, sep = "-"), ".jpeg", sep = ""))
  gstem(data, scale = scale)
  dev.off()
}
questions <- questions[((10+answers)):((9+answers)*(n+1)),]
write.table(questions, sep=",", file=paste(title, ".csv", sep = ""),
            row.names=F, col.names=F)

##### RFTableMC1 #####
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
loc.path <- "Images/"
e.path <- "Images/"
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
               points.per.q, difficulty, paste(e.path, paste(title, i, sep = "-"), ".jpeg", sep = ""),
               points, hint, feedback)
  options <- c(rep("",7), ans.txt, rep("",2))
  options[corr.ind] <- corr.ans
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),1] <- param
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),2] <- content
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),3] <- options
  jpeg(filename=paste(loc.path, paste(title, i, sep = "-"), ".jpeg", sep = ""),
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
loc.path <- "Images/"
e.path <- "Images/"
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
             else{if(corr.ans == fivenum(data)[2]){sample(c(sort(data)[1:floor(dat.size/8)],
                                                            sort(data)[ceiling(3*dat.size/8):dat.size]),
                                                          size = answers)}
                  else{if(corr.ans == fivenum(data)[3]){sample(c(sort(data)[1:floor(3*dat.size/8)],
                                                                 sort(data)[ceiling(5*dat.size/8):dat.size]),
                                                               size = answers)}
                       else{if(corr.ans == fivenum(data)[4]){sample(c(sort(data)[1:floor(5*dat.size/8)],
                                                                      sort(data)[ceiling(7*dat.size/8):dat.size]),
                                                                    size = answers)}
                            else{if(corr.ans == max(data)){sample(sort(data)[1:(dat.size-3)],
                                                                  size = answers)}}}}}
  content <- c(type, ID, ID, paste(quest.txt1, data1, quest.txt2, sep = ""),
               points.per.q, difficulty, paste(e.path, paste(title, i, sep = "-"), ".jpeg", sep = ""),
               points, hint, feedback)
  options <- c(rep("",7), ans.txt, rep("",2))
  options[corr.ind] <- corr.ans
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),1] <- param
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),2] <- content
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),3] <- options
  jpeg(filename=paste(loc.path, paste(title, i, sep = "-"), ".jpeg", sep = ""))
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
loc.path <- 
e.path <- 
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
  up.min <- round(corr.ans + sd(data)/8, digits = digits)
  down.max <- round(corr.ans - sd(data)/8, digits = digits)
  ans.txt <- round(sample(c(median(data), mean(data),
                            if(data1 == "first quartile")
                              {if((dat.size1 %% 2) == 0){c(sort(data)[ceiling((dat.size1+1)/4)],
                                                           sort(data)[floor((dat.size1+1)/4)])}
                               else{c(sort(data)[1+(dat.size1+1)/4], sort(data)[(dat.size1+1)/4-1])}},
                            if(data1 == "third quartile")
                              {if((dat.size1 %% 2) == 0){c(sort(data)[ceiling(3*(dat.size1+1)/4)],
                                                           sort(data)[floor(3*(dat.size1+1)/4)])}
                               else{c(sort(data)[1+3*(dat.size1+1)/4], sort(data)[3*(dat.size1+1)/4-1])}},
                            seq(corr.ans - 3*sd(data), down.max, 10^-digits),
                            seq(up.min, corr.ans + 3*sd(data), 10^-digits)),
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

##### ErrorMC1 #####
title <- "ErrorMC1"
n = 200
type <- "MC"
answers <- 3
points.per.q <- 4
difficulty <- 1
quest.txt1 <- "Researchers examining "
quest.txt2 <- " found that there were "
quest.txt3 <- ". In fact, there were "
quest.txt4 <- ". What type of error, if any, was committed?"
studies <- c("differences in mean survival times between two groups of mice",
             "differences in mean weight loss between groups of people on several diets",
             "differences in mean scores on a reading comprehension exam given to students who declared separate majors",
             "differences in the proportion of college graduates between groups of people from two separate countries",
             "differences in mean credit card debt between separate classes of students")
type1 <- "significant differences between the groups"
type2 <- "no significant differences between the groups"
dat.size = 
digits = 
loc.path <- 
e.path <- 
hint <- "No Error is not always wrong. It's not a trick answer."
feedback <- "Type 1: Finding significant differences when there are none. Type 2: Finding no significant differences when there are some. No Error: Finding the truth."
param <- c("NewQuestion","ID","Title","QuestionText","Points","Difficulty",
           rep("Option", answers),"Hint","Feedback")
questions <- data.frame()
for(i in 1:n)
{
  ID <- paste(title, i, sep = "-")
  points <- sample(c(rep(0,answers-1),100),replace=F)
  corr.ind <- 6 + which.max(points)
  data1 <- sample(studies, size = 1)
  data2 <- sample(c(type1, type2), size = 1)
  data3 <- sample(c(type1, type2), size = 1)
  corr.ans <- if((data2 == type1) & (data3 == type2)){"Type 1"}
              else{if((data2 == type2) & (data3 == type1)){"Type 2"}
                   else{"No Error"}}
  decis <- sample(c(1,2), size = 1)
  ans.txt <- if(which.max(points) == 1)
               {
               if((data2 == type1) & (data3 == type2)){c(" ", sample(c("Type 2", "No Error"), size = 2))}
               else{if((data2 == type2) & (data3 == type1)){c(" ", sample(c("Type 1", "No Error"), size = 2))}
                    else{c(" ", sample(c("Type 1", "Type 2"), size = 2))}}
               }
             else{if((which.max(points) == 2) & (decis == 1))
                    {
                    if((data2 == type1) & (data3 == type2)){c("Type 2", " ", "No Error")}
                    else{if((data2 == type2) & (data3 == type1)){c("Type 1", " ", "No Error")}
                         else{c("Type 1", " ", "Type 2")}}
                    }
                  else{if((which.max(points) == 2) & (decis == 2))  
                         {
                         if((data2 == type1) & (data3 == type2)){c("No Error", " ", "Type 2")}
                         else{if((data2 == type2) & (data3 == type1)){c("No Error", " ", "Type 1")}
                              else{c("Type 2", " ", "Type 1")}}
                         }
                       else{if(which.max(points) == 3)
                              {
                              if((data2 == type1) & (data3 == type2)){c(sample(c("Type 2", "No Error"), size = 2), " ")}
                              else{if((data2 == type2) & (data3 == type1)){c(sample(c("Type 1", "No Error"), size = 2), " ")}
                                   else{c(sample(c("Type 1", "Type 2"), size = 2), " ")}}
                              }
                           }
                      }
                 }
  content <- c(type, ID, ID, paste(quest.txt1, data1, quest.txt2, data2, quest.txt3, data3,
                                   quest.txt4, collapse = "", sep = ""),
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

##### StuDesMC1 #####
title <- "StuDesMC1"
n = 200
type <- "MC"
answers <- 5
points.per.q <- 4
difficulty <- 1
quest.txt1 <- "A friend of yours wonders, '"
quest.txt2 <- "' You respond by asking, '"
quest.txt3 <- "' "
quest.txt4 <- " just ..."
wonder1 <- "Which diet, Atkins or Zone, is best?"
wonder2 <- "Which exercise program, Zumba or Insanity, is most effective?"
wonder3 <- "Which drugs are most damaging to youths?"
wonder4 <- "Which type of credit card is the most generous?"
wonder5 <- "Which language is the most difficult to learn?"
response1 <- "Which diet results in the greatest reduction in BMI over the first three months?"
response2 <- "Which program results in the largest muscle mass increase over the first three months?"
response3 <- "Which drugs (marijuana, alcohol, etc.) are most associated with academic drop-out rates in youths?"
response4 <- "Which brand of credit card (Visa, Mastercard, etc.) averages the lowest interest rates?"
response5 <- "Which language (Chinese, Arabic, etc.) takes the longest average learning-time to reach fluency as rated by a native speaker?"
dat.size = 
  digits = 
  loc.path <- 
  e.path <- 
  hint <- "The important concepts here are from the Knowledge and Data chapter of your coursepack."
feedback <- "Coming up with a basic idea using vague, often normative, language is conceptualizing a problem. Attaching a measurable outcome to the question is operationalizing the problem."
param <- c("NewQuestion","ID","Title","QuestionText","Points","Difficulty",
           rep("Option", answers),"Hint","Feedback")
questions <- data.frame()
for(i in 1:n)
{
  ID <- paste(title, i, sep = "-")
  points <- sample(c(rep(0,answers-1),100),replace=F)
  corr.ind <- 6 + which.max(points)
  data1 <- sample(c(wonder1, wonder2, wonder3, wonder4, wonder5), size = 1)
  data2 <- if(data1 == wonder1){response1}
           else{if(data1 == wonder2){response2}
                else{if(data1 == wonder3){response3}
                     else{if(data1 == wonder4){response4}
                          else{response5}
                         }
                    }
               }
  data3 <- sample(c("You", "Your friend"), size = 1)
  corr.ans <- if(data3 == "You"){"... Operationalized the Problem"}
              else{"... Conceptualized the Problem"}
  ans.txt <- if(data3 == "You"){c("... Conceptualized the Problem",
                                  sample(c("... Designed the Study", "... Committed the Lack of Evidence Fallacy",
                                           "... Committed the Anecdotal Evidence Fallacy",
                                           "... Committed the Correlation Equals Causation Fallacy",
                                           "... Collected the Data", "... Analyzed the Data",
                                           "... Drew Conclusions from the Study", "... Disseminated Results",
                                           "... Committed a Type 1 Error", "... Committed a Type 2 Error"),
                                         size = answers - 1))
                               }
             else{c(sample(c("... Designed the Study", "... Committed the Lack of Evidence Fallacy",
                             "... Committed the Anecdotal Evidence Fallacy",
                             "... Committed the Correlation Equals Causation Fallacy",
                             "... Collected the Data", "... Analyzed the Data",
                             "... Drew Conclusions from the Study", "... Disseminated Results",
                             "... Committed a Type 1 Error", "... Committed a Type 2 Error"),
                           size = answers - 1),
                    "... Operationalized the Problem")
                 }
  content <- c(type, ID, ID, paste(quest.txt1, data1, quest.txt2, data2, quest.txt3, data3,
                                   quest.txt4, collapse = "", sep = ""),
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

##### StuDesMC2 #####
title <- "StuDesMC2"
n = 200
type <- "MC"
answers <- 5
points.per.q <- 4
difficulty <- 1
quest.txt1 <- "A team of researchers "
quest.txt2 <- " What type of study is this?"
study1 <- "randomly separates their study's participants into two groups, giving one group a placebo and the other a new treatment to be tested. As the treatment is very experimental, both participants and researchers know whether a specific participant is receiving the new treatment or not."
study2 <- "randomly separates their study's participants into two groups, giving one group a placebo and the other a new treatment to be tested. As the treatment is not experimental, both participants and researchers do not know whether a specific participant is receiving the new treatment or not."
study3 <- "randomly separates their study's participants into two groups, giving one group a placebo and the other a new treatment to be tested. As the treatment is somewhat experimental, participants do not know whether they receive the placebo but researchers do know whether a specific participant receives the placebo."
study4 <- "studies a population of people who lost weight, asking each person in a sample from this population whether they were on a diet and, if so, which diet was it?"
study5 <- "studies whether hormone therapy affects menstrual cycles in women. All women in the sample under study were monitored for 2 months without hormone therapy, monitored for 2 more months while being given hormone therapy A, and monitored for 2 more months (for 6 months total) while being given hormone therapy B. Each woman thus acted as her own control. At the end of the 6 months, all participants' menstrual cycles were compared."
study6 <- "studies the effect of a new pesticide on two randomly selected samples from a species of crop-destroying beetles. One group of beetles receives a standard pesticide, and the other receives the new pesticide. Neither the beetles nor the researchers know which pesticide is which, and survival rates for each group are compared throughout administration of several doses."
study7 <- "studies the effect of a new medicine meant to treat the common cold. Two randomly selected groups of people are chosen for the study; one group receives a standard cold medication while the other gets the new medication. Due to an error in study design, the researchers are aware of which treatment each participant is receiving, although the participants themselves are unaware whether they received the standard or new medications."
study8 <- "conducts a study of a new medicine meant to treat the common cold. Two randomly selected groups of people are chosen for the study; one group receives a standard cold medication while the other gets the new medication. Due to an error in study design, both the study's participants and the researchers are aware of which treatment each participant is receiving."
study9 <- "conducts a preliminary study of a population of people who are suffering from Alzheimer's disease. The researchers are interested in learning whether potassium intake plays a role in contracting Alzheimer's. Study participants are asked, when possible, about their long-term dietary habits; of particular interest is whether participants regularly consumed foods or supplements that contain potassium such as bananas."
study10 <- "conducts a study of a typical course of treatment for recurring pneumonia that involves a sequence of three medications A, B, and C. The sequence is of interest, so subjects are given the sequence of medications A, B, then C on the first occurence of pneumonia, then subjects are given the sequence C, B, then A on the second occurence, etc. All sequences are given to each subject, so that each subject acts as their own control while comparing the effects of the different sequences on pneumonia."
dat.size = 
digits = 
loc.path <- 
e.path <- 
hint <- "The important concepts here are from the Study Designs chapter of your coursepack."
feedback <- "RCT: randomization, control group. Single-Blinding: researchers aware, subjects unaware. Double-Blinding: both researchers and patients unaware. Non-Blnded: Neither patients nor researchers aware. Case-Control: Looks back from outcome to treatment, no randomization. Case-Crossover: Participants act as own control group."
param <- c("NewQuestion","ID","Title","QuestionText","Points","Difficulty",
           rep("Option", answers),"Hint","Feedback")
questions <- data.frame()
for(i in 1:n)
{
  ID <- paste(title, i, sep = "-")
  points <- sample(c(rep(0,answers-1),100),replace=F)
  corr.ind <- 6 + which.max(points)
  data1 <- sample(c(study1, study2, study3, study4, study5,
                    study6, study7, study8, study9), size = 1)
  corr.ans <- if(data1 == study1 | data1 == study8){"Non-Blinded Randomized Controlled Trial"}
              else{if(data1 == study2 | data1 == study6){"Double-Blinded Randomized Controlled Trial"}
                   else{if(data1 == study3 | data1 == study7){"Single-Blinded Randomized Controlled Trial"}
                        else{if(data1 == study4 | data1 == study9){"Case-Control Study"}
                             else{if(data1 == study5 | data1 == study10){"Case-Crossover Study"}}}}}
  ans.txt <- if(corr.ans == "Non-Blinded Randomized Controlled Trial")
               {sample(c("Double-Blinded Randomized Controlled Trial", "Single-Blinded Randomized Controlled Trial",
                         "Case-Control Study", "Case-Crossover Study", "Type 1 Study", "Type 2 Study", "Standard Deviation Study"),
                       size = answers)}
             else{if(corr.ans == "Double-Blinded Randomized Controlled Trial")
                    {sample(c("Non-Blinded Randomized Controlled Trial", "Single-Blinded Randomized Controlled Trial",
                              "Case-Control Study", "Case-Crossover Study", "Type 1 Study", "Type 2 Study", "Standard Deviation Study"),
                            size = answers)}
                  else{if(corr.ans == "Single-Blinded Randomized Controlled Trial")
                         {sample(c("Non-Blinded Randomized Controlled Trial", "Double-Blinded Randomized Controlled Trial",
                                   "Case-Control Study", "Case-Crossover Study", "Type 1 Study", "Type 2 Study", "Standard Deviation Study"),
                                 size = answers)}
                       else{if(corr.ans == "Case-Control Study")
                              {sample(c("Non-Blinded Randomized Controlled Trial", "Single-Blinded Randomized Controlled Trial",
                                        "Double-Blinded Randomized Controlled Trial", "Case-Crossover Study", "Type 1 Study", "Type 2 Study", "Standard Deviation Study"),
                                      size = answers)}
                            else{if(corr.ans == "Case-Crossover Study")
                                   {sample(c("Non-Blinded Randomized Controlled Trial", "Single-Blinded Randomized Controlled Trial",
                                             "Case-Control Study", "Double-Blinded Randomized Controlled Trial", "Type 1 Study", "Type 2 Study", "Standard Deviation Study"),
                                           size = answers)}}}}}
  content <- c(type, ID, ID, paste(quest.txt1, data1, quest.txt2, collapse = "", sep = ""),
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

##### FallaciesMC1 #####
title <- "FallaciesMC1"
n = 200
type <- "MC"
answers <- 5
points.per.q <- 4
difficulty <- 1
quest.txt1 <- "While watching a debate online, you listen as one of the debaters claims, '"
quest.txt2 <- "' This person has just... "
listen1 <- "There is no hard evidence that she stole the money, thus we must conclude that she did not steal the money."
listen2 <- "Since there is no proof that this drug is unsafe, we must conclude that it is safe."
listen3 <- "We cannot show for certain God exists, so we must conclude that God does not exist."
listen4 <- "My cousin said this was the best golf club on the market, so it must be the best club on the market."
listen5 <- "I read that homelessness cannot be solved, so it's insoluble."
listen6 <- "I just read about a taste test where 90% of participants preferred Coke to Pepsi, so America loves Coke more than Pepsi."
listen7 <- "You can always find firefighters at fires, so firefighters must cause fires."
listen8 <- "Old single people are always bitter, so being single makes them bitter."
listen9 <- "Adults are often more conservative than youths, so conservative values are produced by aging."
dat.size = 
digits = 
loc.path <- 
e.path <- 
hint <- "The important concepts here are from the Knowledge and Data chapter of your coursepack."
feedback <- "Lack of Evidence: Reasoning the contrary is true because of a lack of evidence. Anecdotal Evidence: Generalizing on the grounds of just a few examples. Correlation/Causation: Reasoning that two things happening together means either causes the other."
param <- c("NewQuestion","ID","Title","QuestionText","Points","Difficulty",
           rep("Option", answers),"Hint","Feedback")
questions <- data.frame()
for(i in 1:n)
{
  ID <- paste(title, i, sep = "-")
  points <- sample(c(rep(0,answers-1),100),replace=F)
  corr.ind <- 6 + which.max(points)
  data1 <- sample(c(listen1, listen2, listen3, listen4, listen5,
                    listen6, listen7, listen8, listen9), size = 1)
  corr.ans <- if(data1 == listen1 | data1 == listen2 | data1 == listen3){"... Committed the Lack of Evidence Fallacy"}
  else{if(data1 == listen4 | data1 == listen5 | data1 == listen6){"... Committed the Anecdotal Evidence Fallacy"}
    else{if(data1 == listen7 | data1 == listen8 | data1 == listen9){"... Committed the Correlation Equals Causation Fallacy"}}}
  ans.txt <- if(corr.ans == "... Committed the Lack of Evidence Fallacy")
  {sample(c("... Conceptualized a Study's Problem", "... Designed a Study",
            "... Committed the Anecdotal Evidence Fallacy", "Operationalized a Study's Problem",
            "... Committed the Correlation Equals Causation Fallacy", "... Collected Data for a Study",
            "... Committed a Type 1 Error", "... Committed a Type 2 Error"),
          size = answers)}
  else{if(corr.ans == "... Committed the Anecdotal Evidence Fallacy")
  {sample(c("... Conceptualized a Study's Problem", "... Designed a Study",
            "... Committed the Lack of Evidence Fallacy", "... Operationalized a Study's Problem",
            "... Committed the Correlation Equals Causation Fallacy", "... Collected Data for a Study",
            "... Committed a Type 1 Error", "... Committed a Type 2 Error"),
          size = answers)}
    else{if(corr.ans == "... Committed the Correlation Equals Causation Fallacy")
    {sample(c("... Conceptualized a Study's Problem", "... Designed a Study",
              "... Committed the Anecdotal Evidence Fallacy", "... Operationalized a Study's Problem",
              "... Committed the Lack of Evidence Fallacy", "... Collected Data for a Study",
              "... Committed a Type 1 Error", "... Committed a Type 2 Error"),
            size = answers)}}}
  content <- c(type, ID, ID, paste(quest.txt1, data1, quest.txt2, collapse = "", sep = ""),
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

##### ConfoundersMC1 #####
title <- "ConfoundersMC1"
n = 200
type <- "MC"
answers <- 5
points.per.q <- 4
difficulty <- 1
quest.txt1 <- "Some student research assistants are helping study "
quest.txt2 <- ". The study primarily involves three variables. First, the "
quest.txt3 <- " sometimes leads to a change in a second variable, the "
quest.txt4 <- ". However, sometimes a third variable, the "
quest.txt5 <- " (which is associated with the "
quest.txt6 <- " and causes changes in the "
quest.txt7 <- ") interferes with the result. In statistics, what do we call the "
quest.txt8 <- "?"
example1 <- c("flowers", "flowers' petal length", "flowers' reproductive success", "flowers' height")
example2 <- c("other students' academic success", "amount of time students spend studying", "students' GPAs", "students' work schedules")
example3 <- c("beehives", "number of bees in the hive", "survival of the beehive through winter", "availability of nectar in summer")
example4 <- c("the student debt problem", "amount of debt accrued while in school", "amount of debt remaining ten years later", "choice of major")
example5 <- c("mental health", "amount of time a patient spends outside", "state of a patients' depression", "patients' occupation")
dat.size = 
digits = 
loc.path <- 
e.path <- 
hint <- ""
feedback <- ""
param <- c("NewQuestion","ID","Title","QuestionText","Points","Difficulty",
           rep("Option", answers),"Hint","Feedback")
questions <- data.frame()
for(i in 1:n)
{
  ID <- paste(title, i, sep = "-")
  points <- sample(c(rep(0,answers-1),100),replace=F)
  corr.ind <- 6 + which.max(points)
  decis1 <- sample(1:5, size = 1)
  data <- get(paste("example", decis1, sep = ""))
  decis2 <- sample(2:4, size = 1)
  corr.ans <- if(decis2 == 2){"The Probable Cause"}
              else{if(decis2 == 3){"The Outcome/Effect"}
                   else{if(decis2 == 4){"The Confounder"}}}
  ans.txt <- if(decis2 == 2){c(sample(c("The Outcome/Effect", "The Confounder",
                                        "The Standard Deviation", "The Fallacy"),
                                      size = answers-1),
                               sample(c("All of the Above", "None of the Above"),
                                      size = 1))}
             else{if(decis2 == 3){c(sample(c("The Confounder", "The Standard Deviation",
                                             "The Probable Cause", "The Fallacy"),
                                           size = answers-1),
                                    sample(c("All of the Above", "None of the Above"),
                                           size = 1))}
                  else{if(decis2 == 4){c(sample(c("The Outcome/Effect", "The Standard Deviation",
                                                  "The Probable Cause", "The Fallacy"),
                                                size = answers-1),
                                         sample(c("All of the Above", "None of the Above"),
                                                size = 1))}}}
  content <- c(type, ID, ID, paste(quest.txt1, data[1], quest.txt2, data[2], quest.txt3, data[3],
                                   quest.txt4, data[4], quest.txt5, data[2], quest.txt6, data[3],
                                   quest.txt7, data[decis2], quest.txt8,
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