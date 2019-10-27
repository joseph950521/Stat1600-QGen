##### I currently plan to create a bank of 5000 random questions per test,      #####
##### which requires 25 distinct generators per test as they appear below. So,  #####
##### 75 total genrators for a semester of three tests. Once finished with      #####
##### individual generators, I will begin function wrapping, creating either a  #####
##### single function with many adaptive arguments, or a suite of functions.    #####

##### These will help streamline test writing, requiring only that one runs the #####
##### code and occasionally re-uploads the test banks. The longest part of the  #####
##### process is waiting on the e-learning uploads of the images and CSVs.      #####                                                               #####

##### 25/25 Generators for test 1 completed  #####
##### 18/25 Generators for test 2 completed  #####
##### 25/25 Generators for test 3 completed  #####

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
  ans.txt <- sample(c(ifelse(data == "the least robust.", "The median", "The mean"), "The 10% trimmed mean",
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
dat.size = seq(21, 31, by = 2)
digits = -2
scale = 2
loc.path <- "Images/"
e.path <- "Images/"
hint <- "Pay close attention to the key given for the plot. As an example, if the key read 'the decimal point is 1 digit to the right of the |, and the number of interest was labeled 1 | 2, then the answer would be 12."
feedback <- "Stem and leaf plots are like histograms flipped over. The min is at the top, the middle halfway down, and the max is at the bottom."
param <- c("NewQuestion","ID","Title","QuestionText","Points","Difficulty", "Image",
           rep("Option", answers),"Hint","Feedback")
questions <- data.frame()
for(i in 1:n)
{
  ID <- paste(title, i, sep = "-")
  points <- sample(c(rep(0, answers-1), 100), replace=F)
  corr.ind <- 7 + which.max(points)
  dat.size1 <- sample(dat.size, size = 1)
  left.data <- c(sample(seq(1000, 3500, by = 10^(-digits)), size = ceiling(dat.size1/6)),
                 sample(seq(3600, 6100, by = 10^(-digits)), size = ceiling(2*dat.size1/6)),
                 sample(seq(6200, 7900, by = 10^(-digits)), size = ceiling(3*dat.size1/6)))
  right.data <- c(sample(seq(6200, 8700, by = 10^(-digits)), size = ceiling(dat.size1/6)),
                  sample(seq(3600, 6100, by = 10^(-digits)), size = ceiling(2*dat.size1/6)),
                  sample(seq(2000, 3500, by = 10^(-digits)), size = ceiling(3*dat.size1/6)))
  sym.data <- c(sample(seq(6200, 7900, by = 10^(-digits)), size = ceiling(1.5*dat.size1/6)),
                sample(seq(3600, 6100, by = 10^(-digits)), size = ceiling(3*dat.size1/6)),
                sample(seq(2000, 3500, by = 10^(-digits)), size = ceiling(1.5*dat.size1/6)))
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
example6 <- c("sports health and football", "length of kickoff plays", "number of concussions during kickoff plays", "speed of the players")
example7 <- c("consumers in the music industry", "location of consumers", "consumers' preferred genre of music", "consumers' education level")
example8 <- c("the craft brewing industry", "size of beer-brewing companies", "growth of these companies for the year", "average price of their products")
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
  decis1 <- sample(1:8, size = 1)
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

##### BinomExactMC1 #####
title <- "BinomExactMC1"
n = 200
type <- "MC"
answers <- 4
points.per.q <- 4
difficulty <- 1
quest.txt1 <- "A student must answer "
quest.txt2 <- " true/false questions for a test, but the student did not study well. If the student randomly guesses on each question, what is the probability that the student answers "
quest.txt3 <- " questions correctly?"
dat.size = 1
digits = 3
loc.path <- 
  e.path <- 
  hint <- "First, you need to find the number of successes, the number of trials, and infer the probability p from the context of this question. Round and select the closest answer."
feedback <- "1: Find successes x. 2: Find trials n. 3: Infer p = 0.5. 4: Apply exact binomial formula."
param <- c("NewQuestion","ID","Title","QuestionText","Points","Difficulty",
           rep("Option", answers),"Hint","Feedback")
questions <- data.frame()
for(i in 1:n)
{
  ID <- paste(title, i, sep = "-")
  points <- sample(c(rep(0,answers-1),100),replace=F)
  corr.ind <- 6 + which.max(points)
  data1 <- sample(10:20, size = 1)
  data2 <- sample(1:7, size = 1)
  corr.ans <- round(dbinom(data2, data1, 0.5), digits = digits)
  up.min <- round(corr.ans + .05, digits)
  down.max <- round(corr.ans - .05, digits)
  ans.txt <- sample(if(corr.ans <= .05){seq(up.min, 1 + 10^-digits, 10^-digits)}
                    else{if(corr.ans >= .95){seq(0 - 10^-digits, down.max, 10^-digits)}
                      else{c(seq(0 - 10^-digits, down.max, 10^-digits),
                             seq(up.min, 1 + 10^-digits, 10^-digits))}},
                    size = answers)
  content <- c(type, ID, ID, paste(quest.txt1, data1, quest.txt2,
                                   data2, quest.txt3,
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

##### BinomExactMC2 #####
title <- "BinomExactMC2"
n = 200
type <- "MC"
answers <- 4
points.per.q <- 4
difficulty <- 1
quest.txt1 <- "A student must answer "
quest.txt2 <- " multiple choice questions for a test, but the student did not study well. There are "
quest.txt3 <- " answers per question but only one is correct. If the student randomly guesses on each question, what is the probability that the student answers "
quest.txt4 <- " questions correctly?"
dat.size = 1
digits = 3
loc.path <- 
  e.path <- 
  hint <- "First, you need to find the number of successes, the number of trials, and infer the probability p from the context of this question. Apply the correct formula and round and select the closest answer."
feedback <- "1: Find successes x. 2: Find trials n. 3: Infer p. 4: Apply exact binomial formula."
param <- c("NewQuestion","ID","Title","QuestionText","Points","Difficulty",
           rep("Option", answers),"Hint","Feedback")
questions <- data.frame()
for(i in 1:n)
{
  ID <- paste(title, i, sep = "-")
  points <- sample(c(rep(0,answers-1),100),replace=F)
  corr.ind <- 6 + which.max(points)
  data1 <- sample(10:20, size = 1)
  data2 <- sample(3:6, size = 1)
  data3 <- sample(1:7, size = 1)
  corr.ans <- round(dbinom(data3, data1, 1/data2), digits = digits)
  up.min <- round(corr.ans + .05, digits)
  down.max <- round(corr.ans - .05, digits)
  ans.txt <- sample(if(corr.ans <= .05){seq(up.min, 1 + 10^-digits, 10^-digits)}
                    else{if(corr.ans >= .95){seq(0 - 10^-digits, down.max, 10^-digits)}
                      else{c(seq(0 - 10^-digits, down.max, 10^-digits),
                             seq(up.min, 1 + 10^-digits, 10^-digits))}},
                    size = answers)
  content <- c(type, ID, ID, paste(quest.txt1, data1, quest.txt2,
                                   data2, quest.txt3, data3, quest.txt4,
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

##### BinomDistMC1 #####
title <- "BinomDistMC1"
n = 200
type <- "MC"
answers <- 4
points.per.q <- 4
difficulty <- 1
quest.txt1 <- "A student must answer "
quest.txt2 <- " multiple choice questions for a test, but the student did not study well. There are "
quest.txt3 <- " answers per question but only one is correct. If the student randomly guesses on each question, what is the probability that the student answers "
quest.txt4 <- " questions correctly?"
dat.size = 1
digits = 3
loc.path <- 
  e.path <- 
  hint <- "First, you need to find the number of successes, the number of trials, and infer the probability p from the context of this question. Apply the correct formula and round and select the closest answer."
feedback <- "1: Find successes x. 2: Find trials n. 3: Infer p. 4: Apply exact binomial formula."
param <- c("NewQuestion","ID","Title","QuestionText","Points","Difficulty",
           rep("Option", answers),"Hint","Feedback")
questions <- data.frame()
for(i in 1:n)
{
  ID <- paste(title, i, sep = "-")
  points <- sample(c(rep(0,answers-1),100),replace=F)
  corr.ind <- 6 + which.max(points)
  data1 <- sample(6:10, size = 1)
  data2 <- sample(3:6, size = 1)
  decis1 <- sample(2:4, size = 1)
  decis2 <- sample(c("or fewer", "fewer than"), size = 1)
  data3 <- if(decis2 == "or fewer"){paste(decis1, decis2, sep = " ")}
           else{paste(decis2, decis1, sep = " ")}
  corr.ans <- round(if(decis2 == "or fewer"){pbinom(decis1, data1, 1/data2)}
                    else{pbinom((decis1-1), data1, 1/data2)},
                    digits = digits)
  up.min <- round(corr.ans + .05, digits)
  down.max <- round(corr.ans - .05, digits)
  ans.txt <- sample(if(corr.ans <= .05){seq(up.min, 1 + 10^-digits, 10^-digits)}
                    else{if(corr.ans >= .95){seq(0 - 10^-digits, down.max, 10^-digits)}
                      else{c(seq(0 - 10^-digits, down.max, 10^-digits),
                             seq(up.min, 1 + 10^-digits, 10^-digits))}},
                    size = answers)
  content <- c(type, ID, ID, paste(quest.txt1, data1, quest.txt2,
                                   data2, quest.txt3, data3, quest.txt4,
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

##### BinomDistMC2 #####
title <- "BinomDistMC2"
n = 200
type <- "MC"
answers <- 4
points.per.q <- 4
difficulty <- 1
quest.txt1 <- "A student must answer "
quest.txt2 <- " multiple choice questions for a test, but the student did not study well. There are "
quest.txt3 <- " answers per question but only one is correct. If the student randomly guesses on each question, what is the probability that the student answers "
quest.txt4 <- " questions correctly?"
dat.size = 1
digits = 3
loc.path <- 
  e.path <- 
  hint <- "First, you need to find the number of successes, the number of trials, and infer the probability p from the context of this question. Apply the correct formula and round and select the closest answer."
feedback <- "1: Find successes x. 2: Find trials n. 3: Infer p. 4: Apply exact binomial formula."
param <- c("NewQuestion","ID","Title","QuestionText","Points","Difficulty",
           rep("Option", answers),"Hint","Feedback")
questions <- data.frame()
for(i in 1:n)
{
  ID <- paste(title, i, sep = "-")
  points <- sample(c(rep(0,answers-1),100),replace=F)
  corr.ind <- 6 + which.max(points)
  data1 <- sample(6:7, size = 1)
  data2 <- sample(3:6, size = 1)
  decis1 <- sample(3:4, size = 1)
  decis2 <- sample(c("or more", "greater than"), size = 1)
  data3 <- if(decis2 == "or more"){paste(decis1, decis2, sep = " ")}
           else{paste(decis2, decis1, sep = " ")}
  corr.ans <- round(if(decis2 == "or more"){pbinom((decis1-1), data1, 1/data2, lower.tail = F)}
                    else{pbinom(decis1, data1, 1/data2, lower.tail = F)},
                    digits = digits)
  up.min <- round(corr.ans + .05, digits)
  down.max <- round(corr.ans - .05, digits)
  ans.txt <- sample(if(corr.ans <= .05){seq(up.min, 1 + 10^-digits, 10^-digits)}
                    else{if(corr.ans >= .95){seq(0 - 10^-digits, down.max, 10^-digits)}
                      else{c(seq(0 - 10^-digits, down.max, 10^-digits),
                             seq(up.min, 1 + 10^-digits, 10^-digits))}},
                    size = answers)
  content <- c(type, ID, ID, paste(quest.txt1, data1, quest.txt2,
                                   data2, quest.txt3, data3, quest.txt4,
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

##### BinomApporxMC1 #####
title <- "BinomApproxMC1"
n = 200
type <- "MC"
answers <- 4
points.per.q <- 4
difficulty <- 1
quest.txt1 <- "A brewery pulls "
quest.txt2 <- " bottles of beer to sample them for quality control. In the past, there has been a "
quest.txt3 <- " probability that a randomly selected bottle of beer is defective. Assuming this trend continues (independence), what is the probability that "
quest.txt4 <- " bottles of beer in this particular sample are defective?"
dat.size = 1
digits = 2
loc.path <- 
  e.path <- 
  hint <- "You'll need to apply the normal approximation here."
feedback <- "1: Find successes x. 2: Find trials n. 3. Find probability p. 4. Use the normal approximation formula with appropriate continuity correction. 5. Use the z table to find the probability."
param <- c("NewQuestion","ID","Title","QuestionText","Points","Difficulty",
           rep("Option", answers),"Hint","Feedback")
questions <- data.frame()
for(i in 1:n)
{
  ID <- paste(title, i, sep = "-")
  points <- sample(c(rep(0,answers-1),100),replace=F)
  corr.ind <- 6 + which.max(points)
  data1 <- sample(900:1100, size = 1)
  data2 <- sample(seq(.11, .16, .01), size = 1)
  decis1 <- sample(125:160, size = 1)
  decis2 <- sample(c("or fewer", "fewer than"), size = 1)
  data3 <- if(decis2 == "or fewer"){paste(decis1, decis2, sep = " ")}
           else{paste(decis2, decis1, sep = " ")}
  corr.ans <- round(if(decis2 == "or fewer"){pnorm(round((decis1+.5 - data1*data2)/sqrt(data1*data2*(1-data2)), digits = 2))}
                    else{pnorm(round((decis1-.5 - data1*data2)/sqrt(data1*data2*(1-data2)), digits = 2))},
                    digits = digits)
  up.min <- round(corr.ans + .05, digits)
  down.max <- round(corr.ans - .05, digits)
  ans.txt <- sample(if(corr.ans <= .05){seq(up.min, 1 + 10^-digits, 10^-digits)}
                    else{if(corr.ans >= .95){seq(0 - 10^-digits, down.max, 10^-digits)}
                      else{c(seq(0 - 10^-digits, down.max, 10^-digits),
                             seq(up.min, 1 + 10^-digits, 10^-digits))}},
                    size = answers)
  content <- c(type, ID, ID, paste(quest.txt1, data1, quest.txt2,
                                   data2, quest.txt3, data3, quest.txt4,
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

##### BinomApporxMC2 #####
title <- "BinomApproxMC2"
n = 200
type <- "MC"
answers <- 4
points.per.q <- 4
difficulty <- 1
quest.txt1 <- "A brewery pulls "
quest.txt2 <- " bottles of beer to sample them for quality control. In the past, there has been a "
quest.txt3 <- " probability that a randomly selected bottle of beer is non-defective. Assuming this trend continues (independence), what is the probability that "
quest.txt4 <- " bottles of beer in this particular sample are non-defective?"
dat.size = 1
digits = 2
loc.path <- 
  e.path <- 
  hint <- "You'll need to apply the normal approximation here."
feedback <- "1: Find successes x. 2: Find trials n. 3. Find probability p. 4. Use the normal approximation formula with appropriate continuity correction. 5. Use the z table to find the probability (make sure you use 1 - probability to get upper tail)."
param <- c("NewQuestion","ID","Title","QuestionText","Points","Difficulty",
           rep("Option", answers),"Hint","Feedback")
questions <- data.frame()
for(i in 1:n)
{
  ID <- paste(title, i, sep = "-")
  points <- sample(c(rep(0,answers-1),100),replace=F)
  corr.ind <- 6 + which.max(points)
  data1 <- sample(905:915, size = 1)
  data2 <- sample(seq(.942, .962, .001), size = 1)
  decis1 <- sample(858:871, size = 1)
  decis2 <- sample(c("or more", "more than"), size = 1)
  data3 <- if(decis2 == "or more"){paste(decis1, decis2, sep = " ")}
  else{paste(decis2, decis1, sep = " ")}
  corr.ans <- round(if(decis2 == "or more"){pnorm(round((decis1-.5 - data1*data2)/sqrt(data1*data2*(1-data2)),
                                                        digits = 2),
                                                  lower.tail = F)}
                    else{pnorm(round((decis1+.5 - data1*data2)/sqrt(data1*data2*(1-data2)),
                                     digits = 2),
                               lower.tail = F)},
                    digits = digits)
  up.min <- round(corr.ans + .05, digits)
  down.max <- round(corr.ans - .05, digits)
  ans.txt <- sample(if(corr.ans <= .05){seq(up.min, 1 + 10^-digits, 10^-digits)}
                    else{if(corr.ans >= .95){seq(0 - 10^-digits, down.max, 10^-digits)}
                      else{c(seq(0 - 10^-digits, down.max, 10^-digits),
                             seq(up.min, 1 + 10^-digits, 10^-digits))}},
                    size = answers)
  content <- c(type, ID, ID, paste(quest.txt1, data1, quest.txt2,
                                   data2, quest.txt3, data3, quest.txt4,
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

##### BinomApproxMC3 #####
title <- "BinomApproxMC3"
n = 200
type <- "MC"
answers <- 2
points.per.q <- 4
difficulty <- 1
quest.txt1 <- "A brewery pulls "
quest.txt2 <- " bottles of beer to sample them for quality control. In the past, there has been a "
quest.txt3 <- " probability that a randomly selected bottle of beer is non-defective. Managment would like to know what the probability that "
quest.txt4 <- " bottles of beer in this particular sample are non-defective. Is it appropriate to apply normal approximation in order to solve this problem?"
dat.size = 1
digits = 2
loc.path <- 
  e.path <- 
  hint <- "This is mainly an interpretive question. You do not need to calculate the probability, although some other small calculations may be necessary."
feedback <- "Did you check the size of np and nq?"
param <- c("NewQuestion","ID","Title","QuestionText","Points","Difficulty",
           rep("Option", answers),"Hint","Feedback")
questions <- data.frame()
for(i in 1:n)
{
  ID <- paste(title, i, sep = "-")
  points <- sample(c(rep(0,answers-1),100),replace=F)
  corr.ind <- 6 + which.max(points)
  data1 <- sample(50:160, size = 1)
  data2 <- sample(seq(.942, .962, .001), size = 1)
  decis1 <- sample((data1-40):(data1-10), size = 1)
  decis2 <- sample(c("or more", "more than"), size = 1)
  data3 <- if(decis2 == "or more"){paste(decis1, decis2, sep = " ")}
           else{paste(decis2, decis1, sep = " ")}
  corr.ans <- if((data1*data2 > 5) & (data1*(1-data2) > 5)){"Yes"}else{"No"}
  ans.txt <- if(corr.ans == "Yes"){rep("No", 2)}else{rep("Yes", 2)}
  content <- c(type, ID, ID, paste(quest.txt1, data1, quest.txt2,
                                   data2, quest.txt3, data3, quest.txt4,
                                   collapse = "", sep= ""),
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

##### IndpMC1 #####
# Is independence required for binom? What is independence?

##### BinomEVSDMC1 #####
title <- "BinomEVSDMC1"
n = 200
type <- "MC"
answers <- 4
points.per.q <- 4
difficulty <- 1
quest.txt1 <- "A binomial process has "
quest.txt2 <- " trials and a probability of success of "
quest.txt3 <- ". What is the "
quest.txt4 <- " of this process' distribution?"
dat.size = 1
digits = 2
loc.path <- 
e.path <- 
hint <- "Find the appropriate formula from the coursepack's chapter on the binomial distribution."
feedback <- "Expected Value = np. Standard Deviation = sqrt(npq)."
param <- c("NewQuestion","ID","Title","QuestionText","Points","Difficulty",
           rep("Option", answers),"Hint","Feedback")
questions <- data.frame()
for(i in 1:n)
{
  ID <- paste(title, i, sep = "-")
  points <- sample(c(rep(0,answers-1),100),replace=F)
  corr.ind <- 6 + which.max(points)
  data1 <- sample(500:1500, size = 1)
  data2 <- sample(seq(.01, .99, 10^-digits), size = 1)
  data3 <- sample(c("expected value", "standard deviation"), size = 1)
  corr.ans <- if(data3 == "expected value"){data1*data2}else{sqrt(data1*data2*(1-data2))}
  up.min <- round(corr.ans + 5, digits)
  down.max <- round(corr.ans - 5, digits)
  ans.txt <- sample(if(corr.ans <= 15){seq(up.min, 100, 10^-digits)}
                    else{if(corr.ans >= 1000){seq(800, down.max, 10^-digits)}
                      else{c(seq(10^-digits, down.max, 10^-digits),
                             seq(up.min, up.min + 100, 10^-digits))}},
                    size = answers)
  content <- c(type, ID, ID, paste(quest.txt1, data1, quest.txt2,
                                   data2, quest.txt3, data3, quest.txt4,
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

##### PropEVSDMC1 #####
title <- "PropEVSDMC1"
n = 200
type <- "MC"
answers <- 4
points.per.q <- 4
difficulty <- 1
quest.txt1 <- "In general, "
quest.txt2 <- " of all students default on their student loan debt. A sample of "
quest.txt3 <- " students with student loan debt is randomly selected and it's found that "
quest.txt4 <- " of them default. What is the "
quest.txt5 <- "?"
dat.size = 1
digits = 2
loc.path <- 
  e.path <- 
  hint <- "Find the appropriate formula from the coursepack's chapter on the distribution of the sample proportion."
feedback <- "Sample proportion = x/n. Standard Error = sqrt(pq/n). Population proportion = p."
param <- c("NewQuestion","ID","Title","QuestionText","Points","Difficulty",
           rep("Option", answers),"Hint","Feedback")
questions <- data.frame()
for(i in 1:n)
{
  ID <- paste(title, i, sep = "-")
  points <- sample(c(rep(0,answers-1),100),replace=F)
  corr.ind <- 6 + which.max(points)
  data1 <- sample(seq(.15, .30, 10^-digits), size = 1)
  data2 <- sample(500:600, size = 1)
  data3 <- sample((data2 - 450):(data2 - 470), size = 1)
  data4 <- sample(c("sample proportion", "population proportion", "standard error of the sample proportion",
                    "standard deviation of the sample proportion's sampling distribution"), size = 1)
  corr.ans <- if(data4 == "sample proportion"){data3/data2}
              else{if(data4 == "population proportion"){data1}
                   else{if(data4 == "standard error of the sample proportion"){sqrt((data3/data2*(1-data3/data2))/data2)}
                        else{sqrt((data1*(1-data1))/data2)}}}
  up.min <- round(corr.ans + .02, digits)
  down.max <- round(corr.ans - .02, digits)
  ans.txt <- sample(if(corr.ans <= .03){seq(up.min, .2, 10^-digits)}
                    else{if(corr.ans >= .2){seq(.1, down.max, 10^-digits)}
                      else{c(seq(10^-digits, down.max, 10^-digits),
                             seq(up.min, .3, 10^-digits))}},
                    size = answers)
  content <- c(type, ID, ID, paste(quest.txt1, data1, quest.txt2,
                                   data2, quest.txt3, data3, quest.txt4,
                                   data4, quest.txt5,
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

#### PropDistMC1 #####
title <- "PropDistMC1"
n = 200
type <- "MC"
answers <- 5
points.per.q <- 4
difficulty <- 1
quest.txt1 <- "A student conducts a survey for her class project. She asks "
quest.txt2 <- " other students a series of questions about their study habits. She knows from previous research that "
quest.txt3 <- " of all students study for 4 or more hours per day. If "
quest.txt4 <- " of her own sample study for 4 or more hours per day, what is the probability of observing this sample proportion or less?"
dat.size = 1
digits = 2
loc.path <- 
  e.path <- 
  hint <- "You'll need to correctly distinguish between sample and population when calculating Z for this question. Pick the closest answer."
feedback <- "Z = (phat - p)/sqrt(pq/n). The probability can be found directly from the Z table."
param <- c("NewQuestion","ID","Title","QuestionText","Points","Difficulty",
           rep("Option", answers),"Hint","Feedback")
questions <- data.frame()
for(i in 1:n)
{
  ID <- paste(title, i, sep = "-")
  points <- sample(c(rep(0,answers-1),100),replace=F)
  corr.ind <- 6 + which.max(points)
  data1 <- sample(20:40, size = 1)
  data2 <- sample(seq(.4,.55,.01), size = 1)
  data3 <- sample(seq(.3,.45,.01), size = 1)
  corr.ans <- round(pnorm(round((data3-data2)/sqrt(data2*(1-data2)/data1), digits = digits)), digits = digits)
  up.min <- round(corr.ans + .05, digits)
  down.max <- round(corr.ans - .05, digits)
  ans.txt <- sample(if(corr.ans <= .05){seq(up.min, 1 + 10^-digits, 10^-digits)}
                    else{if(corr.ans >= .95){seq(0 - 10^-digits, down.max, 10^-digits)}
                      else{c(seq(0 - 10^-digits, down.max, 10^-digits),
                             seq(up.min, 1 + 10^-digits, 10^-digits))}},
                    size = answers)
  content <- c(type, ID, ID, paste(quest.txt1, data1, quest.txt2,
                                   data2, quest.txt3, data3, quest.txt4,
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

##### PropDistMC2 #####
title <- "PropDistMC2"
n = 200
type <- "MC"
answers <- 5
points.per.q <- 4
difficulty <- 1
quest.txt1 <- "A student conducts a survey for her class project. She asks "
quest.txt2 <- " other students a series of questions about their study habits. She knows from previous research that "
quest.txt3 <- " of all students study for 4 or more hours per day. If "
quest.txt4 <- " of her own sample study for 4 or more hours per day, what is the probability of observing this sample proportion or greater?"
dat.size = 1
digits = 2
loc.path <- 
  e.path <- 
  hint <- "You'll need to correctly distinguish between sample and population when calculating Z for this question. Pick the closest answer."
feedback <- "Z = (phat - p)/sqrt(pq/n). The probability can be found by applying the rule for complements to the probability found in the Z table."
param <- c("NewQuestion","ID","Title","QuestionText","Points","Difficulty",
           rep("Option", answers),"Hint","Feedback")
questions <- data.frame()
for(i in 1:n)
{
  ID <- paste(title, i, sep = "-")
  points <- sample(c(rep(0,answers-1),100),replace=F)
  corr.ind <- 6 + which.max(points)
  data1 <- sample(20:40, size = 1)
  data2 <- sample(seq(.4,.55,.01), size = 1)
  data3 <- sample(seq(.3,.45,.01), size = 1)
  corr.ans <- round(pnorm(round((data3-data2)/sqrt(data2*(1-data2)/data1), digits = digits), lower.tail = F), digits = digits)
  up.min <- round(corr.ans + .05, digits)
  down.max <- round(corr.ans - .05, digits)
  ans.txt <- sample(if(corr.ans <= .05){seq(up.min, 1 + 10^-digits, 10^-digits)}
                    else{if(corr.ans >= .95){seq(0 - 10^-digits, down.max, 10^-digits)}
                      else{c(seq(0 - 10^-digits, down.max, 10^-digits),
                             seq(up.min, 1 + 10^-digits, 10^-digits))}},
                    size = answers)
  content <- c(type, ID, ID, paste(quest.txt1, data1, quest.txt2,
                                   data2, quest.txt3, data3, quest.txt4,
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

##### PropCIMC1 #####
title <- "PropCIMC1"
n = 200
type <- "MC"
answers <- 5
points.per.q <- 4
difficulty <- 1
quest.txt1 <- "A student conducts a survey for her class project. She asks "
quest.txt2 <- " other students a series of questions about their study habits. She knows from previous research that "
quest.txt3 <- " of all students study for 4 or more hours per day. If "
quest.txt4 <- " of her own sample study for 4 or more hours per day, what is the 95% confidence interval for the true population proportion?"
dat.size = 1
digits = 2
loc.path <- 
  e.path <- 
  hint <- "You'll need to calculate a confidence interval for a single proportion. Pick the closest answer."
feedback <- "Did you use phat - 1.96*sqrt(phat*qhat/n), and phat + 1.96*sqrt(phat*qhat/n)?"
param <- c("NewQuestion","ID","Title","QuestionText","Points","Difficulty",
           rep("Option", answers),"Hint","Feedback")
questions <- data.frame()
for(i in 1:n)
{
  ID <- paste(title, i, sep = "-")
  points <- sample(c(rep(0,answers-1),100),replace=F)
  corr.ind <- 6 + which.max(points)
  data1 <- sample(20:40, size = 1)
  data2 <- sample(seq(.4,.55,.01), size = 1)
  data3 <- sample(seq(.3,.45,.01), size = 1)
  lb <- round(data3 - 1.96*sqrt(data3*(1-data3)/data1), digits = digits)
  ub <- round(data3 + 1.96*sqrt(data3*(1-data3)/data1), digits = digits)
  corr.ans <- paste("(", paste(lb, ub, sep = ", "), ")", sep = "")
  up.min <- round(ub + .05, digits)
  down.max <- round(lb - .05, digits)
  seq1 <- seq(up.min, 1 + 10^-digits, 10^-digits)
  mat1 <- expand.grid(seq1,seq1)
  mat1 <- mat1[mat1[,1] < mat1[,2], ]
  seq2 <- seq(0 - 10^-digits, down.max, 10^-digits)
  mat2 <- expand.grid(seq2,seq2)
  mat2 <- mat2[mat2[,1] < mat2[,2], ]
  seq3 <- c(seq(0 - 10^-digits, down.max, 10^-digits), seq(up.min, 1 + 10^-digits, 10^-digits))
  mat3 <- expand.grid(seq3,seq3)
  mat3 <- mat3[mat3[,1] < mat3[,2], ]
  if(lb <= .05){ans.txt1 <- mat1[sample(nrow(mat1), size = answers), ]}
                    else{if(ub >= .95){ans.txt1 <- mat2[sample(nrow(mat2), size = answers), ]}
                      else{ans.txt1 <- mat3[sample(nrow(mat3), size = answers), ]}}
  ans.txt <- c()
  for(l in 1:answers){ans.txt[l] <- paste("(", paste(ans.txt1[l,1], ans.txt1[l,2], sep = ", "), ")", sep = "")}
  content <- c(type, ID, ID, paste(quest.txt1, data1, quest.txt2,
                                   data2, quest.txt3, data3, quest.txt4,
                                   collapse = "", sep= ""),
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

##### PropCIMC2 #####
title <- "PropCIMC2"
n = 200
type <- "MC"
answers <- 2
points.per.q <- 4
difficulty <- 1
quest.txt1 <- "A student conducts a survey for her class project. She asks "
quest.txt2 <- " other students a series of questions about their study habits. She knows from previous research that "
quest.txt3 <- " of all students study for 4 or more hours per day. Based on her sample data, she calculates a 95% confidence interval of "
quest.txt4 <- ". Was her confidence interval successful?"
dat.size = 1
digits = 2
loc.path <- 
  e.path <- 
  hint <- "This is an interpretive question. No calculation is necessary. Consider each bound inclusive."
feedback <- "Does the CI contain the true population proportion?"
param <- c("NewQuestion","ID","Title","QuestionText","Points","Difficulty",
           rep("Option", answers),"Hint","Feedback")
questions <- data.frame()
for(i in 1:n)
{
  ID <- paste(title, i, sep = "-")
  points <- sample(c(rep(0,answers-1),100),replace=F)
  corr.ind <- 6 + which.max(points)
  data1 <- sample(20:40, size = 1)
  data2 <- sample(seq(.4,.6,.01), size = 1)
  sampprop <- sample(seq(.1,.9,.01), size = 1)
  lb <- sampprop - 1.96*sqrt(sampprop*(1-sampprop)/data1)
  ub <- sampprop + 1.96*sqrt(sampprop*(1-sampprop)/data1)
  data3 <- paste("(", paste(round(lb, digits = digits),
                            round(ub, digits = digits),
                            sep = ", "), ")",
                 sep = "")
  corr.ans <- if((data2 <= ub) & (data2 >= lb)){"Yes"}else{"No"}
  ans.txt <- c(if(corr.ans == "Yes"){rep("No", 2)}else{rep("Yes", 2)})
  content <- c(type, ID, ID, paste(quest.txt1, data1, quest.txt2,
                                   data2, quest.txt3, data3, quest.txt4,
                                   collapse = "", sep= ""),
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

##### 2PropPESEMC1 #####
title <- "2PropPESEMC1"
n = 200
type <- "MC"
answers <- 4
points.per.q <- 4
difficulty <- 1
quest.txt1 <- "A new pesticide is tested on a group of crop-destroying beetles. The sample data shows that "
quest.txt2 <- " of this first group dies as a result. A second group of beetles is dosed with a standard pesticide, and "
quest.txt3 <- " of this second group dies as a result. "
quest.txt4 <- " beetles are in the first test-pesticide group and "
quest.txt5 <- " beetles are in the second standard-pesticide group. What is the "
quest.txt6 <- " of the difference between proportions in the test group vs. the standard group?"
dat.size = 1
digits = 2
loc.path <- 
  e.path <- 
  hint <- "Note that this questions concerns the DIFFERENCE between two proporitons."
feedback <- "The difference is p1 - p2. The standard error is sqrt(p1(1-p1)/n1 + p2(1-p2)/n2)"
param <- c("NewQuestion","ID","Title","QuestionText","Points","Difficulty",
           rep("Option", answers),"Hint","Feedback")
questions <- data.frame()
for(i in 1:n)
{
  ID <- paste(title, i, sep = "-")
  points <- sample(c(rep(0,answers-1),100),replace=F)
  corr.ind <- 6 + which.max(points)
  data1 <- sample(seq(.6, .7, 10^-digits), size = 1)
  data2 <- sample(seq(.5,.59, 10^-digits), size = 1)
  data3 <- sample(50:75, size = 1)
  data4 <- sample(50:75, size = 1)
  data5 <- sample(c("point estimate", "standard error"), size = 1)
  corr.ans <- if(data5 == "point estimate"){data1-data2}else{sqrt(data1*(1-data1)/data3 + data2*(1-data2)/data4)}
  up.min <- round(corr.ans + .03, digits)
  down.max <- round(corr.ans - .03, digits)
  ans.txt <- sample(if(corr.ans <= .04){seq(up.min, .2, 10^-digits)}
                    else{if(corr.ans >= .17){seq(.01, down.max, 10^-digits)}
                      else{c(seq(10^-digits, down.max, 10^-digits),
                             seq(up.min, .3, 10^-digits))}},
                    size = answers)
  content <- c(type, ID, ID, paste(quest.txt1, data1, quest.txt2,
                                   data2, quest.txt3, data3, quest.txt4,
                                   data4, quest.txt5, data5, quest.txt6,
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

##### 2PropCIMC1 #####
title <- "2PropCIMC1"
n = 200
type <- "MC"
answers <- 5
points.per.q <- 4
difficulty <- 1
quest.txt1 <- "A new pesticide is tested on a group of crop-destroying beetles. The sample data shows that "
quest.txt2 <- " of this first group dies as a result. A second group of beetles is dosed with a standard pesticide, and "
quest.txt3 <- " of this second group dies as a result. "
quest.txt4 <- " beetles are in the first test-pesticide group and "
quest.txt5 <- " beetles are in the second standard-pesticide group. What is the 95% CI for the difference between proportions in the test group vs. the standard group?"
dat.size = 1
digits = 2
loc.path <- 
  e.path <- 
  hint <- "You'll need to calculate a confidence interval for the difference between 2 independent proportions. Pick the closest answer."
feedback <- "Did you use phat1 - phat2 - 1.96*sqrt(phat1*qhat1/n1 + phat2*qhat2/n2), and phat1 - phat2 + 1.96*sqrt(phat*qhat/n1 + phat2*qhat2/n2)?"
param <- c("NewQuestion","ID","Title","QuestionText","Points","Difficulty",
           rep("Option", answers),"Hint","Feedback")
questions <- data.frame()
for(i in 1:n)
{
  ID <- paste(title, i, sep = "-")
  points <- sample(c(rep(0,answers-1),100),replace=F)
  corr.ind <- 6 + which.max(points)
  data1 <- sample(seq(.65, .8, 10^-digits), size = 1)
  data2 <- sample(seq(.3,.55, 10^-digits), size = 1)
  data3 <- sample(100:400, size = 1)
  data4 <- sample(100:400, size = 1)
  lb <- round(data1 - data2 - 1.96*sqrt(data1*(1-data1)/data3 + data2*(1-data2)/data4),
              digits = digits)
  ub <- round(data1 - data2 + 1.96*sqrt(data1*(1-data1)/data3 + data2*(1-data2)/data4),
              digits = digits)
  corr.ans <- paste("(", paste(lb, ub, sep = ", "), ")", sep = "")
  up.min <- round(ub + .05, digits)
  down.max <- round(lb - .05, digits)
  if(lb <= .05){
    seq1 <- seq(up.min, 1 + 10^-digits, 10^-digits)
    mat1 <- expand.grid(seq1,seq1)
    mat1 <- mat1[mat1[,1] < mat1[,2], ]
    ans.txt1 <- mat1[sample(nrow(mat1), size = answers), ]}
    else{if(ub >= .95){
    seq2 <- seq(0 - 10^-digits, down.max, 10^-digits)
    mat2 <- expand.grid(seq2,seq2)
    mat2 <- mat2[mat2[,1] < mat2[,2], ]
    ans.txt1 <- mat2[sample(nrow(mat2), size = answers), ]}
    else{
    seq3 <- c(seq(0 - 10^-digits, down.max, 10^-digits), seq(up.min, 1 + 10^-digits, 10^-digits))
    mat3 <- expand.grid(seq3,seq3)
    mat3 <- mat3[mat3[,1] < mat3[,2], ]
    ans.txt1 <- mat3[sample(nrow(mat3), size = answers), ]}}
  ans.txt <- c()
  for(l in 1:answers){ans.txt[l] <- paste("(", paste(ans.txt1[l,1], ans.txt1[l,2], sep = ", "), ")", sep = "")}
  content <- c(type, ID, ID, paste(quest.txt1, data1, quest.txt2,
                                   data2, quest.txt3, data3, quest.txt4,
                                   data4, quest.txt5,
                                   collapse = "", sep= ""),
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

##### 2PropCIMC2 #####
title <- "2PropCIMC2"
n = 200
type <- "MC"
answers <- 4
points.per.q <- 4
difficulty <- 1
quest.txt1 <- "A researcher compares proportions from two independent samples. She calculates a 95% confidence interval of "
quest.txt2 <- " for the difference between the two proportions. Which of the following statistical inferences should she make?"
dat.size = 1
digits = 2
loc.path <- 
  e.path <- 
  hint <- "This is an interpretive question. No calculation is necessary. Consider each bound inclusive."
feedback <- "Does the CI contain 0?"
param <- c("NewQuestion","ID","Title","QuestionText","Points","Difficulty",
           rep("Option", answers),"Hint","Feedback")
questions <- data.frame()
for(i in 1:n)
{
  ID <- paste(title, i, sep = "-")
  points <- sample(c(rep(0,answers-1),100),replace=F)
  corr.ind <- 6 + which.max(points)
  lb <- sample(seq(-.2, .11, 10^-digits), size = 1)
  ub <- sample(seq(lb + .03, .2), size = 1)
  data1 <- paste("(", paste(round(lb, digits = digits),
                            round(ub, digits = digits),
                            sep = ", "), ")",
                 sep = "")
  corr.ans <- if((lb <= 0) & (ub >= 0)){"The difference between the proportions is insignificant at the 95% confidence level."}
              else{"The difference between the proportions is significant at the 95% confidence level."}
  ans.txt <- if(corr.ans == "The difference between the proportions is insignificant at the 95% confidence level.")
               {c("The difference between the proportions is significant at the 95% confidence level.",
                  "The difference between the sample proportions is not contained within the confidence interval.",
                  "Zero is not contained within the confidence interval.",
                  "The result is significant, thus the p-value is less than 0.05.")}
             else{c("The difference between the proportions is insignificant at the 95% confidence level.",
                    "The difference between the sample proportions is contained within the confidence interval.",
                    "Zero is contained within the confidence interval.",
                    "The result is insignificant, thus the p-value is greater than 0.05.")}
  content <- c(type, ID, ID, paste(quest.txt1, data1, quest.txt2,
                                   collapse = "", sep= ""),
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

##### RRPESEMC1 #####
library(gridExtra)
title <- "RRPESEMC1"
n = 200
type <- "MC"
answers <- 5
points.per.q <- 4
difficulty <- 1
quest.txt1 <- "Both a new medication and placebo were administered to two separate groups of subjects. The subjects were monitored for recovery from an illness and the results were recorded in the 2x2 contingency table above. What is the "
quest.txt2 <- " for recovery in subjects who received the medication vs. those who received the placebo?"
Treatments <- c("Medication", "Placebo")
digits = 2
loc.path <- "Images/"
e.path <- "Images/"
hint <- "You can follow your a, b, c, d formula given in lectures and the coursepack."
feedback <- "RR = (a/(a+b))/(c/(c+d)). SE = sqrt(1/a + 1/b - 1/(a+b) - 1/(c+d))"
param <- c("NewQuestion","ID","Title","QuestionText","Points","Difficulty", "Image",
           rep("Option", answers),"Hint","Feedback")
questions <- data.frame()
for(i in 1:n)
{
  ID <- paste(title, i, sep = "-")
  points <- sample(c(rep(0,answers-1),100),replace=F)
  corr.ind <- 7 + which.max(points)
  data1 <- sample(c("risk ratio", "standard error of the natural log of the risk ratio"), size = 1)
  Recovered <- c(sample(50:100, size = 1), sample(25:75, size = 1))
  Unrecovered <- sample(100:200, size = 2)
  data <- data.frame(Treatments, Recovered, Unrecovered, stringsAsFactors = FALSE)
  corr.ans <- if(data1 == "risk ratio"){round(data[1,2]/(data[1,2]+data[1,3])/(data[2,2]/(data[2,2]+data[2,3])),
                                              digits = digits)}
              else{round(sqrt(1/data[1,2] + 1/data[2,2]- 1/(data[1,2]+data[1,3]) - 1/(data[2,2]+data[2,3])),
                         digits = digits)}
  up.min <- corr.ans + .08
  down.max <- corr.ans - .08
  ans.txt <- sample(if(corr.ans <= .1){seq(up.min, 1, 10^-digits)}
                    else{if(corr.ans >= 1.3){seq(.01, down.max, 10^-digits)}
                      else{c(seq(10^-digits, down.max, 10^-digits),
                             seq(up.min, up.min + 2.2, 10^-digits))}},
                    size = answers)
  content <- c(type, ID, ID, paste(quest.txt1, data1, quest.txt2, sep = ""),
               points.per.q, difficulty, paste(e.path, paste(title, i, sep = "-"), ".jpeg", sep = ""),
               points, hint, feedback)
  options <- c(rep("",7), ans.txt, rep("",2))
  options[corr.ind] <- corr.ans
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),1] <- param
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),2] <- content
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),3] <- options
  jpeg(filename=paste(loc.path, paste(title, i, sep = "-"), ".jpeg", sep = ""),
       height = 30*nrow(data), width = 81*ncol(data))
  p <- tableGrob(data)
  grid.arrange(p)
  dev.off()
}
questions <- questions[((10+answers)):((9+answers)*(n+1)),]
write.table(questions, sep=",", file=paste(title, ".csv", sep = ""),
            row.names=F, col.names=F)

##### ORPESEMC1 #####
library(gridExtra)
title <- "ORPESEMC1"
n = 200
type <- "MC"
answers <- 5
points.per.q <- 4
difficulty <- 1
quest.txt1 <- "Both a new medication and placebo were administered to two separate groups of subjects. The subjects were monitored for recovery from an illness and the results were recorded in the 2x2 contingency table above. What is the "
quest.txt2 <- " for recovery in subjects who received the medication vs. those who received the placebo?"
Treatments <- c("Medication", "Placebo")
digits = 2
loc.path <- "Images/"
e.path <- "Images/"
hint <- "You can follow your a, b, c, d formula given in lectures and the coursepack."
feedback <- "OR = ad/(bc). SE = sqrt(1/a + 1/b + 1/c + 1/d)"
param <- c("NewQuestion","ID","Title","QuestionText","Points","Difficulty", "Image",
           rep("Option", answers),"Hint","Feedback")
questions <- data.frame()
for(i in 1:n)
{
  ID <- paste(title, i, sep = "-")
  points <- sample(c(rep(0,answers-1),100),replace=F)
  corr.ind <- 7 + which.max(points)
  data1 <- sample(c("odds ratio", "standard error of the natural log of the odds ratio"), size = 1)
  Recovered <- c(sample(50:100, size = 1), sample(25:75, size = 1))
  Unrecovered <- sample(100:200, size = 2)
  data <- data.frame(Treatments, Recovered, Unrecovered, stringsAsFactors = FALSE)
  corr.ans <- if(data1 == "odds ratio"){round(data[1,2]*data[2,3]/(data[1,3]*data[2,2]),
                                              digits = digits)}
  else{round(sqrt(1/data[1,2] + 1/data[2,2] + 1/data[1,3] + 1/data[2,3]),
             digits = digits)}
  up.min <- corr.ans + .08
  down.max <- corr.ans - .08
  ans.txt <- sample(if(corr.ans <= .1){seq(up.min, 1, 10^-digits)}
                    else{if(corr.ans >= 1.3){seq(.01, down.max, 10^-digits)}
                      else{c(seq(10^-digits, down.max, 10^-digits),
                             seq(up.min, up.min + 2.2, 10^-digits))}},
                    size = answers)
  content <- c(type, ID, ID, paste(quest.txt1, data1, quest.txt2, sep = ""),
               points.per.q, difficulty, paste(e.path, paste(title, i, sep = "-"), ".jpeg", sep = ""),
               points, hint, feedback)
  options <- c(rep("",7), ans.txt, rep("",2))
  options[corr.ind] <- corr.ans
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),1] <- param
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),2] <- content
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),3] <- options
  jpeg(filename=paste(loc.path, paste(title, i, sep = "-"), ".jpeg", sep = ""),
       height = 30*nrow(data), width = 81*ncol(data))
  p <- tableGrob(data)
  grid.arrange(p)
  dev.off()
}
questions <- questions[((10+answers)):((9+answers)*(n+1)),]
write.table(questions, sep=",", file=paste(title, ".csv", sep = ""),
            row.names=F, col.names=F)

##### ORCIMC1 #####
library(gridExtra)
title <- "ORCIMC1"
n = 200
type <- "MC"
answers <- 5
points.per.q <- 4
difficulty <- 1
quest.txt1 <- "Both a new medication and placebo were administered to two separate groups of subjects. The subjects were monitored for recovery from an illness and the results were recorded in the 2x2 contingency table above. What is the 95% CI for the odds ratio given recovery in subjects who received the medication vs. those who received the placebo?"
Treatments <- c("Medication", "Placebo")
digits = 2
loc.path <- "Images/"
e.path <- "Images/"
hint <- "You can follow your a, b, c, d formula given in lectures and the coursepack."
feedback <- "OR = ad/(bc). SE = sqrt(1/a + 1/b + 1/c + 1/d). ME for ln(OR) = 1.96*SE."
param <- c("NewQuestion","ID","Title","QuestionText","Points","Difficulty", "Image",
           rep("Option", answers),"Hint","Feedback")
questions <- data.frame()
for(i in 1:n)
{
  ID <- paste(title, i, sep = "-")
  points <- sample(c(rep(0,answers-1),100),replace=F)
  corr.ind <- 7 + which.max(points)
  Recovered <- c(sample(50:100, size = 1), sample(25:75, size = 1))
  Unrecovered <- sample(100:200, size = 2)
  data <- data.frame(Treatments, Recovered, Unrecovered, stringsAsFactors = FALSE)
  lb <- round(exp(log(data[1,2]*data[2,3]/(data[1,3]*data[2,2])) - 1.96*sqrt(1/data[1,2] + 1/data[2,2] + 1/data[1,3] + 1/data[2,3])),
                digits = digits)
  ub <- round(exp(log(data[1,2]*data[2,3]/(data[1,3]*data[2,2])) + 1.96*sqrt(1/data[1,2] + 1/data[2,2] + 1/data[1,3] + 1/data[2,3])),
              digits = digits)
  corr.ans <- paste("(", paste(lb, ub, sep = ", "), ")", sep = "")
  up.min <- round(ub + .08, digits)
  down.max <- round(lb - .08, digits)
  if(lb <= .4){
    seq1 <- seq(up.min, 5 + 10^-digits, 10^-digits)
    mat1 <- expand.grid(seq1,seq1)
    mat1 <- mat1[mat1[,1] < mat1[,2], ]
    ans.txt1 <- mat1[sample(nrow(mat1), size = answers), ]}
  else{if(ub >= 3.2){
    seq2 <- seq(-0.1 - 10^-digits, down.max, 10^-digits)
    mat2 <- expand.grid(seq2,seq2)
    mat2 <- mat2[mat2[,1] < mat2[,2], ]
    ans.txt1 <- mat2[sample(nrow(mat2), size = answers), ]}
    else{
      seq3 <- c(seq(-0.1 - 10^-digits, down.max, 10^-digits), seq(up.min, 5 + 10^-digits, 10^-digits))
      mat3 <- expand.grid(seq3,seq3)
      mat3 <- mat3[mat3[,1] < mat3[,2], ]
      ans.txt1 <- mat3[sample(nrow(mat3), size = answers), ]}}
  ans.txt <- c()
  for(l in 1:answers){ans.txt[l] <- paste("(", paste(ans.txt1[l,1], ans.txt1[l,2], sep = ", "), ")", sep = "")}
  content <- c(type, ID, ID, quest.txt1,
               points.per.q, difficulty, paste(e.path, paste(title, i, sep = "-"), ".jpeg", sep = ""),
               points, hint, feedback)
  options <- c(rep("",7), ans.txt, rep("",2))
  options[corr.ind] <- corr.ans
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),1] <- param
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),2] <- content
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),3] <- options
  jpeg(filename=paste(loc.path, paste(title, i, sep = "-"), ".jpeg", sep = ""),
       height = 30*nrow(data), width = 81*ncol(data))
  p <- tableGrob(data)
  grid.arrange(p)
  dev.off()
}
questions <- questions[((10+answers)):((9+answers)*(n+1)),]
write.table(questions, sep=",", file=paste(title, ".csv", sep = ""),
            row.names=F, col.names=F)

##### ORCIMC2 #####
title <- "ORCIMC2"
n = 200
type <- "MC"
answers <- 4
points.per.q <- 4
difficulty <- 1
quest.txt1 <- "A researcher compares odds from two independent samples. She calculates a 95% confidence interval of "
quest.txt2 <- " for the odds ratio between the two samples. Which of the following statistical inferences should she make?"
dat.size = 1
digits = 2
loc.path <- 
  e.path <- 
  hint <- "This is an interpretive question. No calculation is necessary. Consider each bound inclusive."
feedback <- "Does the CI contain 1?"
param <- c("NewQuestion","ID","Title","QuestionText","Points","Difficulty",
           rep("Option", answers),"Hint","Feedback")
questions <- data.frame()
for(i in 1:n)
{
  ID <- paste(title, i, sep = "-")
  points <- sample(c(rep(0,answers-1),100),replace=F)
  corr.ind <- 6 + which.max(points)
  lb <- sample(seq(.1, 1.5, 10^-digits), size = 1)
  ub <- sample(seq(lb + .05, lb + 2), size = 1)
  data1 <- paste("(", paste(round(lb, digits = digits),
                            round(ub, digits = digits),
                            sep = ", "), ")",
                 sep = "")
  corr.ans <- if((lb <= 1) & (ub >= 1)){"The odds ratio is insignificant at the 95% confidence level."}
  else{"The odds ratio is significant at the 95% confidence level."}
  ans.txt <- if(corr.ans == "The odds ratio is insignificant at the 95% confidence level.")
  {c("The odds ratio is significant at the 95% confidence level.",
     "The sample odds ratio is not contained within the confidence interval.",
     "One is not contained within the confidence interval.",
     "The result is significant, thus the p-value is less than 0.05.")}
  else{c("The odds ratio is insignificant at the 95% confidence level.",
         "The sample odds ratio is contained within the confidence interval.",
         "One is contained within the confidence interval.",
         "The result is insignificant, thus the p-value is greater than 0.05.")}
  content <- c(type, ID, ID, paste(quest.txt1, data1, quest.txt2,
                                   collapse = "", sep= ""),
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

##### RRCIMC1 #####
library(gridExtra)
title <- "RRCIMC1"
n = 200
type <- "MC"
answers <- 5
points.per.q <- 4
difficulty <- 1
quest.txt1 <- "Both a new medication and placebo were administered to two separate groups of subjects. The subjects were monitored for recovery from an illness and the results were recorded in the 2x2 contingency table above. What is the 95% CI for the risk ratio given recovery in subjects who received the medication vs. those who received the placebo?"
Treatments <- c("Medication", "Placebo")
digits = 2
loc.path <- "Images/"
e.path <- "Images/"
hint <- "You can follow your a, b, c, d formula given in lectures and the coursepack."
feedback <- "RR = a/(a+b)/(c/(c+d)). SE = sqrt(1/a + 1/c - 1/(a+b) - 1/(c+d)). ME for ln(RR) = 1.96*SE."
param <- c("NewQuestion","ID","Title","QuestionText","Points","Difficulty", "Image",
           rep("Option", answers),"Hint","Feedback")
questions <- data.frame()
for(i in 1:n)
{
  ID <- paste(title, i, sep = "-")
  points <- sample(c(rep(0,answers-1),100),replace=F)
  corr.ind <- 7 + which.max(points)
  Recovered <- c(sample(50:100, size = 1), sample(25:75, size = 1))
  Unrecovered <- sample(100:200, size = 2)
  data <- data.frame(Treatments, Recovered, Unrecovered, stringsAsFactors = FALSE)
  lb <- round(exp(log(data[1,2]/(data[1,2]+data[1,3])/(data[2,2]/(data[2,2]+data[2,3]))) - 1.96*sqrt(1/data[1,2] + 1/data[2,2]- 1/(data[1,2]+data[1,3]) - 1/(data[2,2]+data[2,3]))),
              digits = digits)
  ub <- round(exp(log(data[1,2]/(data[1,2]+data[1,3])/(data[2,2]/(data[2,2]+data[2,3]))) + 1.96*sqrt(1/data[1,2] + 1/data[2,2]- 1/(data[1,2]+data[1,3]) - 1/(data[2,2]+data[2,3]))),
              digits = digits)
  corr.ans <- paste("(", paste(lb, ub, sep = ", "), ")", sep = "")
  up.min <- round(ub + .08, digits)
  down.max <- round(lb - .08, digits)
  if(lb <= .4){
    seq1 <- seq(up.min, 5 + 10^-digits, 10^-digits)
    mat1 <- expand.grid(seq1,seq1)
    mat1 <- mat1[mat1[,1] < mat1[,2], ]
    ans.txt1 <- mat1[sample(nrow(mat1), size = answers), ]}
  else{if(ub >= 3.2){
    seq2 <- seq(-0.1 - 10^-digits, down.max, 10^-digits)
    mat2 <- expand.grid(seq2,seq2)
    mat2 <- mat2[mat2[,1] < mat2[,2], ]
    ans.txt1 <- mat2[sample(nrow(mat2), size = answers), ]}
    else{
      seq3 <- c(seq(-0.1 - 10^-digits, down.max, 10^-digits), seq(up.min, 5 + 10^-digits, 10^-digits))
      mat3 <- expand.grid(seq3,seq3)
      mat3 <- mat3[mat3[,1] < mat3[,2], ]
      ans.txt1 <- mat3[sample(nrow(mat3), size = answers), ]}}
  ans.txt <- c()
  for(l in 1:answers){ans.txt[l] <- paste("(", paste(ans.txt1[l,1], ans.txt1[l,2], sep = ", "), ")", sep = "")}
  content <- c(type, ID, ID, quest.txt1,
               points.per.q, difficulty, paste(e.path, paste(title, i, sep = "-"), ".jpeg", sep = ""),
               points, hint, feedback)
  options <- c(rep("",7), ans.txt, rep("",2))
  options[corr.ind] <- corr.ans
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),1] <- param
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),2] <- content
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),3] <- options
  jpeg(filename=paste(loc.path, paste(title, i, sep = "-"), ".jpeg", sep = ""),
       height = 30*nrow(data), width = 81*ncol(data))
  p <- tableGrob(data)
  grid.arrange(p)
  dev.off()
}
questions <- questions[((10+answers)):((9+answers)*(n+1)),]
write.table(questions, sep=",", file=paste(title, ".csv", sep = ""),
            row.names=F, col.names=F)

##### RRCIMC2 #####
title <- "RRCIMC2"
n = 200
type <- "MC"
answers <- 4
points.per.q <- 4
difficulty <- 1
quest.txt1 <- "A researcher compares risk using two independent samples. She calculates a 95% confidence interval of "
quest.txt2 <- " for the risk ratio between the two samples. Which of the following statistical inferences should she make?"
dat.size = 1
digits = 2
loc.path <- 
  e.path <- 
  hint <- "This is an interpretive question. No calculation is necessary. Consider each bound inclusive."
feedback <- "Does the CI contain 1?"
param <- c("NewQuestion","ID","Title","QuestionText","Points","Difficulty",
           rep("Option", answers),"Hint","Feedback")
questions <- data.frame()
for(i in 1:n)
{
  ID <- paste(title, i, sep = "-")
  points <- sample(c(rep(0,answers-1),100),replace=F)
  corr.ind <- 6 + which.max(points)
  lb <- sample(seq(.1, 1.5, 10^-digits), size = 1)
  ub <- sample(seq(lb + .05, lb + 2), size = 1)
  data1 <- paste("(", paste(round(lb, digits = digits),
                            round(ub, digits = digits),
                            sep = ", "), ")",
                 sep = "")
  corr.ans <- if((lb <= 1) & (ub >= 1)){"The risk ratio is insignificant at the 95% confidence level."}
  else{"The risk ratio is significant at the 95% confidence level."}
  ans.txt <- if(corr.ans == "The risk ratio is insignificant at the 95% confidence level.")
  {c("The risk ratio is significant at the 95% confidence level.",
     "The sample risk ratio is not contained within the confidence interval.",
     "One is not contained within the confidence interval.",
     "The result is significant, thus the p-value is less than 0.05.")}
  else{c("The risk ratio is insignificant at the 95% confidence level.",
         "The sample risk ratio is contained within the confidence interval.",
         "One is contained within the confidence interval.",
         "The result is insignificant, thus the p-value is greater than 0.05.")}
  content <- c(type, ID, ID, paste(quest.txt1, data1, quest.txt2,
                                   collapse = "", sep= ""),
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

########## Test 3 ##########

##### MeanDistMC1 #####
title <- "MeanDistMC1"
n = 200
type <- "MC"
answers <- 5
points.per.q <- 4
difficulty <- 1
quest.txt1 <- "A student conducts a survey for her class project. She asks "
quest.txt2 <- " other students a series of questions about their study habits. She knows from previous research that all students study an average of "
quest.txt3 <- " hours per day. If students in her own sample study an average of "
quest.txt4 <- " with a standard deviation of "
quest.txt5 <- " hours per day, then what is the probability of observing this sample mean or less?"
dat.size = 1
digits = 2
loc.path <- 
  e.path <- 
  hint <- "You'll need to correctly distinguish between sample and population when calculating Z for this question. Pick the closest answer."
feedback <- "Z = (xbar - mu)/(s/sqrt(n)). The probability can be found directly from the Z table."
param <- c("NewQuestion","ID","Title","QuestionText","Points","Difficulty",
           rep("Option", answers),"Hint","Feedback")
questions <- data.frame()
for(i in 1:n)
{
  ID <- paste(title, i, sep = "-")
  points <- sample(c(rep(0,answers-1),100),replace=F)
  corr.ind <- 6 + which.max(points)
  data1 <- sample(5:8, size = 1)
  data2 <- sample(seq(2,4,10^-digits), size = 1)
  data3 <- sample(seq(2,4,10^-digits), size = 1)
  data4 <- sample(seq(1.8,3,10^-digits), size = 1)
  corr.ans <- round(pnorm(round((data3-data2)/(data4/sqrt(data1)), digits = digits)), digits = digits)
  up.min <- round(corr.ans + .05, digits)
  down.max <- round(corr.ans - .05, digits)
  ans.txt <- sample(if(corr.ans <= .05){seq(up.min, 1 + 10^-digits, 10^-digits)}
                    else{if(corr.ans >= .95){seq(0 - 10^-digits, down.max, 10^-digits)}
                      else{c(seq(0 - 10^-digits, down.max, 10^-digits),
                             seq(up.min, 1 + 10^-digits, 10^-digits))}},
                    size = answers)
  content <- c(type, ID, ID, paste(quest.txt1, data1, quest.txt2,
                                   data2, quest.txt3, data3, quest.txt4,
                                   data4, quest.txt5,
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

##### MeanDistMC2 #####
title <- "MeanDistMC2"
n = 200
type <- "MC"
answers <- 5
points.per.q <- 4
difficulty <- 1
quest.txt1 <- "A student conducts a survey for her class project. She asks "
quest.txt2 <- " other students a series of questions about their study habits. She knows from previous research that all students study an average of "
quest.txt3 <- " hours per day. If students in her own sample study an average of "
quest.txt4 <- " with a standard deviation of "
quest.txt5 <- " hours per day, then what is the probability of observing this sample mean or greater?"
dat.size = 1
digits = 2
loc.path <- 
  e.path <- 
  hint <- "You'll need to correctly distinguish between sample and population when calculating Z for this question. Pick the closest answer."
feedback <- "Z = (xbar - mu)/(s/sqrt(n)). The probability can be found directly from the Z table."
param <- c("NewQuestion","ID","Title","QuestionText","Points","Difficulty",
           rep("Option", answers),"Hint","Feedback")
questions <- data.frame()
for(i in 1:n)
{
  ID <- paste(title, i, sep = "-")
  points <- sample(c(rep(0,answers-1),100),replace=F)
  corr.ind <- 6 + which.max(points)
  data1 <- sample(5:8, size = 1)
  data2 <- sample(seq(2,4,10^-digits), size = 1)
  data3 <- sample(seq(2,4,10^-digits), size = 1)
  data4 <- sample(seq(1.8,3,10^-digits), size = 1)
  corr.ans <- 1-round(pnorm(round((data3-data2)/(data4/sqrt(data1)), digits = digits)), digits = digits)
  up.min <- round(corr.ans + .05, digits)
  down.max <- round(corr.ans - .05, digits)
  ans.txt <- sample(if(corr.ans <= .05){seq(up.min, 1 + 10^-digits, 10^-digits)}
                    else{if(corr.ans >= .95){seq(0 - 10^-digits, down.max, 10^-digits)}
                      else{c(seq(0 - 10^-digits, down.max, 10^-digits),
                             seq(up.min, 1 + 10^-digits, 10^-digits))}},
                    size = answers)
  content <- c(type, ID, ID, paste(quest.txt1, data1, quest.txt2,
                                   data2, quest.txt3, data3, quest.txt4,
                                   data4, quest.txt5,
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

##### MeanCIMC1 #####
title <- "MeanCIMC1"
n = 200
type <- "MC"
answers <- 5
points.per.q <- 4
difficulty <- 1
quest.txt1 <- "The CDC collects biometric data on the US population. A sample of "
quest.txt2 <- " US citizens' heights are measured. The average height of the sample is "
quest.txt3 <- " inches and its standard deviation is "
quest.txt4 <- " inches. What is the 95% confidence interval for the average height of all US citizens?"
dat.size = 1
digits = 2
loc.path <- 
  e.path <- 
  hint <- "Use your confidence interval formula for one sample mean questions. Pick the closest answer."
feedback <- "Did you use xbar - 1.96*s/sqrt(n), and xbar + 1.96*s/sqrt(n)?"
param <- c("NewQuestion","ID","Title","QuestionText","Points","Difficulty",
           rep("Option", answers),"Hint","Feedback")
questions <- data.frame()
for(i in 1:n)
{
  ID <- paste(title, i, sep = "-")
  points <- sample(c(rep(0,answers-1),100),replace=F)
  corr.ind <- 6 + which.max(points)
  data1 <- sample(seq(20,40), size = 1)
  data2 <- sample(seq(54,66), size = 1)
  data3 <- sample(seq(3,18), size = 1)
  lb <- round(data2 - 1.96*data3/sqrt(data1), digits = digits)
  ub <- round(data2 + 1.96*data3/sqrt(data1), digits = digits)
  corr.ans <- paste("(", paste(lb, ub, sep = ", "), ")", sep = "")
  up.min <- round(ub + .2, digits)
  down.max <- round(lb - .2, digits)
  seq1 <- seq(up.min, up.min + 12, 10^-digits)
  mat1 <- expand.grid(seq1,seq1)
  mat1 <- mat1[mat1[,1] < mat1[,2], ]
  seq2 <- seq(down.max - 12, down.max, 10^-digits)
  mat2 <- expand.grid(seq2,seq2)
  mat2 <- mat2[mat2[,1] < mat2[,2], ]
  seq3 <- c(seq(down.max - 12, down.max, 10^-digits), seq(up.min, up.min + 12, 10^-digits))
  mat3 <- expand.grid(seq3,seq3)
  mat3 <- mat3[mat3[,1] < mat3[,2], ]
  if(lb <= 49){ans.txt1 <- mat1[sample(nrow(mat1), size = answers), ]}
  else{if(ub >= 70){ans.txt1 <- mat2[sample(nrow(mat2), size = answers), ]}
    else{ans.txt1 <- mat3[sample(nrow(mat3), size = answers), ]}}
  ans.txt <- c()
  for(l in 1:answers){ans.txt[l] <- paste("(", paste(ans.txt1[l,1], ans.txt1[l,2], sep = ", "), ")", sep = "")}
  content <- c(type, ID, ID, paste(quest.txt1, data1, quest.txt2,
                                   data2, quest.txt3, data3, quest.txt4,
                                   collapse = "", sep= ""),
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

##### MeanCIMC2 #####
title <- "MeanCIMC2"
n = 200
type <- "MC"
answers <- 2
points.per.q <- 4
difficulty <- 1
quest.txt1 <- "The CDC conducts biometric research on the US populaton. A sample of "
quest.txt2 <- " US citizens' heights is collected. Previous CDC research indicates that all US citizens have an average height of "
quest.txt3 <- ". Using the new sample, the CDC calculates a 95% confidence interval of "
quest.txt4 <- " for the height of all US citizens. Was the confidence interval successful?"
dat.size = 1
digits = 2
loc.path <- 
  e.path <- 
  hint <- "This is an interpretive question. No calculation is necessary. Consider each bound inclusive."
feedback <- "Does the CI contain the true population mean?"
param <- c("NewQuestion","ID","Title","QuestionText","Points","Difficulty",
           rep("Option", answers),"Hint","Feedback")
questions <- data.frame()
for(i in 1:n)
{
  ID <- paste(title, i, sep = "-")
  points <- sample(c(rep(0,answers-1),100),replace=F)
  corr.ind <- 6 + which.max(points)
  data1 <- sample(20:30, size = 1)
  data2 <- sample(seq(54, 66, 10^-digits), size = 1)
  samp <- sample(seq(54, 66, 10^-digits), size = 1)
  lb <- samp - 1.96*sample(6:10, size = 1)/sqrt(data1)
  ub <- samp + 1.96*sample(6:10, size = 1)/sqrt(data1)
  data3 <- paste("(", paste(round(lb, digits = digits),
                            round(ub, digits = digits),
                            sep = ", "), ")",
                 sep = "")
  corr.ans <- if((data2 <= ub) & (data2 >= lb)){"Yes"}else{"No"}
  ans.txt <- c(if(corr.ans == "Yes"){rep("No", 2)}else{rep("Yes", 2)})
  content <- c(type, ID, ID, paste(quest.txt1, data1, quest.txt2,
                                   data2, quest.txt3, data3, quest.txt4,
                                   collapse = "", sep= ""),
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

##### 2MeanCIMC1 #####
title <- "2MeanCIMC1"
n = 200
type <- "MC"
answers <- 5
points.per.q <- 4
difficulty <- 1
quest.txt1 <- "NASA compares the ages of stars in two distinct clusters. A sample from the first cluster has an average age of "
quest.txt2 <- " billion years with a standard deviation of "
quest.txt3 <- " billion years. A sample from the second cluster has an average age of "
quest.txt4 <- " billion years with a standard deviation of "
quest.txt5 <- " billion years. NASA samples "
quest.txt6 <- " stars from the first cluster, and "
quest.txt7 <- " stars from the second cluster. What is the 95% CI for the difference between mean ages of the first cluster vs. the second cluster?"
dat.size = 1
digits = 2
loc.path <- 
  e.path <- 
  hint <- "Is this a 1 or 2 sample mean CI? Are the groups independent or not? Pick the closest answer."
feedback <- "Did you use xbar1 - xbar2 +/- 1.96*sqrt((s1)^2/(n1) + (s2)^2/(n2))?"
param <- c("NewQuestion","ID","Title","QuestionText","Points","Difficulty",
           rep("Option", answers),"Hint","Feedback")
questions <- data.frame()
for(i in 1:n)
{
  ID <- paste(title, i, sep = "-")
  points <- sample(c(rep(0,answers-1),100),replace=F)
  corr.ind <- 6 + which.max(points)
  data1 <- sample(seq(2, 7, 10^-digits), size = 1)
  data2 <- sample(seq(.5, 3, 10^-digits), size = 1)
  data3 <- sample(seq(1, 6, 10^-digits), size = 1)
  data4 <- sample(seq(.5, 3, 10^-digits), size = 1)
  data5 <- sample(100:200, size = 1)
  data6 <- sample(100:200, size = 1)
  lb <- round(data1 - data3 - 1.96*sqrt(data2^2/data5 + data4^2/data6),
              digits = digits)
  ub <- round(data1 - data3 + 1.96*sqrt(data2^2/data5 + data4^2/data6),
              digits = digits)
  corr.ans <- paste("(", paste(lb, ub, sep = ", "), ")", sep = "")
  up.min <- round(ub + .1, digits)
  down.max <- round(lb - .1, digits)
  if(lb <= -4){
    seq1 <- round(seq(up.min, up.min + 3, 10^-digits), digits)
    mat1 <- expand.grid(seq1,seq1)
    mat1 <- mat1[mat1[,1] < mat1[,2], ]
    ans.txt1 <- mat1[sample(nrow(mat1), size = answers), ]}
  else{if(ub >= 6){
    seq2 <- round(seq(down.max - 3, down.max, 10^-digits), digits)
    mat2 <- expand.grid(seq2,seq2)
    mat2 <- mat2[mat2[,1] < mat2[,2], ]
    ans.txt1 <- mat2[sample(nrow(mat2), size = answers), ]}
    else{
      seq3 <- round(c(seq(down.max - 3, down.max, 10^-digits), seq(up.min, up.min + 3, 10^-digits)), digits)
      mat3 <- expand.grid(seq3,seq3)
      mat3 <- mat3[mat3[,1] < mat3[,2], ]
      ans.txt1 <- mat3[sample(nrow(mat3), size = answers), ]}}
  ans.txt <- c()
  for(l in 1:answers){ans.txt[l] <- paste("(", paste(ans.txt1[l,1], ans.txt1[l,2], sep = ", "), ")", sep = "")}
  content <- c(type, ID, ID, paste(quest.txt1, data1, quest.txt2,
                                   data2, quest.txt3, data3, quest.txt4,
                                   data4, quest.txt5, data5, quest.txt6,
                                   data6, quest.txt7,
                                   collapse = "", sep= ""),
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

##### 2MeanCIMC2 #####
title <- "2MeanCIMC2"
n = 200
type <- "MC"
answers <- 4
points.per.q <- 4
difficulty <- 1
quest.txt1 <- "A researcher compares means from two independent samples. She calculates a 95% confidence interval of "
quest.txt2 <- " for the difference between the two means. Which of the following statistical inferences should she make?"
dat.size = 1
digits = 2
loc.path <- 
  e.path <- 
  hint <- "This is an interpretive question. No calculation is necessary. Consider each bound inclusive."
feedback <- "Does the CI contain 0?"
param <- c("NewQuestion","ID","Title","QuestionText","Points","Difficulty",
           rep("Option", answers),"Hint","Feedback")
questions <- data.frame()
for(i in 1:n)
{
  ID <- paste(title, i, sep = "-")
  points <- sample(c(rep(0,answers-1),100),replace=F)
  corr.ind <- 6 + which.max(points)
  lb <- sample(seq(-.2, .11, 10^-digits), size = 1)
  ub <- sample(seq(lb + .03, .2), size = 1)
  data1 <- paste("(", paste(round(lb, digits = digits),
                            round(ub, digits = digits),
                            sep = ", "), ")",
                 sep = "")
  corr.ans <- if((lb <= 0) & (ub >= 0)){"The difference between the true means is insignificant at the 95% confidence level."}
  else{"The difference between the true means is significant at the 95% confidence level."}
  ans.txt <- if(corr.ans == "The difference between the true means is insignificant at the 95% confidence level.")
  {c("The difference between the true means is significant at the 95% confidence level.",
     "The difference between the sample means is not contained within the confidence interval.",
     "Zero is not contained within the confidence interval.",
     "The result is significant, thus the p-value is less than 0.05.")}
  else{c("The difference between the true means is insignificant at the 95% confidence level.",
         "The difference between the sample means is contained within the confidence interval.",
         "Zero is contained within the confidence interval.",
         "The result is insignificant, thus the p-value is greater than 0.05.")}
  content <- c(type, ID, ID, paste(quest.txt1, data1, quest.txt2,
                                   collapse = "", sep= ""),
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

##### 2MeanCIMC3 #####
title <- "2MeanCIMC3"
n = 200
type <- "MC"
answers <- 4
points.per.q <- 4
difficulty <- 1
quest.txt1 <- "A researcher compares means from two dependent samples. She calculates a 95% confidence interval of "
quest.txt2 <- " for the average difference. Which of the following statistical inferences should she make?"
dat.size = 1
digits = 2
loc.path <- 
  e.path <- 
  hint <- "This is an interpretive question. No calculation is necessary. Consider each bound inclusive."
feedback <- "Does the CI contain 0?"
param <- c("NewQuestion","ID","Title","QuestionText","Points","Difficulty",
           rep("Option", answers),"Hint","Feedback")
questions <- data.frame()
for(i in 1:n)
{
  ID <- paste(title, i, sep = "-")
  points <- sample(c(rep(0,answers-1),100),replace=F)
  corr.ind <- 6 + which.max(points)
  lb <- sample(seq(-.2, .11, 10^-digits), size = 1)
  ub <- sample(seq(lb + .03, .2), size = 1)
  data1 <- paste("(", paste(round(lb, digits = digits),
                            round(ub, digits = digits),
                            sep = ", "), ")",
                 sep = "")
  corr.ans <- if((lb <= 0) & (ub >= 0)){"The difference between the true means is insignificant at the 95% confidence level."}
  else{"The difference between the true means is significant at the 95% confidence level."}
  ans.txt <- if(corr.ans == "The difference between the true means is insignificant at the 95% confidence level.")
  {c("The difference between the true means is significant at the 95% confidence level.",
     "The difference between the sample means is not contained within the confidence interval.",
     "Zero is not contained within the confidence interval.",
     "The result is significant, thus the p-value is less than 0.05.")}
  else{c("The difference between the true means is insignificant at the 95% confidence level.",
         "The difference between the sample means is contained within the confidence interval.",
         "Zero is contained within the confidence interval.",
         "The result is insignificant, thus the p-value is greater than 0.05.")}
  content <- c(type, ID, ID, paste(quest.txt1, data1, quest.txt2,
                                   collapse = "", sep= ""),
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

##### 2MeanCIMC4 #####
title <- "2MeanCIMC4"
n = 200
type <- "MC"
answers <- 5
points.per.q <- 4
difficulty <- 1
quest.txt1 <- "The CDC collects biometric data on the US population. Two weight measurements (12 months apart) are taken on a sample of "
quest.txt2 <- " US citizens. The average difference of weights in the sample is "
quest.txt3 <- " lbs and its standard deviation is "
quest.txt4 <- " lbs. What is the 95% confidence interval for the average difference in weight of all US citizens?"
dat.size = 1
digits = 2
loc.path <- 
  e.path <- 
  hint <- "Are the two samples independent or dependent? Pick the closest answer."
feedback <- "Did you use Dbar - 1.96*s/sqrt(n), and Dbar + 1.96*s/sqrt(n)?"
param <- c("NewQuestion","ID","Title","QuestionText","Points","Difficulty",
           rep("Option", answers),"Hint","Feedback")
questions <- data.frame()
for(i in 1:n)
{
  ID <- paste(title, i, sep = "-")
  points <- sample(c(rep(0,answers-1),100),replace=F)
  corr.ind <- 6 + which.max(points)
  data1 <- sample(seq(20,40), size = 1)
  data2 <- sample(seq(-10, 10), size = 1)
  data3 <- sample(seq(4,9), size = 1)
  lb <- round(data2 - 1.96*data3/sqrt(data1), digits = digits)
  ub <- round(data2 + 1.96*data3/sqrt(data1), digits = digits)
  corr.ans <- paste("(", paste(lb, ub, sep = ", "), ")", sep = "")
  up.min <- round(ub + .2, digits)
  down.max <- round(lb - .2, digits)
  seq1 <- seq(up.min, up.min + 12, 10^-digits)
  mat1 <- expand.grid(seq1,seq1)
  mat1 <- mat1[mat1[,1] < mat1[,2], ]
  seq2 <- seq(down.max - 12, down.max, 10^-digits)
  mat2 <- expand.grid(seq2,seq2)
  mat2 <- mat2[mat2[,1] < mat2[,2], ]
  seq3 <- c(seq(down.max - 12, down.max, 10^-digits), seq(up.min, up.min + 12, 10^-digits))
  mat3 <- expand.grid(seq3,seq3)
  mat3 <- mat3[mat3[,1] < mat3[,2], ]
  if(lb <= 49){ans.txt1 <- mat1[sample(nrow(mat1), size = answers), ]}
  else{if(ub >= 70){ans.txt1 <- mat2[sample(nrow(mat2), size = answers), ]}
    else{ans.txt1 <- mat3[sample(nrow(mat3), size = answers), ]}}
  ans.txt <- c()
  for(l in 1:answers){ans.txt[l] <- paste("(", paste(ans.txt1[l,1], ans.txt1[l,2], sep = ", "), ")", sep = "")}
  content <- c(type, ID, ID, paste(quest.txt1, data1, quest.txt2,
                                   data2, quest.txt3, data3, quest.txt4,
                                   collapse = "", sep= ""),
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

##### 2MeanPESEMC1 #####
library(gridExtra)
title <- "2MeanPESEMC1"
n = 200
type <- "MC"
answers <- 5
points.per.q <- 4
difficulty <- 1
quest.txt1 <- "Both a new cardiovascular medication and placebo were administered to two separate groups of subjects. The subjects' heartrates (bpm) were monitored and recorded in the above table. The standard deviation of the medicated group is "
quest.txt2 <- ", and the standard deviation of the placebo group is "
quest.txt3 <- ". What is the "
quest.txt4 <- " of the difference between the mean heartrate of medication vs.the mean heartrate of placebo groups?"
digits = 2
loc.path <- "Images/"
e.path <- "Images/"
hint <- "Are the groups independent or dependent? Are you looking for center or spread? Pick the closest answer."
feedback <- "Point Estimate = mean(medication) - mean(placebo). SE = sqrt(smed^2/nmean + splac^2/nplac."
param <- c("NewQuestion","ID","Title","QuestionText","Points","Difficulty", "Image",
           rep("Option", answers),"Hint","Feedback")
questions <- data.frame()
for(i in 1:n)
{
  ID <- paste(title, i, sep = "-")
  points <- sample(c(rep(0,answers-1),100),replace=F)
  corr.ind <- 7 + which.max(points)
  decis <- sample(6:8, size = 1)
  Medication <- sample(80:140, size = decis)
  Placebo <- sample(90:160, size = decis)
  data <- data.frame(Medication, Placebo, stringsAsFactors = FALSE)
  data1 <- round(sd(Medication), digits)
  data2 <- round(sd(Placebo), digits)
  data3 <- sample(c("point estimate", "standard error"), size = 1)
  corr.ans <- if(data3 == "point estimate"){round(mean(Medication) - mean(Placebo),
                                              digits = digits)}
  else{round(sqrt(data1^2/decis + data2^2/decis),
             digits = digits)}
  up.min <- corr.ans + .2
  down.max <- corr.ans - .2
  ans.txt <- sample(if(corr.ans <= -15){seq(up.min, up.min + 5, 10^-digits)}
                    else{if(corr.ans >= 15){seq(down.max - 5, down.max, 10^-digits)}
                      else{c(seq(down.max - 5, down.max, 10^-digits),
                             seq(up.min, up.min + 5, 10^-digits))}},
                    size = answers)
  content <- c(type, ID, ID, paste(quest.txt1, data1, quest.txt2, data2, quest.txt3, data3, quest.txt4, sep = ""),
               points.per.q, difficulty, paste(e.path, paste(title, i, sep = "-"), ".jpeg", sep = ""),
               points, hint, feedback)
  options <- c(rep("",7), ans.txt, rep("",2))
  options[corr.ind] <- corr.ans
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),1] <- param
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),2] <- content
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),3] <- options
  jpeg(filename=paste(loc.path, paste(title, i, sep = "-"), ".jpeg", sep = ""),
       height = 25*nrow(data), width = 80*ncol(data))
  p <- tableGrob(data)
  grid.arrange(p)
  dev.off()
}
questions <- questions[((10+answers)):((9+answers)*(n+1)),]
write.table(questions, sep=",", file=paste(title, ".csv", sep = ""),
            row.names=F, col.names=F)

##### 2MeanPESEMC2 #####
library(gridExtra)
title <- "2MeanPESEMC2"
n = 200
type <- "MC"
answers <- 5
points.per.q <- 4
difficulty <- 1
quest.txt1 <- "A new cardiovascular medication was administered to a single group of subjects. The subjects' heartrates (bpm) were monitored and recorded twice: once before administering the medicine and once after. The standard deviation of the differences between heartrates at the two times was "
quest.txt2 <- ". What is the "
quest.txt3 <- " of the average difference in heartrates between the two times (Before - After)?"
digits = 2
loc.path <- "Images/"
e.path <- "Images/"
hint <- "Are the groups independent or dependent? Are you looking for center or spread? Pick the closest answer."
feedback <- "Point Estimate = mean(Before - After). SE = sd/sqrt(n)."
param <- c("NewQuestion","ID","Title","QuestionText","Points","Difficulty", "Image",
           rep("Option", answers),"Hint","Feedback")
questions <- data.frame()
for(i in 1:n)
{
  ID <- paste(title, i, sep = "-")
  points <- sample(c(rep(0,answers-1),100),replace=F)
  corr.ind <- 7 + which.max(points)
  decis <- sample(6:8, size = 1)
  Before <- sample(80:140, size = decis)
  After <- sample(90:160, size = decis)
  data <- data.frame(Before, After, stringsAsFactors = FALSE)
  data1 <- round(sd(Before - After), digits)
  data2 <- sample(c("point estimate", "standard error"), size = 1)
  corr.ans <- if(data2 == "point estimate"){round(mean(Before - After),
                                                  digits = digits)}
  else{round(data1/sqrt(decis),
             digits = digits)}
  up.min <- corr.ans + .2
  down.max <- corr.ans - .2
  ans.txt <- sample(if(corr.ans <= -15){seq(up.min, up.min + 5, 10^-digits)}
                    else{if(corr.ans >= 15){seq(down.max - 5, down.max, 10^-digits)}
                      else{c(seq(down.max - 5, down.max, 10^-digits),
                             seq(up.min, up.min + 5, 10^-digits))}},
                    size = answers)
  content <- c(type, ID, ID, paste(quest.txt1, data1, quest.txt2, data2, quest.txt3, sep = ""),
               points.per.q, difficulty, paste(e.path, paste(title, i, sep = "-"), ".jpeg", sep = ""),
               points, hint, feedback)
  options <- c(rep("",7), ans.txt, rep("",2))
  options[corr.ind] <- corr.ans
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),1] <- param
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),2] <- content
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),3] <- options
  jpeg(filename=paste(loc.path, paste(title, i, sep = "-"), ".jpeg", sep = ""),
       height = 25*nrow(data), width = 65*ncol(data))
  p <- tableGrob(data)
  grid.arrange(p)
  dev.off()
}
questions <- questions[((10+answers)):((9+answers)*(n+1)),]
write.table(questions, sep=",", file=paste(title, ".csv", sep = ""),
            row.names=F, col.names=F)

##### MeanPESEMC1 #####
library(gridExtra)
title <- "MeanPESEMC1"
n = 200
type <- "MC"
answers <- 5
points.per.q <- 4
difficulty <- 1
quest.txt1 <- "A new cardiovascular medication was administered to a single group of subjects. The subjects' heartrates (bpm) were recorded above after the medication was administered. The standard deviation of the heartrates was "
quest.txt2 <- ". What is the "
quest.txt3 <- " of the average heartrate for all patients who receive this medicine?"
digits = 2
loc.path <- "Images/"
e.path <- "Images/"
hint <- "Is this a one or two sample problem? Are you looking for center or spread? Pick the closest answer."
feedback <- "Point Estimate = mean(bpm). SE = s/sqrt(n)"
param <- c("NewQuestion","ID","Title","QuestionText","Points","Difficulty", "Image",
           rep("Option", answers),"Hint","Feedback")
questions <- data.frame()
for(i in 1:n)
{
  ID <- paste(title, i, sep = "-")
  points <- sample(c(rep(0,answers-1),100),replace=F)
  corr.ind <- 7 + which.max(points)
  decis <- sample(6:8, size = 1)
  BPM <- sample(80:140, size = decis)
  data <- data.frame(BPM, stringsAsFactors = FALSE)
  data1 <- round(sd(BPM), digits)
  data2 <- sample(c("point estimate", "standard error"), size = 1)
  corr.ans <- if(data2 == "point estimate"){round(mean(BPM),
                                                  digits = digits)}
  else{round(data1/sqrt(decis),
             digits = digits)}
  up.min <- corr.ans + .2
  down.max <- corr.ans - .2
  ans.txt <- sample(if((corr.ans <= 90) & (data2 == "point estimate")){seq(up.min, up.min + 20, 10^-digits)}
                    else{if((corr.ans >= 130) & (data2 == "point estimate")){seq(down.max - 20, down.max, 10^-digits)}
                      else{if((corr.ans <= 6) & (data2 == "standard error")){seq(up.min, up.min + 5, 10^-digits)}
                        else{if((corr.ans >= 10) & (data2 == "standard error")){seq(down.max - 5, down.max, 10^-digits)}
                          else{if((corr.ans > 6) & (corr.ans < 10) & (data2 == "standard error")){
                            c(seq(down.max - 5, down.max, 10^-digits), seq(up.min, up.min + 5, 10^-digits))}
                      else{c(seq(down.max - 20, down.max, 10^-digits),
                             seq(up.min, up.min + 20, 10^-digits))}}}}},
                    size = answers)
  content <- c(type, ID, ID, paste(quest.txt1, data1, quest.txt2, data2, quest.txt3, sep = ""),
               points.per.q, difficulty, paste(e.path, paste(title, i, sep = "-"), ".jpeg", sep = ""),
               points, hint, feedback)
  options <- c(rep("",7), ans.txt, rep("",2))
  options[corr.ind] <- corr.ans
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),1] <- param
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),2] <- content
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),3] <- options
  jpeg(filename=paste(loc.path, paste(title, i, sep = "-"), ".jpeg", sep = ""),
       height = 25*nrow(data), width = 65*ncol(data))
  p <- tableGrob(data)
  grid.arrange(p)
  dev.off()
}
questions <- questions[((10+answers)):((9+answers)*(n+1)),]
write.table(questions, sep=",", file=paste(title, ".csv", sep = ""),
            row.names=F, col.names=F)

##### ChisqMC1 #####
library(gridExtra)
title <- "ChisqMC1"
n = 200
type <- "MC"
answers <- 5
points.per.q <- 4
difficulty <- 1
quest.txt1 <- "The above table contains frequency data across two categorical variables. What is the expected count for the "
quest.txt2 <- " cell?"
digits = 2
loc.path <- "Images/"
e.path <- "Images/"
hint <- "You need the formula for expected counts from your chapter on chi squares. Pick the closest answer."
feedback <- "row total * col total / grand total"
param <- c("NewQuestion","ID","Title","QuestionText","Points","Difficulty", "Image",
           rep("Option", answers),"Hint","Feedback")
questions <- data.frame()
for(i in 1:n)
{
  ID <- paste(title, i, sep = "-")
  points <- sample(c(rep(0,answers-1),100),replace=F)
  corr.ind <- 7 + which.max(points)
  decis1 <- sample(3:7, size = 1)
  decis2 <- sample(3:7, size = 1)
  rn <- c()
  cn <- c()
  for(d in 1:decis1){rn[d] <- paste("Group", d)}
  for(e in 1:decis2){cn[e] <- paste("Level", e)}
  data <- matrix(sample(20:200, size = decis1*decis2), nrow = decis1, ncol = decis2,
              dimnames = list(rn, cn))
  data <- addmargins(data, FUN = list(Total = sum), quiet = TRUE)
  decis3 <- c(sample(1:decis1, size = 1), sample(1:decis2, size = 1))
  data1 <- paste(rn[decis3[1]], cn[decis3[2]], sep = ", ")
  corr.ans <- round((data[decis3[1], decis2 + 1])*(data[decis1 + 1, decis3[2]])/sum(data[-(decis1 + 1), decis2 + 1]),
                    digits = digits)
  up.min <- corr.ans + .2
  down.max <- corr.ans - .2
  ans.txt <- sample(if(corr.ans <= 40){seq(up.min, up.min + 20, 10^-digits)}
                    else{if(corr.ans >= 180){seq(down.max - 20, down.max, 10^-digits)}
                      else{c(seq(down.max - 20, down.max, 10^-digits),
                             seq(up.min, up.min + 20, 10^-digits))}},
                    size = answers)
  content <- c(type, ID, ID, paste(quest.txt1, data1, quest.txt2, sep = ""),
               points.per.q, difficulty, paste(e.path, paste(title, i, sep = "-"), ".jpeg", sep = ""),
               points, hint, feedback)
  options <- c(rep("",7), ans.txt, rep("",2))
  options[corr.ind] <- corr.ans
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),1] <- param
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),2] <- content
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),3] <- options
  jpeg(filename=paste(loc.path, paste(title, i, sep = "-"), ".jpeg", sep = ""),
       height = 25*nrow(data), width = 65*ncol(data))
  p <- tableGrob(data)
  grid.arrange(p)
  dev.off()
}
questions <- questions[((10+answers)):((9+answers)*(n+1)),]
write.table(questions, sep=",", file=paste(title, ".csv", sep = ""),
            row.names=F, col.names=F)

##### ChisqMC2 #####
library(gridExtra)
title <- "ChisqMC2"
n = 200
type <- "MC"
answers <- 5
points.per.q <- 4
difficulty <- 1
quest.txt1 <- "The above table contains frequency data across two categorical variables. What are the degrees of freedom for the chi square test of association between the two variables?"
digits = 2
loc.path <- "Images/"
e.path <- "Images/"
hint <- "You need the formula for degrees of freedom from your chapter on chi squares."
feedback <- "df = (#rows - 1)*(#cols - 1)"
param <- c("NewQuestion","ID","Title","QuestionText","Points","Difficulty", "Image",
           rep("Option", answers),"Hint","Feedback")
questions <- data.frame()
for(i in 1:n)
{
  ID <- paste(title, i, sep = "-")
  points <- sample(c(rep(0,answers-1),100),replace=F)
  corr.ind <- 7 + which.max(points)
  decis1 <- sample(3:7, size = 1)
  decis2 <- sample(3:7, size = 1)
  rn <- c()
  cn <- c()
  for(d in 1:decis1){rn[d] <- paste("Group", d)}
  for(e in 1:decis2){cn[e] <- paste("Level", e)}
  data <- matrix(sample(20:200, size = decis1*decis2), nrow = decis1, ncol = decis2,
                 dimnames = list(rn, cn))
  data <- addmargins(data, FUN = list(Total = sum), quiet = TRUE)
  corr.ans <- (decis1 - 1)*(decis2 - 1)
  df <- 2:7
  ans.txt <- sample(unique(apply(expand.grid(df, df), 1, prod))[unique(apply(expand.grid(df, df), 1, prod)) != corr.ans],
                    size = answers)
  content <- c(type, ID, ID, quest.txt1,
               points.per.q, difficulty, paste(e.path, paste(title, i, sep = "-"), ".jpeg", sep = ""),
               points, hint, feedback)
  options <- c(rep("",7), ans.txt, rep("",2))
  options[corr.ind] <- corr.ans
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),1] <- param
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),2] <- content
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),3] <- options
  jpeg(filename=paste(loc.path, paste(title, i, sep = "-"), ".jpeg", sep = ""),
       height = 25*nrow(data), width = 65*ncol(data))
  p <- tableGrob(data)
  grid.arrange(p)
  dev.off()
}
questions <- questions[((10+answers)):((9+answers)*(n+1)),]
write.table(questions, sep=",", file=paste(title, ".csv", sep = ""),
            row.names=F, col.names=F)

##### ChisqMC3 #####
library(gridExtra)
title <- "ChisqMC3"
n = 200
type <- "MC"
answers <- 5
points.per.q <- 4
difficulty <- 1
quest.txt1 <- "Researchers collect data on two variables, the country of origin and religion of their subjects. There are "
quest.txt2 <- " countries and "
quest.txt3 <- " religions in the study. The researchers run a chi square test of association between country and religion. Using the table above, determine the critical value for this test."
digits = 2
loc.path <- "Images/"
e.path <- "Images/"
hint <- "You need the formula for degrees of freedom from your chapter on chi squares."
feedback <- "df = (#countries - 1)*(#religions - 1)"
param <- c("NewQuestion","ID","Title","QuestionText","Points","Difficulty", "Image",
           rep("Option", answers),"Hint","Feedback")
questions <- data.frame()
for(i in 1:n)
{
  ID <- paste(title, i, sep = "-")
  points <- sample(c(rep(0,answers-1),100),replace=F)
  corr.ind <- 7 + which.max(points)
  data1 <- sample(2:4, size = 1)
  data2 <- sample(2:4, size = 1)
  DF <- 1:10
  CV <- round(qchisq(.95, 1:10), digits = digits)
  data <- rbind(DF, CV)
  corr.ans <- CV[(data1-1)*(data2-1)]
  ans.txt <- sample(CV[CV != corr.ans], size = answers)
  content <- c(type, ID, ID, paste(quest.txt1, data1, quest.txt2, data2, quest.txt3, sep = ""),
               points.per.q, difficulty, paste(e.path, title, ".jpeg", sep = ""),
               points, hint, feedback)
  options <- c(rep("",7), ans.txt, rep("",2))
  options[corr.ind] <- corr.ans
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),1] <- param
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),2] <- content
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),3] <- options
  jpeg(filename=paste(loc.path, title, ".jpeg", sep = ""),
       height = 22*nrow(data), width = 43*ncol(data))
  p <- tableGrob(data)
  grid.arrange(p)
  dev.off()
}
questions <- questions[((10+answers)):((9+answers)*(n+1)),]
write.table(questions, sep=",", file=paste(title, ".csv", sep = ""),
            row.names=F, col.names=F)

##### ChisqMC4 #####
library(gridExtra)
title <- "ChisqMC4"
n = 200
type <- "MC"
answers <- 2
points.per.q <- 4
difficulty <- 1
quest.txt1 <- "Researchers collect data on two variables, the country of origin and religion of their subjects. There are "
quest.txt2 <- " countries and "
quest.txt3 <- " religions in the study. The researchers run a chi square test of association between country and religion. They calculate a chi square test statistic of "
quest.txt4 <- ". Using all the above information, interpret the result of the chi square test of association."
digits = 2
loc.path <- "Images/"
e.path <- "Images/"
hint <- "You need the formula for degrees of freedom from your chapter on chi squares. You'll need to draw a comparison between the calculated test statistic and the critical value."
feedback <- "df = (#countries - 1)*(#religions - 1). To conclude association, the test statistic > the critical value."
param <- c("NewQuestion","ID","Title","QuestionText","Points","Difficulty", "Image",
           rep("Option", answers),"Hint","Feedback")
questions <- data.frame()
for(i in 1:n)
{
  ID <- paste(title, i, sep = "-")
  points <- sample(c(rep(0,answers-1),100),replace=F)
  corr.ind <- 7 + which.max(points)
  data1 <- sample(2:4, size = 1)
  data2 <- sample(2:4, size = 1)
  DF <- 1:10
  CV <- round(qchisq(.95, 1:10), digits = digits)
  data <- rbind(DF, CV)
  data3 <- sample(seq(1, 20, 10^-digits), size = 1)
  corr.ans <- if(data3 > CV[(data1-1)*(data2-1)]){"Religion and Country are associated with more than 95% confidence."}
              else{"Religion and Country are not associated with more than 95% confidence."}
  ans.txt <- if(corr.ans == "Religion and Country are associated with more than 95% confidence."){rep("Religion and Country are not associated with more than 95% confidence.", 2)}
             else{rep("Religion and Country are associated with more than 95% confidence.", 2)}
  content <- c(type, ID, ID, paste(quest.txt1, data1, quest.txt2, data2, quest.txt3, data3, quest.txt4, sep = ""),
               points.per.q, difficulty, paste(e.path, title, ".jpeg", sep = ""),
               points, hint, feedback)
  options <- c(rep("",7), ans.txt, rep("",2))
  options[corr.ind] <- corr.ans
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),1] <- param
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),2] <- content
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),3] <- options
  jpeg(filename=paste(loc.path, title, ".jpeg", sep = ""),
       height = 22*nrow(data), width = 43*ncol(data))
  p <- tableGrob(data)
  grid.arrange(p)
  dev.off()
}
questions <- questions[((10+answers)):((9+answers)*(n+1)),]
write.table(questions, sep=",", file=paste(title, ".csv", sep = ""),
            row.names=F, col.names=F)

##### ChisqMC5 #####
library(gridExtra)
title <- "ChisqMC5"
n = 200
type <- "MC"
answers <- 5
points.per.q <- 4
difficulty <- 1
quest.txt1 <- "The above table contains frequency data across two categorical variables. Given that the "
quest.txt2 <- " cell's expected count is "
quest.txt3 <- ", calculate that cell's contribution to the summation in a chi square test statistic."
digits = 2
loc.path <- "Images/"
e.path <- "Images/"
hint <- "You need the formula for the chi square test statistic from your chapter on chi squares. You don't need to calculate the entire chi square test statistic. Pick the closest answer."
feedback <- "(E - O)^2/E"
param <- c("NewQuestion","ID","Title","QuestionText","Points","Difficulty", "Image",
           rep("Option", answers),"Hint","Feedback")
questions <- data.frame()
for(i in 1:n)
{
  ID <- paste(title, i, sep = "-")
  points <- sample(c(rep(0,answers-1),100),replace=F)
  corr.ind <- 7 + which.max(points)
  decis1 <- sample(3:7, size = 1)
  decis2 <- sample(3:7, size = 1)
  rn <- c()
  cn <- c()
  for(d in 1:decis1){rn[d] <- paste("Group", d)}
  for(e in 1:decis2){cn[e] <- paste("Level", e)}
  data <- matrix(sample(20:200, size = decis1*decis2), nrow = decis1, ncol = decis2,
                 dimnames = list(rn, cn))
  data <- addmargins(data, FUN = list(Total = sum), quiet = TRUE)
  decis3 <- c(sample(1:decis1, size = 1), sample(1:decis2, size = 1))
  data1 <- paste(rn[decis3[1]], cn[decis3[2]], sep = ", ")
  data2 <- round((data[decis3[1], decis2 + 1])*(data[decis1 + 1, decis3[2]])/sum(data[-(decis1 + 1), decis2 + 1]), digits = digits)
  o <- data[decis3[1], decis3[2]]
  corr.ans <- round((data2 - o)^2/data2, digits = digits)
  up.min <- corr.ans + .3
  down.max <- corr.ans - .3
  ans.txt <- sample(if(corr.ans <= 10){seq(up.min, up.min + 10, 10^-digits)}
                    else{if(corr.ans >= 40){seq(down.max - 20, down.max, 10^-digits)}
                      else{c(seq(down.max - 10, down.max, 10^-digits),
                             seq(up.min, up.min + 20, 10^-digits))}},
                    size = answers)
  content <- c(type, ID, ID, paste(quest.txt1, data1, quest.txt2, data2, quest.txt3, sep = ""),
               points.per.q, difficulty, paste(e.path, paste(title, i, sep = "-"), ".jpeg", sep = ""),
               points, hint, feedback)
  options <- c(rep("",7), ans.txt, rep("",2))
  options[corr.ind] <- corr.ans
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),1] <- param
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),2] <- content
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),3] <- options
  jpeg(filename=paste(loc.path, paste(title, i, sep = "-"), ".jpeg", sep = ""),
       height = 25*nrow(data), width = 65*ncol(data))
  p <- tableGrob(data)
  grid.arrange(p)
  dev.off()
}
questions <- questions[((10+answers)):((9+answers)*(n+1)),]
write.table(questions, sep=",", file=paste(title, ".csv", sep = ""),
            row.names=F, col.names=F)

##### CorrMC1 #####
title <- "CorrMC1"
n = 200
type <- "MC"
answers <- 4
points.per.q <- 4
difficulty <- 1
quest.txt1 <- "Four scatterplots of two variables Y vs. X are depicted above. Determine which image depicts the strongest correlation between Y and X."
digits = 2
loc.path <- "Images/"
e.path <- "Images/"
hint <- "No calculation is necessary. Just interpret the above graph."
feedback <- "Strongest correlations have points closest to a line of best fit."
param <- c("NewQuestion","ID","Title","QuestionText","Points","Difficulty", "Image",
           rep("Option", answers),"Hint","Feedback")
questions <- data.frame()
for(i in 1:n)
{
  ID <- paste(title, i, sep = "-")
  points <- sample(c(rep(0,answers-1),100),replace=F)
  corr.ind <- 7 + which.max(points)
  X <- sample(1:30, size = 20); X1 <- X; X2 <- X; X3 <- X; X4 <- X
  decis <- sample(1:4, size = 1)
  if(decis == 1)
  {
  Y1 <- sample(10:20, size = 1) + sample(3:6, size = 1)*X + rnorm(length(X),0,30)
  Y2 <- sample(10:20, size = 1) + sample(seq(-6, -3, 1), size = 1)*X + rnorm(length(X),0,2)
  Y3 <- sample(seq(-20, -10, 1), size = 1) + sample(seq(-4, -3, 1), size = 1)*X + rnorm(length(X),0,30)
  Y4 <- sample(seq(-20, -10, 1), size = 1) + sample(seq(2, 3, 1), size = 1)*X + rnorm(length(X), 0, 30)
  }
  else{if(decis == 2)
         {
         Y2 <- sample(10:20, size = 1) + sample(3:6, size = 1)*X + rnorm(length(X),0,30)
         Y3 <- sample(10:20, size = 1) + sample(3:6, size = 1)*X + rnorm(length(X),0,2)
         Y4 <- sample(seq(-20, -10, 1), size = 1) + sample(seq(-4, -3, 1), size = 1)*X + rnorm(length(X),0,30)
         Y1 <- sample(seq(-20, -10, 1), size = 1) + sample(seq(2, 3, 1), size = 1)*X + rnorm(length(X), 0, 30)
         }
         else{if(decis == 3)
                {
                Y3 <- sample(10:20, size = 1) + sample(3:6, size = 1)*X + rnorm(length(X),0,30)
                Y4 <- sample(10:20, size = 1) + sample(seq(-6, -3, 1), size = 1)*X + rnorm(length(X),0,2)
                Y1 <- sample(seq(-20, -10, 1), size = 1) + sample(seq(-4, -3, 1), size = 1)*X + rnorm(length(X),0,30)
                Y2 <- sample(seq(-20, -10, 1), size = 1) + sample(seq(2, 3, 1), size = 1)*X + rnorm(length(X), 0, 30)
                }
                else{if(decis == 4)
                {
                Y4 <- sample(10:20, size = 1) + sample(3:6, size = 1)*X + rnorm(length(X),0,30)
                Y1 <- sample(10:20, size = 1) + sample(3:6, size = 1)*X + rnorm(length(X),0,2)
                Y2 <- sample(seq(-20, -10, 1), size = 1) + sample(seq(-4, -3, 1), size = 1)*X + rnorm(length(X),0,30)
                Y3 <- sample(seq(-20, -10, 1), size = 1) + sample(seq(2, 3, 1), size = 1)*X + rnorm(length(X), 0, 30)
                }}}}
  r1 <- cor(X1, Y1); r2 <- cor(X2, Y2); r3 <- cor(X3, Y3); r4 <- cor(X4, Y4)
  r <- abs(c(r1,r2,r3,r4))
  corr.ans <- which.max(r)
  ans.txt <- 1:4
  ans.txt <- c(ans.txt[ans.txt != corr.ans], "None of the Above")
  content <- c(type, ID, ID, quest.txt1,
               points.per.q, difficulty, paste(e.path, paste(title, i, sep = "-"), ".jpeg", sep = ""),
               points, hint, feedback)
  options <- c(rep("",7), ans.txt, rep("",2))
  options[corr.ind] <- corr.ans
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),1] <- param
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),2] <- content
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),3] <- options
  jpeg(filename=paste(loc.path, paste(title, i, sep = "-"), ".jpeg", sep = ""))
  par(mfrow=c(2,2))
  plot(X1, Y1, xlim=c(0,30), ylim = c(-200, 200)); title(1)
  plot(X2, Y2, xlim=c(0,30), ylim = c(-200, 200)); title(2)
  plot(X3, Y3, xlim=c(0,30), ylim = c(-200, 200)); title(3)
  plot(X4, Y4, xlim=c(0,30), ylim = c(-200, 200)); title(4);
  dev.off()
}
questions <- questions[((10+answers)):((9+answers)*(n+1)),]
write.table(questions, sep=",", file=paste(title, ".csv", sep = ""),
            row.names=F, col.names=F)

##### CorrMC2 #####
title <- "CorrMC2"
n = 200
type <- "MC"
answers <- 5
points.per.q <- 4
difficulty <- 1
quest.txt1 <- "A researcher compares the correlation coefficients between six pairs of variables. She calculates them as follows: "
quest.txt2 <- ". Which of the correlations is the strongest?"
dat.size = 1
digits = 2
loc.path <- 
  e.path <- 
  hint <- "This is an interpretive question. No calculation is necessary."
feedback <- "Stronger correlations are closer to -1 or 1. Weaker correlations are closer to 0."
param <- c("NewQuestion","ID","Title","QuestionText","Points","Difficulty",
           rep("Option", answers),"Hint","Feedback")
questions <- data.frame()
for(i in 1:n)
{
  ID <- paste(title, i, sep = "-")
  points <- sample(c(rep(0,answers-1),100),replace=F)
  corr.ind <- 6 + which.max(points)
  r <- sample(seq(-1,1,10^-digits), size = 6)
  data1 <- paste(r[1], r[2], r[3], r[4], r[5], r[6], sep = ", ")
  corr.ans <- r[which.max(abs(r))]
  ans.txt <- r[-which.max(abs(r))]
  content <- c(type, ID, ID, paste(quest.txt1, data1, quest.txt2,
                                   collapse = "", sep= ""),
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

##### CorrMC3 #####
title <- "CorrMC3"
n = 200
type <- "MC"
answers <- 5
points.per.q <- 4
difficulty <- 1
quest.txt1 <- "A researcher compares the correlation coefficients between six pairs of variables. She calculates them as follows: "
quest.txt2 <- ". Which of the correlations is the weakest?"
dat.size = 1
digits = 2
loc.path <- 
  e.path <- 
  hint <- "This is an interpretive question. No calculation is necessary."
feedback <- "Stronger correlations are closer to -1 or 1. Weaker correlations are closer to 0."
param <- c("NewQuestion","ID","Title","QuestionText","Points","Difficulty",
           rep("Option", answers),"Hint","Feedback")
questions <- data.frame()
for(i in 1:n)
{
  ID <- paste(title, i, sep = "-")
  points <- sample(c(rep(0,answers-1),100),replace=F)
  corr.ind <- 6 + which.max(points)
  r <- sample(seq(-1,1,10^-digits), size = 6)
  data1 <- paste(r[1], r[2], r[3], r[4], r[5], r[6], sep = ", ")
  corr.ans <- r[which.min(abs(r))]
  ans.txt <- r[-which.min(abs(r))]
  content <- c(type, ID, ID, paste(quest.txt1, data1, quest.txt2,
                                   collapse = "", sep= ""),
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

##### CorrMC4 #####
library(gridExtra)
title <- "CorrMC4"
n = 200
type <- "MC"
answers <- 5
points.per.q <- 4
difficulty <- 1
quest.txt1 <- "The Z-scores = (X- mean)/SD for two variables X and Y are given above. Calculate the correlation coefficient r between the two variables."
digits = 2
loc.path <- "Images/"
e.path <- "Images/"
hint <- "You'll need to follow the last couple steps of calculating correlation coefficients. Pick the closest answer."
feedback <- "r = sum(Zx*Zy)/(n-1)"
param <- c("NewQuestion","ID","Title","QuestionText","Points","Difficulty", "Image",
           rep("Option", answers),"Hint","Feedback")
questions <- data.frame()
for(i in 1:n)
{
  ID <- paste(title, i, sep = "-")
  points <- sample(c(rep(0,answers-1),100),replace=F)
  corr.ind <- 7 + which.max(points)
  decis <- sample(6:8, size = 1)
  X <- sample(80:140, size = decis)
  Y <- sample(90:160, size = decis)
  Zx <- round((X-mean(X))/sd(X), digits)
  Zy <- round((Y-mean(Y))/sd(Y), digits)
  data <- data.frame(Zx, Zy, stringsAsFactors = FALSE)
  corr.ans <- round(sum(Zx*Zy)/(decis-1), digits)
  up.min <- corr.ans + .1
  down.max <- corr.ans - .1
  ans.txt <- sample(if(corr.ans <= -.8){seq(up.min, up.min + 1, 10^-digits)}
                    else{if(corr.ans >= .8){seq(down.max - 1, down.max, 10^-digits)}
                      else{c(seq(down.max - .25, down.max, 10^-digits),
                             seq(up.min, up.min + .25, 10^-digits))}},
                    size = answers)
  content <- c(type, ID, ID, quest.txt1,
               points.per.q, difficulty, paste(e.path, paste(title, i, sep = "-"), ".jpeg", sep = ""),
               points, hint, feedback)
  options <- c(rep("",7), ans.txt, rep("",2))
  options[corr.ind] <- corr.ans
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),1] <- param
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),2] <- content
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),3] <- options
  jpeg(filename=paste(loc.path, paste(title, i, sep = "-"), ".jpeg", sep = ""),
       height = 25*nrow(data), width = 60*ncol(data))
  p <- tableGrob(data)
  grid.arrange(p)
  dev.off()
}
questions <- questions[((10+answers)):((9+answers)*(n+1)),]
write.table(questions, sep=",", file=paste(title, ".csv", sep = ""),
            row.names=F, col.names=F)

##### RegMC1 #####
library(gridExtra)
title <- "RegMC1"
n = 200
type <- "MC"
answers <- 5
points.per.q <- 4
difficulty <- 1
quest.txt1 <- "A new cardiovascular medication was administered to a group of subjects. The subjects' heartrates (bpm) and weights (lbs) were recorded above after the medication was administered. The standard deviation of the heartrates was "
quest.txt2 <- ", and the standard deviation of the weights was "
quest.txt3 <- ". The correlation coefficient between heartrate and weight was "
quest.txt4 <- ". What is the "
quest.txt5 <- " of the regression line heartrate (y) vs. weight (x) for all who receive this medicine?"
digits = 2
loc.path <- "Images/"
e.path <- "Images/"
hint <- "You'll need a formula from your chapter on regression. Pick the closest answer."
feedback <- "Slope = r*sy/sx. Intercept = mean(bpm) - mean(lbs)*slope"
param <- c("NewQuestion","ID","Title","QuestionText","Points","Difficulty", "Image",
           rep("Option", answers),"Hint","Feedback")
questions <- data.frame()
for(i in 1:n)
{
  ID <- paste(title, i, sep = "-")
  points <- sample(c(rep(0,answers-1),100),replace=F)
  corr.ind <- 7 + which.max(points)
  decis <- sample(6:10, size = 1)
  BPM <- sample(80:140, size = decis)
  LBS <- sample(110:230, size = decis)
  data <- data.frame(BPM, LBS, stringsAsFactors = FALSE)
  data1 <- round(sd(BPM), digits)
  data2 <- round(sd(LBS), digits)
  data3 <- round(cor(BPM, LBS), digits)
  data4 <- sample(c("slope", "intercept"), size = 1)
  corr.ans <- if(data4 == "slope"){round(data3*data1/data2,
                                                  digits = digits)}
              else{round(mean(BPM)-(mean(LBS)*data3*data1/data2),
                         digits = digits)}
  up.min <- corr.ans + .12
  down.max <- corr.ans - .12
  ans.txt <- sample(if((corr.ans <= -.1) & (data4 == "slope")){seq(up.min, up.min + .4, 10^-digits)}
                    else{if((corr.ans >= .3) & (data4 == "slope")){seq(down.max - .4, down.max, 10^-digits)}
                      else{if((corr.ans <= 70) & (data4 == "intercept")){seq(up.min, up.min + 50, 10^-digits)}
                        else{if((corr.ans >= 180) & (data4 == "intercept")){seq(down.max - 50, down.max, 10^-digits)}
                          else{if((corr.ans > 180) & (corr.ans < 70) & (data4 == "intercept")){
                            c(seq(down.max - 50, down.max, 10^-digits), seq(up.min, up.min + 50, 10^-digits))}
                            else{c(seq(down.max - .4, down.max, 10^-digits),
                                   seq(up.min, up.min + .4, 10^-digits))}}}}},
                    size = answers)
  content <- c(type, ID, ID, paste(quest.txt1, data1, quest.txt2, data2, quest.txt3, data3, quest.txt4, data4, quest.txt5, sep = ""),
               points.per.q, difficulty, paste(e.path, paste(title, i, sep = "-"), ".jpeg", sep = ""),
               points, hint, feedback)
  options <- c(rep("",7), ans.txt, rep("",2))
  options[corr.ind] <- corr.ans
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),1] <- param
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),2] <- content
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),3] <- options
  jpeg(filename=paste(loc.path, paste(title, i, sep = "-"), ".jpeg", sep = ""),
       height = 25*nrow(data), width = 65*ncol(data))
  p <- tableGrob(data)
  grid.arrange(p)
  dev.off()
}
questions <- questions[((10+answers)):((9+answers)*(n+1)),]
write.table(questions, sep=",", file=paste(title, ".csv", sep = ""),
            row.names=F, col.names=F)

##### RegMC2 #####
title <- "RegMC2"
n = 200
type <- "MC"
answers <- 3
points.per.q <- 4
difficulty <- 1
quest.txt1 <- "The regression of two variables Y vs. X is depicted above. Determine whether the slope is positive, negative, or zero."
digits = 2
loc.path <- "Images/"
e.path <- "Images/"
hint <- "No calculation is necessary. Just interpret the above graph."
feedback <- "Lines tilted up from left to right are positive, tilted down are negative, and flat liens are zero."
param <- c("NewQuestion","ID","Title","QuestionText","Points","Difficulty", "Image",
           rep("Option", answers),"Hint","Feedback")
questions <- data.frame()
for(i in 1:n)
{
  ID <- paste(title, i, sep = "-")
  points <- sample(c(rep(0,answers-1),100),replace=F)
  corr.ind <- 7 + which.max(points)
  decis <- sample(1:3, size = 1)
  X <- sample(1:40, size = 10)
  Y <- if(decis == 1){sample(10:20, size = 1) + sample(seq(0.25, 5, by = 0.25), size = 1)*X + rnorm(10,0,3)}
       else{if(decis == 2){sample(10:20, size = 1) - sample(seq(0.25, 5, by = 0.25), size = 1)*X + rnorm(10,0,3)}
            else{rep(sample(10:20, size = 1), length(X))}}
  corr.ans <- if(decis == 1){"Positive"}else{if(decis == 2){"Negative"}else{"Zero"}}
  ans.txt <- if(decis == 1){c("Negative", "Zero", "Neither of These")}else{if(decis == 2){c("Positive", "Zero", "Neither of These")}else{c("Positive", "Negative", "Neither of These")}}
  content <- c(type, ID, ID, quest.txt1,
               points.per.q, difficulty, paste(e.path, paste(title, i, sep = "-"), ".jpeg", sep = ""),
               points, hint, feedback)
  options <- c(rep("",7), ans.txt, rep("",2))
  options[corr.ind] <- corr.ans
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),1] <- param
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),2] <- content
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),3] <- options
  jpeg(filename=paste(loc.path, paste(title, i, sep = "-"), ".jpeg", sep = ""))
  plot(X, Y, xlim = c(-7,7), ylim = c(3,28), type = "n"); abline(lm(Y~X)); title("Y vs. X");
  dev.off()
}
questions <- questions[((10+answers)):((9+answers)*(n+1)),]
write.table(questions, sep=",", file=paste(title, ".csv", sep = ""),
            row.names=F, col.names=F)

##### RegPESEMC1 #####
library(gridExtra)
title <- "RegPESEMC1"
n = 200
type <- "MC"
answers <- 5
points.per.q <- 4
difficulty <- 1
quest.txt1 <- "A new cardiovascular medication was administered to a group of subjects. The subjects' heartrates (bpm) and weights (lbs) were recorded above after the medication was administered. The standard deviation of the heartrates was "
quest.txt2 <- ", and the standard deviation of the weights was "
quest.txt3 <- ". The correlation coefficient between heartrate and weight was "
quest.txt4 <- ". What is the "
quest.txt5 <- " for the slope of the regression line heartrate (y) vs. weight (x) for all who receive this medicine?"
digits = 2
loc.path <- "Images/"
e.path <- "Images/"
hint <- "You'll need a formula from your chapter on regression. Are you looking for center or spread? Pick the closest answer."
feedback <- "Point Estimate = r*sy/sx. SE = sqrt((1-r^2)/(n-2))*sy/sx"
param <- c("NewQuestion","ID","Title","QuestionText","Points","Difficulty", "Image",
           rep("Option", answers),"Hint","Feedback")
questions <- data.frame()
for(i in 1:n)
{
  ID <- paste(title, i, sep = "-")
  points <- sample(c(rep(0,answers-1),100),replace=F)
  corr.ind <- 7 + which.max(points)
  decis <- sample(6:10, size = 1)
  BPM <- sample(80:140, size = decis)
  LBS <- sample(110:230, size = decis)
  data <- data.frame(BPM, LBS, stringsAsFactors = FALSE)
  data1 <- round(sd(BPM), digits)
  data2 <- round(sd(LBS), digits)
  data3 <- round(cor(BPM, LBS), digits)
  data4 <- sample(c("point estimate", "standard error"), size = 1)
  corr.ans <- if(data4 == "point estimate"){round(data3*data1/data2,
                                                  digits = digits)}
              else{round(sqrt((1-data3^2)/(decis-2))*(data1/data2),
                         digits = digits)}
  up.min <- corr.ans + .12
  down.max <- corr.ans - .12
  ans.txt <- sample(if((corr.ans <= -.1) & (data4 == "point estimate")){seq(up.min, up.min + .4, 10^-digits)}
                    else{if((corr.ans >= .3) & (data4 == "point estimate")){seq(down.max - .4, down.max, 10^-digits)}
                      else{if((corr.ans <= .1) & (data4 == "standard error")){seq(up.min, up.min + .4, 10^-digits)}
                        else{if((corr.ans >= .3) & (data4 == "standard error")){seq(down.max - .4, down.max, 10^-digits)}
                          else{if((corr.ans > .1) & (corr.ans < .3) & (data4 == "standard error")){
                            c(seq(down.max - .4, down.max, 10^-digits), seq(up.min, up.min + .4, 10^-digits))}
                            else{c(seq(down.max - .4, down.max, 10^-digits),
                                   seq(up.min, up.min + .4, 10^-digits))}}}}},
                    size = answers)
  content <- c(type, ID, ID, paste(quest.txt1, data1, quest.txt2, data2, quest.txt3, data3, quest.txt4, data4, quest.txt5, sep = ""),
               points.per.q, difficulty, paste(e.path, paste(title, i, sep = "-"), ".jpeg", sep = ""),
               points, hint, feedback)
  options <- c(rep("",7), ans.txt, rep("",2))
  options[corr.ind] <- corr.ans
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),1] <- param
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),2] <- content
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),3] <- options
  jpeg(filename=paste(loc.path, paste(title, i, sep = "-"), ".jpeg", sep = ""),
       height = 25*nrow(data), width = 65*ncol(data))
  p <- tableGrob(data)
  grid.arrange(p)
  dev.off()
}
questions <- questions[((10+answers)):((9+answers)*(n+1)),]
write.table(questions, sep=",", file=paste(title, ".csv", sep = ""),
            row.names=F, col.names=F)

##### RegCIMC1 #####
title <- "RegCIMC1"
n = 200
type <- "MC"
answers <- 5
points.per.q <- 4
difficulty <- 1
quest.txt1 <- "NASA compares the ages of stars (in billions of years) to their surface temperatures (in thousands of degrees Kelvin). A sample of "
quest.txt2 <- " stars has a standard deviation in age of "
quest.txt3 <- " billion years, and a standard deviation in temperature of "
quest.txt4 <- " thousands of degrees. The sample's age and temperature have a correlation coefficient of "
quest.txt5 <- ". What is the 95% CI for the slope of the regression line temperature (y) vs. age (x)?"
dat.size = 1
digits = 2
loc.path <- 
  e.path <- 
  hint <- "You need to use the given statistics to find slope, standard error, and the lower and upper bounds."
feedback <- "Did you use r*sy/sx +/- 1.96*sqrt((1-r^2)/(n-2))*(sy/sx)?"
param <- c("NewQuestion","ID","Title","QuestionText","Points","Difficulty",
           rep("Option", answers),"Hint","Feedback")
questions <- data.frame()
for(i in 1:n)
{
  ID <- paste(title, i, sep = "-")
  points <- sample(c(rep(0,answers-1),100),replace=F)
  corr.ind <- 6 + which.max(points)
  data1 <- sample(50:150, size = 1)
  data2 <- sample(seq(2, 4, 10^-digits), size = 1)
  data3 <- sample(seq(5, 7, 10^-digits), size = 1)
  data4 <- sample(seq(-1, 1, 10^-digits), size = 1)
  lb <- round(data4*data3/data2 - 1.96*sqrt((1-data4^2)/(data1-2))*(data3/data2),
              digits = digits)
  ub <- round(data4*data3/data2 + 1.96*sqrt((1-data4^2)/(data1-2))*(data3/data2),
              digits = digits)
  corr.ans <- paste("(", paste(lb, ub, sep = ", "), ")", sep = "")
  up.min <- round(ub + .1, digits)
  down.max <- round(lb - .1, digits)
  if(lb <= -1.9){
    seq1 <- round(seq(up.min, up.min + 2, 10^-digits), digits)
    mat1 <- expand.grid(seq1,seq1)
    mat1 <- mat1[mat1[,1] < mat1[,2], ]
    ans.txt1 <- mat1[sample(nrow(mat1), size = answers), ]}
  else{if(ub >= 1.9){
    seq2 <- round(seq(down.max - 2, down.max, 10^-digits), digits)
    mat2 <- expand.grid(seq2,seq2)
    mat2 <- mat2[mat2[,1] < mat2[,2], ]
    ans.txt1 <- mat2[sample(nrow(mat2), size = answers), ]}
    else{
      seq3 <- round(c(seq(down.max - 2, down.max, 10^-digits), seq(up.min, up.min + 2, 10^-digits)), digits)
      mat3 <- expand.grid(seq3,seq3)
      mat3 <- mat3[mat3[,1] < mat3[,2], ]
      ans.txt1 <- mat3[sample(nrow(mat3), size = answers), ]}}
  ans.txt <- c()
  for(l in 1:answers){ans.txt[l] <- paste("(", paste(ans.txt1[l,1], ans.txt1[l,2], sep = ", "), ")", sep = "")}
  content <- c(type, ID, ID, paste(quest.txt1, data1, quest.txt2,
                                   data2, quest.txt3, data3, quest.txt4,
                                   data4, quest.txt5,
                                   collapse = "", sep= ""),
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

##### RegCIMC2 #####
title <- "RegCIMC2"
n = 200
type <- "MC"
answers <- 4
points.per.q <- 4
difficulty <- 1
quest.txt1 <- "A researcher compares variables x and y using linear regression. She calculates a 95% confidence interval of "
quest.txt2 <- " for the slope of the regression line. Which of the following statistical inferences should she make?"
dat.size = 1
digits = 2
loc.path <- 
  e.path <- 
  hint <- "This is an interpretive question. No calculation is necessary. Consider each bound inclusive."
feedback <- "Does the CI contain 0?"
param <- c("NewQuestion","ID","Title","QuestionText","Points","Difficulty",
           rep("Option", answers),"Hint","Feedback")
questions <- data.frame()
for(i in 1:n)
{
  ID <- paste(title, i, sep = "-")
  points <- sample(c(rep(0,answers-1),100),replace=F)
  corr.ind <- 6 + which.max(points)
  lb <- sample(seq(-.2, .11, 10^-digits), size = 1)
  ub <- sample(seq(lb + .03, .2), size = 1)
  data1 <- paste("(", paste(round(lb, digits = digits),
                            round(ub, digits = digits),
                            sep = ", "), ")",
                 sep = "")
  corr.ans <- if((lb <= 0) & (ub >= 0)){"The slope is insignificant at the 95% confidence level."}
  else{"The slope is significant at the 95% confidence level."}
  ans.txt <- if(corr.ans == "The slope is insignificant at the 95% confidence level.")
  {c("The slope is significant at the 95% confidence level.",
     "The sample slope is not contained within the confidence interval.",
     "Zero is not contained within the confidence interval.",
     "The result is significant, thus the p-value is less than 0.05.")}
  else{c("The slope is insignificant at the 95% confidence level.",
         "The sample slope is contained within the confidence interval.",
         "Zero is contained within the confidence interval.",
         "The result is insignificant, thus the p-value is greater than 0.05.")}
  content <- c(type, ID, ID, paste(quest.txt1, data1, quest.txt2,
                                   collapse = "", sep= ""),
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

##### 2PropTestMC1 #####
title <- "2PropTestMC1"
n = 200
type <- "MC"
answers <- 5
points.per.q <- 4
difficulty <- 1
quest.txt1 <- "A new pesticide is tested on a group of crop-destroying beetles. The sample data shows that "
quest.txt2 <- " of this first group dies as a result. A second group of beetles is dosed with a standard pesticide, and "
quest.txt3 <- " of this second group dies as a result. "
quest.txt4 <- " beetles are in the first test-pesticide group and "
quest.txt5 <- " beetles are in the second standard-pesticide group. What is the Z test statistic for a hypothesis test on the difference between proportions?"
dat.size = 1
digits = 2
loc.path <- 
  e.path <- 
  hint <- "You need to calculate the absolute value of the Z test statistic. Pick the closest answer."
feedback <- "Did you use (phat1 - phat2)/SE, and take its absolute value?"
param <- c("NewQuestion","ID","Title","QuestionText","Points","Difficulty",
           rep("Option", answers),"Hint","Feedback")
questions <- data.frame()
for(i in 1:n)
{
  ID <- paste(title, i, sep = "-")
  points <- sample(c(rep(0,answers-1),100),replace=F)
  corr.ind <- 6 + which.max(points)
  data1 <- sample(seq(.4, .5, 10^-digits), size = 1)
  data2 <- sample(seq(.35,.45, 10^-digits), size = 1)
  data3 <- sample(100:200, size = 1)
  data4 <- sample(100:200, size = 1)
  corr.ans <- round((data1-data2)/sqrt(data1*(1-data1)/data3+data2*(1-data2)/data4), digits)
  up.min <- round(corr.ans + .05, digits)
  down.max <- round(corr.ans - .05, digits)
  ans.txt <- sample(if(corr.ans < -.8){seq(up.min, 4, 10^-digits)}
                    else{if(corr.ans > 2.85){seq(-1, down.max, 10^-digits)}
                      else{c(seq(-1, down.max, 10^-digits),
                             seq(up.min, 4, 10^-digits))}},
                    size = answers)
  content <- c(type, ID, ID, paste(quest.txt1, data1, quest.txt2,
                                   data2, quest.txt3, data3, quest.txt4,
                                   data4, quest.txt5,
                                   collapse = "", sep= ""),
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

##### 2PropTestMC2 #####
title <- "2PropTestMC2"
n = 200
type <- "MC"
answers <- 5
points.per.q <- 4
difficulty <- 1
quest.txt1 <- "A new pesticide is tested on a group of crop-destroying beetles. The sample data shows that "
quest.txt2 <- " of this first group dies as a result. A second group of beetles is dosed with a standard pesticide, and "
quest.txt3 <- " of this second group dies as a result. "
quest.txt4 <- " beetles are in the first test-pesticide group and "
quest.txt5 <- " beetles are in the second standard-pesticide group. What is the p-value for a hypothesis test on the difference between proportions?"
dat.size = 1
digits = 4
loc.path <- 
  e.path <- 
  hint <- "You need to use your Z-table. Pick the closest answer."
feedback <- "Did you use 2*(1-area below absolute value of Z)?"
param <- c("NewQuestion","ID","Title","QuestionText","Points","Difficulty",
           rep("Option", answers),"Hint","Feedback")
questions <- data.frame()
for(i in 1:n)
{
  ID <- paste(title, i, sep = "-")
  points <- sample(c(rep(0,answers-1),100),replace=F)
  corr.ind <- 6 + which.max(points)
  data1 <- sample(seq(.43, .51, 10^-digits), size = 1)
  data2 <- sample(seq(.34,.42, 10^-digits), size = 1)
  data3 <- sample(100:250, size = 1)
  data4 <- sample(100:250, size = 1)
  z <- round((data1-data2)/sqrt(data1*(1-data1)/data3+data2*(1-data2)/data4), digits)
  area <- round(pnorm(abs(z)), digits = digits)
  corr.ans <- round(2*(1-area), digits)
  up.min <- round(corr.ans + .05, digits)
  down.max <- round(corr.ans - .05, digits)
  ans.txt <- sample(if(corr.ans < .05){seq(up.min, 1.05, 10^-digits)}
                    else{if(corr.ans > .95){seq(-.05, down.max, 10^-digits)}
                      else{c(seq(-.05, down.max, 10^-digits),
                             seq(up.min, 1.05, 10^-digits))}},
                    size = answers)
  content <- c(type, ID, ID, paste(quest.txt1, data1, quest.txt2,
                                   data2, quest.txt3, data3, quest.txt4,
                                   data4, quest.txt5,
                                   collapse = "", sep= ""),
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

##### 2PropTestMC3 #####
