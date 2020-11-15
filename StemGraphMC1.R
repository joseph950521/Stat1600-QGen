##### StemGraphMC1 #####
library(fmsb) # the pacakge useded for this particular question to create stem and leaf plots
StemGraphMC1= function(
title = "StemGraphMC1", # Question-bank title that will be easily viewable in e-learning
n = 200, # Number of questions to generate
type = "MC", # The question type, one of many possible types on e-learning
answers = 5, # Number of answers per MC question
points.per.q = 4, # Number of points the question is worth (on the test)
difficulty = 1, # An easily viewable difficulty level on e-learning
quest.txt1 = "While analyzing a dataset, a researcher makes a stem and leaf plot of one of her variables. The stem and leaf plot she makes is depicted. What is the ",
quest.txt2 = " value?", # The above question text is the static text for the full question
dat.size = seq(21, 31, by = 2), # This is the sequence of number of values to be randomly generated for the dataset
digits = -2, # This is the number of decimal places to round off the data
scale = 2, # parameter to control plot length of the graph
loc.path = "/Users/josephyang/Desktop/School Stuff/STAT 1600/Course Development/Question Generators/Stat1600-QGen/StemGraphMC1 images/", # This is the local path used to store any randomly generated image files
e.path = "Images/", # This is the path on e-learning used to store any above-implemented image files
hint = "Pay close attention to the key given for the plot. As an example, if the key read 'the decimal point is 1 digit to the right of the |, and the number of interest was labeled 1 | 2, then the answer would be 12.", # This is a student hint, visible to them during the exam on e-learning
feedback = "Stem and leaf plots are like histograms flipped over. The min is at the top, the middle halfway down, and the max is at the bottom." # This is student feedback, visible after the exam
)
{
param <- c("NewQuestion","ID","Title","QuestionText","Points","Difficulty", "Image",
           rep("Option", answers),"Hint","Feedback") # These are setting row names for the CSV file
questions <- data.frame() # This opens a data frame to store the randomly generated questions below
for(i in 1:n)
{
  ID <- paste(title, i, sep = "-") # The ID of the specific question within the bank, title + question number in the loop
  points <- sample(c(rep(0, answers-1), 100), replace=F) # The proportion of points assigned to each possible answer, 1 if correct or 0 if incorrect
  corr.ind <- 7 + which.max(points) # This is the row index of the correct answer
  dat.size1 <- sample(dat.size, size = 1)
  left.data <- c(sample(seq(1000, 3500, by = 10^(-digits)), size = ceiling(dat.size1/6)),
                 sample(seq(3600, 6100, by = 10^(-digits)), size = ceiling(2*dat.size1/6)),
                 sample(seq(6200, 7900, by = 10^(-digits)), size = ceiling(3*dat.size1/6))) # randomly generating a dataset that is left skewed
  right.data <- c(sample(seq(6200, 8700, by = 10^(-digits)), size = ceiling(dat.size1/6)),
                  sample(seq(3600, 6100, by = 10^(-digits)), size = ceiling(2*dat.size1/6)),
                  sample(seq(2000, 3500, by = 10^(-digits)), size = ceiling(3*dat.size1/6))) # randomly generating a dataset that is right skewed
  sym.data <- c(sample(seq(6200, 7900, by = 10^(-digits)), size = ceiling(1.5*dat.size1/6)),
                sample(seq(3600, 6100, by = 10^(-digits)), size = ceiling(3*dat.size1/6)),
                sample(seq(2000, 3500, by = 10^(-digits)), size = ceiling(1.5*dat.size1/6))) # randomly generating a dataset that is symmetric in both sides
  data.dec <- sample(c(1,2,3), size = 1) # creating a vector for taking decision to use the a specific kind of data
  data <- sample(if(data.dec == 1){sym.data}
                 else{if(data.dec == 2){left.data}
                   else{right.data}},
                 size = dat.size1) # creating a dataset based on a random decision value
  data1 <- sample(c("smallest", "second smallest", "third smallest", "median",
                    "third largest", "second largest", "largest"), size = 1) # these are the options created for the question
  corr.ans <- if(data1 == "smallest"){min(data)}
  else{if(data1 == "second smallest"){sort(data)[2]}
    else{if(data1 == "third smallest"){sort(data)[3]}
      else{if(data1 == "median"){median(data)}
        else{if(data1 == "third largest"){sort(data)[dat.size1-2]}
          else{if(data1 == "second largest"){sort(data)[dat.size1-1]}
            else{if(data1 == "largest"){max(data)}}}}}}} # This is the correct answer to the question
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
                                             sample(c(10*corr.ans, corr.ans/10), size = 1))}}}}}}} # These are randomly generated incorrect answers.
  content <- c(type, ID, ID, paste(quest.txt1, data1, quest.txt2, sep = ""),
               points.per.q, difficulty, paste(e.path, paste(title, i, sep = "-"), ".jpeg", sep = ""),
               points, hint, feedback) # This is collecting a lot of the above information into a single vector
  options <- c(rep("",7), ans.txt, rep("",2)) # This is collecting the incorrect answers above, and indexing them correctly by row
  options[corr.ind] <- corr.ans # This is imputing the correct answer at the appropriate row index
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),1] <- param # This is indexing and storing all the row names
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),2] <- content # Indexing and storing all the content
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),3] <- options # Indexing and storing the answers, both incorrect and correct
  jpeg(filename=paste(loc.path, paste(title, i, sep = "-"), ".jpeg", sep = "")) # creating the image files in the designated place
  gstem(data, scale = scale) # function to draw the stem and leaf plot in a graphic device
  dev.off()
}
questions <- questions[((10+answers)):((9+answers)*(n+1)),] # Storing only what's needed for e-learning upload as a CSV file
write.table(questions, sep=",", file=paste(title, ".csv", sep = ""),
            row.names=F, col.names=F) # Writing the CSV file
}

StemGraphMC1() # creating the csv file