##### MeanMC3 #####
MeanMC3= function(
  title = "MeanMC3", # Question-bank title that will be easily viewable in e-learning
  n = 200, # Number of questions to generate
  type = "MC", # The question type, one of many possible types on e-learning
  answers = 4, # Number of answers per MC question
  points.per.q = 4, # Number of points the question is worth (on the test)
  difficulty = 1, # An easily viewable difficulty level on e-learning
  quest.txt1 = "While doing some data entry, you realize that you've made a big mistake. Every data value in your mistaken dataset is ",
  quest.txt2 = " than it should have been. The mean of the mistaken dataset was ",
  quest.txt3 = ". You correct the error and recalculate the mean. What is the new corrected mean?  Mistaken Data:  ", # The above 3 question texts are static text for the full question
  dat.size = c(10:20), # This is the vector of number of values to be randomly generated for the dataset
  digits = 1, # This is the number of decimal places to round off the data
  loc.path , # This is the local path used to store any randomly generated image files
  e.path , # This is the path on e-learning used to store any above-implemented image files
  hint = "There is a shortcut here.", # This is a student hint, visible to them during the exam on e-learning
  feedback = "In the case of multiplication, the new mean will be x times the old mean, where x changes depending on the context of the question. In the case of addition or subtraction, the new mean will be the old mean plus or minus x." # This is student feedback, visible after the exam
)
{
  param <- c("NewQuestion","ID","Title","QuestionText","Points","Difficulty",
             rep("Option", answers),"Hint","Feedback") # These are setting row names for the CSV file
  questions <- data.frame() # This opens a data frame to store the randomly generated questions below
  for(i in 1:n)
  {
    ID <- paste(title, i, sep = "-") # The ID of the specific question within the bank, title + question number in the loop
    points <- sample(c(rep(0,answers-1),100),replace=F) # The proportion of points assigned to each possible answer, 1 if correct or 0 if incorrect
    corr.ind <- 6 + which.max(points) # This is the row index of the correct answer
    dat.size1 <- sample(dat.size, size = 1) #randomly generating a sample of size within 10 to 20
    data <- round(rnorm(dat.size1,mean=rnorm(1,mean=900,sd=400),sd=200) + (0.5*rt(dat.size1,df=30)), digits = digits) # This is how we're randomly generating data for this question
    data1 <- sample(c("ten times smaller", "ten times larger", "five times smaller", "five times larger",
                      "ten points less", "ten points greater", "five points less", "five points greater"),
                    size = 1) # creating designated limit points for the generated data
    data2 <- round(mean(data), digits = digits) #this is the primary value generated for getting the correct answer
    scale <- if(data1 == "ten times smaller"){10}
    else{if(data1 == "ten times larger"){.1}
      else{if(data1 == "five times smaller"){5}
        else{{.2}}}} #this is randomly generated scale value
    constant <- if(data1 == "ten points less"){10} 
    else{if(data1 == "ten points greater"){-10}
      else{if(data1 == "five points less"){5}
        else{if(data1 == "five points greater"){-5}}}} #this is randomly generated constant value
    corr.ans <- if(data1 == "ten times smaller" | data1 == "ten times larger" | 
                   data1 == "five times smaller" | data1 == "five times larger")
    {round(scale*data2, digits = digits)}
    else{round(data2 + constant, digits = digits)} #this is the correct answer to the question
    up.min <- round(corr.ans + sd(data)/8, digits = digits) # This is the minimum value for incorrect answers above the correct answer
    down.max <- round(corr.ans - sd(data)/8, digits = digits) # This is the maximum value for incorrect answers below the correct answer
    ans.txt <- round(sample(c(if(data1 == "ten times smaller" | data1 == "ten times larger" |
                                 data1 == "five times smaller" | data1 == "five times larger")
    {c(sum(scale*data), sum(scale*data)/(dat.size1-1), data2)}
    else{c(sum(data + constant), sum(data + constant)/(dat.size1-1), data2)},
    seq(corr.ans - 2*sd(data), down.max, 10^-digits),
    seq(up.min, corr.ans + 2*sd(data), 10^-digits)),
    size = answers),
    digits = digits) # These are randomly generated incorrect answers.
    content <- c(type, ID, ID, paste(quest.txt1, data1, quest.txt2, data2, quest.txt3,
                                     paste(as.character(data), collapse=",  ",sep=""),
                                     collapse = "", sep = ""),
                 points.per.q, difficulty, points, hint, feedback) # This is collecting a lot of the above information into a single vector
    options <- c(rep("",6), ans.txt, rep("",2)) # This is collecting the incorrect answers above, and indexing them correctly by row
    options[corr.ind] <- corr.ans # This is imputing the correct answer at the appropriate row index
    questions[(1+(8+answers)*i):((8+answers)*(i+1)),1] <- param # This is indexing and storing all the row names
    questions[(1+(8+answers)*i):((8+answers)*(i+1)),2] <- content # Indexing and storing all the content
    questions[(1+(8+answers)*i):((8+answers)*(i+1)),3] <- options # Indexing and storing the answers, both incorrect and correct
  }
  questions <- questions[(9+answers):((8+answers)*(n+1)),] # Storing only what's needed for e-learning upload as a CSV file
  write.table(questions, sep=",", file=paste(title, ".csv", sep = ""),
              row.names=F, col.names=F) # Writing the CSV file
}
MeanMC3() # creating the csv file