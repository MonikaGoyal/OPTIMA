library(twitteR)
library(stringr)
library(ROAuth)
library(RCurl)
library(ggplot2)
library(reshape)
library(tm)
library(RJSONIO)
library(wordcloud)
library(gridExtra)
library(plyr)
library(shinyIncubator)
library(shiny)
library(Rstem)
library(sentiment)
library(e1071)

#API Keys for Authentication
api_key <- "oD4SchhDCgMvWXRvUHkaUMgPn"
api_secret <- "3COFC1BiladtmqW0xJsM0SZjAyndKZ8Lz6PFmkrP08bOsAwgtu"
access_token <- "2835319130-An0HLJM4taHAPdpeDsSkHgaK3XtPMrfcKnH04Z4"
access_token_secret <- "KShxGZ3anjz1gPy1edPqZ4ZJydS8naExWHPnSCRNKgi3N"
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

# Function to create a data frame from tweets
shinyServer(function(input, output,session) {
  
  CleanTweets<-function(tweets)
  {
    # Remove redundant spaces
    tweets <- str_replace_all(tweets," "," ")
    # Get rid of URLs
    tweets <- str_replace_all(tweets, "http://t.co/[a-z,A-Z,0-9]*{8}","")
    # Take out retweet header, there is only one
    tweets <- str_replace(tweets,"RT @[a-z,A-Z]*: ","")
    # Get rid of hashtags
    tweets <- str_replace_all(tweets,"#[a-z,A-Z]*","")
    # Get rid of references to other screennames
    tweets <- str_replace_all(tweets,"@[a-z,A-Z]*","")
    return(tweets) 
  } 
  
#Search tweets and create a data frame 
  TweetFrame<-function(searchTerm, maxTweets)
  {
    twtList<-searchTwitter(searchTerm,n=maxTweets,lang="en")
    twtList1<- do.call("rbind",lapply(twtList,as.data.frame))
    twtList1$text<-iconv(twtList1$text, 'UTF-8', 'ASCII') 
    return(twtList1)  
  }

#Function to generate word cloud 
  wordcloudentity<-function(entitycleantext)
  {
    tweetCorpus<-Corpus(VectorSource(CleanTweets(entitycleantext)))
    tweetTDM<-TermDocumentMatrix(tweetCorpus,control=list(removePunctuation=TRUE,
                                                          stopwords=c(stopwords('english')),
                                                          removeNumbers=TRUE,tolower=TRUE))
    tdMatrix <- as.matrix(tweetTDM) # creating a data matrix
    sortedMatrix<-sort(rowSums(tdMatrix),decreasing=TRUE) # calculate row sum of each term and sort in descending order (high freq to low)
    cloudFrame<-data.frame(word=names(sortedMatrix),freq=sortedMatrix)#extracting names from named list in prev command and binding together into a dataframe with frequencies - called cloudFrame, names in separate columns
    
    wcloudentity<-wordcloud(cloudFrame$word,cloudFrame$freq,max.words=100, colors=brewer.pal(8,"Dark2"),scale=c(8,1), random.order=TRUE)
    print(wcloudentity)
  }
  
# Scoring sentiment expressed - Breen's algorithm
  score.sentiment = function(sentences, pos.words, neg.words)
  { 
    # we got a vector of sentences. plyr will handle a list
    # or a vector as an "l" for us
    # we want a simple array ("a") of scores back, so we use 
    # "l" + "a" + "ply" = "laply":
    scores = laply(sentences, function(sentence, pos.words, neg.words) {
      
      # clean up sentences with R's regex-driven global substitute, gsub():
      sentence = gsub('[[:punct:]]', '', sentence)
      sentence = gsub('[[:cntrl:]]', '', sentence)
      sentence = gsub('\\d+', '', sentence)
      # and convert to lower case:
      sentence = tolower(sentence)
      
      # split into words. str_split is in the stringr package
      word.list = str_split(sentence, '\\s+')
      # sometimes a list() is one level of hierarchy too much
      words = unlist(word.list)
      
      # compare our words to the dictionaries of positive & negative terms
      pos.matches = match(words, pos.words)
      neg.matches = match(words, neg.words)
      
      # match() returns the position of the matched term or NA
      # we just want a TRUE/FALSE:
      pos.matches = !is.na(pos.matches)
      neg.matches = !is.na(neg.matches)
      
      # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
      score = sum(pos.matches) - sum(neg.matches)
      
      return(score)
    }, pos.words, neg.words)
    
    scores.df = data.frame(score=scores, text=sentences, size=seq(length(scores)))
    return(scores.df)
  }
  
#calling the above sentiment scoring function, the text of tweets serve as inputs
  sentimentalanalysis<-function(entity1text,entity1entry){
    
    # A compiled list of words expressing positive and negative sentiments ----
    #http://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html
    # List of words and additional information on the original source from Jeffrey Breen's github site at:
    #https://github.com/jeffreybreen/twitter-sentiment-analysis-tutorial-201107/tree/master/data/opinion-lexicon-English
    
    positivewords=readLines("positive-words.txt")
    negativewords=readLines("negative-words.txt")
    
    #Applying score.sentiment algorithm to cleaned tweets and getting data frames of tweets, net sentiment score for a tweet 
    #(number of positive sentiments minus negative sentiments)
    
    entity1score = score.sentiment(CleanTweets(entity1text),positivewords,negativewords)
    
    # Adding a dummy variable useful for a ggplot
    entity1score$entity = entity1entry
    
    #combine all of this
    entityscores<-rbind(entity1score)  
  }   

#Classify Tweets among Positive, Negative and Neutral Classes  
classify <- function(searchTerm, maxTweets)
{
  doc <-searchTwitter(searchTerm,n=maxTweets,lang="en")
  doc = sapply(doc, function(x) x$getText())
  doc = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", doc)
  doc = gsub("@\\w+", "", doc)
  doc = gsub("[[:punct:]]", "", doc)
  doc = gsub("http\\w+", "", doc)
  doc = gsub("[ \t]{2,}", "", doc)
  doc = gsub("^\\s+|\\s+$", "", doc)
  # define "tolower error handling" function 
  try.error = function(x)
  {
    # create missing value
    y = NA
    # tryCatch error
    try_error = tryCatch(tolower(x), error=function(e) e)
    # if not an error
    if (!inherits(try_error, "error"))
      y = tolower(x)
    # result
    return(y)
  } 
  doc = sapply(doc, try.error)
  doc = doc[!is.na(doc)]
  names(doc) = NULL
  #doc<-Corpus(VectorSource(CleanTweets(entitycleantext)))
  # classify emotion
  class_emo = classify_emotion(doc, algorithm="bayes", prior=1.0)
  # get emotion best fit
  emotion = class_emo[,7]
  # substitute NA's by "unknown"
  emotion[is.na(emotion)] = "unknown"
  # classify polarity
  class_pol = classify_polarity(doc, algorithm="bayes")
  # get polarity best fit
  polarity = class_pol[,4]
  # data frame with results
  sent_df = data.frame(text=doc, emotion=emotion,
                       polarity=polarity, stringsAsFactors=FALSE)
  # sort data frame
  sent_df = within(sent_df,
                   emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))
  return(sent_df)
}

  
# Time for execution
#tab 1- Raw Tweets as per Keyword Extracted from Twitter
  entity1<-reactive({
    if(input$actb>=0 ){ 
      withProgress(session, min=1, max=15, expr={
        for(i in 1:15) {
          setProgress(message = 'Calculation in progress',
                      detail = 'This may take a while...',
                      value=i)
          
          Sys.sleep(0.1)
        }
      })}
    entity1<-TweetFrame(input$entity1, input$maxTweets)}
  )
output$tableentity1 <- renderTable({tab<-entity1()[1]})

#tab 2 - Word Clouds to highlight terms used in tweets associated with the two entities
output$entity1wc<-renderText({
  input$entity1})
output$entity1wcplot<-renderPlot({
  if(input$actb>=0 ){ 
    withProgress(session, min=1, max=15, expr={
      for(i in 1:15) {
        setProgress(message = 'Calculation in progress',
                    detail = 'This may take a while...',
                    value=i)
        
        Sys.sleep(0.1)
      }
    })}
wordcloudentity(entity1()$text)})
    
#tab 3- Creating sentiment scores
  entityscores<-reactive({
    if(input$actb>=0 ){ 
      withProgress(session, min=1, max=15, expr={
        for(i in 1:15) {
          setProgress(message = 'Calculation in progress',
                      detail = 'This may take a while...',
                      value=i)
          Sys.sleep(0.1)
        }
      })}
    entityscores<-sentimentalanalysis(entity1()$text,input$entity1)})
  
  output$sentiboxplot<-renderPlot({
    if(input$actb>=0 ){ 
      withProgress(session, min=1, max=15, expr={
        for(i in 1:15) {
          setProgress(message = 'Calculation in progress',
                      detail = 'This may take a while...',
                      value=i)
          Sys.sleep(0.1)
        }
      })}
    cutoff <- data.frame(yintercept=0, cutoff=factor(0))
    sentiboxplot<-ggplot(entityscores(),aes(x=size,y=score))+
      facet_grid(entity ~ .)+
      geom_point(color = "black",size = 2, alpha = 1/2)+
      geom_smooth(method = "loess",se=FALSE,col='red',size=1.5, alpha = 0.7)+
      geom_hline(aes(yintercept=yintercept, linetype=cutoff), data=cutoff)+
      xlab('Tweet number')+
      ylab('Sentiment Score')+
      theme_bw()
    print(sentiboxplot)})
  
  #getting a feel for how sentiments were scored by scanning 10 tweets per entity and sentiment scores 
  #output$sentiheadtable<-renderTable({tab<-head(entityscores(),10)})
  #output$sentitailtable<-renderTable({tab<-tail(entityscores(),10)})
  
#tab 4- Generating SentiPlots (Emotions and Polarity)
naive<-reactive({
  if(input$actb>=0 ){ 
    withProgress(session, min=1, max=15, expr={
      for(i in 1:15) {
        setProgress(message = 'Calculation in progress',
                    detail = 'This may take a while...',
                    value=i)
        
        Sys.sleep(0.1)
      }
    })}
  naive<-classify(input$entity1, input$maxTweets)}
) 

#Download the Classified Data for further Analysis
output$downloadData <- downloadHandler(
  filename = function() {
    paste('Tweets', Sys.Date(), '.csv', sep = ".")
  },
  content = function(file) {
    write.csv(naive(), file)
  }
)
#output$NB<-renderTable({tab<-naive()})
output$emotionplot<-renderPlot({
    if(input$actb>=0 ){ 
    withProgress(session, min=1, max=15, expr={
      for(i in 1:15) {
        setProgress(message = 'Calculation in progress',
                    detail = 'This may take a while...',
                    value=i)
        
        Sys.sleep(0.1)
      }
    })}
    
emotionplot <- ggplot(naive(), aes(x=emotion)) +
      geom_bar(aes(y=..count.., fill=emotion)) +
      scale_fill_brewer(palette="Dark2") +
      labs(x="emotion categories", y="number of tweets") +
      labs(title = "Sentiment Analysis of Tweets \n(classification by emotion)",
           plot.title = element_text(size=12))
    print(emotionplot)
  })
  
output$polarityplot<-renderPlot({
    if(input$actb>=0 ){ 
      withProgress(session, min=1, max=15, expr={
        for(i in 1:15) {
          setProgress(message = 'Calculation in progress',
                      detail = 'This may take a while...',
                      value=i) 
          Sys.sleep(0.1)
        }
      })}
polarityplot <- ggplot(naive(), aes(x=polarity)) +
      geom_bar(aes(y=..count.., fill=polarity)) +
      scale_fill_brewer(palette="RdGy") +
      labs(x="polarity categories", y="number of tweets") +
      labs(title = "Sentiment Analysis of Tweets \n(classification by polarity)",
           plot.title = element_text(size=12))
    print(polarityplot)
  })

#tab 5- Naive Bayes Machine Learning Classifier
output$Contents <- renderTable({
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    
    dataset<-read.csv(inFile$datapath)
    model <- naiveBayes(polarity ~ ., data = dataset)
    index <- 1:nrow(dataset)
    testindex <- sample(index, trunc(length(index)/3))
    trainData <- dataset[testindex,]
    testData <- dataset[-testindex,]
    model <- naiveBayes(trainData[,1:3], trainData[,4])
    predicted <- predict(model, testData[,-4])
    pred <- predict(model, testData)
    table(pred, testData$polarity) 
  })
  
#tab 5- SVM Machine Classifier
output$Contents1 <- renderTable({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    
    dataset<-read.csv(inFile$datapath)
    model  <- svm(as.factor(polarity)~., data = dataset)
    summary(model)
    index <- 1:nrow(dataset)
    testindex <- sample(index, trunc(length(index)/3))
    testset <- dataset[testindex,]
    trainset <- dataset[-testindex,]
    model  <- svm(as.factor(polarity)~., data = trainset)
    prediction <- predict(model, testset[,-4])
    tab <- table(pred = prediction, true = testset[,4])
    tab
  })
  
})
