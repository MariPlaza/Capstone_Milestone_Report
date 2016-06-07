# This function allows to calculate the statistics from a file in terms of size, number of lines and length of the lines. 
Files_Stats <- function(filename, file){
    size <- file.info(filename)$size/1024^2 #size
    Line_Count <- length(file)
    Smallest_Line <- min(nchar(file))
    Longest_Line <- max(nchar(file))
    Files_Stats <- data.frame(filename, size, Line_Count, Smallest_Line, Longest_Line)
    return(Files_Stats)
}

# This function allows to clean a corpus considering a variety of possibilities.
Cleaned_Corpus <- function (OriginalText, ProfanityURL ='http://www.bannedwordlist.com/lists/swearWords.txt',
                            RemoveUperCase = TRUE, 
                            RemovePunctuation = TRUE, 
                            RemoveNumbers = TRUE,
                            WhiteSpace = TRUE, 
                            Profanity = TRUE, 
                            RemoveInternetInfo = TRUE, 
                            RemoveCommonWords = TRUE, 
                            RemoveRadicals = TRUE,
                            OutputPlainText = TRUE) 
{
    ProfanityFile <- suppressWarnings(readLines(ProfanityURL, encoding = "UTF-8", warn=TRUE, skipNul=TRUE))
    Cleaned_Corpus <- OriginalText
    #convert_to_and <- function(corpus) gsub(pattern = "&", replacement = " and ", corpus)
    #Remove all information related with typical Internet patterns. 
    if (RemoveInternetInfo) {
        Empty_Function <- content_transformer(function(x, pattern) gsub(pattern, "", x))
        Space_Function <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
        Cleaned_Corpus <- tm_map(Cleaned_Corpus, Empty_Function, "#\\w+") # Remove Hashtags
        Cleaned_Corpus <- tm_map(Cleaned_Corpus, Empty_Function, "(\\b\\S+\\@\\S+\\..{1,3}(\\s)?\\b)") # Remove emails
        Cleaned_Corpus <- tm_map(Cleaned_Corpus, Empty_Function, "@\\w+") # Remove @
        Cleaned_Corpus <- tm_map(Cleaned_Corpus, Empty_Function, "http[^[:space:]]*") # Remove web pages starting with http
        Cleaned_Corpus <- tm_map(Cleaned_Corpus, Space_Function, "/|@|\\|") # Remove slashes
    }
    #Remove words that are very frequent in a specific language. 
    if (RemoveCommonWords) Cleaned_Corpus<-tm_map(Cleaned_Corpus, removeWords, stopwords("english"))
    #Transform upercases in lowercases to facilitate the comparison. 
    if (RemoveUperCase) Cleaned_Corpus<- tm_map(Cleaned_Corpus, tolower) #Convert to lower case
    #Remove punctuation from Corpus 
    if (RemovePunctuation) Cleaned_Corpus<- tm_map(Cleaned_Corpus, removePunctuation) #Remove all punctuation
    #Remove numbers
    if (RemoveNumbers) Cleaned_Corpus<- tm_map(Cleaned_Corpus, removeNumbers) #Remove all numbers
    #Remove unnecesary white spaces 
    if (WhiteSpace) Cleaned_Corpus<- tm_map(Cleaned_Corpus, stripWhitespace) #Remove unnecesary white spaces
    #Remove words that are considered bad in our language. The default list is provided by http://www.bannedwordlist.com/lists/swearWords.txt
    if (Profanity) Cleaned_Corpus<- tm_map(Cleaned_Corpus, removeWords, ProfanityFile)
    #Left just the roots of the words. 
    if (RemoveRadicals) Cleaned_Corpus<-tm_map(Cleaned_Corpus, stemDocument)
    #Transform the corpus in Plain text. 
    if (OutputPlainText) Cleaned_Corpus<-tm_map(Cleaned_Corpus, PlainTextDocument)
    rm(ProfanityFile)
    
    return(Cleaned_Corpus)
}

#Calculate the matrix corresponding to a n-gram model including just the topN combinations. If you want to have the most possible combination, please do not provide this parameter. 
Matrix_N_Gram <- function(Corpus, n_gram, files_names, topN = 50000000)
{
    Token_Gram <- function(x) NGramTokenizer(x, Weka_control(min = n_gram, max = n_gram))    
    Matrix_Gram <- TermDocumentMatrix(Corpus, control = list(tokenize = Token_Gram))
    n<-1
    repeat{
        Gram_HF_Terms <- findFreqTerms(Matrix_Gram, lowfreq = n)
        if(length(Gram_HF_Terms)<topN){
            break
        }
        n <- n*2
    }
    Matrix_Gram <- as.matrix(Matrix_Gram[Gram_HF_Terms,])
    colnames(Matrix_Gram) <- files_names
    
    Matrix_Gram <- as.data.frame(Matrix_Gram)
    Matrix_Gram$words <- row.names(Matrix_Gram)
    return(Matrix_Gram)
}

#Create a graph for the n-gram model. 
Graph_N_Gram <- function(Matrix_NGram,n_gram,showlegend=FALSE){
    DataPlot <- melt(Matrix_NGram, id.vars=c("words"))
    colnames(DataPlot)[2] <- "Source"
    
    # Create the plot for Matrix 1 and format it. 
    plot <- ggplot(DataPlot, aes(x=reorder(words,-value), y=value, fill=Source)) + geom_bar(stat="identity")
    plot <- plot + xlab("")  + ylab("")  + ggtitle(paste0("N-Grams:",n_gram))  + theme(legend.position="none")
    plot <- plot + theme(panel.grid.major = element_line(colour = "white")) + theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
    
    if (showlegend){
        plot <- plot + theme(legend.position="top")
    }
    return(plot)
}

