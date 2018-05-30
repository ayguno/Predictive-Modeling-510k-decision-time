##########################################
# Parsing PDF files 
##########################################

# Note that the error message that will be catched in mac and windows are different
# For windows: "error in openconnectioncon rb cannot open the connection"
# For mac:     "error in openconnectioncon rb cannot open connection"


require(hunspell)
require(pdftools)
require(stringr)
require(tesseract)

current <- readRDS("current.rds")
current <- current[order(current$YEAR, decreasing = TRUE),]
# Keep only documents with potentially available summary statements
current_summary <- current[current$STATEORSUMM == "Summary",]
# Remove documents from 2000, since we learned they are not available
yindx <- substr(as.character(current_summary$KNUMBER),2,3)
remove2000 <- which(yindx != "00")
current_summary <- current_summary[remove2000,]

# Write the first document to start the file
K_number <- current_summary$KNUMBER[1]
# Note that for 2017, the mainurl is now different
pdfmain <- "https://www.accessdata.fda.gov/cdrh_docs/pdf17/"
pdfurl <- paste0(pdfmain,K_number,".pdf") 
# Note that wrapping the pdf_url into paste and collapse ~ combines all the pages 
# of the pdf text into a single character string object:
txt <- paste(pdf_text(pdfurl), sep = " ", collapse = "~")
closeAllConnections()

txt <- tolower(gsub("[^A-Za-z0-9 ]","",txt))
# Remove the extra white space
txt <- gsub("\\s+", " ", str_trim(txt))
count <-str_count(txt,tolower(K_number))
txt_frame <- data.frame(KNUMBER = K_number, txt = txt, 
                        Kcount = count, stringsAsFactors = F)
write.table(txt_frame, "Ksummary_text.csv", col.names = T, row.names = F, sep = ",")

successfull = 1

# Enter main scraping loop in which we will scrape other documents
for (i in 2:nrow(current_summary)){
        
K_number <- current_summary$KNUMBER[i]        
yindx <- substr(as.character(K_number),2,3)

if (yindx >17 ) 
        { yindx <- ""
}else if (yindx < 10)
        { yindx <- substr(as.character(yindx),2,2)
}
 
###################################
# First try to read directly
###################################

pdfurl <- paste0("https://www.accessdata.fda.gov/cdrh_docs/pdf",
                  yindx,"/",K_number,".pdf")
is_this_error <- try(txt <- paste(pdf_text(pdfurl), sep = " ", collapse = "~"))
closeAllConnections()
# Retain alphanumeric letters
txt <- tolower(gsub("[^A-Za-z0-9 ]","",is_this_error))
# Remove the extra white space
txt <- gsub("\\s+", " ", str_trim(txt))
count <-str_count(txt,tolower(K_number))

# if count is successful store the text data
if (count > 0){
       txt_frame <- data.frame(KNUMBER = K_number, txt = txt, 
                                Kcount = count, stringsAsFactors = F)
       
# if file is not accessible try alternative an source
} else if ((txt == "error in openconnectioncon rb cannot open the connection") & count == 0){
       
        pdfurl <- paste0("https://www.accessdata.fda.gov/cdrh_docs/reviews/",K_number,".pdf")
        is_this_error <- try(txt <- paste(pdf_text(pdfurl), sep = " ", collapse = "~"))
        closeAllConnections()
        # Retain alphanumeric letters
        txt <- tolower(gsub("[^A-Za-z0-9 ]","",is_this_error))
        # Remove the extra white space
        txt <- gsub("\\s+", " ", str_trim(txt))
        count <-str_count(txt,tolower(K_number))
        # if count is successful store the text data
        if (count > 0){
        txt_frame <- data.frame(KNUMBER = K_number, txt = txt, 
                                Kcount = count, stringsAsFactors = F)
        cat("Alternative source parsed document:", K_number, "\n")
        # if this is again connection problem, this means the file does not exist 
        }else if ((txt == "error in openconnectioncon rb cannot open the connection") & count == 0){
        txt_frame = NULL     
        }else{
                # At this point the file may exist in the alternative url, but it might be image
                # Try text recognition
                 cat("The file may exist in the alternative url, but it might be image document:", K_number, "\n")
                 cat("Try text recognition in the alternative url:", K_number, "\n")
                # Start by converting to image(s)
                try(images <- pdf_convert(pdf = pdfurl, format = "png", dpi = 400))
                closeAllConnections()
                txt <- NULL
                try(for(j in 1:length(images)){
                txt[j] <- ocr(image = images[j])
                cat("Completed extracting image:",j, "\n")
                })
                txt <- paste0(txt,sep = " ", collapse = "~")
                unlink(images)
                txt <- gsub("[^A-Za-z0-9 ]","",txt)
                # Perform a spellcheck using hunspell package at this point
                cat("Performing a spellcheck...\n")
                bad_words <- hunspell(txt)
                bad_words[[1]]
                suggestions <- hunspell_suggest(bad_words[1][[1]])
                # Get the first suggestion for each 'bad word'
                suggestions <- sapply(suggestions, function(x){
                return(x[1])})
                if(length(suggestions) > 0){
                       conversion_table <- data.frame(bad_words = bad_words[1][[1]],
                                       suggestions = suggestions, stringsAsFactors = FALSE)
                        # Don't keep NAs
                        conversion_table <- conversion_table[complete.cases(conversion_table),]
                        if (nrow(conversion_table) >0){
                                # Last step is to replace all bad words with the selected ones
                                for (replacing in 1:nrow(conversion_table)){
                                        txt <- gsub(pattern = conversion_table$bad_words[replacing],
                                        replacement = conversion_table$suggestions[replacing],
                                                x = txt)}
                        }
                         
                } 
                cat("...completed spellcheck.\n")
                txt <- gsub("[^A-Za-z0-9 ]","",txt)
                # Remove the extra white space and convert to lowercase
                txt <- tolower(gsub("\\s+", " ", str_trim(txt)))
                count <-str_count(txt,tolower(K_number))
                
                if(count > 0 ){
                        cat("Text recognition from the alternative url is sucessful for:", K_number, "\n")
                        txt_frame <- data.frame(KNUMBER = K_number, txt = txt, 
                                                Kcount = count, stringsAsFactors = F)
                }else{
                        txt_frame <- NULL 
                }
                        
        }
        
# if the file seem to be accesible from the first link but could't read the pdf, 
# the file could be image OR the text in the original link does not contain the K_number
# First look at if alternative link has text that can contain K_number, if yes store it        
# If the above fails try to recover data using text recognition. 
# If all fails, check for another common adobe error, if txt is not that error, store it otherwise skip       
} else if ((txt != "error in openconnectioncon rb cannot open the connection") & count == 0){
        
        # First look at if alternative link has text that can contain K_number, if yes store it
        pdfurl <- paste0("https://www.accessdata.fda.gov/cdrh_docs/reviews/",K_number,".pdf")
        is_this_error <- try(txt <- paste(pdf_text(pdfurl), sep = " ", collapse = "~"))
        closeAllConnections()
        # Retain alphanumeric letters
        txt1 <- tolower(gsub("[^A-Za-z0-9 ]","",is_this_error))
        # Remove the extra white space
        txt1 <- gsub("\\s+", " ", str_trim(txt1))
        count <-str_count(txt1,tolower(K_number))
        # if count is successful store the text data
        if (count > 0){
        txt_frame <- data.frame(KNUMBER = K_number, txt = txt1, 
                                Kcount = count, stringsAsFactors = F)
        cat("Alternative source parsed document:", K_number, "\n")
        # if this is again connection problem, this means the file does not exist 
        }else if ((txt1 == "error in openconnectioncon rb cannot open the connection") & count == 0){
                txt_frame = NULL     
        }else{
                # At this point the file may exist in the alternative url, but it might be image
                # Try text recognition
                 cat("The file may exist in the alternative url, but it might be image document:", K_number, "\n")
                 cat("Try text recognition in the alternative url:", K_number, "\n")
                # Start by converting to image(s)
                try(images <- pdf_convert(pdf = pdfurl, format = "png", dpi = 400))
                closeAllConnections()
                txt3 <- NULL
                try(for(j in 1:length(images)){
                txt3[j] <- ocr(image = images[j])
                cat("Completed extracting image:",j, "\n")
                })
                txt3 <- paste0(txt3,sep = " ", collapse = "~")
                unlink(images)
                txt3 <- gsub("[^A-Za-z0-9 ]","",txt3)
                # Perform a spellcheck using hunspell package at this point
                cat("Performing a spellcheck...\n")
                bad_words <- hunspell(txt3)
                suggestions <- hunspell_suggest(bad_words[1][[1]])
                # Get the first suggestion for each 'bad word'
                suggestions <- sapply(suggestions, function(x){
                return(x[1])})
                if(length(suggestions) > 0){
                       conversion_table <- data.frame(bad_words = bad_words[1][[1]],
                                       suggestions = suggestions, stringsAsFactors = FALSE)
                        # Don't keep NAs
                        conversion_table <- conversion_table[complete.cases(conversion_table),]
                        if (nrow(conversion_table) >0){
                                # Last step is to replace all bad words with the selected ones
                                for (replacing in 1:nrow(conversion_table)){
                                        txt3 <- gsub(pattern = conversion_table$bad_words[replacing],
                                        replacement = conversion_table$suggestions[replacing],
                                                x = txt3)}
                        }
                         
                } 
                cat("...completed spellcheck.\n")
                # Remove the extra white space and convert to lowercase
                txt3 <- tolower(gsub("\\s+", " ", str_trim(txt3)))
                count <-str_count(txt3,tolower(K_number))
                
                if(count > 0 ){
                        cat("Try text recognition from the alternative url is sucessful for:", K_number, "\n")
                        txt_frame <- data.frame(KNUMBER = K_number, txt = txt3, 
                                                Kcount = count, stringsAsFactors = F)
                }else{
                        txt_frame <- NULL 
                }
        
        }
        
        # If the above fails try to recover data using text recognition from the first link
        if(is.null(txt_frame)){
                # Get back the first link
                pdfurl <- paste0("https://www.accessdata.fda.gov/cdrh_docs/pdf",
                  yindx,"/",K_number,".pdf")
                # Start by converting to image(s)
                try(images <- pdf_convert(pdf = pdfurl, format = "png", dpi = 400))
                closeAllConnections()
                txt2 <- NULL
                try(for(j in 1:length(images)){
                txt2[j] <- ocr(image = images[j])
                cat("Completed extracting image:",j, "\n")
                })
                txt2 <- paste0(txt2,sep = " ", collapse = "~")
                unlink(images)
                txt2 <- gsub("[^A-Za-z0-9 ]","",txt2)
                # Perform a spellcheck using hunspell package at this point
                cat("Performing a spellcheck...\n")
                bad_words <- hunspell(txt2)
                suggestions <- hunspell_suggest(bad_words[1][[1]])
                # Get the first suggestion for each 'bad word'
                suggestions <- sapply(suggestions, function(x){
                return(x[1])})
                if(length(suggestions) > 0){
                       conversion_table <- data.frame(bad_words = bad_words[1][[1]],
                                       suggestions = suggestions, stringsAsFactors = FALSE)
                        # Don't keep NAs
                        conversion_table <- conversion_table[complete.cases(conversion_table),]
                        if (nrow(conversion_table) >0){
                                # Last step is to replace all bad words with the selected ones
                                for (replacing in 1:nrow(conversion_table)){
                                        txt2 <- gsub(pattern = conversion_table$bad_words[replacing],
                                        replacement = conversion_table$suggestions[replacing],
                                                x = txt2)}
                        }
                         
                } 
                cat("...completed spellcheck.\n")
                # Remove the extra white space and convert to lowercase
                txt2 <- tolower(gsub("\\s+", " ", str_trim(txt2)))
                count <-str_count(txt2,tolower(K_number))
                
                if(count > 0 ){
                        cat("Text recognition from the first url is sucessful for:", K_number, "\n")
                        txt_frame <- data.frame(KNUMBER = K_number, txt = txt2, 
                                                Kcount = count, stringsAsFactors = F)
                        # If all fails, check for another common adobe error. If txt is not that error, store it, otherwise skip
                }else if (txt != "for the best experience open this pdf portfolio in acrobat x or adobe reader x or later get adobe reader now"){
                cat("All attempts failed to recover K_number, but storing adobe error-free, potentially useful text for:", K_number, "\n")
                txt_frame <- data.frame(KNUMBER = K_number, txt = txt, 
                                                Kcount = count, stringsAsFactors = F)
                }else{
                        cat("Adobe error is detected for:", K_number, "\n")
                        txt_frame <- NULL 
                }
        }                        
}


if(!is.null(txt_frame)){
  # Continue writing into the same file by appending into it 
        write.table(txt_frame, "Ksummary_text.csv", ,sep = ",", append = T, 
             col.names = F, row.names = F)    
        successfull = successfull + 1
        cat("Completed parsing document:", K_number, "\n")      
        
}else{
        cat("All attempts failed parsing document:", K_number, "\n") 
        cat("Skipping document:", K_number, "\n") 
}

cat("The number of documents scanned so far: ", i ,"\n")
cat("The number of succesful scans so far: ", successfull ,"\n") 
cat(strrep("*", 40),"\n")

}
