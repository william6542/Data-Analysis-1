##############################################################################
## Assignment 3: Books in Translation

## This assignment draws from the Publisher's Weekly Translation Database, which includes "all works of fiction, poetry, children’s books, and nonfiction translated into English and published in the U.S. after January 2008." The data includes information about the book's original language, country, publisher, author, translator, author gender, translator gender, price, and more.

## https://www.publishersweekly.com/pw/translation/home/index.html

## FAQs https://www.publishersweekly.com/pw/corp/translation-database-FAQ.html

## Note: You will make 1 line plot and 2 bar plots in this assignment, which you will need to export as PNG image files and submit with your assignment to Gradescope
##############################################################################

################## Name: WILLIAM WU 

## 1. Load the Publisher's Weekly Translation dataset from the URL below and save it to a variable called `pw_df` (2 points)
# https://raw.githubusercontent.com/melaniewalsh/Neat-Datasets/main/PW-Translation-2022-11-01.csv
# Make sure you use the special argument that ensures text columns will be interpreted correctly
      ##### I know what this is, as in, the sep = ",", stringAsFactors = FALSE          But for some reason, when I add that, it returns the same file, all jammed together. But if I just put in the above, it automatically separates everything for me??? 
# Hint: This CSV file is separated by commas, unlike the version we used in class 
pw_df <- read.csv("https://raw.githubusercontent.com/melaniewalsh/Neat-Datasets/main/PW-Translation-2022-11-01.csv", sep = ",", stringsAsFactors = FALSE)

## 2. load DPLYR and ggplot2 (1 point)
library(dplyr)
library(ggplot2)

## 3. How many rows are in this dataset?
# Use a built-in R function to find the number of rows, save it to the variable `num_rows`, then print `num_rows` (1 point)
num_rows <- nrow(pw_df)
print(num_rows)           #### there are 9380 rows in the data frame

## 4. How many columns are in this dataset?
# Use a built-in R function to find the number of columns, save it to the variable `num_cols`, then print `num_cols` (1 point)
num_cols <- ncol(pw_df)
print(num_cols)         #### there are 18 columns in the data frame 

## 5. How many books from each language have been translated into English and published since 2008? 
# Calculate how many books from each language have been published (3 points)
## Save as total_languages
total_languages <- pw_df %>% group_by(language) %>% summarize(total_books_per_language = n())

## 6. Use a function to slice the top 7 languages with the most number of books translated into English
## Save as top_languages
top_languages <- total_languages %>% slice_max(n = 7, order_by = total_books_per_language)

# Now we want to make a line plot that charts the number of book translated into English for these top 7 languages between 2008 and 2022.
# First, we will need to calculate the number of books translated into English per language per year.
# Then, we will need to filter for just the top 7 languages.

## 7. How many books from each language were published per year? (3 points)
## Save as total_languages_per_year
total_languages_per_year <- pw_df %>% group_by(language, pubdate.yr) %>% summarize(total= n())

## 8. Now, we need to filter this dataframe for only the top 7 languages (2 points)
# We can't use our slicing tool for this task because it would only give us the top languages published in a given year, not overall
# So we'll need to use the work we did in Question 6 when we found the top 7 languages overall with our slicing tool
# Pro tip: You can extract any column from a dataframe and turn it into a vector by using dollar sign $ notation like so
top_languages_vector <- top_languages$language
# Now we have a vector that we can use to filter the dataframe total_languages_per_year for only the top 7 languages
# Hint:You may need to use %in% notation
## Save as top_languages_per_year 
top_languages_per_year <- total_languages_per_year %>% filter(language %in% top_languages_vector)
    #### I think this is correct. 

## 9. Finally, make a line plot of the top 7 languages and how many books have been translated into English from those languages from 2008-2022
# Also choose your own color palette
# And add a meaningful title, x/y axis labels, and a legend title (10 points)
library(plotly)
      #### use https://r-graph-gallery.com/line-chart-several-groups-ggplot2.html  for reference

interactive_top_languages_per_year_plot <- top_languages_per_year %>% ggplot(aes(x=pubdate.yr, y=total, group=language, color=language)) + geom_line() +ggtitle("Top Translated Languages") + labs(y="Total Translations per Year", x="Year Down Here", color = "Custom Legend Title") + scale_color_manual(values =c("Chinese" = "darkred", "French" = "blue", "German" = "darkgoldenrod1", "Italian" = "chartreuse2", "Japanese" = "azure4", "Spanish" = "darkorchid2", "Swedish"="deeppink1"))

ggplotly(interactive_top_languages_per_year_plot)

# Export the line plot as a PNG file, and be sure to submit it with your R script  ##COMPLETED

## 10. What do you make of the pattern represented in the line plot above? Does the fluctuation of languages over time surprise you? Why or why not? Answer in at least 2-3 sentences (2 points)

##### The stability of translated languages like Swedish, Japanese and Chinese does not surprise me. What surprises me is the jump that four big European languages exhibit around 2012-2015. These are Italian (to the least extent), Spanish, German, and most of all French. Why did said languages, and especially French and German, jump so high and so quickly in 2012-2013? Was there some popular thing going on that made French suddenly super popular? 

##### My guess is that starting with the slow internationalization of culture and the trans-national movements towards more diveristy, Americans are starting to appreciate the new voices in literature in foreign nations, especially those that are already culturally familiar to them. This was especially true after the recession, I think, in which American culture and ideology couldn't give people enough confidence and appreciation for life: instead, they turned to Europe for beauty. Now that the recession is over (although its effects are not), we can see the chart declining. 

##### The question then becomes: well then why did covid slowly kill translated works? I have two answers. The first is that Covid prevented translators from working. The second is that Covid was so damaging to American self image and worldwide cultural awareness that it simply just shut itself off from the world. We are so damaged as a nation that we can not find beauty and art anywhere anymore, thus, the appropriate (not morally, but naturally) reaction, which is to isolate oneself. 

## 11. Calculate the number of authors per gender across the entire dataset
## Save as author_gender (3 points)
author_gender <- pw_df %>% group_by(auth.gender) %>% summarize(total_gender = n())
  #### I edited and overwrote the row names, because N/A was blank. 

  #### Trying to overwrite blank spaces and add NA    
  #### reference: https://statisticsglobe.com/replace-blank-by-na-in-r
author_gender[author_gender == ""] <- NA
                            
## 12. Make a bar plot of author_gender, and add a meaningful title  (3 points)
a_g_plot <- author_gender %>% ggplot(aes(x=auth.gender, y=total_gender, fill=auth.gender, group=auth.gender, color=auth.gender)) + geom_col() +ggtitle("Bar Plot: Number of Authors per Gender") + labs(x="Author Gender", y="Total Number")

ggplotly(a_g_plot)
  #### For some reason, when I change this legend title, the default title "auth.gender" does not remove itself..... Whereas forthe above line plot of languages it worked....  unsure what's going on. 

# Export the bar plot as a PNG file, and be sure to submit it with your R script   ##COMPLETED

#_________________________________________________________________________________________________#

## 13. Calculate the number of translators per gender across the entire dataset (3 points)
## Save as translator_gender 
translator_gender <- pw_df %>% group_by(trnsl.gend) %>% summarize(total_translator_gender = n())
translator_gender[translator_gender == ""] <- NA

## 14. Make a bar plot of translator_gender, and add a meaningful title (3 points)
translator_gender_plot <- translator_gender %>% ggplot(aes(x=trnsl.gend, y=total_translator_gender, fill=trnsl.gend, group=trnsl.gend, color=trnsl.gend)) +geom_col() + ggtitle("Bar Plot: Number of Translators per Gender") + labs(x="Translator Gender", y="Total Number")

ggplotly(translator_gender_plot)
  #### Same problem here: for some reason the Custom Legend Title doesn't remove the default title. It just stacks below it...
# Export the bar plot as a PNG file, and be sure to submit it with your R script    ## COMPLETED

## 15. What do you make of the differences between author gender and translator gender? What do you think explains the similarity or difference? Answer in at least 2-3 sentences (2 points)
      #### The gap between male and female in translators is much less than in authors. I think this reason is because, as usual, women are not taught to have independence of action, strong will, and autonomy. They are always seen, and society expects, them to act in a supporting and following role, not in a leading role. Unfortunatley, it seems even in developed countries, this is still the case. But, is it actually the case? I will ask this next. 

## 16. Now it's your turn! Ask your own question about the PW Translation data, and then answer it with code (4 points)
## First, write down your question in English language words here:
# Your question here

      #### WHAT IS THE NUMBER OF AUTHORS BY GENDER BY EACH COUNTRY? DO CERTAIN COUNTRIES HAVE MORE OF A CERTAIN GENDER? HOW CAN WE EXPLAIN DIFFERENCES OR TRENDS IN DIFFERENT REGIONS?      EXAMINE THE TOP 10 COUNTRIES 

# Then answer it with code below
  #### first, find translations by country
country_translations <- pw_df %>% group_by(country) %>% summarize(total = n())

  #### second, slice the top 10 countries
top_countries <- country_translations %>% slice_max(n = 10, order_by = total)

  #### third, use top_countries to filter top 10 gender by country 
gender_by_country <- pw_df %>% group_by(country, auth.gender) %>% summarize(total = n())

top_countries_vector <- top_countries$country

top_gender_by_country <- gender_by_country %>% filter(country %in% top_countries_vector)
  #### adding NA to blank space, overwrite
top_gender_by_country[top_gender_by_country == ""] <- NA

  ###### put into bar plot
interactive_top_gender_by_country <- top_gender_by_country %>% ggplot(aes(y=total, x=country, group=country, color=auth.gender, fill=auth.gender)) + geom_col() + ggtitle("Bar Plot of Author Genders by Top 10 Countries") + labs(y="Total Authors of all Genders")

ggplotly(interactive_top_gender_by_country)

## 17. Were you able to answer your question? What do the patterns or results mean? Why does this question matter? Answer in at least 2-3 sentences (2 points)
    #####  ANSWER: So it looks like for all the top 10 countries, WITH THE EXCEPTION OF SWEDEN, the ratio of male to female publishers is around 2:1. Generally, the ratio tilts in favor of males, with some goin to 3:1 lke Russia. Sweden is th eonly nation in which the ratio of male to female publishers looks equal on the chart! (I checked my dataframe, and the ratio is 180 males to 161 females! wow!)

    #####  The other interesting thing is how almost all the top 10 countries have very little "Both" and "undisclosed" genders.

    ##### And the final interesting thing, is that the countries with most "NA" are France and Germany. I wonder why this is, since shouldn't it generally be easy for Americans to tell what gender a name is in German or French? (of course I'm making gender assumptions here, but please excuse this for the sake of the argument and questions). The other more plausible answer is that the raw data is obtained from self reported sources, and Germany and France out of all these countries are most aware of privacy concerns and data rights, so they choose to not fill in any data at all concerning gender. 

## 18. What's one more question that you would like to explore with this data if you had time? (2 points)
    #### I would want to see the correlation between price and country, see if the top 10 countries have significant price differences within the US (I am assuming the listed prices are prices in USD in the US)

