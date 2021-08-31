library(tidyverse)
library(ggplot2)
library(GGally)
library(tidytext)
library(wordcloud2)
library(radarchart)

data <- read_csv("~/Desktop/R/data/data/news_articles.csv.gz")


# check the number of news by each source
data %>%
    group_by(source_name) %>%
    count(source_name) %>%
    arrange(desc(n))

data %>% 
    filter(source_name == "460.0")

data <- data %>%
    filter(!source_name == "460.0")


# check whether counts of NA values from reaction,comment and share columns are same.
data %>%
    filter(is.na(engagement_reaction_count) & is.na(engagement_comment_count) & is.na(engagement_comment_plugin_count) & is.na(engagement_share_count)) %>%
    nrow()

#so drop NA values and combine engagement_comment_count and engagement_comment_plugin_count as they share same category in comment
clean_at_counts <- data %>%
    filter(!(is.na(engagement_reaction_count) & is.na(engagement_comment_count) & is.na(engagement_comment_plugin_count) & is.na(engagement_share_count))) %>%
    mutate(comment_count = engagement_comment_count + engagement_comment_plugin_count) 

    
clean_at_counts %>% 
    select("source_name", "engagement_reaction_count", "comment_count", "engagement_share_count") %>%
    group_by(source_name) %>%
    summarize(
        # Calculate the average counts for each sources
        average_reaction_count = sum(engagement_reaction_count,na.rm=TRUE)/n(),
        average_comment_count = sum(comment_count)/n(),
        average_share_count = sum(engagement_share_count)/n(),
        # sum for showing from largest to smallest at graph/ nothing more
        sum = average_reaction_count + average_comment_count + average_share_count,
    ) %>%
    ungroup() %>%
    # make source name as factor according to sum (from largest to smallest)
    mutate(source_name = fct_reorder(source_name, sum)) %>%
    gather(category, count, -c(source_name,sum)) %>%
    ggplot(aes(x = source_name, y = count, fill = category)) +
    geom_col() +
    coord_flip() +
    theme_bw() +
    labs(title = "Average Engagements of News Sources", y = "Average Counts per news", x = "", fill = "Category") +
    scale_fill_manual(labels = c("Average Comment Counts","Average Reaction Counts","Average Share Counts"), values = c("orchid","green","royalblue")) +
    theme(legend.position = c(0.8,0.2))

# -------------------------------------------------------------------------------
# So, according to the graph, CNN and New York Times has most people interest rate than other during this period and interesting thing is ESPN has no engagements. May be people watch sport news from live channels and less read from website. Next is Reuters has too many share counts compared to comment and reaction count. 


# Top articles???
# top_article is said "value indicating if article was listed as a top article on publisher website" but one interesting thing about top article is, though there was no comment, reaction, share,but said to top articles.  

data %>%
    filter(top_article == 1 & engagement_comment_count == 0 & engagement_reaction_count == 0 & engagement_share_count == 0 & engagement_comment_plugin_count == 0) %>%
    group_by(source_name) %>%
    summarize(number_of_top_articles_without_engagements = n())

# -------------------------------------------------
clean_at_counts$top_article <- factor(clean_at_counts$top_article, levels = c(1,0), labels = c("Yes","No"))
clean_at_counts[,c(11,12,14,16)] %>%
    gather(category, count, -top_article) %>%
    ggplot(aes(x = category, count, fill = top_article)) +
    geom_boxplot() +
    scale_y_log10() +
    theme_bw() +
    labs(x = "", y = "Log of counts", fill = "Top Article") +
    scale_fill_manual(values = c("royalblue", "green"))
# on average, it is true that top articles have greater number of people interest rate but there are outliers in not top article that beats the maximum numbers of top articles. To be sure, top articles have greater number of people interest rate, let's check.

t.test(comment_count ~ top_article, data = clean_at_counts, conf.level = 0.99)
t.test(engagement_reaction_count ~ top_article, data = clean_at_counts, conf.level = 0.99)
t.test(engagement_share_count ~ top_article, data = clean_at_counts, conf.level = 0.99)

# Yes, it is true with even confidence level 99%. 

# But there are outliers in not rating as top article, and so let's check are there articles rating as not top article that beats the maximum engagement numbers of top article according to news sources.

summary_engagement <- clean_at_counts %>%
    select("source_name", "top_article", "engagement_reaction_count", "comment_count", "engagement_share_count") %>%
    group_by(source_name, top_article) %>%
    summarize(
        Comment = max(comment_count),
        Reaction = max(engagement_reaction_count),
        Share = max(engagement_share_count)
    ) %>%
    gather(category, max_count, -c(source_name, top_article)) %>%
    spread(top_article, max_count) %>%
    mutate(difference = `Yes` - `No`) %>%
    filter(source_name != "ESPN")

ggplot(summary_engagement, aes(source_name, difference)) +
    geom_col(fill = "royalblue") +
    coord_flip() +
    facet_wrap(~ category,nrow=2, scales = "free_x") +
    labs(x = "News Sources", y = "", title = "Difference between maximum engagement numbers of \ntop article and not rating as top article") +
    theme_bw()

data.frame(clean_at_counts %>%
    filter(source_name == "CNN" & engagement_reaction_count == max(engagement_reaction_count))) %>%
    select(description, url)
    
# It is about the former US president, Jimmy Carter and it is also the maximum reaction and share counts among all the news. 
# ------------------------------------------------------------------

# Which topics are most interested?
# To determine whether topic is interested or not is difficult as it is not a categorical value. I do not want to use "top_article" parameter to decide as there are outliers in not top articles. I want to use directly from engagement counts to determine whether a topic is interested or not. There are 3 separated parameters; comment, share, and reaction. Are they associated? In general sense, it may be positively correlated. Let's look at with the data set. 

colnames(clean_at_counts)
ggpairs(clean_at_counts[,c(11,12,14,16)], columns = 2:4, aes(color = top_article, alpha = 0.5)) +
    scale_color_manual(values = c("blue","green")) +
    scale_x_log10() + 
    scale_y_log10() 

# Comment and reaction counts are positively strong correlated. It makes sense. So, I will combine these three columns.

total_engagement_df <- clean_at_counts %>%
    mutate(total_engagements = engagement_reaction_count + comment_count + engagement_share_count) %>%
    select(3:6,10,11,17) %>%
    rowid_to_column("news_id")

summary(total_engagement_df$total_engagements)

# median number is 13 and let's cut that point whether people interested or not. 
total_engagement_df <- total_engagement_df %>%
    mutate(interested = ifelse(total_engagements > 0, "Yes","No"),
           interested = as.factor(interested))

table(total_engagement_df$interested)

sapply(total_engagement_df, function(x){sum(is.na(x))})

# which titles are most people interested?
interest_title <- total_engagement_df %>%
    filter(!is.na(title) & interested == "Yes") %>%
    select(news_id, title) %>%
    unnest_tokens(word, title) %>%
    filter(str_detect(word, "[^0-9]")) %>%
    anti_join(stop_words) %>%
    count(word) %>%
    arrange(desc(n)) %>%
    top_n(200)
wordcloud2(interest_title, shape = "cardioid")


two_words <- total_engagement_df %>%
    filter(!is.na(title) & interested == "Yes") %>%
    select(news_id, title) %>%
    unnest_tokens(ngram, title, token="ngrams", n=2, stopwords = stop_words$word) %>% filter(!ngram %in% c("york times","street journal","wall street")) %>%
    count(ngram) %>%
    arrange(desc(n)) %>%
    top_n(50)
    
wordcloud2(two_words)

############
description_text <- total_engagement_df %>%
    mutate(text = ifelse(is.na(content), description, content)) %>%
    filter(!is.na(text)) %>%
    select(1,2,7,9,10)

nrc_words <- description_text %>%
    select(-2) %>% 
    unnest_tokens(word, text) %>%
    anti_join(stop_words) %>%
    inner_join(get_sentiments("nrc"))

top_article_nrc <- nrc_words %>%
    group_by(news_id, top_article) %>%
    count(sentiment) %>%
    spread(sentiment, n, fill = 0) %>%
    gather(category, score, -c("news_id","top_article")) %>%
    group_by(top_article, category) %>%
    summarize(average_score = mean(score)) %>%
    spread(top_article, average_score)


interested_nrc <- nrc_words %>%
    group_by(news_id, interested) %>%
    count(sentiment) %>%
    spread(sentiment, n, fill = 0) %>%
    gather(category, score, -c("news_id","interested")) %>%
    group_by(interested, category) %>%
    summarize(average_score = mean(score)) %>%
    spread(interested, average_score)

chartJSRadar(top_article_nrc, main="Word contents of Top and Not top Articles", responsive=TRUE, polyAlpha = 0, colMatrix = matrix(c(c(0,0,255), c(255,0,0)), nrow = 3))
chartJSRadar(interested_nrc, main="Word contents of Interested and Not interested Articles", responsive=TRUE, polyAlpha = 0, colMatrix = matrix(c(c(0,0,255), c(255,0,0)), nrow = 3))

############
pos_neg_words <- description_text %>%
    select(text) %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words) %>%
    inner_join(get_sentiments("bing")) %>%
    group_by(sentiment) %>%
    count(word) %>%
    group_by(sentiment) %>%
    arrange(desc(n)) %>%
    top_n(20) %>%
    mutate(word = fct_reorder(word, n))

ggplot(pos_neg_words, aes(word, n, fill = sentiment)) +
    geom_col() +
    coord_flip() +
    labs(y = "Count of Words", x = "Words", title = "Words that are most appeared in News Articles", fill = "") + 
    theme_bw() +
    scale_fill_manual(labels = c("Negative word","Positive word"), values = c("red","royalblue")) +
    theme(legend.position = c(0.8,0.2))


#############

radar_df <- function(df) {
    trans <- data.frame(t(df[,2:3]))
    colnames(trans) <- df$category
    min_max <- data.frame(matrix(rep(c(2.5,0),10), nrow=2))
    colnames(min_max) <- df$category
    radar <- rbind(min_max, trans)
    return(radar)
}

top_article <- radar_df(top_article_nrc)
top_article
top_interest <- radar_df(interested_nrc)
top_interest

op <- par(mar = c(1, 2, 2, 2))
par(mfcol = c(1,2))
radarchart(top_article, caxislabels = c(seq(0,2.5,0.5)), seg=5, plty=1, pcol=c("red","blue"), title="Word Contents of top and \nnot top articles",axistype = 3)

legend("bottom",legend = rownames(top_article[-c(1,2),]), col = c("red","blue"), pch = 20,bty = "n",text.col = "black", cex = 1, pt.cex = 1.5,horiz=TRUE,)

radarchart(axistype=3, top_interest, caxislabels = c(seq(0,2.5,0.5)), seg=5, plty=1, pcol=c("red","blue"), title="Word Contents of Engaged and \nNot Engaged articles")

legend("bottom",legend = rownames(top_interest[-c(1,2),]), col = c("red","blue"), pch = 20,bty = "n",text.col = "black", cex = 1, pt.cex = 1.5,horiz=TRUE)

par(op)


pos_neg_words %>%
    filter(diff == 8) %>%
    inner_join(total_engagement_df[,c("news_id","url")], by="news_id")




pos_neg_words %>%
    filter(diff == -8 & news_id == 1890) %>%
    select(text) %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words) %>%
    inner_join(get_sentiments("bing"))














