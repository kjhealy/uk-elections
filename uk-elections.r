###--------------------------------------------------
### Scraping 2015 UK election data from the BBC
###--------------------------------------------------

library(ggplot2)
library(scales)
library(dplyr)
library(rvest)
library(stringr)
library(car)

theme_set(theme_minimal())

get.constituency.results <- function(constituency){
    ## UK Constituency names are unique.
    name <- constituency %>% html_nodes(".constituency-title__title") %>% html_text()

    parties <- constituency %>% html_nodes(".party__name--long") %>% html_text()

    candidates <- constituency %>% html_nodes(".party__result--candidate") %>%
        html_text() %>% str_replace(", with candidate ", "")

    votes <- constituency %>% html_nodes(".party__result--votes") %>% html_text() %>%
        str_replace(",", "") %>% str_replace(" total votes taken.", "") %>% as.numeric()

    vote.share <- constituency %>% html_nodes(".party__result--votesshare") %>%
        html_text() %>% str_replace("% share of the total vote", "") %>% as.numeric()

    pct.swing <- constituency %>% html_nodes(".party__result--votesnet") %>%
        html_text() %>% str_replace("\\+", "") %>%
            str_replace("% change in share of the votes", "") %>% as.numeric()

    results.df <- data.frame(Constituency = name,
                             Party = parties,
                             Candidate = candidates,
                             Votes = votes,
                             Vote.Share = vote.share,
                             Swing = pct.swing
                             )

    return(results.df)
}


###--------------------------------------------------
### Get the data
###--------------------------------------------------


###  If we've done all this already, don't do it again
if(!file.exists("data/constituency_names.csv")){

    constituencies <- read_html("http://www.bbc.com/news/politics/constituencies") %>%
    html_nodes("td , .az-table__row th")

    constituency_names <- read_html("http://www.bbc.com/news/politics/constituencies") %>%
        html_nodes(".az-table")

    constituency_urls <- constituencies %>% html_nodes("a") %>% html_attrs() %>%
        paste("http://bbc.com", ., sep="")

    write.csv(constituency_urls, file="data/constituency_urls.csv")

    constituency_names <- lapply(constituency_names, html_table)
    library(data.table)
    constituency_names.df <- data.frame(rbindlist(constituency_names))
    detach(package:data.table)

    colnames(constituency_names.df) <- c("Constituency", "Nation")
    constituency_names.df$URL <- constituency_urls
    write.csv(constituency_names.df, file="data/constituency_names.csv")
}


###  And if we've done all *this* already, don't do it again
if(!file.exists("data/uk-election-results-2015.csv")){
### Get and store all constituency pages and data
all.results.list <- list()
for(i in 1:nrow(constituency_names.df)){
    url <- constituency_names.df$URL[i]
    constituency <- read_html(url)
    name <- constituency %>% html_nodes(".constituency-title__title") %>% html_text()
    fname <- paste("data/", name, ".rda", sep="")
    save(constituency, file=fname)

    all.results.list[[name]] <- get.consituency.results(constituency)
    message(paste("Completed", name))
    Sys.sleep(1.5) ## Try to be polite

}

data <- rbind_all(all.results.list)
ind <- match(data$Constituency, constituency_names.df$Constituency)
data$Region <- constituency_names.df$Nation[ind]

write.csv(data, "data/uk-election-results-2015.csv")

} else{

data <- read.csv("data/uk-election-results-2015.csv", row.names=1)

}

data <- data %>% group_by(Constituency) %>% mutate(Total.Votes.Cast=sum(Votes),
                                                   Rank=row_number(desc(Votes))) %>% data.frame()

### Recode Party
### parties by N candidates
main.parties <- data %>% group_by(Party) %>% tally() %>% arrange(desc(n)) %>%
    filter(n>14) %>% data.frame(.)

ind <- data$Party %nin% main.parties$Party

data$Party.all <- data$Party
data$Party <- as.character(data$Party)
data$Party[ind] <- "Other"
data$Party <- factor(data$Party, levels=unique(data$Party))

all.parties <- data %>% group_by(Party.all) %>% tally() %>% arrange(desc(n)) %>% data.frame()

by.mps <- data %>% group_by(Constituency) %>% filter(Votes==max(Votes))  %>%
    ungroup() %>% arrange(desc(Vote.Share))  %>% data.frame(.)

## In Seat order
by.seats <- by.mps %>% group_by(Party) %>% tally() %>% arrange(desc(n)) %>%
    data.frame(.)

library(gdata)
data$Party <- reorder.factor(data$Party, new.order=by.seats$Party)
by.mps$Party <- reorder.factor(by.mps$Party, new.order=by.seats$Party)
detach(package:gdata)

biggest.winners <- by.mps %>% filter(percent_rank(Votes)>0.97) %>%
    select(Constituency, Votes, Party) %>% arrange(desc(Votes)) %>%
    data.frame()


biggest.constituencies <- by.mps %>% ungroup() %>%
    filter(percent_rank(Total.Votes.Cast) > 0.97) %>% arrange(desc(Total.Votes.Cast)) %>%
        select(Constituency, Party, Total.Votes.Cast) %>% data.frame()


smallest.constituencies <- by.mps %>% ungroup() %>%
    filter(percent_rank(Total.Votes.Cast) < 0.03) %>% arrange(desc(Total.Votes.Cast)) %>%
        select(Constituency, Party, Total.Votes.Cast) %>% data.frame()

safest.seats <- by.mps %>% filter(Vote.Share>63) %>% select(Constituency, Vote.Share, Party) %>%
    data.frame()

by.nc <- data %>% group_by(Constituency) %>%
    summarize(N.cands=length(Candidate),
              Ballots=sum(Votes)) %>%
        ungroup() %>% arrange(desc(N.cands))

###--------------------------------------------------
### Now we can start looking at the data
###--------------------------------------------------


## Con, Lab, SNP, LibDem, DUP, Sinn Fein, Plaid Cymru, SDLP, UUP,
## UKIP, Green, Independent
uk.colors <- data.frame(Party=levels(by.mps$Party),
                        party.color=c("#1577C7", "#E8251F",
                            "#EAC135", "#FA8324",
                            "#BC1D40", "#126140",
                            "#559D37", "#6AA769",
                            "#6EB2E4", "#6E3485",  "#999999",
                            "#7EC031", "#999999"),
                        stringsAsFactors = FALSE)

not.gb <- c("Democratic Unionist Party", "Sinn Fein", "Social Democratic & Labour Party",
            "Ulster Unionist Party", "Independent")
gb.colors <- uk.colors %>% filter(Party %nin% not.gb)

## Look up party colors as needed
pc.look <- function(parties){
    x <- match(parties, uk.colors$Party)
    colors <- uk.colors$party.color[x]
    return(colors)
}


p <- ggplot(by.mps, aes(x=Total.Votes.Cast, y=Vote.Share, color=Party))
p +  geom_point() + scale_color_manual(values=uk.colors$party.color)

by.gb.mps <- data %>% group_by(Constituency) %>%
    filter(Votes==max(Votes) & Region!="Northern Ireland" )  %>%
    ungroup() %>% arrange(desc(Vote.Share))  %>% data.frame(.)

p <- ggplot(by.gb.mps, aes(x=Total.Votes.Cast, y=Vote.Share/100, color=Party))
p +  geom_point() + scale_color_manual(values=gb.colors$party.color) +
    labs(x="Votes Cast in Constituency", y="Vote Share of Winner", color="Winner's Party") +
        scale_x_continuous(labels=comma) + scale_y_continuous(labels=percent)

p <- ggplot(safest.seats, aes(x=reorder(as.character(Constituency), Vote.Share, order=TRUE),
                              y=Vote.Share,
                              color=Party))

p + geom_point(size=3) + coord_flip() + scale_color_manual(values=pc.look(c("Conservative", "Labour", "Other"))) + labs(x="", y="Winning Candidate's Vote Share") + ggtitle("Safest Seats") + theme(legend.position="top")
