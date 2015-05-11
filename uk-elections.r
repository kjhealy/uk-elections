###--------------------------------------------------
### Scraping 2015 UK election data from the BBC
###--------------------------------------------------

library(ggplot2)
library(scales)
library(dplyr)
library(rvest)
library(stringr)
library(car)

library(rgeos)
library(maptools)
library(rgdal)

theme_set(theme_minimal())

## Convenience "not in" function
"%nin%" <- function(x, y) {
  return( !(x %in% y) )
}

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

    ## The BBC URL is also the topoJSON identifier; useful later
    constituency_ids <- str_replace(constituency_urls,
                                     "http://bbc.com/news/politics/constituencies/",
                                     "")
    constituency_names <- lapply(constituency_names, html_table)
    library(data.table)
    constituency_names.df <- data.frame(rbindlist(constituency_names))
    detach(package:data.table)

    colnames(constituency_names.df) <- c("Constituency", "Nation")
    constituency_names.df$URL <- constituency_urls
    constituency_names.df$id <- constituency_ids

    write.csv(constituency_names.df, file="data/constituency_names.csv")
} else {
    constituency_names.df <- read.csv("data/constituency_names.csv", header=TRUE, row.names=1)
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

    all.results.list[[name]] <- get.constituency.results(constituency)
    message(paste("Completed", name))
    Sys.sleep(1.5) ## Try to be polite

}

data <- rbind_all(all.results.list)
ind <- match(data$Constituency, constituency_names.df$Constituency)
data$Region <- constituency_names.df$Nation[ind]

### Recode Party
### parties by N candidates
main.parties <- data %>% group_by(Party) %>% tally() %>% arrange(desc(n)) %>%
    filter(n>14) %>% data.frame(.)

ind <- data$Party %nin% main.parties$Party

data$Party.all <- data$Party
data$Party <- as.character(data$Party)
data$Party[ind] <- "Other"
data$Party <- factor(data$Party, levels=unique(data$Party))

write.csv(data, "data/uk-election-results-2015.csv")

} else{

data <- read.csv("data/uk-election-results-2015.csv", row.names=1)

}

data <- data %>% group_by(Constituency) %>% mutate(Total.Votes.Cast=sum(Votes),
                                                   Rank=row_number(desc(Votes))) %>% data.frame()

## Code the Speaker of the House as Conservative
data %>% filter(Constituency == "Buckingham")
data$Party[data$Candidate == "John Bercow"] <- "Conservative"

all.parties <- data %>% group_by(Party.all) %>% tally() %>% arrange(desc(n)) %>% data.frame()

by.mps <- data %>% group_by(Constituency) %>% filter(Votes==max(Votes))  %>%
    ungroup() %>% arrange(desc(Vote.Share))  %>% data.frame(.)

by.party <- by.mps %>% group_by(Party) %>% summarize(Seats=n()) %>% arrange(desc(Seats))

## In Seat order
by.seats <- data %>% group_by(Constituency) %>% filter(Votes==max(Votes)) %>%
    group_by(Party) %>% tally() %>% arrange(desc(n)) %>%
    data.frame(.)

library(gdata)
data$Party <- as.factor(data$Party)
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

## Convenience function to check color labels
pal <- function(col, border = "light gray")
{
  n <- length(col)
  plot(0, 0, type="n", xlim = c(0, 1), ylim = c(0, 1), axes=FALSE, xlab = "", ylab = "")
  rect(0:(n-1)/n, 0, 1:n/n, 1, col = col, border = border)
}

uk.colors <- data.frame(Party=levels(by.mps$Party),
                        party.color=c(
                            "#1577C7", # Conservative
                            "#E8251F", # Labour
                            "#EAC135", # SNP
                            "#BC1D40", # DUP
                            "#FA8324", # Lim-Dems
                            "#126140", # Sinn Fein
                            "#559D37", # Plaid Cymru
                            "#6AA769", # SDLP
                            "#6EB2E4", # UUP
                            "#7EC031", # Greens
                            "#999999", # Independent
                            "#6E3485" # UKIP
                            ),
                        stringsAsFactors = FALSE)

pal(uk.colors$party.color)

not.gb <- c("Democratic Unionist Party", "Sinn Fein", "Social Democratic & Labour Party",
            "Ulster Unionist Party", "Independent")
gb.colors <- uk.colors %>% filter(Party %nin% not.gb)


exclude <- c("Democratic Unionist Party", "Sinn Fein", "Social Democratic & Labour Party",
            "Ulster Unionist Party")

runner.up.colors <- uk.colors %>% filter(Party %nin% exclude) ## for runner up map below



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



###--------------------------------------------------
### Maps
###--------------------------------------------------

uk.map <- readOGR("maps/topo_wpc.json", "wpc")

## The name field didn't get imported properly for some reason.
ind <- match(uk.map@data$id, constituency_names.df$id)
uk.map@data$name <- constituency_names.df$Constituency[ind]

constituencies.map <- data.frame(id=0:(length(uk.map@data$name)-1),
                       Constituency=as.character(uk.map@data$name))

uk.map.df <- fortify(uk.map)

uk.map.df <- merge(uk.map.df, constituencies.map, by="id")

## Now we have a map of all the constituencys and winners
uk.map.df <- merge(uk.map.df, by.mps, by="Constituency")

library(gdata)
uk.map.df$Party <- factor(uk.map.df$Party, levels=gb.colors$Party, ordered=TRUE)
uk.map.df$Party <- droplevels(uk.map.df$Party)
uk.map.df$Party <- reorder.factor(uk.map.df$Party,
                                  new.order=as.character(by.seats$Party), order=TRUE)
detach(package:gdata)



### Make the maps
p <- ggplot(data=uk.map.df,
            aes(x=long, y=lat,
                group=group))

p1 <- p + geom_map(data = uk.map.df,
                   map = uk.map.df,
                   aes(map_id=id, x=long, y=lat, group=group, fill=Party),
                   color="white", size=0.2)

p2 <- p1 + geom_map(data=subset(uk.map.df, Constituency=="York Central"),
                    map=subset(uk.map.df, Constituency=="York Central"),
                    aes(map_id=id, x=long, y=lat, group=group, fill=Party),
                    color="white", size=0.2)

p3 <- p2 + scale_fill_manual(values=gb.colors$party.color)

pdf(file="figures/uk-2015-winners.pdf", height=15, width=10)

p4 <- p3 + coord_map(projection="albers", at0 = 51, lat1 = 0) + labs(x=NULL, y=NULL, fill="") +
    theme(panel.grid=element_blank(),
          axis.ticks=element_blank(),
          panel.border=element_blank(),
          axis.text=element_blank(),
          legend.position=c(0.8, 0.55))

print(p4)
credit()
dev.off()

ggsave("figures/uk-2015-winners.png",
       p4,
       height=15,
       width=10,
       dpi=300)


### Let's see who came in second.
by.runner.up <- data %>% group_by(Constituency) %>% filter(Rank==2)  %>%
    ungroup() %>% arrange(desc(Vote.Share))  %>% data.frame(.)

library(gdata)
by.runner.up$Party <- reorder.factor(by.runner.up$Party, new.order=by.seats$Party)
detach(package:gdata)

runner.up.df <- fortify(uk.map)

runner.up.df <- merge(runner.up.df, constituencies.map, by="id")

## Now we have a map of all the constituencys and winners
runner.up.df <- merge(runner.up.df, by.runner.up, by="Constituency")


p <- ggplot(data=runner.up.df,
            aes(x=long, y=lat,
                group=group))

p1 <- p + geom_map(data = runner.up.df,
                   map = runner.up.df,
                   aes(map_id=id, x=long, y=lat, group=group, fill=Party),
                   color="white", size=0.2)

p2 <- p1 + geom_map(data=subset(runner.up.df, Constituency=="York Central"),
                    map=subset(runner.up.df, Constituency=="York Central"),
                    aes(map_id=id, x=long, y=lat, group=group, fill=Party),
                    color="white", size=0.2)

p3 <- p2 + scale_fill_manual(values=runner.up.colors$party.color)

pdf(file="figures/uk-2015-runners-up.pdf", height=15, width=10)

p4 <- p3 + coord_map(projection="albers", at0 = 51, lat1 = 0) + labs(x=NULL, y=NULL, fill="") +
    theme(panel.grid=element_blank(),
          axis.ticks=element_blank(),
          panel.border=element_blank(),
          axis.text=element_blank(),
          legend.position=c(0.8, 0.55)) +
        ggtitle("Who Came Second? Election Constituencies by Runner-Up Candidate")

print(p4)
credit()
dev.off()

ggsave("figures/uk-2015-runners-up.png",
       p4,
       height=15,
       width=10,
       dpi=300)
