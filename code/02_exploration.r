# 02_exploration.r
# :purpose: Run initial check, cleaning, and visualisations
# :author: jon.sedar@applied.ai
# :date: Thu 03 Oct 2013

# how many transactions, dates, customers, products and product subclasses?
nrow(dt)                                                        # 817,741
nrow(dt[,1,by=trans_date])                                      # 120
nrow(dt[,1,by=cust_id])                                         # 32,266
nrow(dt[,1,by=prod_id])                                         # 23,812
nrow(dt[,1,by=prod_cat])                                        # 2,012

# does each customer have a single age_band and res_area?
nrow(dt[,1,by=list(cust_id,age_band)])                         # 32,266
nrow(dt[,1,by=list(cust_id,res_area)])                         # 32,266

# does each prod_id fit underneath exactly one prod_cat?
transcatid <- dt[,list(nbask=length(trans_id)),by=list(prod_cat,prod_id)]
transid <- transcatid[,list(ncat=length(prod_cat),nbask=sum(nbask)),by=prod_id]
transid[,length(prod_id),by=ncat]

# no! there's  some prodids which have 2 or 3 prod_cats.

# solution: dedupe. keep prodid-prodcat combos with the largest nbask
ids <- transid[ncat>1,prod_id]
transcatid[prod_id %in% ids,rank:=rank(-nbask),by=prod_id]
#transcatid[prod_id == "0021000767472"]
goodprodcat <- transcatid[is.na(rank) | rank ==1,list(prod_cat,prod_id)]
setkeyv(goodprodcat,c("prod_cat","prod_id"))

# only keep rows in dt where prodcat prodid combo is in the goodlist
dtr <- copy(dt)
setkeyv(dtr,c("prod_cat","prod_id"))
dt <- merge(dtr,goodprodcat)

# reset trans_id to be seqential
dt[,trans_id:=seq(1,nrow(dt),1)]
setkey(dt,trans_id)


# what did we lose?
nrow(dtr) - nrow(dt)
(nrow(dtr) - nrow(dt)) / nrow(dtr)
# lost 4811 rows or 0.6 %, wont lose sleep

# how many transactions, dates, customers, products and product subclasses?
nrow(dt)                                                        # 812,930
nrow(dt[,1,by=trans_date])                                      # 120
nrow(dt[,1,by=cust_id])                                         # 32,261
nrow(dt[,1,by=prod_id])                                         # 23,798
nrow(dt[,1,by=prod_cat])                                        # 2,010




### basic viz ------------------------------------------------------------------

### a few quick plots
# transactions by date
p1 <- ggplot(dt[,list(num_trans=length(trans_id)),by=trans_date]) +
    geom_bar(aes(x=trans_date,y=num_trans),stat='identity',alpha=0.8)
plot(p1)

# smoothed plot
p1b <- ggplot(dt[,list(num_trans=length(trans_id)),by=trans_date]) +
    geom_point(aes(x=trans_date,y=num_trans),stat='identity',alpha=0.8) + 
    geom_smooth(aes(x=trans_date,y=num_trans),method='loess',alpha=0.8)
plot(p1b)


# histogram items per customer
p2 <- ggplot(dt[,list(numitem=length(trans_id)),by=cust_id]) +
    geom_bar(aes(x=numitem),stat='bin',binwidth=10,alpha=0.8,fill=colorBlind[2]) +
    coord_cartesian(xlim=c(0,200))
plot(p2)

# summarize baskets by cust age and location
dtt <- dt[,list(baskets=1,items=sum(quantity)),by=list(cust_id,age_band,res_area,trans_date)]
dttt <- dtt[,list(numbask=sum(baskets),numitem=sum(items)),by=list(cust_id,age_band,res_area)]

# histogram baskets per customer
p3 <- ggplot(dttt) +
    geom_bar(aes(x=numbask),stat='bin',binwidth=2,alpha=0.8,color = colorBlind[3]) +
    coord_cartesian(xlim=c(0,100))
plot(p3)


# scatterplot items per basket
p4a <- ggplot(dttt) +
    geom_point(aes(x=numbask,y=numitem),size=1,alpha=0.8) +
    geom_smooth(aes(x=numbask,y=numitem),method="lm") +
    coord_cartesian(xlim=c(0,50),ylim=c(0,400))
plot(p4a)


# scatterplot items per basket by age_band
p4 <- ggplot(dttt) +
    geom_point(aes(x=numbask,y=numitem,color=age_band),size=1,alpha=0.8) +
    geom_smooth(aes(x=numbask,y=numitem),method="lm",color=colorBlind[1]) +
    facet_wrap(~age_band) +
    coord_cartesian(xlim=c(0,50),ylim=c(0,400))
plot(p4)

# scatterplot items per basket by res_area
p5 <- ggplot(dttt) +
    geom_point(aes(x=numbask,y=numitem,color=res_area),size=1,alpha=0.8) +
    geom_smooth(aes(x=numbask,y=numitem),method="lm",color=colorBlind[1]) +
    facet_wrap(~res_area) +
    coord_cartesian(xlim=c(0,50),ylim=c(0,400))
plot(p5)
