# 03_featurecreation.r
# :purpose: Create features for modelling from the raw transactional data
# :author: jon.sedar@applied.ai
# :date: Thu 03 Oct 2013


# I want, per customer:
    # 1. convert categoricals back to numerics:
        # age_band can be coerced back into a numeric younger-to-older scale
# coerce back to real age
        # res_area can be coerced back into a numeric closer-to-farther scale
    # 2. counts:
        # total baskets (==unique days)
        # total items
        # total spend
        # unique prod_subclass
        # unique prod_id
    # 3. distributions (triangle stats will do)
        # items per basket
        # spend per basket
        # product_ids per basket
        # product_subclass per basket
        # duration between visits
    # 4. product preferences
        # prop. of baskets in the N bands of product cats and ids by item popularity
        # prop. of baskets in the N bands of product ids by item price



### 1. factors to numerics -----------------------------------------------------

    # age_band: 10 possible values, A <25,B 25-29,C 30-34,D 35-39,E 40-44,F 45-49,G 50-54,H 55-59,I 60-64,J >65
    # so just convert straight back to numeric, low num younger, high num older
    
    # res_area: 8 factors that we already processed into distance order,
    # so simply as.numeric them too!

dtnums <- dt[,list(cust_id,age_band=as.numeric(age_band),res_area=as.numeric(res_area))]
dtnums <- dtnums[,list(age_band=mean(age_band),res_area=mean(res_area)),by=cust_id]
setkey(dtnums,cust_id)




### 2. Counts ------------------------------------------------------------------

# iterate the grouping and take length of last-grouped dimension
counts <- dt[,list(nbask=length(trans_id),nitem=sum(quantity),spend=sum(quantity*price))
                        ,by=list(cust_id,prod_cat,prod_id)]
counts <- counts[,list(nbask=sum(nbask),nitem=sum(nitem),spend=sum(spend),nprodid=length(prod_id))
                       ,by=list(cust_id,prod_cat)]
counts <- counts[,list(nbask=as.numeric(sum(nbask)),nitem=sum(nitem),spend=sum(spend)
                       ,nprodid=as.numeric(sum(nprodid)),nprodcat=as.numeric(length(prod_cat)))
                       ,by=cust_id]
setkey(counts,cust_id)



### 3. Distributions --------------------------------------------------------------

# items and spend per basket
dists_ispb <- dt[,list(nitem=sum(quantity),spend=sum(quantity*price)),by=list(cust_id,trans_date)]
dists_ispb <- dists_ispb[,list(ipb_max=max(nitem),ipb_med=median(nitem),ipb_min=min(nitem)
                               ,spb_max=max(spend),spb_med=median(spend),spb_min=min(spend))
                         ,by=cust_id]
setkey(dists_ispb,cust_id)


# product ids and subclasses per basket
dists_pppb <- dt[,list(nprodid=length(prod_id)),by=list(cust_id,trans_date,prod_cat)]
dists_pppb <- dists_pppb[,list(nprodid=sum(nprodid),nprodct=length(prod_cat))
                         ,by=list(cust_id,trans_date)]
dists_pppb[,c("nprodid","nprodct"):=list(as.numeric(nprodid),as.numeric(nprodct))]
dists_pppb <- dists_pppb[,list(prodctpb_max=max(nprodct),prodctpb_med=median(nprodct)
                               ,prodctpb_min=min(nprodct))
                         ,by=cust_id]
setkey(dists_pppb,cust_id)


# average duration between visits
custday <- dt[,list(nbaskpd=as.numeric(length(trans_id))),by=list(cust_id,trans_date)]
setkeyv(custday,c("cust_id","trans_date"))
difme <- function(x){
    d <- diff(c(NA,x))
    dd <- d/86400
    return(dd)   
}

custday[,dif:=difme(trans_date),by=cust_id]
avedurs <- custday[,list(dur_max=max(dif,na.rm=T),dur_med=median(dif,na.rm=T)
                        ,dur_min=min(dif,na.rm=T)
                        #,baskpd_max=max(nbaskpd),baskpd_med=median(nbaskpd)#,baskpd_min=min(nbaskpd)
                         ),by=cust_id]
avedurs[is.na(dur_med),c("dur_max","dur_min"):=list(NA,NA)]


### causes issue: if customers visited once only, they have NA for duration between visits!

### solutions:
    # Option A: remove them from modelling?
    # wasteful in this case (lose 30%!)
    # BUT maybe we dont care about classifying one-time shoppers
    
    avedurs <- copy(avedurs[!is.na(dur_med)])


    # Option B: or give then all the same value
    # but which value? all == 0 isn't quite true, and a large number will skew clustering
    
    # avedurs[is.na(dur_med),c("dur_max","dur_med","dur_min"):=list(0,0,0)]
    

    # Option C: impute values based on the global mean and SD of each col
    # usually a reasonable fix, except for ratio columns, where clumsy, 
    # and may be equally suspect since we have to mess with values to make realistic

#     ave_imp <- data.table(imputeData(avedurs[,list(dur_max,dur_med,dur_min)]))
#     avedurs <- data.table(avedurs[,list(cust_id)],ave_imp)
#     setkey(avedurs,cust_id)


### 4. Product Preferences -----------------------------------------------------

## First, lets simply create popularity-based classifications at prod_cat level
popcatid <- dt[,list(nbask=length(trans_id)),by=list(prod_cat,prod_id)]
popcat <- popcatid[,list(nbask=sum(nbask),nprodid=length(prod_id)),by=prod_cat]

# create classifications
popcat[,popcat_nbask:=LETTERS[as.numeric(cut2(nbask,g=5))]]
popcat[,popcat_size:=LETTERS[as.numeric(cut2(nprodid,g=5))]]

# check: see the most popular cats are those with the widest num of product ids, makes sense
dcast(popcat,popcat_nbask~popcat_size,value.var="prod_cat")


## Second, lets create popularity-based classifications at prod_id level
popid <- popcatid[,list(nbask=sum(nbask)),by=prod_id]

# create classifications
popid[,popid_nbask:=LETTERS[as.numeric(cut2(nbask,g=5))]]


## Third, create price-based classifications at the prod_id level
priceid <- dt[,list(aveprice=median(price)),by=prod_id]
priceid[,prodid_pricerank:=LETTERS[as.numeric(cut2(aveprice,g=5))]]
# A low, E high


## Finally, Merge these popularity calculations back onto the dt transactions
dtp <- copy(dt)

# attach prod_cat
setkey(dtp,prod_cat)
dtpop <- merge(dtp,popcat[,list(prod_cat,popcat_nbask,popcat_size)])
dtpop[,length(trans_id),by=popcat_size]

# attach prod_cid
setkey(dtp,prod_id)
setkey(dtpop,prod_id)
dtpop <- merge(dtpop,popid[,list(prod_id,popid_nbask)])
dtpop[,length(trans_id),by=popid_nbask]

# attach prod price
dtpop <- merge(dtpop,priceid[,list(prod_id,prodid_pricerank)])
dtpop[,length(trans_id),by=prodid_pricerank]


## group by various cat and id pops and preserve row totals margin
dtpop_popcat_nbask <- data.table(dcast(dtpop,cust_id~popcat_nbask,value.var="trans_id"))
dtpop_popcat_size <- data.table(dcast(dtpop,cust_id~popcat_size,value.var="trans_id"))
dtpop_popid_nbask <- data.table(dcast(dtpop,cust_id~popid_nbask,value.var="trans_id"))
dtpop_prodprice <- data.table(dcast(dtpop,cust_id~prodid_pricerank,value.var="trans_id"))



## divide each feature by the row total to make proportional
dtpop_popcat_nbask <- data.table(
        dtpop_popcat_nbask[,1,with=F]
        ,dtpop_popcat_nbask[,2:6,with=F]/rowSums(dtpop_popcat_nbask[,2:6,with=F])
)
setkey(dtpop_popcat_nbask,cust_id)

dtpop_popcat_size <- data.table(
        dtpop_popcat_size[,1,with=F]
        ,dtpop_popcat_size[,2:6,with=F]/rowSums(dtpop_popcat_size[,2:6,with=F])
)
setkey(dtpop_popcat_size,cust_id)

dtpop_popid_nbask <- data.table(
        dtpop_popid_nbask[,1,with=F]
        ,dtpop_popid_nbask[,2:6,with=F]/rowSums(dtpop_popid_nbask[,2:6,with=F])
)
setkey(dtpop_popid_nbask,cust_id)

dtpop_prodprice <- data.table(
        dtpop_prodprice[,1,with=F]
        ,dtpop_prodprice[,2:6,with=F]/rowSums(dtpop_prodprice[,2:6,with=F])
)
setkey(dtpop_prodprice,cust_id)


dtpop_prodprice[1:100]

setnames(dtpop_popcat_nbask,c("cust_id","popcat_nbaskA","popcat_nbaskB","popcat_nbaskC"
                              ,"popcat_nbaskD","popcat_nbaskE"))
setnames(dtpop_popcat_size,c("cust_id","popcat_sizeA","popcat_sizeB","popcat_sizeC"
                              ,"popcat_sizeD","popcat_sizeE"))
setnames(dtpop_popid_nbask,c("cust_id","popid_nbaskA","popid_nbaskB","popid_nbaskC"
                              ,"popid_nbaskD","popid_nbaskE"))
setnames(dtpop_prodprice,c("cust_id","prodpriceA","prodpriceB","prodpriceC"
                              ,"prodpriceD","prodpriceE"))


dtpop_popcat_nbask[,c("popcat_nbaskA","popcat_nbaskB","popcat_nbaskC"
                              ,"popcat_nbaskD","popcat_nbaskE"):=lapply(.SD,as.numeric),.SDcols=2:6]
dtpop_popcat_size[,c("popcat_sizeA","popcat_sizeB","popcat_sizeC"
                              ,"popcat_sizeD","popcat_sizeE"):=lapply(.SD,as.numeric),.SDcols=2:6]
dtpop_popid_nbask[,c("popid_nbaskA","popid_nbaskB","popid_nbaskC"
                              ,"popid_nbaskD","popid_nbaskE"):=lapply(.SD,as.numeric),.SDcols=2:6]
dtpop_prodprice[,c("prodpriceA","prodpriceB","prodpriceC"
                              ,"prodpriceD","prodpriceE"):=lapply(.SD,as.numeric),.SDcols=2:6]



### And finally, merge all ! -------------------------------------------

cst <- merge(dtnums,counts)
cst <- merge(cst,dists_ispb)
cst <- merge(cst,dists_pppb)
cst <- merge(cst,avedurs)
cst <- merge(cst,dtpop_popcat_nbask,all.x=T)
cst <- merge(cst,dtpop_popcat_size,all.x=T)
cst <- merge(cst,dtpop_popid_nbask,all.x=T)
cst <- merge(cst,dtpop_prodprice,all.x=T)

data_saver(cst,dirs['data'])  


### Lets take a look at our features:

str(cst)


