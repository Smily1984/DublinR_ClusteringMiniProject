# 01_sourcing.r
# :purpose: Load the ta-feng dataset, clean, parse, process into user features
# :author: jon.sedar@applied.ai
# :date: Thu 03 Oct 2013
# :notes: Raw data via http://recsyswiki.com/wiki/Grocery_shopping_datasets

get_data <- function(){
    ### get data from raw text files
        
    # D11: Transaction data collected in November, 2000
    # D12: Transaction data collected in December, 2000
    # D01: Transaction data collected in January, 2001
    # D02: Transaction data collected in February, 2001
    
    # Format of Transaction Data
    # --------------------------
    # First line: Column definition in Traditional Chinese
    #     §È¥¡;∑|≠˚•d∏π;¶~ƒ÷;∞œ∞Ï;∞”´~§¿√˛;∞”´~ΩsΩX;º∆∂q;¶®•ª;æP∞‚
    # Second line and the rest: data columns separated by ";"
    
    # Column definition
    # -----------------
    # 1: Transaction date and time (time invalid and useless)
    # 2: Customer ID
    # 3: Age: 10 possible values,
    #    A <25,B 25-29,C 30-34,D 35-39,E 40-44,F 45-49,G 50-54,H 55-59,I 60-64,J >65
    #    actually upon inspection there's 22362 rows with value K, will assume it's J+
    # 4: Residence Area: 8 possible values, 
    #    A-F: zipcode area: 105,106,110,114,115,221,G: others, H: Unknown
    #    Distance to store, from the closest: 115,221,114,105,106,110
    #    "E","F","D","A","B","C","G","H"
    # 5: Product subclass
    # 6: Product ID
    # 7: Amount
    # 8: Asset
    # 9: Sales price
    
    # pre-clean in terminal: 
    # awk -F";" 'gsub("\:","",$1)' D02 | awk 'gsub("000000","",$2)' | 
    #    awk '{print $1","$2","$3","$4","$5","$6","$7","$8","$9}' > D02.csv
    
    
    colsdt <- data.table(names=c("trans_date","cust_id","age_band"
                               ,"res_area","prod_cat","prod_id"
                               ,"quantity","asset","price")
                       ,types=c("character","character","character"
                                ,"character","character","character"
                                ,"numeric","character","numeric"))
    
    dtnov <- data.table(read.table(str_c(dirs['data'],"/TaFengDataSet/D11.csv")
                                    ,sep=",",header=F,encoding="UTF-8"
                                    ,col.names=colsdt$names,colClasses=colsdt$types
                                    ,stringsAsFactors=F))
    
    dtdec <- data.table(read.table(str_c(dirs['data'],"/TaFengDataSet/D12.csv")
                                    ,sep=",",header=F,encoding="UTF-8"
                                    ,col.names=colsdt$names,colClasses=colsdt$types
                                    ,stringsAsFactors=F))
    
    dtjan <- data.table(read.table(str_c(dirs['data'],"/TaFengDataSet/D01.csv")
                                    ,sep=",",header=F,encoding="UTF-8"
                                    ,col.names=colsdt$names,colClasses=colsdt$types
                                    ,stringsAsFactors=F))
    
    dtfeb <- data.table(read.table(str_c(dirs['data'],"/TaFengDataSet/D02.csv")
                                    ,sep=",",header=F,encoding="UTF-8"
                                    ,col.names=colsdt$names,colClasses=colsdt$types
                                    ,stringsAsFactors=F))

    
    ### combine tables, and clean / parse datatypes
    dtraw <- rbindlist(list(dtnov,dtdec,dtjan,dtfeb))
    rm(dtnov,dtdec,dtjan,dtfeb)
    
    dtraw[,trans_id:=seq(1,nrow(dtraw),1)]
    setkey(dtraw,trans_id)
    dtraw[,trans_date:=ymd(trans_date)]
    dtraw[,age_band:=factor(age_band,levels=c("A","B","C","D","E","F","G","H","I","J","K"))]
    dtraw[,res_area:=factor(res_area,levels=c("E","F","D","A","B","C","G","H"))]
    dtraw[,prod_cat:=factor(prod_cat)]
    dtraw[,prod_id:=factor(prod_id)]
    setcolorder(dtraw,c(which(colnames(dtraw) %in% "trans_id"),which(!colnames(dtraw) %in% "trans_id")))
        
    return(dtraw)
}


