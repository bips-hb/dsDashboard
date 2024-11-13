###############################################################################
#Program: descriptivetools.R
#
#Project: STB
#
#R-Version: R version 4.2.1 (2022-06-23 ucrt)
#
#Purpose: Helper functions for using DataShield
#
#Prequeries: -
#
#Output: A shiny dashboard
#
#Date/Changes/Author: 08/11/2024 / First production of program / Andreas Mändle
#
#Code review: Timm Intemann, 08/11/2024
###############################################################################


#' Summary for a table in DataSHIELD
#'
#' Output: number.of.rows, number.of.columns, variables.held for a table object
#' per server
#'
#' The output is more convenient than using the ds.summary function directly,
#' as it turns the information in a data.frame.
#'
#' @param x Table name as string.
#' @param datasources A list of 'OpalConnection'.
#' @return A data.frame
#' @examples
#' if (require(DSLite)) {
#' data('CNSIM1')
#' data('CNSIM2')
#'
#' # build a DSLite server with the datasets inside
#' dslite.server1 <- DSLite::newDSLiteServer(tables=list(table1=CNSIM1))
#' dslite.server2 <- DSLite::newDSLiteServer(tables=list(table2=CNSIM2))
#'
#' # build DS login information
#' builder <- DSI::newDSLoginBuilder()
#' builder$append(server = 'server1', driver = 'DSLiteDriver', url = 'dslite.server1')
#' builder$append(server = 'server2', driver = 'DSLiteDriver', url = 'dslite.server2')
#' logindata <- builder$build()
#'
#' # do login and table assignment
#' conns <- datashield.login(logindata)
#' datashield.assign.table(conns, 'tab1', table = list(server1='table1', server2='table2'))
#'
#' dsTableSummary("tab1")
#' }
#'
#' # output:
#' #    server number.of.rows number.of.columns variables.held
#' # 1 server1           2163                11   LAB_TSC,....
#' # 2 server2           3088                11   LAB_TSC,....
dsTableSummary <- function(x=NULL, datasources=NULL) {
  s_tab <- dsBaseClient::ds.summary(x = x, datasources = datasources)
  df <- as.data.frame(do.call(rbind, s_tab))
  df <- data.frame(cbind(server=row.names(df),df))
  rownames(df) <- NULL
  df <- df[colnames(df)!="class"]
  # unlist coloumns that can be simplified (to e.g. numeric, factor,...)
  df <- data.frame(lapply(df, function(x) {if (length(x)==length(unlist(x))) unlist(x) else I(x)}))
  rownames(df)<-NULL
  #colnames(df) <- c("class", "length", "category", "count", "server")
  return(df)
}

#' Get unique variable names of a table object across all servers
#'
#' @param x Table name as string.
#' @param datasources A list of 'OpalConnection'. Default is to use all findable connections.
#' @param fastmode Deprecated. Should be TRUE.
#' @param combined If FALSE, the available variables are returned per server, otherwise accross all servers.
#'
#' @return A vector.
#' @examples
#' dsUniqueVarnames("tab1")
#' # output:
#'  [1] "LAB_TSC"            "LAB_TRIG"           "LAB_HDL"            "LAB_GLUC_ADJUSTED"  "PM_BMI_CONTINUOUS"  "DIS_CVA"            "MEDI_LPD"
#'  [8] "DIS_DIAB"           "DIS_AMI"            "GENDER"             "PM_BMI_CATEGORICAL"
dsUniqueVarnames <- function(x=NULL, datasources=DSI::datashield.connections_find(), fastmode = T, combined=T) {
  if (fastmode) {
    # prepare empty dataframe with columns server and variable
    rs <- data.frame(server=character(),
                     variable=character())

    # Try to call colnamesDS on all specified datasources and return NULL in case of errors.
    tryCatch(
      DSI::datashield.aggregate(
        conns=datasources,
        expr=call("colnamesDS", x),
        success=function(server, rs) rs <<- rbind(data.frame(server=server,
                                                             variable=rs)),
        error=function(server, e) print(paste0(server,": ", e)),
      ),
      error=function(e) NULL
    )

    # returns results per datasource (if !combined) or over all datasources (if combined)
    if (combined) return(unique(rs$variable)) else return(rs)
  }
  ## legacy version  below is removed => call error if fastmode=FALSE
  stop("fastmode=FALSE is not supported any more")
}

#' Summary for categorical variable across all servers
#'
#' @param x Name of referring to a data.frame column as string.
#' @param datasources A list of 'OpalConnection'.
#' @return A data.frame.
#' @examples
#' dsCatVarSummary("tab1$GENDER")
#' #    server length level value
#' # 1 server1   2163     0  1092
#' # 2 server2   3088     0  1585
#' # 3 server1   2163     1  1071
#' # 4 server2   3088     1  1503
#'
#' dsCatVarSummary("tab1$DIS_AMI")
#' #        id length   level value
#' # 1 invalid     NA invalid    NA
dsCatVarSummary <- function(x=NULL, datasources=NULL) {
  # get summaries using DataSHIELD for all servers in datasources
  s_tab <- dsBaseClient::ds.summary(x = x, datasources = datasources)
  # concatenate available categories for the variable as comma separated string
  s_tab <- lapply(s_tab, function(x) {
    if(class(s_tab[[1]])!="character") x$categories<-paste(x$categories, collapse=",") # otherwise failure with invalid objects
    x
  })
  # add server names to each list element in s_tab
  s_tab <- lapply(1:length(s_tab), function(x) s_tab[[x]] <- data.frame(server=names(s_tab)[x], s_tab[[x]]))
  # bind rows to convert list to a flat data.frame
  df <- do.call(dplyr::bind_rows, s_tab)
  # unlist columns that can be simplified (to e.g. numeric, factor,...)
  df <- data.frame(lapply(df, function(x) {if (length(x)==length(unlist(x))) unlist(x) else I(x)}), check.names = F)
  # check if object is too short (an invalid object! has usually at least 5 columns!)
  if(dim(df)[2]< 5) {
    return(data.frame(id="invalid",length=NA, level="invalid", value=NA))
  }
  # drop unnecessary columns
  df <- dplyr::select(df, -class, -categories)
  # reshape: wide to long
  df <- stats::reshape(df, varying=3:length(df),direction="long", v.names = "value", times = names(df)[3:length(df)], timevar = "level", idvar="server")
  # in column "level" replace "count.of..'XXX'." by XXX
  df$level <- gsub("count.of..(.*).", '\\1', df$level)
  # remove rownames
  rownames(df)<-NULL
  # replace level count NA by 0
  df$value[is.na(df$value)]<-0
  # return data.frame
  return(df)
}


#' Summary for one (and only one!) categorical variable of a table across all servers
#'
#' @param x A character string specifying the name of a data.frame.
#' @param datasources A list of \code{\link{DSI::DSConnection-class}} objects.
#' @return A data.frame.
#' @examples
#' dsCatAgg("tab1$GENDER")
#' #   GENDER number of observations
#' # 1      0                   2677
#' # 2      1                   2574
#'
#' dsCatAgg("tab1$DIS_AMI")
#' #   DIS_AMI number of observations
#' # 1 invalid                     NA
dsCatAgg <- function(x=NULL, datasources=NULL) {
  # get summary for column x with dsCatVarSummary
  df <- dsCatVarSummary(x = x, datasources = datasources)
  # aggregate over all servers: sum over level counts
  df <- stats::aggregate(df[,"value",drop=FALSE], by = list(df$level), FUN = "sum")
  # extract variable name from string x (part after $ in TABLE$VARNAME)
  varname <- unlist(regmatches(x, regexpr("\\$", x), invert = TRUE))[2]
  # set name for columns of data.frame
  colnames(df) <- c(varname, "number of observations")
  # return result
  return(df)
}

#' Check if columns of a DataSHIELD table are numeric
#'
#' @param x A character string referring to a data.frame.
#' @param x.vars Optional character string vector; a subset of the variables in table x, on which the function shall be applied
#' @param datasources A list of \code{\link{DSI::DSConnection-class}} objects. Default is to use all findable connections.
#' @return A vector of boolean.
#' @examples
#' dsIsNumeric("tab1")
#' # LAB_TSC           LAB_TRIG            LAB_HDL  LAB_GLUC_ADJUSTED  PM_BMI_CONTINUOUS
#' #    TRUE               TRUE               TRUE               TRUE               TRUE
#' # DIS_CVA           MEDI_LPD           DIS_DIAB            DIS_AMI             GENDER
#' #   FALSE              FALSE              FALSE              FALSE              FALSE
#' # PM_BMI_CATEGORICAL
#' #              FALSE
dsIsNumeric <- function(x=NULL, x.vars=NULL, datasources=DSI::datashield.connections_find()) {
  # use variables x.vars if available, else (if NULL) check all variables from table x
  if (!is.null(x.vars))
    tbPars <- x.vars
  else
    tbPars <- dsUniqueVarnames(x, datasources=datasources)

  # DataSHIELD logging: off
  options(datashield.progress=F)

  # integer and numeric columns are identified as numeric (other types are assumed as categorical variables)
  numCols <- sapply(tbPars,function(tbCol){
    tryCatch({
      colTypes <<- DSI::datashield.aggregate(datasources, call("classDS",paste0(x,'$',tbCol)))
      return("integer" %in% colTypes | "numeric" %in% colTypes)
    },
    error= function(e) {
      return(NA)} )
  })

  # DataSHIELD logging: on
  options(datashield.progress=T)

  # return result
  return(numCols)
}

#' Summary for all numeric variables in a DataSHIELD table
#'
#' Quantiles (5%, 10%, 25%, 50%, 75%, 90%, 95%) and mean are computed for all
#' numeric variables.
#'
#' @param x A character string referring to a DataSHIELD table.
#' @param datasources A list of \code{\link{DSI::DSConnection-class}} objects. Default is to use all findable connections as datasource.
#' @return A data.frame.
#' @examples
#' dsNumVarSummary("tab1")
#' #       LAB_TSC    LAB_TRIG   LAB_HDL LAB_GLUC_ADJUSTED PM_BMI_CONTINUOUS
#' # 5%   4.118277 -0.47120403 0.8606589          4.095639          19.45742
#' # 10%  4.503466  0.07673459 1.0385205          4.539658          21.20121
#' # 25%  5.132161  1.01642189 1.2964949          5.222477          24.16518
#' # 50%  5.834384  2.07638965 1.5704848          6.046338          27.36816
#' # 75%  6.543257  3.08549769 1.8418712          6.857015          30.66310
#' # 90%  7.233349  4.04673538 2.0824057          7.642129          33.76176
#' # 95%  7.648063  4.58631916 2.2191369          8.126197          35.57591
#' # Mean 5.856428  2.06854312 1.5619572          6.110854          27.44250
dsNumVarSummary <- function(x=NULL, datasources=DSI::datashield.connections_find()) {
  # get numeric columns
  numCols <- dsIsNumeric(x, datasources=datasources)

  # ds.quantileMean is applied to all numeric columns; all results are returned in a data.frame
  sapply(names(numCols)[numCols==T], function(tbCol) {
    dsBaseClient::ds.quantileMean(paste0(x,'$',tbCol), type = "combine", datasources = datasources)
  })
}

#' Summary for all non-numeric variables in a DataSHIELD table
#'
#' @param x A character string referring to a DataSHIELD table.
#' @param datasources A list of \code{\link{DSI::DSConnection-class}} objects.
#' @return A list of data.frame.
#' @examples
#' dsCatSummary("tab1")
#' # $DIS_CVA
#' # level number of observations
#' # 1     0                   5248
#' # 2     1                      3
#' #
#' # $MEDI_LPD
#' # level number of observations
#' # 1     0                   5144
#' # 2     1                    107
#' #
#' # $DIS_DIAB
#' # level number of observations
#' # 1     0                   5174
#' # 2     1                     77
#' #
#' # $DIS_AMI
#' # level number of observations
#' # 1 invalid                     NA
#' #
#' # $GENDER
#' # level number of observations
#' # 1     0                   2677
#' # 2     1                   2574
#' #
#' # $PM_BMI_CATEGORICAL
#' # level number of observations
#' # 1     1                   1540
#' # 2     2                   1989
#' # 3     3                   1475
dsCatSummary <- function(x=NULL, datasources=DSI::datashield.connections_find()) {
  # get names of non-numeric columns
  numCols <- dsIsNumeric(x, datasources=datasources)
  notNumeric <- names(numCols)[!numCols]
  # for each non-numeric column get summary via dsCatAgg
  foo<-lapply(notNumeric, function(col) {
    dsCatAgg(paste0(x,"$",col), datasources=datasources)
  })
  # set variable names for the elements of the resulting list
  names(foo) <- sapply(foo, function(x) { colnames(x)[1] })
  # for each data.frame in the list rename the first column as "level"
  lapply(foo, function(x) { colnames(x)[1]<-"level"; return(x) })
}

#' A tidy summary for all non-numeric variables in a DataSHIELD table
#'
#' @param x A character string referring to a DataSHIELD table.
#' @param datasources A list of \link[DSI]{DSConnection-class} objects. Default
#' is to use all findable connections.
#' @return A data.frame.
#' @examples
#' dsCatTidy("tab1")
#' #              variable   level number of observations
#' # 1             DIS_CVA       0                   5248
#' # 2             DIS_CVA       1                      3
#' # 3            MEDI_LPD       0                   5144
#' # 4            MEDI_LPD       1                    107
#' # 5            DIS_DIAB       0                   5174
#' # 6            DIS_DIAB       1                     77
#' # 7             DIS_AMI invalid                     NA
#' # 8              GENDER       0                   2677
#' # 9              GENDER       1                   2574
#' # 10 PM_BMI_CATEGORICAL       1                   1540
#' # 11 PM_BMI_CATEGORICAL       2                   1989
#' # 12 PM_BMI_CATEGORICAL       3                   1475
dsCatTidy <- function(x=NULL, datasources=DSI::datashield.connections_find()) {
  # get summary of categorical columns in x
  df <- dsCatSummary(x = x, datasources = datasources)

  # write listname in new coloumn "variable" (for each data.frame in list df)
  df <- lapply(seq_along(df), function(i,x, names){
    x[[i]] <- cbind(variable=NA, x[[i]])
    x[[i]]$variable <- names[i]
    return(x[[i]])
  }, names=names(df), x=df)

  # rbind dfs in list and return
  do.call(rbind, df)
}

#' A summary for a numeric variable in a DataSHIELD table
#'
#' The summary contains the number of observations, quantiles (5%, 10%, 25%, 50%,
#' 75%, 90%, 95%) and the mean.
#'
#' A similar function for all columns of a table is `dsNumVarSummary` which returns results for all numeric columns of a table -- however it does not
#' return the number of observations.
#'
#' @param x A character string referring to a numeric column in a DataSHIELD table.
#' @param datasources A list of \link[DSI]{DSConnection-class} objects.
#' Default is to use all findable datasources.
#' @return A data.frame.
#' @examples
#' dsNumSummary("tab1$LAB_HDL")
#' #    LAB_HDL        value
#' #  1       N 5251.0000000
#' #  2      5%    0.8606589
#' #  3     10%    1.0385205
#' #  4     25%    1.2964949
#' #  5     50%    1.5704848
#' #  6     75%    1.8418712
#' #  7     90%    2.0824057
#' #  8     95%    2.2191369
#' #  9    Mean    1.5619572
dsNumSummary <- function(x, datasources=DSI::datashield.connections_find()) {
  # extract variable name from x (TABLE$VARIABLENAME)
  varname <- unlist(regmatches(x, regexpr("\\$", x), invert = TRUE))[2]
  # extract table name from x (TABLE$VARIABLENAME)
  tabname <- unlist(regmatches(x, regexpr("\\$", x), invert = TRUE))[1]

  # PART 1: similarly to dsNumVarSummary get quantile and mean combined over
  #         all datasources
  # if not numeric, return NA
  numCols <- dsIsNumeric(tabname, datasources=datasources)
  if (!varname %in% names(numCols[numCols==T])) return(NA)
  df_summ_1 <- dsBaseClient::ds.quantileMean(x, type = "combine", datasources = datasources)

  # PART 2: aggregate over all servers to get sum over level counts
  N_combined <- dsBaseClient::ds.length(x, type = 'combine', datasources = datasources)
  N_combined <- setNames(N_combined, "N")

  # combine PART 1 and 2
  df <- unlist(c(N_combined, df_summ_1))
  df <- data.frame(df)

  # format output nicely
  colnames(df) <- "value" # varname
  # colnames als spalte
  df <- cbind(variable=NA,df)
  df$variable <- rownames(df)
  colnames(df)[1] <- varname
  rownames(df) <- NULL
  return(df)
}

#' A summary for all (numeric and non-numeric) variables in a DataSHIELD table
#'
#' @param x A character string referring to a numeric column in a DataSHIELD table.
#' @param datasources A list of \link[DSI]{DSConnection-class} objects.
#' @return A data.frame.
#' @examples
#' rs <- dsSummary("tab1")
#' head(rs, 20)
#' #              variable feature   value
#' # 1             LAB_TSC       N 5251.00
#' # 2             LAB_TSC      5%    4.12
#' # 3             LAB_TSC     10%    4.50
#' # 4             LAB_TSC     25%    5.13
#' # 5             LAB_TSC     50%    5.83
#' # 6             LAB_TSC     75%    6.54
#' # 7             LAB_TSC     90%    7.23
#' # 8             LAB_TSC     95%    7.65
#' # 9             LAB_TSC    Mean    5.86
#' # 10           LAB_TRIG       N 5251.00
#' # 11           LAB_TRIG      5%   -0.47
#' # 12           LAB_TRIG     10%    0.08
#' # 13           LAB_TRIG     25%    1.02
#' # 14           LAB_TRIG     50%    2.08
#' # 15           LAB_TRIG     75%    3.09
#' # 16           LAB_TRIG     90%    4.05
#' # 17           LAB_TRIG     95%    4.59
#' # 18           LAB_TRIG    Mean    2.07
#' # 19            LAB_HDL       N 5251.00
#' # 20            LAB_HDL      5%    0.86
dsSummary <- function(x=NULL, datasources=DSI::datashield.connections_find()) {
  # check if numeric or not
  numCols <- dsIsNumeric(x, datasources=datasources)
  notNumeric <- names(numCols)[!numCols]
  notNumeric <- notNumeric[!is.na(notNumeric)]
  areNumeric <- names(numCols)[numCols]
  areNumeric <- areNumeric[!is.na(areNumeric)]
  areNA <- numCols[is.na(numCols)]

  # make list to collect summaries
  sList <- as.list(numCols)

  # na cases
  sList[areNA] <- lapply(areNA, function(col) {
    tdf <- data.frame(variable = col, feature=NA, value=NA)
    tdf[,2] <- formatC(NA,format="d")
    tdf
  })

  # non-numeric cases
  sList[notNumeric] <- lapply(notNumeric, function(col) {
    tdf <- dsCatAgg(paste0(x,"$",col), datasources = datasources)
    tdf[,2] <- formatC(tdf[,2],format="d")
    tdf
  })

  # numeric cases
  sList[areNumeric] <- lapply(areNumeric, function(col) {
    tdf <- dsNumSummary(paste0(x,"$",col), datasources = datasources)
    tdf[,2] <- formatC(tdf[,2],format="f", digits=2)
    tdf
  })

  # make names of column 1 and 2 equal for all data.frames in sList
  sList <- lapply(sList, function(x) { colnames(x)[1]<-"feature"; return(x) })
  sList <- lapply(sList, function(x) { colnames(x)[2]<-"value"; return(x) })

  # make it tidy
  df <- sList

  # write listname in a new coloumn "variable"
  df <- lapply(seq_along(df), function(i,x, names){
    x[[i]] <- cbind(variable=NA, x[[i]])
    x[[i]]$variable <- names[i]
    return(x[[i]])
  }, names=names(df), x=df)

  # rbind data.frames in list
  do.call(rbind, df)
}

#' Assign all tables from all servers to a symbol in DataSHIELD
#'
#' The function returns the table names in case of success.
#'
#' @param datasources A list of \link[DSI]{DSConnection-class} objects.
#' @return A data.frame containing the table names.
#' @examples
#' conns <- DSI::datashield.connections_find()
#' tabNamesTidy <- assignAllTables(conns)
#' tabNamesTidy
#' #        id     .x
#' # 1 server1 table1
#' # 2 server2 table2
assignAllTables <- function(datasources) {
  # get all table names
  tabNames <- DSI::datashield.tables(conns = datasources)

  # arrange names in a tidy way
  tabNamesTidy <- purrr::map_df(tabNames, ~as.data.frame(.x), .id="id")

  # for each table: assign it to a symbol
  lapply(1:(dim(tabNamesTidy)[1]), function(i,names){
    tryCatch( {
      DSI::datashield.assign.table(conns = datasources[[names[i,1]]],
                                   symbol = paste0("tab",i),
                                   table = names[i,2])
    }, error = function(e) {return(c(tabNamesTidy[i,],"error"))})

    return(c(tabNamesTidy[i,],"ok"))
  }, names=tabNamesTidy)

  # add symbol for use in DataSHIELD session to table
  tabNamesTidy$symbol <- paste0("tab",1:nrow(tabNamesTidy))

  return(tabNamesTidy)
}

#' Summarize the requested tables (DataSHIELD server list)
#'
#' Gets row and column counts for all given tables.
#'
#' @param datasources A list of \link[DSI]{DSConnection-class} objects.
#' @param tabList A data.frame with the table names and server locations.
#' @return A data.frame containing the table names, server location, symbols and dimensions.
#' @examples
#' conns <- DSI::datashield.connections_find()
#' tabSummary(tabNamesTidy, datasources=conns)
#' #     id  server   name rows cols
#' # 1 tab1 server1 table1 2163   11
#' # 2 tab2 server2 table2 3088   11
tabSummary <- function(tabList, datasources) {
  # loop through lines of tabList
  df <- lapply(1:(dim(tabList)[1]), function(i) {
    # get number of rows an columns in table i
    tabDims <- DSI::datashield.aggregate(datasources[unlist(tabList[i,1])], call("dimDS", tabList[i,"symbol"]))
    tabDims <- do.call(rbind,tabDims)
    tabDims <- c(sum(tabDims[,1]), unique(tabDims[,2]))

    # if it is a list, take the first list element
    if (is.list(tabDims) && length(tabDims)>=1) tabDims <- tabDims[[1]]

    # if dimensions are not as expected, return NA as column and row counts
    if (length(tabDims)!=2) tabDims <- c(NA,NA)

    # add number of rows and columns to server summary table
    df <- cbind(tabList[i,3],I(tabList[i,1]),tabList[i,2],t(unname(unlist(tabDims))))

    data.frame(df)
  })

  # unite all data.frames (rows)
  df <- as.data.frame(do.call(rbind, df))

  # set useful names for the columns
  colnames(df) <- c("id","server","name","rows","cols")
  return(df)
}

#' Check datasources
#'
#' Checks if datasources are a list of DSConnection-class.
#'
#' @param datasources A list of \link[DSI]{DSConnection-class} objects.
#' @return An integer. If datasources check passes, number of connections are returned, otherwise -1 is returned.
#' @examples
#' checkDatasources()
#' # 2
checkDatasources <- function(datasources=DSI::datashield.connections_find()) {
  # Ensure that datasources are a list of DSConnection-class
  if(!(is.list(datasources) && all(unlist(lapply(datasources, function(d) {methods::is(d,"DSConnection")}))))){
    warning("'datasources' must be a list of DSConnection-class objects", call.=FALSE)
    return(-1)
  } else {
    return(length(datasources))
  }
}

#' Get Range
#'
#' Uses the histogramDS1 server function to get a safe range (spanning over
#' minimum and maximum, were the exact range is increased with some privacy
#' preserving noise).
#'
#' If safemode is FALSE the function will fail if histogramDS1 fails for any
#' server.
#'
#' @param x A character string referring to a numeric column in a DataSHIELD table.
#' @param type Boolean. If "combined" then the results are aggregated over all servers,
#' otherwise a list of all server results is returned.
#' @param k Numeric. Only used if `method==2`. The number of the nearest neighbors for which their centroid is calculated. Default 3.
#' @param noise Numeric. Only used if `method==3`. The percentage of the initial variance that is used as the variance of the embedded noise if the argument method is set to 'probabilistic'.
#' @param safemode Boolean, if TRUE, histogramDS1 is called for each datasource
#' separately, to ensure that a result can be returned even if one datasource creates an error.
#' @param datasources A list of \link[DSI]{DSConnection-class} objects. Default
#' is to use all findable datasources.
#' @param stdnames Character vector. If there are more than one datasources and
#' type is not "combined", then this defines how the rows of the resulting table
#' are labelled. Default is to use the names of the datasources.
#' @param method A numeric. Either 1, 2 or 3 indicating the method of disclosure
#' control that is used for the generation of the histogram. If the value is equal to 1 then the
#' 'smallCellsRule' is used. If the value is equal to 2 then the 'deterministic' method is used.
#' If the value is set to 3 then the 'probabilistic' method is used.
#' @return A numeric vector (if '"combined" or only 1 study) or a matrix (else).
#' @examples
#' getRange("tab1$LAB_TRIG")
#' getRange("tab1$LAB_TRIG", safemode=T)
#' getRange("tab1$LAB_TRIG", type="separate", safemode=T)
getRange <- function(x, type="combined", k=3, noise=0.25, safemode=F, datasources=DSI::datashield.connections_find(), stdnames = names(datasources), method = 1) {
  # get range; loop through studies
  # define the call for getting the range
  call_1 <- paste0("histogramDS1(", x, ",", method, ",", k, ",", noise, ")")
  # if safemode, the aggregate call is sent to each datasource in a separately,
  # otherwise aggregate is only called once.
  if (safemode) {
    ranges <- lapply(1:length(datasources), function(i) {
      rangesCi <- unlist(DSI::datashield.aggregate(datasources[i], as.symbol(call_1)))  # CHECK IF RIGHT???????? must datasources be argument of lapply function??
      return(rangesCi)
    })
  } else {
    ranges <- DSI::datashield.aggregate(datasources, as.symbol(call_1))
  }

  # join all data.frames into one
  ranges <- do.call(rbind, ranges)

  # for nonnumeric vectors "The input vector is not a numeric!" is
  # returned instead of minimum and maximum
  # handle this error by checking if the ranges are as expected:
  if (dim(ranges)[2]<2) {
    warning(paste("getRange failed. Make sure that",x,"is a numeric vector."))
    return(c(NA,NA))
  }

  # if results are not combined, stdnames will be added as rownames
  if (type != "combined") {
    colnames(ranges) <- c("min","max")
    rownames(ranges) <- stdnames
  } else {
    ranges <- setNames(c(min(ranges[,1]),max(ranges[,2])), c("min","max"))
  }

  # returns the ranges
  return(ranges)
}


#' Plot data in a histogram with ggplot2
#'
#' @param x A character string referring to a numeric column in a DataSHIELD table.
#' @param bins either a numeric defining the number of bins or "Sturges" to
#' choose the bin number automatically. Default is "Sturges".
#' @param plot If "combined" then a plot combining the results from all servers,
#' if "separate" plots for each datasource, if "all" plots for each datasource
#' and combined are plotted. Otherwise only data are returned as a list and no
#' plot is generated.
#' @param k Numeric. Only used if `method==2`. The number of the nearest neighbors for which their centroid is calculated. Default 3.
#' @param noise Numeric. Only used if `method==3`. The percentage of the initial variance that is used as the variance of the embedded noise if the argument method is set to 'probabilistic'.
#' @param freq Boolean.
#' @param safemode Boolean. If FALSE, no plot is generated if any datasource fails,
#' otherwise the failing datasources are ignored and the results shouw only
#' data from non-failing datasources.
#' @param range Numeric vector of length 2. A range for the histogram can be specified.
#' @param datasources A list of \link[DSI]{DSConnection-class} objects.
#' @param stdnames Character vector. If there are more than one datasources and
#' type is not "combined", then this defines how the rows of the resulting table
#' are labelled. Default is to use the names of the datasources.
#' @param method A numeric. Either 1, 2 or 3 indicating the method of disclosure
#' control that is used for the generation of the histogram. If the value is equal to 1 then (only!) the
#' 'smallCellsRule' is used. If the value is equal to 2 then the 'deterministic' method is used additionally to method 1 for bins with count zero according to method 1.
#' If the value is set to 3 then the 'probabilistic' method is used additionally to method 1 for bins with count zero according to method 1.
#' @return A data.frame containing the table names, server location, symbols and dimensions.
#' @examples
#' ggHistDS("tab1$LAB_TRIG")
#' ggHistDS("tab1$LAB_TRIG", safemode=T, plot="separate")
#' ggHistDS("tab1$LAB_TRIG", safemode=T, plot="all")
#' ggHistDS("tab1$LAB_TRIG", safemode=T, plot=F)
#' # [[1]]
#' # [[1]]$histobject
#' # $breaks
#' # [1] -3.4839583 -2.2829666 -1.0819749  0.1190168  1.3200085  2.5210003  3.7219920  4.9229837  6.1239754  7.3249671  8.5259588  9.7269505
#' # [13] 10.9279422 12.1289340
#' #
#' # $counts
#' # [1]   4  33 135 396 531 450 192  50   6   1   0   0   3
#' #
#' # $density
#' # [1] 0.001849295 0.015256686 0.062413717 0.183080236 0.245493952 0.208045722 0.088766175 0.023116191 0.002773943 0.000000000 0.000000000
#' # [12] 0.000000000 0.001386971
#' #
#' # $mids
#' # [1] -2.8834624 -1.6824707 -0.4814790  0.7195127  1.9205044  3.1214961  4.3224878  5.5234795  6.7244713  7.9254630  9.1264547 10.3274464
#' # [13] 11.5284381
#' #
#' # $xname
#' # [1] "xvect"
#' #
#' # $equidist
#' # [1] TRUE
#' #
#' # attr(,"class")
#' # [1] "histogram"
#' #
#' # [[1]]$invalidcells
#' # [1] 1
#' #
#' # [[1]]$simulatedObs
#' # [1] 3
#' #
#' # [[1]]$uncertainCells
#' # [1] 10 11 12
#' #
#' # ....
ggHistDS <- function(x, bins = "Sturges", plot="combined", k=3, noise=0.25, freq=T, safemode=F, range=NULL, ..., datasources=DSI::datashield.connections_find(), stdnames = names(datasources), method = 2) {
  # number of studies
  num.sources <- checkDatasources(datasources)
  if(num.sources<1) return(NULL)

  # get range (use method 1 for range determination if method == 1 or 2, otherwiese use method 3 if method == 3)
  if (is.null(range)){
    ranges <- getRange(x, type="combined", k=k, noise=noise, safemode=safemode, datasources=datasources, stdnames = stdnames, method = ifelse(method < 3, 1, 3) )
  } else {
    ranges=range
  }
  ######################
  # get histogram data #
  ######################

  # get the axis label
  varname <- x

  # guess a good number of bins (cf. grDevices::nclass.Sturges) based on vector length
  lengths <- DSI::datashield.aggregate(conns=datasources, call("lengthDS", x))
  lengths$combined <- sum(unlist(lengths),na.rm=T)
  if (bins=="Sturges") {
    bins <- lapply(lengths, function(x) ceiling(log2(x) + 1) )
  }

  # call for the histogram object using method 1
  call <- paste0("histogramDS2(", x, ",", bins, ",", ranges[1], ",", ranges[2], ",", 1, ",", k, ",", noise, ")")
  outputs <- DSI::datashield.aggregate(datasources, call)

  # improve plot with a second call to histogramDS2 using method 2 or 3 if method is 2 or 3 repectively.
  if (method > 1) {
  # this fallback to improve the histogram can fail if the range is not wide enough
  call2 <- paste0("histogramDS2(", x, ",", bins, ",", ranges[1], ",", ranges[2], ",", method, ",", k, ",", noise, ")")
  k_outputs <- DSI::datashield.aggregate(datasources, call2)

  # merge the two results: replace missing bins from the more exact k-nearest neighbor method (1)
  # with bin values from Gaussian noise method (2)
  outputs_adj <- lapply(1:length(outputs), function(i) {
    emptycells <- which(outputs[[i]]$histobject$counts==0)
    outputs[[i]]$histobject$counts[emptycells] <- k_outputs[[i]]$histobject$counts[emptycells]
    outputs[[i]]$simulatedObs <- length(emptycells)
    outputs[[i]]$uncertainCells <- emptycells
    return(outputs[[i]])
  })
  outputs <- outputs_adj
  }

  # compute combined results (this block could be skipped, if !startsWith("comb",type))
  # copy data from first study
  new_el <- outputs[[1]]
  # if there are more than one datasources, add the information
  if (num.sources>1) {
    # adjust counts for combined histogram
    new_el$histobject$counts <- colSums(do.call(rbind, lapply(outputs, function(x) x$histobject$counts)))

    # get global number of invalid cells
    new_el$invalidcells <- sum(sapply(outputs, function(x) x$invalidcells))
  }
  # adjust density for combined histogram
  new_el$histobject$density <- new_el$histobject$counts/lengths$combined
  outputs$combined <- new_el

  # combined data plot
  if (startsWith("combined",as.character(plot))) {
    graphics::plot(outputs$combined$histobject, freq=freq, xlab=varname, main='Histogram of the pooled data')
  } else if (startsWith("separate",as.character(plot))) {
    #separate plots
    for (i in 1:(length(outputs)-1))
      graphics::plot(outputs[[i]]$histobject, freq=freq, xlab=varname, main=paste('Histogram of the datasource',i))
  } else if (startsWith("all",as.character(plot))) {
    #all plots
    for (i in 1:(length(outputs)-1))
      graphics::plot(outputs[[i]]$histobject, freq=freq, xlab=varname, main=paste('Histogram of the datasource',i))
    graphics::plot(outputs$combined$histobject, freq=freq, xlab=varname, main='Histogram of the pooled data')
  } else { # no plot
    message('"Plot is only drawn if plot=="combined" or plot="all" or plot="separate".')
  }

  return(outputs)
}

#' Get factor levels for a variable over all server connections
#'
#' @param var A character string. Name of the variable in the DataShield table object.
#' @param tab A character string. Name of the DataShield table object.
#' @param datasources A list of \link[DSI]{DSConnection-class} objects. Default is to use all findable datasources.
#' @return A numeric vector with the names of the factor levels.
#' @examples
#' dsUniqueLevels("PM_BMI_CATEGORICAL","tab1")
#' # [1] "1" "2" "3"
#' dsUniqueLevels("tab1$PM_BMI_CATEGORICAL")
#' # [1] "1" "2" "3"
dsUniqueLevels <- function(var, tab=NULL, datasources=DSI::datashield.connections_find()) {
  # if table is given separately build the complete variable object string
  if (!is.null(tab)) var <- paste0(tab,"$",var)
  # get levels using DataSHIELD
  levs <- DSI::datashield.aggregate(conns=datasources, paste0("levelsDS(",var,")"))
  # join all data.frames into one
  levs_df <- do.call(dplyr::bind_rows, levs)
  # only return unique values from all datasources
  unique(levs_df$Levels)
}

#' Get subsets of a DataShield object for each factor level of a factor variable
#'
#' @param tab A character string. Name of the DataShield table object.
#' @param var A character string. Name of the variable in the DataShield table object.
#' @param lazy A boolean. If TRUE then objects for groupwise tables are only created if they don't exist.
#' @param datasources A list of \link[DSI]{DSConnection-class} objects. Default is to use all findable connections.
#' @return A list of the names of the created DataShield objects.
#' @examples
#' dsSubsetLevels("PM_BMI_CATEGORICAL","tab1")
#' # then these new tables canbe used, e.g. for a plot:
#' ggHistDS("tab1.PM_BMI_CATEGORICAL.1$LAB_TRIG")
dsSubsetLevels <- function(var, tab, lazy=FALSE, datasources=DSI::datashield.connections_find()) {
  levs <- dsUniqueLevels(var, tab, datasources)
  df_sub <- lapply(levs, function(x) {
    tryCatch({
      sub_tab_name <- paste0(tab,".",var,".",x)
      if (!lazy || (sum(unlist(DSI::datashield.aggregate(datasources, call("exists", sub_tab_name)))) < 1)   ) {
        dsBaseClient::ds.Boole(V1 = paste0(tab,"$",var),
                 V2 = x,
                 Boolean.operator = "==",
                 numeric.output = TRUE,
                 newobj = paste0(tab,".",var,".",x,".bool"),
                 datasources = datasources)
        dsBaseClient::ds.dataFrameSubset(df.name = tab,
                           V1.name = paste0(tab,".",var,".",x,".bool"), #paste0(tab,"$",var),
                           V2.name = "1",
                           Boolean.operator = "==",
                           newobj = sub_tab_name,
                           datasources = datasources)
      }
      sub_tab_name
    },
    error = function(cond) {
      message(conditionMessage(cond))
      NA
    }
    )
  })
  list("created subsets"=unlist(df_sub))
}

#' Get two numeric columns of a DataShield data.frame in a non-disclosive (k-nearest-neighbor) way.
#'
#' These data can than be used e.g. for a scatterplot or an analysis on the client side.
#'
#' @param x A character string. Name of the x-variable in the DataShield table object.
#' @param y A character string. Name of the y-variable in the DataShield table object.
#' @param method method	A character string that specifies the method that is used to generated non-disclosive coordinates to be displayed in a scatter plot. This argument can be set as 'deterministic' (method=1) or 'probabilistic' (method=2). Default 'deteministic'.
#' @param k Numeric. Only used if 'deterministic' method is used. The number of the nearest neighbors for which their centroid is calculated. Default 3.
#' @param noise Numeric. Only used if 'probabilistic' method is used. The percentage of the initial variance that is used as the variance of the embedded noise if the argument method is set to 'probabilistic'.
#' @param datasources A list of \link[DSI]{DSConnection-class} objects. Default is to use all findable connections.
#' @return A data.frame with coloumns for x, y and server (source).
#' @examples
#' pair_data <- get_xy("tab1$LAB_TSC","tab1$LAB_TRIG")
#'
#' head(pair_data)
#' #          x         y  server
#' # 1 7.220168 2.4789300 server1
#' # 2 6.033267 1.3209085 server1
#' # 3 6.378069 1.4572257 server1
#' # 4 6.844664 4.4676767 server1
#' # 5 6.441833 0.7183122 server1
#' # 6 6.052160 2.9344455 server1
#'
#' # a simple scatterplot
#' plot(pair_data$x, pair_data$y)
get_xy <- function(x, y, method=1, k=3, noise=0.25, datasources=datashield.connections_find()) {

  # if x is a factor, make it numeric  -- see also: dsResiduals
  x_mod <- x
  if (any(dsBaseClient::ds.class(x, datasources)=="factor")) {
    x_mod <- gsub("\\$", "_", x)
    dsBaseClient::ds.asNumeric(x,x_mod,datasources)
  }

  # build DS call
  call <- paste0("scatterPlotDS(", x_mod, ",", y, ",", method, ",", k, ",", noise, ")")

  # request data from DS server
  output <- DSI::datashield.aggregate(datasources, call)

  # transform all list elements to data frames
  output_dfs <-lapply(output,
                      function(x) {as.data.frame(x, fix.empty.names=F, col.names=c("x","y"))} )

  # pool all data.frames
  pooled <- do.call(rbind, output_dfs)

  # add column for server name
  pooled$server <- rep(names(output_dfs), lapply(output_dfs,function(x) dim(x)[1]))

  #remove rownames
  rownames(pooled) <- NULL

  return(pooled)
}

#' Get two numeric columns and a factor column of a DataShield data.frame in a non-disclosive (k-nearest-neighbor) way.
#'
#' The result can be used e.g. for a grouped scatterplot and for further analysis on the client side.
#'
#' @param x A character string. Name of the x-variable in the DataShield table object.
#' @param y A character string. Name of the y-variable in the DataShield table object.
#' @param g A character string. Name of the factor variable (group) in the DataShield table object.
#' @param tab A character string. Name of the DataShield table object.
#' @param method method	A character string that specifies the method that is used to generated non-disclosive coordinates to be displayed in a scatter plot. This argument can be set as 'deterministic' (method=1) or 'probabilistic' (method=2). Default 'deteministic'.
#' @param k Numeric. Only used if 'deterministic' method is used. The number of the nearest neighbors for which their centroid is calculated. Default 3.
#' @param noise Numeric. Only used if 'probabilistic' method is used. The percentage of the initial variance that is used as the variance of the embedded noise if the argument method is set to 'probabilistic'.

#' @param datasources A list of \link[DSI]{DSConnection-class} objects. Default is to use all findable connections.
#' @return A data.frame with coloumns for x, y, group g and server (source).
#' @examples
#' pair_data <- dsGet_xyg("LAB_TSC","LAB_TRIG","GENDER","tab1")
#'
#' head(pair_data)
#' #    LAB_TSC  LAB_TRIG GENDER  server
#' # 1 6.379269 1.4462051      0 server1
#' # 2 5.427365 4.3632667      0 server1
#' # 3 7.877391 3.2380028      0 server1
#' # 4 5.521844 2.1337910      0 server1
#' # 5 5.139187 2.5045780      0 server1
#' # 6 6.204872 0.6240902      0 server1
#'
#' # a grouped scatterplot
#' plot(pair_data$LAB_TSC, pair_data$LAB_TRIG, col=as.numeric(pair_data$GENDER)+1)
dsGet_xyg <- function(x, y, g, tab, method=1, k=3, noise=0.25, datasources=datashield.connections_find()) {
  # if no group is given, then just redirect to function get_xy
  if (is.null(g)) {
    xydat <- get_xy(paste0(tab,"$",x),paste0(tab,"$", y), method=method, k=k, noise=noise, datasources=datasources)
    names(xydat) <- c(x,y,"server")
    return( xydat )
  }

  # subset the table according to group g (create subset tables for each level of g)
  subsetTableNames <- unname(unlist(dsSubsetLevels(g, tab,datasources=datasources) ))

  # get data for each group
  xyglist <- lapply(subsetTableNames, function(tabname) {
    cbind(get_xy(paste0(tabname,"$",x),paste0(tabname,"$", y), method=method, k=k, noise=noise, datasources=datasources), tabname)
  })

  # pool data for all group levels in a joint data.frame
  xyg_df <- do.call(rbind, xyglist)[,c(1,2,4,3)]

  # format group column
  xyg_df$tabname <- gsub(paste0("^",tab,"\\.",g,"\\."),"",xyg_df$tabname)

  # name columns  according to the variable names
  colnames(xyg_df) <- c(x,y,g,"server")

  # return data
  xyg_df
}

#' Perform generalized linear model regression in a non-disclosive (k-nearest-neighbor) way.
#'
#' The function basically calls ds.glm, but, the data table name is added to the output.
#' This function is basically a more comfortable wrapper for `dsBaseClient::ds.glm`.
#'
#' @param formula An object of class formula describing the model to be fitted.
#' @param data A character string specifying the name of an (optional) data frame that contains all of the variables in the GLM formula.
#' @param family	A character string. Identifies the error distribution function to use in the model ("gaussian", "binomial" or "poisson").
#' @param offset A character string specifying the name of a variable to be used as an offset.
#' @param weights	A character string specifying the name of a variable containing prior regression weights for the fitting process.
#' @param checks	A logical. If TRUE ds.glm checks the structural integrity of the model. Default FALSE. For more information see Details.
#' @param maxit	A numeric scalar denoting the maximum number of iterations that are permitted before non-convergence is declared.
#' @param CI	A numeric value specifying the confidence interval (default 0.95).
#' @param viewIter A logical. If TRUE the results of the intermediate iterations are printed. If FALSE only final results are shown. Default FALSE.
#' @param viewVarCov A logical. If TRUE the variance-covariance matrix of parameter estimates is returned. Default FALSE.
#' @param viewCor A logical. If TRUE the correlation matrix of parameter estimates is returned. Default FALSE.
#' @param datasources A list of \link[DSI]{DSConnection-class} objects.
#' @return A list with parameters similar to glm objects.
#' @examples
#' mod_height_ds_lm <- dsCallGLM(formula = "PM_BMI_CONTINUOUS ~ GENDER + LAB_TRIG",
#'                           data = "tab1",
#'                           family = "gaussian",
#'                           datasources = conns)
#'
#' mod_height_ds_lm$coefficients
#' #               Estimate Std. Error   z-value      p-value  low0.95CI high0.95CI
#' # (Intercept) 26.2806538  0.1574577 166.90614 0.000000e+00 25.9720424 26.5892651
#' # GENDER1     -0.7579016  0.1521374  -4.98169 6.303141e-07 -1.0560855 -0.4597176
#' # LAB_TRIG     0.7469029  0.0481524  15.51123 2.912536e-54  0.6525259  0.8412798
dsCallGLM <- function(formula = NULL, data = NULL, family = c("gaussian", "binomial", "poisson"), offset = NULL,
                      weights = NULL, checks = FALSE, maxit = 20, CI = 0.95, viewIter = FALSE,
                      viewVarCov = FALSE, viewCor = FALSE, datasources = NULL) {
  # match arguments
  family <- match.arg(family)

  # handle dependent factor variables
  # get dependent variable
  dep_var <- rownames(attr(terms(formula(formula)),"factors"))[1]
  # get unique variable names for table
  tabVariables <- dsAnalysisTools::dsUniqueVarnames(data, datasources=datasources)
  # create full name for dependent variable (TABLENAME$VARIABLENAME instead of just VARIABLENAME)
  if (dep_var %in% tabVariables) dep_var_mod <- paste0(data,"$",dep_var)
  # if dependent variable is a factor, make it numeric -- see also functions: get_xy, dsCallGLM, dsResiduals
  if (any(dsBaseClient::ds.class(dep_var_mod, datasources)=="factor")) {
    dep_var_mod2 <- gsub("\\$", "_", dep_var_mod)
    dsBaseClient::ds.asNumeric(dep_var_mod,dep_var_mod2,datasources)
    formula <- paste(dep_var_mod2 ,"~", tail(unlist(strsplit(formula,split="~")),1) )
  }

  # call ds.glm and store the result
  rs <- dsBaseClient::ds.glm(formula, data, family, offset, weights, checks, maxit, CI, viewIter,
               viewVarCov, viewCor, datasources)

  # also store the 'data' string for reference
  rs$dataString <- data

  # return the result
  return(rs)

  # the result's output differs from the `stats::glm` function in the following values:
  # missing return values: $call; ($contrasts); $df.null; $na.action; $null.deviance; $aic
  # misnamed return values: $df.residual ($df); $deviance ($dev)
}

#' Compute residuals for models returned by the dsCallGLM function on the server side.
#'
#' This function is basically a helper function for `dsBaseClient::ds.glm`.
#'
#' @param mod A list object as returned by dsCallGLM.
#' @param datasources A list of \link[DSI]{DSConnection-class} objects.
#' @return NULL
#' @examples
#' mod_height_ds_lm <- dsCallGLM(formula = "PM_BMI_CONTINUOUS ~ GENDER + LAB_TRIG",
#'                           data = "tab1",
#'                           family = "gaussian",
#'                           datasources = conns)
#'
#' dsResiduals(mod_height_ds_lm, datasources=conns)
#' # results are on the server only and can be used by `dsGLM`
#'
#' # test using DSLite:
#' getDSLiteData(conns, "mod_residuals")
#' getDSLiteData(conns, "mod_predicted")
dsResiduals <- function(mod, datasources) {
  # get model estimates
  est <- mod$coefficients[,"Estimate"]
  # get unique variable names for table (needed later)
  tabVariables <- dsAnalysisTools::dsUniqueVarnames(mod$dataString, datasources=datasources)
  # rename labels for independent variables for further use ("1" for Intercept)
  names(est)[1] <- "1"
  # add TABLENAME$ as prefix for variables from object mod$dataString
####  # -- also for derived dummy variables! (e.g. "GENDER1")
####  # regular expression to select all variables that match the pattern "names of a variable in mod$dataString plus an optional integer".
####  pattern <- paste0("^(", paste(tabVariables, collapse = "|"), ")(\\d+)?$")
####  matches <- grepl(pattern, names(tail(est,-1)))
  matches <- names(tail(est,-1)) %in% tabVariables
  addTabName <- c(FALSE, matches)
  names(est)[addTabName] <- paste0(mod$dataString,"$",names(est)[addTabName])
  # get and adapt name for dependent variable
  dep_var <- rownames(attr(terms(formula(mod$formula)),"factors"))[1]
  if (dep_var %in% tabVariables) dep_var <- paste0(mod$dataString,"$",dep_var)

  # if an independent variable is a factor, dummy variables need to be created
  idp_vars <- rownames(attr(terms(formula(mod$formula)),"factors"))[-1]
  for (i in idp_vars) {
    var_name <- paste0(mod$dataString,"$",i)
    if (any(dsBaseClient::ds.class(var_name, datasources)=="factor")) {
      createFactorVars(var_name, datasources=conns)
    }
  }

  # build formula for residuals
  estBRACKETS <- paste0("(",est,")")
  estPRODUCTS <- paste0(estBRACKETS,"*",names(est))

  # if dependent variable is factor make it numeric # cf. get_xy dsCallGLM dsResiduals
  dep_var_mod <- dep_var
  if (any(dsBaseClient::ds.class(dep_var, datasources)=="factor")) {
    dep_var_mod <- gsub("\\$", "_", dep_var)
    dsBaseClient::ds.asNumeric(dep_var,dep_var_mod,datasources)
  }

  # build up formula for residuals
  formula <- paste0(dep_var_mod,"-",paste0(estPRODUCTS,collapse="-"))
  message(paste("COMPUTE RESIDUALS:", formula))
  # compute residuals and save them as mod_residuals
  dsBaseClient::ds.make(toAssign=formula, newobj = "mod_residuals", datasources = datasources)

  # build up formula for predicted values
  formula2 <- paste0(paste0(estPRODUCTS,collapse="+"))
  message(paste("PREDICT VALUES:", formula2))
  # predict values and save them in mod_predicted
  dsBaseClient::ds.make(toAssign=formula2, newobj = "mod_predicted", datasources = datasources)
}

#' Create for each level of a (factor) variable a  boolean factor on the server
#' side in DataShield.
#'
#' @param x A character string referring to a table column in DataShield.
#' @param datasources A list of \link[DSI]{DSConnection-class} objects. Default is to use all findable connections.
#' @return NULL
#' @examples
#' createFactorVars("tab1$GENDER", datasources=conns)
#' # [[1]]
#' # [[1]]$is.object.created
#' # [1] "A data object <GENDER0> has been created in all specified data sources"
#' #
#' # [[1]]$validity.check
#' # [1] "<GENDER0> appears valid in all sources"
#' #
#' #
#' # [[2]]
#' # [[2]]$is.object.created
#' # [1] "A data object <GENDER1> has been created in all specified data sources"
#' #
#' # [[2]]$validity.check
#' # [1] "<GENDER1> appears valid in all sources"
createFactorVars <- function(x, datasources=DSI::datashield.connections_find()) {
  # get variable name from x without table name & "$" (VARNAME instead of TABLE$VARNAME)
  varname <- tail(unlist(strsplit(x,split="\\$")),1)

  # check if x is a factor
  isFactor <- all(dsBaseClient::ds.class(x, datasources = datasources) == "factor")

  # if x is not a factor, turn it into a factor (new name: tempname)
  if (!isFactor) {
    tempname <- gsub("\\$", "_", x)
    dsBaseClient::ds.asFactor(input.var.name = x,
                newobj.name = tempname,
                datasources = datasources) # datasources = conns) #!!!!!!!!!!!!!!!conns??
    x <- tempname
  }

  # get all unique levels of the factor
  fLevels <- dsUniqueLevels(x, datasources = datasources)

  # create dummy variables for each factor level
  lapply(1:length(fLevels), function(i) {
    dsBaseClient::ds.Boole(V1 = x,
             V2 = fLevels[i],
             Boolean.operator = "==",
             numeric.output = TRUE,
             newobj = paste0(varname,fLevels[i]),
             datasources = datasources)
  }  )
}

#' Perform generalized linear model regression and return the model object
#'
#' including non-disclosive k-nearest neighbor estimates for residuals.
#' @param formula An object of class formula describing the model to be fitted.
#' @param data A character string specifying the name of an (optional) data frame that contains all of the variables in the GLM formula.
#' @param family	A character string. Identifies the error distribution function to use in the model ("gaussian", "binomial" or "poisson").
#' @param offset A character string specifying the name of a variable to be used as an offset.
#' @param weights	A character string specifying the name of a variable containing prior regression weights for the fitting process.
#' @param maxit	A numeric scalar denoting the maximum number of iterations that are permitted before non-convergence is declared.
#' @param CI	A numeric value specifying the confidence interval (default 0.95).
#' @param viewIter A logical. If TRUE the results of the intermediate iterations are printed. If FALSE only final results are shown. Default FALSE.
#' @param viewCor A logical. If TRUE the correlation matrix of parameter estimates is returned. Default FALSE.
#' @param datasources A list of \link[DSI]{DSConnection-class} objects. Default is to use all findable connections.
#' @param add_tab_to_depvar A boolean. If TRUE, the table name + "$" is added as prefix to the dependent variable. Default is TRUE.
#' @return A list with parameters similar to glm objects.
#' @examples
#' foo <- dsGLM(formula = "PM_BMI_CONTINUOUS ~ GENDER + LAB_TRIG",
#'              data = "tab1",
#'              family = "gaussian",
#'              datasources = conns)
#' ## qq plot
#' qqnorm(foo$y)
#' qqline(foo$y)
#'
#' foo <- dsGLM(formula = "PM_BMI_CONTINUOUS ~ LAB_HDL + LAB_TRIG",
#'              data = "tab1", family = "gaussian", datasources = conns)
#' # histogram of privacy preserving residuals
#' hist(x = foo$residual, type = "combine", breaks = 18, datasources = conns)
#' # compare that histogram with `dsBaseClient::ds.histogram`
#' dsBaseClient::ds.histogram(x = "mod_residuals",
#'             type = "combine",
#'             num.breaks = 19,
#'             datasources = conns)
#' # scatterplot
#' dsBaseClient::ds.scatterPlot(x="mod_predicted", y="mod_residuals", type="combine", datasources=conns)
#'
#' # attention: it is still not fully compatible with the models from stats::lm:
#' # e.g. this does not work due to lack of the data slot:
#' #stats:::plot.lm(foo)
dsGLM <- function(formula = NULL, data = NULL, family = c("gaussian", "binomial", "poisson"), offset = NULL,
                  weights = NULL, maxit = 20, CI = 0.95, viewIter = FALSE,
                  viewCor = FALSE, datasources = NULL, add_tab_to_depvar = T) {  #viewVarCov = TRUE,
  # match arguments to either "gaussian", "binomial" or "poisson"
  family <- match.arg(family)

  # fit (generalized) linear model via dsCallGLM
  #   viewVarCov is a logical. Has to be TRUE, otherwise dsResiduals will fail.
  mod_glm <- dsCallGLM(formula = formula, data = data, family = family,
                       offset=offset, weights=weights, checks=F, maxit=maxit,
                       CI=CI, viewIter=viewIter, viewVarCov=TRUE,
                       viewCor=viewCor, datasources = datasources)

  # compute residuals (on server-side)
  dsResiduals(mod_glm, datasources=datasources)

  # if add_tab_to_depvar is TRUE, then add tablename + "$" as prefix to the name of the dependent variable
  if (add_tab_to_depvar) dep_var <- paste0(mod_glm$dataString,"$",rownames(attr(terms(formula(mod_glm$formula)),"factors"))[1]) else {
    dep_var <- rownames(attr(terms(formula(mod_glm$formula)),"factors"))[1]
  }

  # get non-disclosive predictions, y and residuals using get_xy
  foo <- dsAnalysisTools::get_xy(dep_var,"mod_residuals", datasources=datasources)
  # set slots of the model analogous to the models from `stats::glm`
  mod_glm$y <- foo$x
  #mod_glm$residual <- foo$y
  mod_glm$residuals <- foo$y
  mod_glm$fitted.values <- mod_glm$y - mod_glm$residuals
  mod_glm$df.residual <- mod_glm$df
  mod_glm$deviance <- mod_glm$dev
  mod_glm$rank <- as.numeric(Matrix::rankMatrix(mod_glm$VarCovMatrix))
  mod_glm$weights <- mod_glm$y*0+1
  mod_glm$prior.weights <- mod_glm$y*0+1 # for stats:::weights.glm^
  #mod_glm$qr # for influence function -- but unavailable in the privacy preserving DataSHIELD glm functions
  mod_glm$call <- match.call()
  mod_glm$terms <- terms(formula(formula))
  class(mod_glm) <- c("lm","glm")

  # return model object
  return(mod_glm)
}


#' Apply a function to subsets of a table in DataSHIELD for each different outcome of a categorical grouping variable
#'
#' This is useful if an analysis has to be performed for each factor level of a grouping variable.
#'
#' @param X A character string specifying a table which is available as a DataShield object.
#' @param G A character string specifying the group variable in X according to which the data are split before applying function FUN.
#' @param FUN	A function which creates a data.frame. It will be applied to groupwise subsets of the table X.
#' @param lazy A boolean. If TRUE then objects for groupwise tables are only created if they don't exist.
#' @param datasources  A list of \link[DSI]{DSConnection-class} objects. Default is to use all findable connections.
#' @param ...	A additional parameters to be passed to FUN.
#' @return A data.frame containing the results of FUN, applied to subgroups G of table X.
#' @examples
#' # get summary statistics for all variables grouped by GENDER
#' genderwise_summary <- dsGapply("tab1","GENDER",dsAnalysisTools::dsSummary, datasources=conns[1])
#' head(genderwise_summary)
#' #   GENDER variable feature   value       tabname
#' # 1      0  LAB_TSC       N 1092.00 tab1.GENDER.0
#' # 2      0  LAB_TSC      5%    4.07 tab1.GENDER.0
#' # 3      0  LAB_TSC     10%    4.50 tab1.GENDER.0
#' # 4      0  LAB_TSC     25%    5.09 tab1.GENDER.0
#' # 5      0  LAB_TSC     50%    5.90 tab1.GENDER.0
#' # 6      0  LAB_TSC     75%    6.67 tab1.GENDER.0
dsGapply <- function(x, G, FUN, lazy=FALSE, datasources=datashield.connections_find(), ...) {
  # trivial case: if no group given, just apply FUN to x
  if (length(G)==0) return( FUN(x, datasources=datasources, ...) )

  # initialize subsetTableNames with original tab name...
  subsetTableNames <- x
  groups <- data.frame(dummy=1)
  # for each group variable...
  for (i in seq_along(G)){
    # ...create subsets of the table x for each distinct group value
    subsetTableNames <- as.vector(sapply(subsetTableNames,
                                         function(x) if(is.na(x)) NA else dsSubsetLevels(G[i], x, lazy=lazy, datasources=datasources)$`created subsets`)) # if could probably be removed, because there should be no NA
    # -> table names are composed by tab, G and level, e.g.: "tab1.GENDER.0" "tab1.GENDER.1"

    # remember groupwise levels in that same order and save them in data.frame groups
    levelsGi <- dsUniqueLevels(G[i], x, datasources=datasources)

    groups <- groups[rep(seq_len(nrow(groups)),each=length(levelsGi)), ,drop=F]
    groups[[G[i]]] <- rep(levelsGi, length.out=length(subsetTableNames))
    # remove where NA
    groups <- groups[!is.na(subsetTableNames),,drop=F]
    subsetTableNames <- subsetTableNames[!is.na(subsetTableNames)]
  }

  summary_g_list <- lapply(subsetTableNames, FUN, datasources=datasources, ...)
  names(summary_g_list) <- subsetTableNames

  # format output: remove unnecessary rownames and joind data.frame objects
  rownames(groups) <- NULL
  summary_g_df <- do.call(rbind, Map(cbind, split(groups[,-1,drop=F],seq(nrow(groups))), summary_g_list,     tabname = names(summary_g_list) ) )
  rownames(summary_g_df) <- NULL

  # return the data.frame
  summary_g_df
}

#' @title Returns the metadata for one (!) specified variable
#'
#' @description This function returns metadata for a specified variable -
#' without unnecessary checks and outputs (as in the slow ds.metadata() from
#' dsBaseClient). Due to the restrictions in dsBase::metadataDS only the
#' following attributes are returned: 'names', 'spec', 'class', 'label',
#' 'opal.value_type', 'opal.entity_type', 'opal.repeatable', 'opal.index',
#' 'opal.nature'
#' The variable does not need to be specified in each server to obtain a result.
#' @details The function returns the metadata, obtained from attributes function.
#' @param x a string character, specifying the variable
#' @param datasources A list of \link[DSI]{DSConnection-class} objects. Default is to use all findable connections.
#' @param silent Logical. If TRUE, DataSHIELD errors are suppressed.
#' @return a data.frame containing the metadata.
#' @export
#' @examples
#' \dontrun{
#'   # connecting to the Opal servers
#'   require('DSI')
#'   require('DSOpal')
#'   require('dsBaseClient')
#'
#'   builder <- DSI::newDSLoginBuilder()
#'   builder$append(server="study1", url="https://opal-demo.obiba.org",
#'                  user="administrator", password="password",
#'                  table = "CNSIM.CNSIM1")
#'   builder$append(server="study3", url="https://opal-demo.obiba.org",
#'                  user="administrator", password="password",
#'                  table = "CNSIM.CNSIM3")
#'   logindata <- builder$build()
#'
#'   # Then perform login in each server
#'   conns <- datashield.login(logins=logindata, assign = TRUE, symbol = "D")
#'
#'   # Get the metadata associated with table 'D'
#'   dsBetter.metadata(x = 'D$LAB_TSC', datasources = conns)
#'   #           server                     label opal.value_type    opal.entity_type opal.repeatable opal.index   opal.nature  class       obj.name
#'   # LAB_TSC "study1" "Total Serum Cholesterol"       "decimal"       "Participant"               0          0  "CONTINUOUS"     NA    "D$LAB_TSC"
#'   # LAB_TSC "study3" "Total Serum Cholesterol"       "decimal"       "Participant"               0          0  "CONTINUOUS"     NA    "D$LAB_TSC"
#'
#'   # clear the Datashield R sessions and logout
#'   DSI::datashield.logout(conns)
#' }
dsBetter.metadata <- function (x, datasources=DSI::datashield.connections_find(), silent=FALSE) {
  # create list object to hold metadata
  rslist <- list()
  # turn of console log messages
  options(datashield.progress=F)
  # get metadata using DataSHIELD
  tryCatch(
    DSI::datashield.aggregate(
      conns=datasources,
      expr=call("metadataDS", x),
      success=function(server, rs) rslist[[server]] <<- rs,
      error=if (silent) function(server, e) NULL else function(server, e) print(paste0(server,": ", e)),
    ),
    error=function(e) NULL
  )
  # turn console log messages on again
  options(datashield.progress=T)

  # format metadata to a nice data.frame
  # a simple rs <- data.table::rbindlist(rslist, fill=TRUE) would not be safe,
  # because it does not handle the case when vector values like c("factor","ordered") are present...
  servernames <- names(rslist)

  # collect alle metadata attributes and add an attribute "class"
  # unname: if several servers (server1, server2,..) exist, then names will be different (server1.name, server2) => better remove the names
  allnames <- unique(c(names(unlist(unname(rslist),recursive=F)),"class")) # e.g. if only numeric variables exist, "class" will be missing => add it

  # which metadata attributes are missing?
  missnames <- lapply(rslist, function(x) setdiff(allnames, names(x)) )

  # go for all connections through missing metadata attributes
  for (i in 1:length(rslist)) {
    for (j in missnames[[i]]) {
      # set missing attribute to NA
      if (!is.null(j)) rslist[[i]][[j]] <- NA
    }
  }

  # join all data.frames into one
  rs <- do.call(rbind,rslist)

  # add coloumns for server and object name
  rs <- cbind(server=servernames,rs, obj.name=x)

  # set rownames to variable name
  rownames(rs) <- rep( gsub("^[^$]+[$]", "", x), length(servernames) )

  # return the result
  rs
}

#' @title Gets metadata for multiple variables of a table
#'
#' @description This function gets the metadata for multiple variables of a
#' given object on the server. Basically the DataSHIELD function ds.metadata is
#' applied to all variables given in x.
#' By default the result is returned as a data.frame.
#' @param x a character string specifying the name of the table object.
#' @param datasources a list of \code{\link{dsBaseClient::DSConnection-class}}
#' objects obtained after login. If the \code{dsBaseClient::datasources} argument is not specified
#' the default set of connections will be used: see \code{\link{dsBaseClient::datashield.connections_default}}.
#' @param simplify a boolean. If TRUE (default) the result is simplified to a data.frame. Otherwise a list is returned.
#' @return \code{ds.metadata} returns to the metadata of the associated table held at the server.
#' @export
#' @examples
#' \dontrun{
#'   # connecting to the Opal servers
#'
#'   require('DSI')
#'   require('DSOpal')
#'   require('dsBaseClient')
#'
#'   builder <- DSI::newDSLoginBuilder()
#'   builder$append(server="study1", url="https://opal-demo.obiba.org",
#'                  user="administrator", password="password",
#'                  table = "CNSIM.CNSIM1")
#'   builder$append(server="study3", url="https://opal-demo.obiba.org",
#'                  user="administrator", password="password",
#'                  table = "CNSIM.CNSIM3")
#'   logindata <- builder$build()
#'
#'   conns <- DSI::datashield.login(logins = logindata, assign = TRUE, symbol = "D")
#'
#'   # Get the metadata associated with table 'D'
#'   metadata <- ds.meta(x = c('D$LAB_TSC','D$LAB_TRIG','D$LAB_HDL'), datasources = conns)
#'   print(metadata)
#'   #          label                     opal.value_type opal.entity_type opal.repeatable opal.index opal.nature
#'   # LAB_TSC  "Total Serum Cholesterol" "decimal"       "Participant"    0               0          "CONTINUOUS"
#'   # LAB_TRIG "Triglycerides"           "decimal"       "Participant"    0               0          "CONTINUOUS"
#'   # LAB_HDL  "HDL Cholesterol"         "decimal"       "Participant"    0               0          "CONTINUOUS"
#'
#'   # clear the Datashield R sessions and logout
#'   DSI::datashield.logout(conns)
#' }
#'
ds.meta <- function(x=NULL, datasources=NULL, simplify=TRUE) {
  # get metadata using DataSHIELD
  #    [[1]]: usually the properties should be the same for all servers. we just use the first one.
  rs <- lapply(x, function(x) ds.metadata(x, datasources)[[1]] )

  # optionally simplyfy rs to a data.frame
  if (simplify) {
    # collect attribute names for all variables
    allnames <- unique(names(unlist(rs,recursive=F)))
    # which attributes are missing for which variable?
    missnames <- lapply(rs, function(x) setdiff(allnames, names(x)) )
    # go through all variables and attributes
    for (i in 1:length(rs)) {
      for (j in missnames[[i]]) {
        # replace missing attributes with NA
        if (!is.null(j)) rs[[i]][[j]] <- NA
      }
    }
    # join list elements into one data.frame
    rs <- do.call(rbind,rs)
    # set row names to variable names
    rownames(rs) <- gsub("^[^$]+[$]","",x)
  }

  # return result
  rs
}

#' @title Gets metadata for multiple variables of a table
#'
#' Data for each connection are returned -- contrary to the function ds.meta
#'
#' @description This function gets the metadata for multiple variables of a given object on the server
#' @param x a character string specifying the variables.
#' @param datasources a list of \code{\link{dsBaseClient::DSConnection-class}}
#' @param summarise_servers Logical. If FALSE, the result will be returned for
#' each variable  on each connection as a separate row in the resulting data.frame.
#' Otherwise information wil be summarized over all connections.
#' objects obtained after login. If the \code{dsBaseClient::datasources} argument is not specified
#' the default set of connections will be used: see \code{\link{dsBaseClient::datashield.connections_default}}.
#' @return \code{ds.metadata} returns to the metadata of the associated table held at the server.
#' @export
#' @examples
#' # Get the metadata associated with table 'D'
#' metadata <- ds.meta2(x = c('D$LAB_TSC','D$LAB_TRIG','D$LAB_HDL'), datasources = conns)
#' print(metadata)
#' #                            label opal.value_type opal.entity_type opal.repeatable opal.index opal.nature class   obj.name var.name           val
#' # LAB_TSC  Total Serum Cholesterol         decimal      Participant               0          0  CONTINUOUS    NA  D$LAB_TSC  LAB_TSC study1,study3
#' # LAB_TRIG           Triglycerides         decimal      Participant               0          0  CONTINUOUS    NA D$LAB_TRIG LAB_TRIG study1,study3
#' # LAB_HDL          HDL Cholesterol         decimal      Participant               0          0  CONTINUOUS    NA  D$LAB_HDL  LAB_HDL study1,study3
ds.meta2 <- function(x=NULL, datasources=NULL, summarise_servers = T) {
  rs <- lapply(x, function(x) dsBetter.metadata(x, datasources) )

  # use variable names as list element names
  names(rs) <- x

  # transform list elements to data.frames
  rs <- lapply(rs, as.data.frame)

 # # save variable name (first rownames label without ".1", ".2",...) in column var.name
 # rs <- lapply(rs, function(x) { x$var.name <- rownames(x)[1]; return(x) } )

  # merge all data.frames
  rs <- as.data.frame(data.table::rbindlist(rs, fill=TRUE))

  # save variable name in column var.name
  rs$var.name <- gsub("^[^$]+[$]","",rs$obj.name)

  # optionally, summarize rows over all servers
  if (summarise_servers) {
    rs <- dplyr::group_by(rs, across(-server))
    rs <- dplyr::summarise(rs, val= paste(server, collapse=","))
  }

  # remove rownames if more than 1 servers (duplicate rownames are anyway not allowed, so it does not make sense to save the variable name as rowname)
  if (nrow(rs)>length(x)) {
    rownames(rs) <- NULL
    } else {
      rs <- as.data.frame(rs)
      rownames(rs) <- rs$var.name
    }

  # return as data.frame
  as.data.frame(rs)
}


#' @title Get variable type as string ("numeric" or "categorical") for all variables of a table in unified form
#'
#' @description The main purpose of this function is to be a helper function for the function all_summaries.
#' @param tab A character string. Name of the DataShield table object.
#' @param datasources a list of \code{\link{dsBaseClient::DSConnection-class}}
#' objects obtained after login. If the \code{dsBaseClient::datasources} argument is not specified
#' the default set of connections will be used: see \code{\link{dsBaseClient::datashield.connections_default}}.
#' @return A data.frame containing the data type for each variable in table tab.
#' @examples
#' get_type("D",DSI::datashield.connections_find())
#' # # A tibble: 11 × 3
#' #    variable           feature value
#' #    <chr>              <chr>   <chr>
#' #  1 LAB_TSC            type    numeric
#' #  2 LAB_TRIG           type    numeric
#' #  3 LAB_HDL            type    numeric
#' #  4 LAB_GLUC_ADJUSTED  type    numeric
#' #  5 PM_BMI_CONTINUOUS  type    numeric
#' #  6 DIS_CVA            type    categorical
#' #  7 MEDI_LPD           type    categorical
#' #  8 DIS_DIAB           type    categorical
#' #  9 DIS_AMI            type    categorical
#' # 10 GENDER             type    categorical
#' # 11 PM_BMI_CATEGORICAL type    categorical
get_type <- function(tab, datasources=DSI::datashield.connections_find()) {
  # for each variable get type
  vars0 <- dsAnalysisTools::dsIsNumeric(tab, datasources=datasources)

  # recode the logical values to "numeric" and "categorical"
  vars <- vars0
  vars[vars0==T] <- "numeric"
  vars[vars0==F] <- "categorical"

  # turn it to a dataframe (each row matches a variable)
  rs <- data.frame(variable=names(vars), type=vars)
  # convert it to a long table (columns: variable, feature and value)
  rs <- tidyr::pivot_longer(rs, type, names_to = "feature", values_to = "value")

  rs
}
#

#' @title Return boxplot data
#'
#' @description Returns the first quantile, median, third quantile and a data privacy preserving safe range (ymin and ymax).
#' @param tab A character string. Name of the DataShield table object.
#' @param variables A character vector. Name of the requested columns of the DataShield table object.
#' @param group A character string or NULL. If ! NUL, then the boxplot data are for grouped boxplots for each group level of the variable "group".
#' @param group2 A character string or NULL. If ! NUL, then the boxplot data are for grouped boxplots for each group level of the variable "group2".
#' @param datasources  A list of \link[DSI]{DSConnection-class} objects.
#' @return A data.frame containing the data type for each variable in table tab.
#' @examples
#' \dontrun{
#'   # connecting to the Opal servers
#'   require('DSI')
#'   require('DSOpal')
#'   require('dsBaseClient')
#'
#'   builder <- DSI::newDSLoginBuilder()
#'   builder$append(server="study1", url="https://opal-demo.obiba.org",
#'                  user="administrator", password="password",
#'                  table = "CNSIM.CNSIM1")
#'   builder$append(server="study3", url="https://opal-demo.obiba.org",
#'                  user="administrator", password="password",
#'                  table = "CNSIM.CNSIM3")
#'   logindata <- builder$build()
#'
#'   conns <- DSI::datashield.login(logins = logindata, assign = TRUE, symbol = "D")
#'
#'   # Get the boxplot data
#'   boxplot_data <- ds.boxplot_data("D", "LAB_HDL", datasources=DSI::datashield.connections_find())
#'   print(boxplot_data$combined)
#'   #          x  ymin lower middle   upper    ymax
#'   #     <char> <num> <num>  <num>   <num>   <num>
#'   # 1: LAB_HDL 0.876 1.302  1.585 1.85025 2.22755
#'
#'   # compute boxplot data, grouped according to GENDER and BMI CATEGORY
#'   boxplot_data <- ds.boxplot_data("D", "LAB_HDL", "GENDER", "PM_BMI_CATEGORICAL", datasources=DSI::datashield.connections_find())
#'   print(boxplot_data$combined)
#'
#'   # clear the Datashield R sessions and logout
#'   DSI::datashield.logout(conns)
#' }
#'
ds.boxplot_data <-  function(x, variables = NULL, group = NULL, group2 = NULL, #xlabel = "x axis", ylabel = "y axis",
                             datasources = DSI::datashield.connections_find()){
  cls <- dsBaseClient:::checkClass(datasources, x)
  if(!any(c("data.frame") %in% cls)) stop("x was expected to refer to a data.frame on the DataShield server")

  # prepare boxplot raw data on the servers
  call1 <- as.symbol(sprintf("boxPlotGG_data_TreatmentDS(%s, c('%s'), %s, %s)",
                   x,
                   paste0(variables, collapse = "','"),
                   if (is.null(group)) "NULL" else paste0("'",group,"'"),
                   if (is.null(group2)) "NULL" else paste0("'",group2,"'")
  ))
  DSI::datashield.assign.expr(datasources, "boxPlotRawData", call1)

  # get boxplot data from DataSHIELD
  call2 <- as.symbol(sprintf("boxPlotGGDS(%s, %s, %s)",
                   "boxPlotRawData",
                   if (is.null(group)) "NULL" else paste0("'",group,"'"),
                   if (is.null(group2)) "NULL" else paste0("'",group2,"'")
  ))
  pt <- DSI::datashield.aggregate(datasources, call2)

  # compute the pooled boxplot using weighted means
  for(i in 1:length(names(datasources))){
    pt$combined <- rbind(pt$combined, pt[[i]]$data)
  }
  pt$combined <- data.table::data.table(pt$combined)
  pt$combined <- panelaggregation::computeWeightedMeans(pt$combined,
                                                        variables = c("ymin", "lower", "middle", "upper", "ymax"),
                                                        weight = "n",
                                                        by = c(if (!is.null(group)) "group" else NULL, if (!is.null(group2)) "group2" else NULL, "x")
  )

  # return all boxplot data (for each connection and pooled)
  pt
}
