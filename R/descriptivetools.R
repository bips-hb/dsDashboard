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
#Date/Changes/Author: XX/XX/2023 / First production of program / Andreas Mändle
###############################################################################
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

hello <- function() {
  print("Hello, world!")
}

#' Summary for a table in data shield
#'
#' Output: variables per server
#'
#' The output is more convenient than using the ds.summary function directly.
#'
#' @param x Table name as string.
#' @param datasources A list of 'OpalConnection'.
#' @return A data.frame
#' @examples
#' #dsTableSummary("tab1")
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
#' @param datasources A list of 'OpalConnection'.
#' @return A vector.
#' @examples
#' #dsUniqueVarnames("tab1")
dsUniqueVarnames <- function(x=NULL, datasources=NULL, fastmode = FALSE, combined=T) {
  if (fastmode) {
    rs <- data.frame(server=character(),
                     variable=character())
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
    if (combined) return(unique(rs$variable)) else return(rs)
  }
  ## legacy version  below

  s_tab1 <- dsBaseClient::ds.summary(x = x, datasources = datasources)
  s_tab1_df <- as.data.frame(do.call(rbind, s_tab1))
  unique(unlist(s_tab1_df$`variables held`))
}

#' Summary for categorical variable across all servers
#'
#' @param x Name of referring to a data.frame column as string.
#' @param datasources A list of 'OpalConnection'.
#' @return A data.frame.
#' @examples
#' #dsCatVarSummary("tab1$GENDER")
#' #dsCatVarSummary("tab1$DIS_AMI")
# dsCatVarSummary <- function(x=NULL, datasources=NULL) {
#   s_tab <- dsBaseClient::ds.summary(x = x, datasources = datasources)
#   s_tab <- lapply(s_tab, function(x) {data.frame(x, check.names = F)})
#   #df <- as.data.frame(do.call(rbind, s_tab))
#   df <- do.call(dplyr::bind_rows, s_tab)
#   df <- data.frame(cbind(server=row.names(df),df), check.names = F)
#   # unlist coloumns that can be simplified (to e.g. numeric, factor,...)
#   df <- data.frame(lapply(df, function(x) {if (length(x)==length(unlist(x))) unlist(x) else I(x)}), check.names = F)
#
#   if(dim(df)[2]<=2) { # probably an invalid object
#     return(data.frame(server="invalid",length=NA, level="invalid", value=NA))
#   }
#   # reshape: wide to long
#   df <- stats::reshape(df, varying=5:length(df),direction="long", v.names = "value", times = names(df)[5:length(df)], timevar = "level", idvar="server", drop=c("class","categories"))
#
#   # replace "count of 'XXX'" by XXX
#   df$level <- gsub("count of '(.*)'", '\\1', df$level)
#
#   rownames(df)<-NULL
#   df$value[is.na(df$value)]<-0
#   return(df)
# }
# dsCatVarSummaryOLD <- function(x=NULL, datasources=NULL) {
#   s_tab <- dsBaseClient::ds.summary(x = x, datasources = datasources)
#   s_tab <- lapply(s_tab, function(x) {data.frame(x, check.names = F)})
#   s_tab <- lapply(1:length(s_tab), function(x) s_tab[[x]] <- data.frame(server=names(s_tab)[x], s_tab[[x]])) # add server names
#   #df <- as.data.frame(do.call(rbind, s_tab))
#   df <- do.call(dplyr::bind_rows, s_tab)
#   df <- data.frame(cbind(id=row.names(df),df), check.names = F)
#   # unlist coloumns that can be simplified (to e.g. numeric, factor,...)
#   df <- data.frame(lapply(df, function(x) {if (length(x)==length(unlist(x))) unlist(x) else I(x)}), check.names = F)
#
#   if(dim(df)[2]<=2) { # probably an invalid object
#     return(data.frame(id="invalid",length=NA, level="invalid", value=NA))
#   }
#   # reshape: wide to long
#   df <- stats::reshape(df, varying=5:length(df),direction="long", v.names = "value", times = names(df)[5:length(df)], timevar = "level", idvar="id", drop=c("class","categories"))
#
#   # replace "count of 'XXX'" by XXX
#   df$level <- gsub("count of '(.*)'", '\\1', df$level)
#
#   rownames(df)<-NULL
#   df$value[is.na(df$value)]<-0
#   return(df)
# }
dsCatVarSummary <- function(x=NULL, datasources=NULL) {
  s_tab <- dsBaseClient::ds.summary(x = x, datasources = datasources)
  # concatenate categories
  s_tab <- lapply(s_tab, function(x) {
    if(class(s_tab[[1]])!="character") x$categories<-paste(x$categories, collapse=",") # otherwise failire with invalid objects
    x
  })
  #s_tab <- lapply(s_tab, function(x) {data.frame(x, check.names = F)})
  s_tab <- lapply(1:length(s_tab), function(x) s_tab[[x]] <- data.frame(server=names(s_tab)[x], s_tab[[x]])) # add server names
  df <- do.call(dplyr::bind_rows, s_tab)
  #df <- data.frame(cbind(id=row.names(df),df), check.names = F)
  # unlist coloumns that can be simplified (to e.g. numeric, factor,...)
  df <- data.frame(lapply(df, function(x) {if (length(x)==length(unlist(x))) unlist(x) else I(x)}), check.names = F)
  if(dim(df)[2]<=2) { # probably an invalid object  !!!!!!!!!!!!!!! <5??????????
    return(data.frame(id="invalid",length=NA, level="invalid", value=NA))
  }
  # drop unnecessary columns
  df <- df %>% dplyr::select(-class, -categories)
  # reshape: wide to long
  df <- stats::reshape(df, varying=3:length(df),direction="long", v.names = "value", times = names(df)[3:length(df)], timevar = "level", idvar="server")
  # replace "count of 'XXX'" by XXX
  df$level <- gsub("count.of..(.*).", '\\1', df$level)

  rownames(df)<-NULL
  df$value[is.na(df$value)]<-0
  return(df)
}


#' Summary for all categorical variables of a table across all servers
#'
#' @param x A character string specifying the name of a data.frame.
#' @param datasources A list of \code{\link{DSI::DSConnection-class}} objects.
#' @return A data.frame.
#' @examples
#' #dsCatAgg("tab1$GENDER")
#' #dsCatAgg("tab1$DIS_AMI")
dsCatAgg <- function(x=NULL, datasources=NULL) {
  message("--dsCatAgg()--")
  df <- dsCatVarSummary(x = x, datasources = datasources)
  df <- stats::aggregate(df[,"value",drop=FALSE], by = list(df$level), FUN = "sum")
  #tryCatch( {
  #  df <<- stats::aggregate(df[,"value",drop=FALSE], by = list(df$level), FUN = "sum")
  #},
  #error = function(e) {warning("--dsCatAgg() failed--"); print(datashield.errors()); df <<- data.frame(value=NA) })

  varname <- unlist(regmatches(x, regexpr("\\$", x), invert = TRUE))[2]
  colnames(df) <- c(varname, "number of observations")
  return(df)
}

#' Check if columns of a DataSHIELD table are numeric
#'
#' @param x A character string referring to a data.frame.
#' @param x.vars Optional character string vector; a subset of the variables in table x, on which the function shall be applied
#' @param datasources A list of \code{\link{DSI::DSConnection-class}} objects.
#' @return A vector of boolean.
#' @examples
#' #dsIsNumeric("tab1")
dsIsNumeric <- function(x=NULL, x.vars=NULL, datasources=NULL) {
  if (!is.null(x.vars))
    tbPars<- x.vars
  else
    tbPars <- dsUniqueVarnames(x, datasources, fastmode=T)
  options(datashield.progress=F)
  numCols <- sapply(tbPars,function(tbCol){
    tryCatch({
      colTypes <<- DSI::datashield.aggregate(datasources, call("classDS",paste0(x,'$',tbCol)))
      return("integer" %in% colTypes | "numeric" %in% colTypes)
    },
    error= function(e) {
      return(NA)} )
  })
  options(datashield.progress=T)
  return(numCols)
}
# dsIsNumeric <- function(x=NULL, datasources=NULL) {
#   tbPars <- dsUniqueVarnames(x, datasources, fastmode=T)
#   options(datashield.progress=F)
#   numCols <- sapply(tbPars,function(tbCol){
#     #colTypes <- dsBaseClient::ds.class(x = paste0(x,'$',tbCol), datasources = datasources)
#     tryCatch({
#       #colTypes <<- dsBaseClient::ds.class(x = paste0(x,'$',tbCol), datasources = datasources)
#       colTypes <<- DSI::datashield.aggregate(datasources, call("classDS",paste0(x,'$',tbCol)))
#       return("integer" %in% colTypes | "numeric" %in% colTypes)
#       },
#       error= function(e) {
#         return(NA)} )
#
#     #return("integer" %in% colTypes | "numeric" %in% colTypes)
#   })
#   options(datashield.progress=T)
#   return(numCols)
# }

#' Summary for all numeric variables in a DataSHIELD table
#'
#' @param x A character string referring to a DataSHIELD table.
#' @param datasources A list of \code{\link{DSI::DSConnection-class}} objects.
#' @return A data.frame.
#' @examples
#' #dsNumVarSummary("tab1")
dsNumVarSummary <- function(x=NULL, datasources=NULL) {
  #from dsIsNumeric:
  tbPars <- dsUniqueVarnames(x, datasources)
  numCols <- sapply(tbPars,function(tbCol){
    colTypes <- dsBaseClient::ds.class(x = paste0(x,'$',tbCol), datasources = datasources)
    return("integer" %in% colTypes | "numeric" %in% colTypes)
  })
  ##end
  sapply(tbPars[numCols], function(tbCol) {
    dsBaseClient::ds.quantileMean(paste0(x,'$',tbCol), type = "combine", datasources = datasources)
  })
}

#' Summary for all non-numeric variables in a DataSHIELD table
#'
#' @param x A character string referring to a DataSHIELD table.
#' @param datasources A list of \code{\link{DSI::DSConnection-class}} objects.
#' @return A data.frame.
#' @examples
#' #dsCatSummary("tab1")
dsCatSummary <- function(x=NULL, datasources=NULL) {
  numCols <- dsIsNumeric(x, datasources)
  notNumeric <- names(numCols)[!numCols]
  foo<-lapply(notNumeric, function(col) {
    dsCatAgg(paste0(x,"$",col), dsCatAgg)
  })
  names(foo) <- sapply(foo, function(x) { colnames(x)[1] })
  lapply(foo, function(x) { colnames(x)[1]<-"level"; return(x) })
}

#' A tidy summary for all non-numeric variables in a DataSHIELD table
#'
#' @param x A character string referring to a DataSHIELD table.
#' @param datasources A list of \link[DSI]{DSConnection-class} objects.
#' @return A data.frame.
#' @examples
#' #dsCatTidy("tab1")
dsCatTidy <- function(x=NULL, datasources=NULL) {
  df <- dsCatSummary(x = x, datasources = datasources)

  # write listname in coloumn "variable"
  df <- lapply(seq_along(df), function(i,x, names){
    x[[i]] <- cbind(variable=NA, x[[i]])
    x[[i]]$variable <- names[i]
    return(x[[i]])
  }, names=names(df), x=df)

  # rbind dfs in list
  do.call(rbind, df)
}

#' A tiny summary for a numeric variables in a DataSHIELD table
#'
#' @param x A character string referring to a numeric column in a DataSHIELD table.
#' @param datasources A list of \link[DSI]{DSConnection-class} objects.
#' @return A data.frame.
#' @examples
#' #dsNumSummary("tab1$LAB_HDL")
dsNumSummary <- function(x=NULL, datasources=NULL) {
  message("--dsNumSummary()--")
  varname <- unlist(regmatches(x, regexpr("\\$", x), invert = TRUE))[2]
  ds_summ <- dsBaseClient::ds.summary(x, datasources = datasources)
  df <- c(setNames(ds_summ[[1]]$length,"N"), ds_summ[[1]]$`quantiles & mean`)
  #df <- dsBaseClient::ds.quantileMean(x, type = "combine", datasources = datasources)[c("25%","50%","75%","Mean")]
  #tryCatch( {
  #  df <<- dsBaseClient::ds.quantileMean(x, type = "combine", datasources = datasources)[c("25%","50%","75%","Mean")]
  #  },
  #  error = function(e) {warning("--dsNumSummary() failed--"); print(datashield.errors()); df <<- data.frame(value=NA) })

  df <- data.frame(df)
  colnames(df) <- "value" # varname
  # colnames als spalte
  df <- cbind(variable=NA,df)
  df$variable <- rownames(df)
  colnames(df)[1] <- varname
  rownames(df) <- NULL
  return(df)
  #dsCatAgg("tab1$GENDER")
}

#' A summary for all (numeric and non-numeric) variables in a DataSHIELD table
#'
#' @param x A character string referring to a numeric column in a DataSHIELD table.
#' @param datasources A list of \link[DSI]{DSConnection-class} objects.
#' @return A data.frame.
#' @examples
#' #dsSummary("tab1")
dsSummary <- function(x=NULL, datasources=NULL) {
  # check if numeric or not
  numCols <- dsIsNumeric(x, datasources)
  notNumeric <- names(numCols)[!numCols]
  notNumeric <- notNumeric[!is.na(notNumeric)]
  areNumeric <- names(numCols)[numCols]
  areNumeric <- areNumeric[!is.na(areNumeric)]
  areNA <- numCols[is.na(numCols)]

  #make list to take summaries
  sList <- as.list(numCols)

  #na cases
  sList[areNA] <- lapply(areNA, function(col) {
    tdf <- data.frame(variable = col, feature=NA, value=NA)
    tdf[,2] <- formatC(NA,format="d")
    tdf
  })

  #notNumericCases
  sList[notNumeric] <- lapply(notNumeric, function(col) {
    tdf <- dsCatAgg(paste0(x,"$",col), datasources = datasources)
    tdf[,2] <- formatC(tdf[,2],format="d")
    tdf
  })

  #numericCases
  sList[areNumeric] <- lapply(areNumeric, function(col) {
    tdf <- dsNumSummary(paste0(x,"$",col), datasources = datasources)
    tdf[,2] <- formatC(tdf[,2],format="f", digits=2)
    tdf
  })

  # make names of coloumn 1 and 2 equal
  sList <- lapply(sList, function(x) { colnames(x)[1]<-"feature"; return(x) })
  sList <- lapply(sList, function(x) { colnames(x)[2]<-"value"; return(x) })
  #return(sList)

  # make it tidy
  df <- sList
  # write listname in a new coloumn "variable"
  df <- lapply(seq_along(df), function(i,x, names){
    x[[i]] <- cbind(variable=NA, x[[i]])
    x[[i]]$variable <- names[i]
    return(x[[i]])
  }, names=names(df), x=df)

  # rbind dfs in list
  do.call(rbind, df)
}

#' Assign all tables from all servers to a symbol in DataSHIELD
#'
#' @param datasources A list of \link[DSI]{DSConnection-class} objects.
#' @return A data.frame containing the table names.
#' @examples
#' #tabNamesTidy <- assignAllTables()
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

    #DSI::datashield.assign.table(conns = datasources[[names[i,1]]],
    #                             symbol = paste0("tab",i),
    #                             table = names[i,2])
    return(c(tabNamesTidy[i,],"ok"))
  }, names=tabNamesTidy)

  return(tabNamesTidy)
}

#' Summarize the requested tables DataSHIELD
#'
#' @param datasources A list of \link[DSI]{DSConnection-class} objects.
#' @param tabList A data.frame with the table names and server locations.
#' @return A data.frame containing the table names, server location, symbols and dimensions.
#' @examples
#' #tabSummary(tabNamesTidy)
tabSummary <- function(tabList, datasources) {
  ##if (is.null(tabList)) tabList <- tabNamesTidy
  # loop through lines of tabList
  df <- lapply(1:(dim(tabList)[1]), function(i,names) {
    #browser()
    #tabDims <- tail(dsBaseClient::ds.dim(x = paste0('tab',i),datasources  = datasources[unlist(tabList[i,1])]),1)[[1]]
    #tabDims <- tail(DSI::datashield.aggregate(datasources[unlist(tabList[i,1])], call("dimDS", paste0("tab",i))),1)
    tabDims <- DSI::datashield.aggregate(datasources[unlist(tabList[i,1])], call("dimDS", paste0("tab",i)))
    tabDims <- do.call(rbind,tabDims)
    tabDims <- c(sum(tabDims[,1]), unique(tabDims[,2]))

    if (is.list(tabDims) && length(tabDims)>=1) tabDims <- tabDims[[1]]
    if (length(tabDims)!=2) tabDims <- c(NA,NA)
    df <- cbind(paste0('tab',i),I(tabList[i,1]),tabList[i,2],t(unname(unlist(tabDims))))
    data.frame(df)
  })
  df <- as.data.frame(do.call(rbind, df))
  colnames(df) <- c("id","server","name","rows","cols")
  return(df)
}

#' Summarize the requested tables DataSHIELD
#'
#' @param datasources A list of \link[DSI]{DSConnection-class} objects.
#' @param tabList A data.frame with the table names and server locations.
#' @return A data.frame containing the table names, server location, symbols and dimensions.
#' @examples
#' #tabSummary(tabNamesTidy)
tabSummaryN <- function(tabList, datasources) {
  ##if (is.null(tabList)) tabList <- tabNamesTidy
  # loop through lines of tabList
  df <- lapply(1:(dim(tabList)[1]), function(i,names) {
    tabDims <- DSI::datashield.aggregate(datasources[unlist(tabList[i,1])], call("dimDS", tabList[i,2]))
    tabDims <- do.call(rbind,tabDims)
    tabDims <- c(sum(tabDims[,1]), unique(tabDims[,2]))
    #tabDims <- tail(DSI::datashield.aggregate(datasources[unlist(tabList[i,1])], call("dimDS", tabList[i,2])),1)

    if (is.list(tabDims) && length(tabDims)>=1) tabDims <- tabDims[[1]]
    if (length(tabDims)!=2) tabDims <- c(NA,NA)
    df <- cbind(tabList[i,2],I(tabList[i,1]),tabList[i,2],t(unname(unlist(tabDims))))
    data.frame(df)
  })
  df <- as.data.frame(do.call(rbind, df))
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
#' #checkDatasources()
checkDatasources <- function(datasources=DSI::datashield.connections_find()) {
  # datasources must be a list of DSConnection-class
  if(!(is.list(datasources) && all(unlist(lapply(datasources, function(d) {methods::is(d,"DSConnection")}))))){
    warning("'datasources' must be a list of DSConnection-class objects", call.=FALSE)
    return(-1)
  } else {
    return(length(datasources))
  }
}

#' Get Range
#'
#' @param datasources A list of \link[DSI]{DSConnection-class} objects.
#' @return A numeric vector (if '"combined" or only 1 study) or a matrix (else).
#' @examples
#' #dsAnalysisTools::getRange("DST$age")
#' #dsAnalysisTools::getRange("DST$age", safemode=T)
getRange <- function(x, type="combined", k=3, noise=0.25, safemode=F, datasources=DSI::datashield.connections_find(), stdnames = names(datasources)) {
  # get range; loop through studies
  call_1 <- paste0("histogramDS1(", x, ",", 1, ",", k, ",", noise, ")")
  if (safemode) {
    ranges <- lapply(1:length(datasources), function(i) {
      rangesCi <- unlist(DSI::datashield.aggregate(datasources[i], as.symbol(call_1)))  # CHECK IF RIGHT???????? must datasources be argument of lapply function??
      return(rangesCi)
    })
  } else {
    ranges <- DSI::datashield.aggregate(datasources, as.symbol(call_1))
  }

  ranges <- do.call(rbind, ranges)

  # for nonnumeric vectors "The input vector is not a numeric!" is
  # returned instead of minimum and maximum
  # handle this:
  if (dim(ranges)[2]<2) {
    warning(paste("getRange failed. Make sure that",x,"is a numeric vector."))
    return(c(NA,NA))
  }

  if (type != "combined") {
    colnames(ranges) <- c("min","max")
    rownames(ranges) <- stdnames
  } else {
    ranges <- setNames(c(min(ranges[,1]),max(ranges[,2])), c("min","max"))
  }

  return(ranges)
}


#' Plot a histogram with ggplot
#'
#' @param x A character string referring to a numeric column in a DataSHIELD table.
#' @param bins either a numeric defining the number of bins or "Sturges" to
#' choose the bin number automatically. Default is "Sturges".
#' @param plot If "combined" then a plot combining the results from all servers
#' is plotted. Otherwise only data are returned and no plot is generated.
#' @param k The number of the nearest neighbors for which their centroid is calculated. Default 3.
#' @param noise The percentage of the initial variance that is used as the variance of the embedded noise if the argument method is set to 'probabilistic'.
#' @param datasources A list of \link[DSI]{DSConnection-class} objects.
#' @param tabList A data.frame with the table names and server locations.
#' @return A data.frame containing the table names, server location, symbols and dimensions.
#' @examples
#' #dsAnalysisTools::ggHistDS("DST$age")
ggHistDS <- function(x, bins = "Sturges", plot="combined", binstabList, k=3, noise=0.25, freq=T, safemode=F, range=NULL, ..., datasources=DSI::datashield.connections_find(), stdnames = names(datasources)) {
  # number of studies
  num.sources <- checkDatasources(datasources)
  if(num.sources<1) return(NULL)

  # get range
  if (is.null(range)){
    ranges <- getRange(x, type="combined", k=k, noise=noise, safemode=safemode, datasources=datasources, stdnames = stdnames)
  } else {
    ranges=range
  }

  # get histogram data

  # get the axis label
  varname <- x

  # guess a good number of bins (cf. grDevices::nclass.Sturges) based on vector length
  lengths <- DSI::datashield.aggregate(conns=datasources, call("lengthDS", x))
  lengths$combined <- sum(unlist(lengths),na.rm=T)
  if (bins=="Sturges") {
    bins <- lapply(lengths, function(x) ceiling(log2(x) + 1) )
  }

  # call for the histogram object
  call <- paste0("histogramDS2(", x, ",", bins, ",", ranges[1], ",", ranges[2], ",", 1, ",", k, ",", noise, ")")
  outputs <- DSI::datashield.aggregate(datasources, call)

  improveplot = T
  #browser()
  if ( improveplot ) { # if we want to improve the plot--- for now ALWAYS
    # should be in a try block... then one could automatically use a second fallback with wider range
    call2 <- paste0("histogramDS2(", x, ",", bins, ",", ranges[1], ",", ranges[2], ",", 2, ",", k, ",", noise, ")")
    k_outputs <- DSI::datashield.aggregate(datasources, call2)
    outputs_adj <- lapply(1:length(outputs), function(i) {
      emptycells <- which(outputs[[i]]$histobject$counts==0)
      outputs[[i]]$histobject$counts[emptycells] <- k_outputs[[i]]$histobject$counts[emptycells]
      outputs[[i]]$simulatedObs <- length(emptycells)
      outputs[[i]]$uncertainCells <- emptycells
      return(outputs[[i]])
    })
    outputs <- outputs_adj
  }

  if ( T ) { # this is quite fast so do these steps anyway # startsWith("combined",type)
    # copy stuff from first study
    new_el <- outputs[[1]]
    if (num.sources>1) {

      # adjust counts for combined histogram
      new_el$histobject$counts <- colSums(do.call(rbind, lapply(outputs, function(x) x$histobject$counts)))

      ## adjust density for combined histogram #!!WRONG WEIGHTS!!!!!!!!!!
      #new_el$histobject$density <- colSums(do.call(rbind, lapply(outputs, function(x) x$histobject$density)))/num.sources

      # get global number of invalid cells
      new_el$invalidcells <- sum(sapply(outputs, function(x) x$invalidcells))
    }
    # adjust density for combined histogram
    new_el$histobject$density <- new_el$histobject$counts/lengths$combined
    outputs$combined <- new_el
  }

  if (startsWith("combined",as.character(plot))) {
    graphics::plot(outputs$combined$histobject, freq=freq, xlab=varname, main='Histogram of the pooled data')
  } else {
    message('"Plot is only drawn if plot=="combined". You can use the returned data to plot each of the individual histograms.')
  }

  return(outputs)
}

#' Get factor levels for a variable over all server connections
#'
#' @param tab A character string. Name of the DataShield table object.
#' @param var A character string. Name of the variable in the DataShield table object.
#' @param datasources A list of \link[DSI]{DSConnection-class} objects.
#' @return A numeric vector with the names of the factor levels.
#' @examples
#' #dsUniqueLevels("PM_BMI_CATEGORICAL","tab1")
#' dsUniqueLevels("tab1$PM_BMI_CATEGORICAL")
dsUniqueLevels <- function(var, tab=NULL, datasources=DSI::datashield.connections_find()) {
  if (!is.null(tab)) var <- paste0(tab,"$",var)
  levs <- DSI::datashield.aggregate(conns=datasources, paste0("levelsDS(",var,")"))
  #levs <- lapply(1:length(levs), function(x) levs[[x]] <- data.frame(server=names(levs)[x], levs[[x]])) # add server names # unnecessary
  levs_df <- do.call(dplyr::bind_rows, levs)
  unique(levs_df$Levels)
}

#' Get subsets of a DataShield object for each factor level of a factor variable
#'
#' @param tab A character string. Name of the DataShield table object.
#' @param var A character string. Name of the variable in the DataShield table object.
#' @param lazy A boolean. If TRUE then objects for groupwise tables are only created if they don't exist.
#' @param datasources A list of \link[DSI]{DSConnection-class} objects.
#' @return A list of the names of the created DataShield objects.
#' @examples
#' #dsSubsetLevels("PM_BMI_CATEGORICAL","tab1")
dsSubsetLevels <- function(var, tab, lazy=FALSE, datasources=DSI::datashield.connections_find()) {
  levs <- dsUniqueLevels(var, tab, datasources)
  df_sub <- lapply(levs, function(x) {
    tryCatch({
      sub_tab_name <- paste0(tab,".",var,".",x)
      if (!lazy || (sum(unlist(DSI::datashield.aggregate(datasources, call("exists", sub_tab_name)))) < 1)   ) {
        ds.Boole(V1 = paste0(tab,"$",var),
                 V2 = x,
                 Boolean.operator = "==",
                 numeric.output = TRUE,
                 newobj = paste0(tab,".",var,".",x,".bool"),
                 datasources = datasources)
        ds.dataFrameSubset(df.name = tab,
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
#' @param x A character string. Name of the x-variable in the DataShield table object.
#' @param y A character string. Name of the y-variable in the DataShield table object.
#' @param method method	A character string that specifies the method that is used to generated non-disclosive coordinates to be displayed in a scatter plot. This argument can be set as 'deteministic' or 'probabilistic'. Default 'deteministic'. For more information see Details.
#' @param k The number of the nearest neighbors for which their centroid is calculated. Default 3.
#' @param noise The percentage of the initial variance that is used as the variance of the embedded noise if the argument method is set to 'probabilistic'.
#' @param datasources A list of \link[DSI]{DSConnection-class} objects.
#' @return A data.frame with coloumns for x, y and server (source).
#' @examples
#' #get_xy("tab1$LAB_TSC","tab1$LAB_TRIG")
get_xy <- function(x, y, method=1, k=3, noise=0.25, datasources=datashield.connections_find()) {

  # if factor make numeric  # cf: dsResiduals
  x_mod <- x
  if (any(ds.class(x, datasources)=="factor")) {
    x_mod <- gsub("\\$", "_", x)
    dsBaseClient::ds.asNumeric(x,x_mod,datasources)
  }

  # build DS call
  call <- paste0("scatterPlotDS(", x_mod, ",", y, ",", method, ",", k, ",", noise, ")")

  # request data from DS server
  output <- DSI::datashield.aggregate(datasources, call)
  # list elements to data frames
  output_dfs <-lapply(output,
                      function(x) {as.data.frame(x, fix.empty.names=F, col.names=c("x","y"))} )
  # pool data
  pooled <- do.call(rbind, output_dfs)
  # add column for server name
  pooled$server <- rep(names(output_dfs), lapply(output_dfs,function(x) dim(x)[1]))
  #remove colnames
  rownames(pooled) <- NULL
  return(pooled)
}

#' Get two numeric columns and a factor coloumn of a DataShield data.frame in a non-disclosive (k-nearest-neighbor) way.
#'
#' @param x A character string. Name of the x-variable in the DataShield table object.
#' @param y A character string. Name of the y-variable in the DataShield table object.
#' @param g A character string. Name of the factor variable (group) in the DataShield table object.
#' @param tab A character string. Name of the DataShield table object.
#' @param method method	A character string that specifies the method that is used to generated non-disclosive coordinates to be displayed in a scatter plot. This argument can be set as 'deteministic' or 'probabilistic'. Default 'deteministic'. For more information see Details.
#' @param k The number of the nearest neighbors for which their centroid is calculated. Default 3.
#' @param noise The percentage of the initial variance that is used as the variance of the embedded noise if the argument method is set to 'probabilistic'.
#' @param datasources A list of \link[DSI]{DSConnection-class} objects.
#' @return A data.frame with coloumns for x, y, group g and server (source).
#' @examples
#' #dsGet_xyg("LAB_TSC","LAB_TRIG","GENDER","tab1")
dsGet_xyg <- function(x, y, g, tab, method=1, k=3, noise=0.25, datasources=datashield.connections_find()) {
  if (is.null(g)) {
    xydat <- get_xy(paste0(tab,"$",x),paste0(tab,"$", y), method=method, k=k, noise=noise, datasources=datasources)
    names(xydat) <- c(x,y,"server")
    return( xydat )
  }
  subsetTableNames <- unname(unlist(dsSubsetLevels(g, tab,datasources=datasources) ))
  xyglist <- lapply(subsetTableNames, function(tabname) {
    cbind(get_xy(paste0(tabname,"$",x),paste0(tabname,"$", y), method=method, k=k, noise=noise, datasources=datasources), tabname)
  })
  xyg_df <- do.call(rbind, xyglist)[,c(1,2,4,3)]
  xyg_df$tabname <- gsub(paste0("^",tab,"\\.",g,"\\."),"",xyg_df$tabname)
  colnames(xyg_df) <- c(x,y,g,"server")
  xyg_df
}

#' Perform generalized linear model regression in a non-disclosive (k-nearest-neighbor) way.
#' It basically calls ds.glm, but, the data table name is added to the output.
#' This function is basically a helper function for ds GLM.
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
#' mod_height_ds_lm <- dsCallGLM(formula = "PM_BMI_CONTINUOUS ~ GENDER2 + LAB_TRIG",
#'                           data = "tab1",
#'                           family = "gaussian",
#'                           datasources = conns)
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
  if (dep_var %in% tabVariables) dep_var_mod <- paste0(data,"$",dep_var)
  # if dependent variable is factor make it numeric # cf. get_xy dsCallGLM dsResiduals
  if (any(ds.class(dep_var_mod, datasources)=="factor")) {
    dep_var_mod2 <- gsub("\\$", "_", dep_var_mod)
    dsBaseClient::ds.asNumeric(dep_var_mod,dep_var_mod2,datasources)
    formula <- paste(dep_var_mod2 ,"~", tail(unlist(strsplit(formula,split="~")),1) )
  }

  # call ds.glm
  rs <- ds.glm(formula, data, family, offset, weights, checks, maxit, CI, viewIter,
               viewVarCov, viewCor, datasources)
  # also return data string
  rs$dataString <- data
  return(rs)
  # missing return values: $call; ($contrasts); $df.null; $na.action; $null.deviance; $aic
  # misnamed return values: $df.residual ($df); $deviance ($dev)
}

#' Compute residuals for models returned by the dsCallGLM function on the server side.
#' This function is basically a helper function for ds GLM.
#' @param mod A list object as returned by dsCallGLM.
#' @param datasources A list of \link[DSI]{DSConnection-class} objects.
#' @return NULL
#' @examples
#' dsResiduals(mod_height_ds_lm, datasources=conns)
dsResiduals <- function(mod, datasources) {
  # get model estimates
  est <- mod$coefficients[,"Estimate"]
  # get unique variable names for table (needed later)
  tabVariables <- dsAnalysisTools::dsUniqueVarnames(mod$dataString, datasources=datasources)
  # rename labels for independent variables for further use ("1" for Intercept; add table name if missing)
  names(est)[1] <- "1"
  addTabName <- c(FALSE, names(tail(est,-1)) %in% tabVariables )
  names(est)[addTabName] <- paste0(mod$dataString,"$",names(est)[addTabName])
  # get and adapt name for dependent variable
  dep_var <- rownames(attr(terms(formula(mod$formula)),"factors"))[1]
  if (dep_var %in% tabVariables) dep_var <- paste0(mod$dataString,"$",dep_var)
  # build formula for residuals
  estBRACKETS <- paste0("(",est,")")
  estPRODUCTS <- paste0(estBRACKETS,"*",names(est))

  # if dependent variable is factor make it numeric # cf. get_xy dsCallGLM dsResiduals
  dep_var_mod <- dep_var
  if (any(ds.class(dep_var, datasources)=="factor")) {
    dep_var_mod <- gsub("\\$", "_", dep_var)
    dsBaseClient::ds.asNumeric(dep_var,dep_var_mod,datasources)
  }

  formula <- paste0(dep_var_mod,"-",paste0(estPRODUCTS,collapse="-"))
  message("COMPUTE RESIDUALS WITH FOLLOWING FORMULA:")
  message(formula)


  # compute residuals
  ds.make(toAssign=formula, newobj = "mod_residuals", datasources = datasources)
  formula2 <- paste0(paste0(estPRODUCTS,collapse="+"))
  message(formula2)
  ds.make(toAssign=formula2, newobj = "mod_predicted", datasources = datasources)
}

#' Create for each level of a (factor) variable a  boolean factor on the server
#' side in DataShield.
#' @param x A charsacter string referring to a table column in DataShield.
#' @param datasources A list of \link[DSI]{DSConnection-class} objects.
#' @return NULL
#' @examples
#' #createFactorVars(vname, datasources=cur_conns)
createFactorVars <- function(x, datasources=DSI::datashield.connections_find()) {
  varname <- tail(unlist(strsplit(x,split="\\$")),1) # without table name & $
  # check if factor
  isFactor <- all(dsBaseClient::ds.class(x, datasources = datasources) == "factor")
  if (!isFactor) {
    tempname <- gsub("\\$", "_", x)
    ds.asFactor(input.var.name = x,
                newobj.name = tempname,
                datasources = datasources) # datasources = conns) #!!!!!!!!!!!!!!!conns??
    x <- tempname
  }
  fLevels <- dsUniqueLevels(x, datasources = datasources)
  lapply(1:length(fLevels), function(i) {
    ds.Boole(V1 = x,
             V2 = fLevels[i],
             Boolean.operator = "==",
             numeric.output = TRUE,
             newobj = paste0(varname,fLevels[i]),
             datasources = datasources)
  }  )
}

#' Perform generalized linear model regression and return model object
#' including non-disclosive k-nearest neighbor estimates for residuals.
#' @param formula An object of class formula describing the model to be fitted.
#' @param data A character string specifying the name of an (optional) data frame that contains all of the variables in the GLM formula.
#' @param family	A character string. Identifies the error distribution function to use in the model ("gaussian", "binomial" or "poisson").
#' @param offset A character string specifying the name of a variable to be used as an offset.
#' @param weights	A character string specifying the name of a variable containing prior regression weights for the fitting process.
#' @param checks	A logical. If TRUE ds.glm checks the structural integrity of the model. Default FALSE. For more information see Details.
#' @param maxit	A numeric scalar denoting the maximum number of iterations that are permitted before non-convergence is declared.
#' @param CI	A numeric value specifying the confidence interval (default 0.95).
#' @param viewIter A logical. If TRUE the results of the intermediate iterations are printed. If FALSE only final results are shown. Default FALSE.
#' @param viewVarCov A logical. Should be TRUE, otherwise dsResiduals will fail.
#' @param viewCor A logical. If TRUE the correlation matrix of parameter estimates is returned. Default FALSE.
#' @param datasources A list of \link[DSI]{DSConnection-class} objects.
#' @return A list with parameters similar to glm objects.
#' @examples
#' foo <- dsGLM(formula = "PM_BMI_CONTINUOUS ~ GENDER2 + LAB_TRIG",
#'              data = "tab1",
#'              family = "gaussian",
#'              datasources = conns)
#' foo <- dsGLM(formula = "PM_BMI_CONTINUOUS ~ LAB_HDL + LAB_TRIG",
#'              data = "tab1", family = "gaussian", datasources = conns)
#' hist(x = foo$residual, type = "combine", breaks = 18, datasources = conns)
#' ds.histogram(x = "mod_residuals",
#'             type = "combine",
#'             num.breaks = 19,
#'             datasources = conns)
#' ds.scatterPlot(x="mod_predicted", y="mod_residuals", type="combine", datasources=conns)
#' qqnorm(foo$y)
#' qqline(foo$y)
#' stats:::plot.lm(foo)
dsGLM <- function(formula = NULL, data = NULL, family = c("gaussian", "binomial", "poisson"), offset = NULL,
                  weights = NULL, maxit = 20, CI = 0.95, viewIter = FALSE,
                  viewCor = FALSE, datasources = NULL, add_tab_to_depvar = T) {  #viewVarCov = TRUE,
  # match arguments
  family <- match.arg(family)
  # fit glm
  mod_glm <- dsCallGLM(formula = formula, data = data, family = family,
                       offset=offset, weights=weights, checks=F, maxit=20,
                       CI=0.95, viewIter=viewIter, viewVarCov=TRUE,
                       viewCor=viewCor, datasources = datasources)
  # compute residuals (on server-side)
  dsResiduals(mod_glm, datasources=datasources)
  # get non-disclosive predictions, y and residuals
  if (add_tab_to_depvar) dep_var <- paste0(mod_glm$dataString,"$",rownames(attr(terms(formula(mod_glm$formula)),"factors"))[1]) else {
    dep_var <- rownames(attr(terms(formula(mod_glm$formula)),"factors"))[1]
  }
  foo <- dsAnalysisTools::get_xy(dep_var,"mod_residuals", datasources=datasources)
  mod_glm$y <- foo$x
  #mod_glm$residual <- foo$y
  mod_glm$residuals <- foo$y
  mod_glm$fitted.values <- mod_glm$y - mod_glm$residuals
  mod_glm$df.residual <- mod_glm$df
  mod_glm$deviance <- mod_glm$dev
  mod_glm$rank <- as.numeric(Matrix::rankMatrix(mod_glm$VarCovMatrix))
  mod_glm$weights <- mod_glm$y*0+1
  mod_glm$prior.weights <- mod_glm$y*0+1 # necessary for stats:::weights.glm^
  #mod_glm$qr # would be necessary for influence function... however this is not straightforward to implement here in a non-disclosive way and probably slow
  mod_glm$call <- match.call()

  return(mod_glm)
}


#' Apply a function to subsets of a table in DataShield for each different outcome of a categorical grouping variable
#' @param X A character string specifying a table which is available as a DataShield object.
#' @param G A character string specifying the group variable in X according to which the data are split before applying function FUN.
#' @param FUN	A function which creates a data.frame. It will be applied to groupwise subsets of the table X.
#' @param lazy A boolean. If TRUE then objects for groupwise tables are only created if they don't exist.
#' @param datasources  A list of \link[DSI]{DSConnection-class} objects.
#' @param ...	A additional parameters to be passed to FUN.
#' @return A data.frame containing the results of FUN, applied to subgroups G of table X.
#' @examples
#' dsGapply(tab,"GENDER",dsAnalysisTools::dsSummary, datasources=conns[1])
dsGapply <- function(x, G, FUN, lazy=FALSE, datasources=datashield.connections_find(), ...) {
  # if no group given, just apply FUN to x
  if (length(G)==0) return( FUN(x, datasources=datasources, ...) )

  # initialize subsetTableNames with original tab name...
  subsetTableNames <- x
  groups <- data.frame(dummy=1)
  # for each group variable...
  for (i in seq_along(G)){
    # ...create subsets of the table x for each distinct group value
    subsetTableNames <- as.vector(sapply(subsetTableNames,
                                         function(x) if(is.na(x)) NA else dsSubsetLevels(G[i], x, lazy=lazy, datasources=datasources)$`created subsets`)) # if could be removed, bec no NA in loop
    # -> table names are composed by tab, G and level, e.g.: "tab1.GENDER.0" "tab1.GENDER.1"

    # remember groupwise levels in that same order and save them in data.frame groups
    levelsGi <- dsUniqueLevels(G[i], x, datasources=datasources)
    ## rep(levelsGi, length.out=length(subsetTableNames)) [!is.na(subsetTableNames)]
    groups <- groups[rep(seq_len(nrow(groups)),each=length(levelsGi)), ,drop=F]
    groups[[G[i]]] <- rep(levelsGi, length.out=length(subsetTableNames))
    # remove where NA
    groups <- groups[!is.na(subsetTableNames),,drop=F]
    subsetTableNames <- subsetTableNames[!is.na(subsetTableNames)]
  }
  #whereNA <- is.na(subsetTableNames)
  #subsetTableNames <- unlist(subsetTableNames[!whereNA])
  summary_g_list <- lapply(subsetTableNames, FUN, datasources=datasources, ...)
  names(summary_g_list) <- subsetTableNames
  # remove where NA
  #summary_g_list <- summary_g_list[!whereNA]
  rownames(groups) <- NULL
  summary_g_df <- do.call(rbind, Map(cbind, split(groups[,-1,drop=F],seq(nrow(groups))), summary_g_list,     tabname = names(summary_g_list) ) )
  rownames(summary_g_df) <- NULL

  summary_g_df
}

#' @title Returns the metadata for a specified variable
#' @description This function returns metadata for a specified variable -
#' without unnecessary checks and outputs (as in the slow ds.metadata() from
#' dsBaseClient). Due to the restrictions in dsBase::metadataDS only the
#' following attributes are returned: 'names', 'spec', 'class', 'label',
#' 'opal.value_type', 'opal.entity_type', 'opal.repeatable', 'opal.index',
#' 'opal.nature'
#' The variable does not need to be specified in each server to obtain a result.
#' @details The function returns the metadata, obtained from attributes function.
#' @param x a string character, specifying the variable
#' @return a data.frame containing the metadata.
#' @export
#' @examples
#' \dontrun{
#'
#'   # connecting to the Opal servers
#'
#'   require('DSI')
#'   require('DSOpal')
#'   require('dsBaseClient')
#'
#'   builder <- DSI::newDSLoginBuilder()
#'   builder$append(server = "study1",
#'                  url = "http://192.168.56.100:8080/",
#'                  user = "administrator", password = "datashield_test&",
#'                  table = "CNSIM.CNSIM1", driver = "OpalDriver")
#'   builder$append(server = "study2",
#'                  url = "http://192.168.56.100:8080/",
#'                  user = "administrator", password = "datashield_test&",
#'                  table = "CNSIM.CNSIM2", driver = "OpalDriver")
#'   builder$append(server = "study3",
#'                  url = "http://192.168.56.100:8080/",
#'                  user = "administrator", password = "datashield_test&",
#'                  table = "CNSIM.CNSIM3", driver = "OpalDriver")
#'   logindata <- builder$build()
#'
#'   connections <- DSI::datashield.login(logins = logindata, assign = TRUE, symbol = "D")
#'
#'   # Get the metadata associated with table 'D'
#'   dsBetter.metadata(x = c('D$LAB_TSC','D$LAB_TRIG','D$LAB_HDL'), datasources = connections)
#'
#'   # clear the Datashield R sessions and logout
#'   DSI::datashield.logout(connections)
#' }
#'
dsBetter.metadata <- function (x, datasources=DSI::datashield.connections_find(), silent=FALSE) {
  rslist <- list()
  options(datashield.progress=F)
  tryCatch(
    DSI::datashield.aggregate(
      conns=datasources,
      expr=call("metadataDS", x),
      success=function(server, rs) rslist[[server]] <<- rs,
      error=if (silent) function(server, e) NULL else function(server, e) print(paste0(server,": ", e)),
    ),
    error=function(e) NULL
  )
  options(datashield.progress=T)

  #rs <- data.table::rbindlist(rslist, fill=TRUE)
  #^^ does not handle the case when vector values like c("factor","ordered") are present...
  # vv therefore do it more complicated
  servernames <- names(rslist)
  simplify=T
  if (simplify) {
   # allnames <- unique(names(unlist(unname(rslist),recursive=F))) # unname: if several servers (server1, server2,..) then names will be different (server1.name, server2)
    allnames <- unique(c(names(unlist(unname(rslist),recursive=F)),"class")) # if only numeric vars, then "class" will be missing
    missnames <- lapply(rslist, function(x) setdiff(allnames, names(x)) )
    #lapply(missnames, function(x) for (i in x) rslist)
    for (i in 1:length(rslist)) {
      for (j in missnames[[i]]) {
        #print(paste(i,j))
        if (!is.null(j)) rslist[[i]][[j]] <- NA
      }
    }
    rs <- do.call(rbind,rslist)
    rs <- cbind(server=servernames,rs, obj.name=x)
    rownames(rs) <- gsub("^[^$]+[$]","",x)
  }
  #rs$server <- names(rslist)
  #rs$obj.name <- x
  rs
}

#'
#' @title Gets metadata for multiple variables of a table
#' @description This function gets the metadata for multiple variables of a given object on the server
#' @param x a character string specifying the name of the table object.
#' @param datasources a list of \code{\link{dsBaseClient::DSConnection-class}}
#' objects obtained after login. If the \code{dsBaseClient::datasources} argument is not specified
#' the default set of connections will be used: see \code{\link{dsBaseClient::datashield.connections_default}}.
#' @param simplify a boolean. If TRUE (default) the result is simplified to a data.frame. Otherwise a list is returned.
#' @return \code{ds.metadata} returns to the metadata of the associated table held at the server.
#' @export
#' @examples
#' \dontrun{
#'
#'   # connecting to the Opal servers
#'
#'   require('DSI')
#'   require('DSOpal')
#'   require('dsBaseClient')
#'
#'   builder <- DSI::newDSLoginBuilder()
#'   builder$append(server = "study1",
#'                  url = "http://192.168.56.100:8080/",
#'                  user = "administrator", password = "datashield_test&",
#'                  table = "CNSIM.CNSIM1", driver = "OpalDriver")
#'   builder$append(server = "study2",
#'                  url = "http://192.168.56.100:8080/",
#'                  user = "administrator", password = "datashield_test&",
#'                  table = "CNSIM.CNSIM2", driver = "OpalDriver")
#'   builder$append(server = "study3",
#'                  url = "http://192.168.56.100:8080/",
#'                  user = "administrator", password = "datashield_test&",
#'                  table = "CNSIM.CNSIM3", driver = "OpalDriver")
#'   logindata <- builder$build()
#'
#'   connections <- DSI::datashield.login(logins = logindata, assign = TRUE, symbol = "D")
#'
#'   # Get the metadata associated with table 'D'
#'   ds.meta(x = c('D$LAB_TSC','D$LAB_TRIG','D$LAB_HDL'), datasources = connections)
#'
#'   # clear the Datashield R sessions and logout
#'   DSI::datashield.logout(connections)
#' }
#'
ds.meta <- function(x=NULL, datasources=NULL, simplify=TRUE) {
  rs <- lapply(x, function(x) ds.metadata(x, datasources)[[1]] ) # [[1]]: usually the properties should be the same for all servers. we just use the first one.
  if (simplify) {
    allnames <- unique(names(unlist(rs,recursive=F)))
    missnames <- lapply(rs, function(x) setdiff(allnames, names(x)) )
    #lapply(missnames, function(x) for (i in x) rs)
    for (i in 1:length(rs)) {
      for (j in missnames[[i]]) {
        #print(paste(i,j))
        if (!is.null(j)) rs[[i]][[j]] <- NA
      }
    }
    rs <- do.call(rbind,rs)
    rownames(rs) <- gsub("^[^$]+[$]","",x)
  }
  rs
}

#' @title Gets metadata for multiple variables of a table
#' @description This function gets the metadata for multiple variables of a given object on the server
#' @param x a character string specifying the name of the table object.
#' @param datasources a list of \code{\link{dsBaseClient::DSConnection-class}}
#' objects obtained after login. If the \code{dsBaseClient::datasources} argument is not specified
#' the default set of connections will be used: see \code{\link{dsBaseClient::datashield.connections_default}}.
#' @return \code{ds.metadata} returns to the metadata of the associated table held at the server.
#' @export
ds.meta2 <- function(x=NULL, datasources=NULL) {
  rs <- lapply(x, function(x) dsBetter.metadata(x, datasources) )
  names(rs) <- x
  #data.table::rbindlist(rs, fill=TRUE)
  if (T) { #always
    rs <- lapply(rs, as.data.frame)
    rs <- as.data.frame(data.table::rbindlist(rs, fill=TRUE))
    #allnames <- unique(names(unlist(unname(rs),recursive=F))) # unname: if several servers (server1, server2,..) then names will be different (server1.name, server2)
    #missnames <- lapply(rs, function(x) setdiff(allnames, names(x)) )
    ##lapply(missnames, function(x) for (i in x) rs)
    #for (i in 1:length(rs)) {
    #  for (j in missnames[[i]]) {
    #    #print(paste(i,j))
    #    if (!is.null(j)) rs[[i]][[j]] <- NA
    #  }
    #}
   # allnames <- lapply(rs, function(x) colnames(x)) %>% unlist() %>% unique()
   # missnames <- lapply(rs, function(x) setdiff(allnames, colnames(x)) )
   # for (i in seq_len(length(rs))) for (j in missnames[[i]]) if (!is.null(j)) rs[[i]][[j]] <- NA
   # rs <- do.call(rbind,rs) # Fehler in (function (..., deparse.level = 1)  :
                            # Anzahl der Spalten der Matrizen muss übereinstimmen (siehe Argument 302)
                            # <- happens because "class" exists only for categorical variables??!
    #rs <- cbind(server=servernames,rs, obj.name=x)
    rownames(rs) <- gsub("^[^$]+[$]","",x)
    rs
  }
}


#' Get variable type as string ("numeric" or "categorical") for all variables of a table in unified form (for use in function all_summaries)
#' @param tab A character string. Name of the DataShield table object.
#' @param datasources  A list of \link[DSI]{DSConnection-class} objects.
#' @return A data.frame containing the data type for each variable in table tab.
#' @examples
#' #  get_type("tab1",DSI::datashield.connections_find())
get_type <- function(tab, datasources=DSI::datashield.connections_find()[1]) {
  # for each variable get type
  vars0 <- dsAnalysisTools::dsIsNumeric(tab, datasources)
  vars <- vars0
  vars[vars0==T] <- "numeric"
  vars[vars0==F] <- "categorical"

  rs <- data.frame(variable=names(vars), type=vars) %>%
    tidyr::pivot_longer(type,names_to = "feature", values_to = "value")

  rs
}
#
