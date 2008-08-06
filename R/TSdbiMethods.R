.onLoad <- function(library, section) {
   require("methods")
   require("TSdbi")
   require("RSQLite")
   }

setClass("TSSQLiteConnection", contains=c("SQLiteConnection","TSdbOptions")) 
setMethod("print", "TSSQLiteConnection", function(x, ...) {
    cat("database: ", x@dbname) 
    if (x@vintage) cat( " Has vintages." )
    if (x@panel) cat( " Has panels." )
    cat("\n")
    print(class(x)) 
    invisible(x)
    })

setMethod("TSconnect",   signature(drv="SQLiteDriver", dbname="character"),
   definition=function(drv, dbname) {
        con <- dbConnect(drv, dbname=dbname)
	if(!dbExistsTable(con, "Meta"))
	  stop("The database does not appear to be a TS database,")
	panel   <- dbExistsTable(con, "Panels")
	vintage <- dbExistsTable(con, "Vintages")
	new("TSSQLiteConnection" , con, dbname=dbname,
	                          vintage=vintage, panel=panel) 
	})

setMethod("TSput", signature(x="ANY", serIDs="character", con="SQLiteConnection"),
   definition= function(x, serIDs=seriesNames(x), con=options()$TSconnection, ...) 
    TSdbi:::TSputSQL(x, serIDs, con, ...) )

setMethod("TSget", signature(serIDs="character", con="SQLiteConnection"),
   definition= function(serIDs, con=options()$TSconnection, ...)
    TSdbi:::TSgetSQL(serIDs, con, ...) )

setMethod("TSdates", signature(serIDs="character", con="SQLiteConnection"),
   definition= function(serIDs, con=options()$TSconnection, ...)
     TSdbi:::TSdatesSQL(serIDs, con, ...) )


setMethod("TSdoc",   signature(x="character", con="SQLiteConnection"),
   definition= function(x, con=options()$TSconnection, ...)
        TSdbi:::TSdocSQL(x=x, con=con, ...) )

setMethod("TSdescription",   signature(x="character", con="SQLiteConnection"),
   definition= function(x, con=options()$TSconnection, ...)
        TSdbi:::TSdescriptionSQL(x=x, con=con, ...) )

setMethod("TSdelete", signature(serIDs="character", con="SQLiteConnection"),
   definition= function(serIDs, con=options()$TSconnection, ...)
       TSdbi:::TSdeleteSQL(serIDs=serIDs, con=con, ...) )
