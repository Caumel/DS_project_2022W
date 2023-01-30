load("/home/caumel/Master/Project/Wildschwein_Eisenstadt/Wichtige Projektfiles/binary data/Accelerationdata_2017.RData", ex <- new.env())
ls.str(ex)


objs <- ls(envir = ex, all.names = TRUE)
for(obj in objs) {
    .x <- get(obj, envir =ex)
    message(sprintf('Saving %s as %s.csv', obj,obj) )
    write.csv(.x, file = paste0('/home/caumel/Master/Project/Wildschwein_Eisenstadt/Wichtige Projektfiles/binary data/','Accelerationdata_2017_',obj, '.csv'))
}



