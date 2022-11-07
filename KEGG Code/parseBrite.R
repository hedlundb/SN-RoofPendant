britelist = data.table::fread('brite_list.txt',sep='\t', header=FALSE)
britekos = do.call('rbind', c(lapply(1:nrow(britelist),
    function(brn)
    {
        brfname=paste0('brite/', gsub('^br:','',britelist[brn,V1]),'.txt')
        print(brfname)
        if(!file.exists(brfname))
        {
            return(NULL)
        }
        
        rawbrite = data.table::fread(brfname, sep='',header=FALSE)
        if(nrow(rawbrite) == 0)
        {
            return(NULL)
        }
        
        if(rawbrite[1,V1] == '<?xml version="1.0" encoding="UTF-8"?>')
        {
            return(NULL)
        }
        
        rawbrite[,heading := cumsum(grepl('^\\+',V1))]

        splitbrite = split(rawbrite,by='heading')

        brite_metadata = lapply(splitbrite,
            function(br)
            {
                maincat = gsub('^\\+.*\t','', br[1,V1])
                if(nchar(maincat) == 2)
                {
                    maincat == ''
                }
                print(paste0("    ", maincat))
                br[,hierarchy := substr(V1, 1,1)]
                
                brh = br[hierarchy %in% LETTERS,]
                l = brh[,unique(hierarchy)]
                splitorder = cbind(brh[,hierarchy],data.table::as.data.table(do.call('cbind', lapply(l, function(x) brh[,cumsum(hierarchy == x)]))))
                colnames(splitorder) = c('h',l)
                
                splitgroups = splitorder[brh[,grepl('K[0-9]{5}',V1)],-1]
                splitgroups[duplicated(get(l[length(l)])), l[length(l)] := 0]
                
                splitgroups[get(l[length(l)]) == 0, l[length(l)] := NA]
                
                grouptext = data.table::as.data.table(do.call('cbind',lapply(colnames(splitgroups),
                    function(na) brh[hierarchy == na,][unlist(splitgroups[,..na]),gsub('^ +','',gsub('^.','',V1))])))
               
                colnames(grouptext) = l
                
                for(ii in 2:length(l))
                {
                    whichs = grouptext[,which(is.na(get(l[ii])))]
                    grouptext[whichs, (l[ii]) := get(l[ii-1])]
                    grouptext[whichs, (l[ii-1]) := '']
                }
                groupdata = grouptext[,data.table::as.data.table(do.call('rbind',strsplit(get(l[length(l)]), '  |; +')))]
                if(ncol(groupdata) < 3)
                {
                    return(NULL)
                } else {
                    groupdata = groupdata[,1:3]
                    groupdata[,V3 := gsub('\t.*','',V3)]
                }
 
                groupdata[,c('A','B'):= grouptext[,mget(l[1:2])]]
                
                return(groupdata)
            })

        britem = do.call('rbind',brite_metadata)
        if(!is.null(britem))
        {
            britekos = britem[grepl('^K[0-9]{5,5}$',V1),]
            colnames(britekos) = c('ko','name','fxn','hierarchy','category')
            britekos[,brname:=britelist[brn,V2]]
            britekos[,br:=britelist[brn,V1]]
            return(britekos)
        }
        return(NULL)
    }),fill=TRUE))
data.table::fwrite(unique(britekos), 'brite_parsed.tsv', sep='\t')