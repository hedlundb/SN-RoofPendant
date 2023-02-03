library('data.table')
library('magrittr')
library('stringi')

ko = fread('KO_RP_unstrat.txt', sep='\t', header=TRUE) %>% melt(id.var='function') %>% setkeyv('function')
mod = fread('module_data.tsv', header=FALSE)

modko = mod[,.(ko = stri_extract_all_regex(V3, 'K[0-9]{5,5}') %>% unlist()), by=.(module = gsub('md:','',V1))] %>% na.omit() %>% setkey(ko)

ko[modko, module := module]

agg = ko[,.(value = sum(value)), by = .(variable, module)][!is.na(module),]

wide = dcast(agg, module ~ variable, value.var = 'value')

fwrite(wide, 'MOD_RP_unstrat.txt', sep='\t')