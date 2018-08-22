
library(rgithubS)
## for TM analysis: from https://github.com/lborke/TManalyzer_dev
library(TManalyzer)


setwd("c:/r/github")

source("BQSearch/BitQuerySearchExport.R")

source("BQSearch/gh_login.R")
# "gh_login.R" führt nur diesen Code aus, Authentifizierungs-Parameter müssen dort zu Beginn manuell eingefügt werden
# ctx = interactive.login("XXX", "YYY", scopes=c("repo"))

# ctx$user$login
# ctx$user$id


## JSON query examples

# npm packages
npm_search = 'name version description author node filename:"package.json" path:"/"'

# search examples
spec_search_term = ""

spec_search_term = "minify javascript"
spec_search_term = "task runner javascript"
spec_search_term = "javascript library"
spec_search_term = "javascript visualization"
spec_search_term = "mongodb"
spec_search_term = "Jonathan Ong"
spec_search_term = "tensorflow"
spec_search_term = "google cloud"

( search_query = paste(spec_search_term, npm_search) )


# packagist - PHP
php_search = 'name description authors require php filename:"composer.json" path:"/"'

( search_query = php_search )

spec_search_term = ""
spec_search_term = "laravel"
spec_search_term = "api"
spec_search_term = "aws sdk"

( search_query = paste(spec_search_term, php_search) )



### I: Search API layer: 

## Full search

sr <- search.code.full(search_query, delay_l = 1)

sr <- search.code.full(search_query, delay_l = 1, print_stats = F)

sr <- search.code.full(search_query, delay_l = 1, max_items = 100)

sr <- search.code.full(search_query, delay_l = 1, max_items = 250)

# "preview" with 100 items, equivalent to simple "search.code"
sr <- search.code.full(search_query, delay_l = 1, max_items = 100, print_stats = F)

# default: gets maximally 1000 items with printing statistics
system.time( sr <- search.code.full(search_query, delay_l = 1) )


sort(table(sr$gh_login), decreasing = T)[1:10]
sort(table(sr$repo_name), decreasing = T)[1:10]
boxplot(sr$scores)



### II: Parser layer - V3 version

# json (npm/PHP)
# system.time( parser_res <- json.search.parser(sr, error_to_results = FALSE) )

# for further filtering of 'sr$full_search'
system.time( parser_res <- json.search.parser(sr, error_to_results = TRUE) )


# some parser evaluation
parser_res$type
parser_res$error_count

parser_res$ok_vec
parser_res$path_vec
parser_res$score_vec

# error evaluation if "error_to_results = TRUE"
parser_res$path_vec[!parser_res$ok_vec]
parser_res$path_vec[parser_res$ok_vec]
# good list
parser_res$parsed_list[which(parser_res$ok_vec)]
# bad list
parser_res$parsed_list[which(!parser_res$ok_vec)]

table(parser_res$ok_vec)



### III: Metadata extraction and analysis layer

# Metainfos = parser_res$parsed_list

# take only items without parser errors, if "error_to_results = TRUE"
Metainfos = parser_res$parsed_list[which(parser_res$ok_vec)]

length(Metainfos)


### some Meta analysis (optional)

metadata.analysis(Metainfos)

# npm
metadata.analysis(Metainfos, npm_dfield_list)


# php
metadata.analysis(Metainfos, php_dfield_list)
metadata.analysis(Metainfos, php_dfield_list, 230)


## some Meta processing/projection

# npm
meta_df_p = meta_dfield.projection(Metainfos, npm_dfield_list, c("n", "dep"))
meta_df_p = meta_dfield.projection(Metainfos, npm_dfield_list, c("n", "v", "l", "m"))


## Generalization
# add some parser/gh infos, works with any metadata format
meta_df_p$gh_path	= parser_res$path_vec[parser_res$ok_vec]
meta_df_p$score		= round(parser_res$score_vec[parser_res$ok_vec], 2)

( meta_df_p_frame = as.data.frame(meta_df_p) )

head(meta_df_p_frame)

# take k random rows from the data.frame
k = 5
meta_df_p_frame[sample(nrow(meta_df_p_frame), k),]



### IV: to TManalyzer

# smart npm
meta_weight = c(d = 5, k = 3, a = 3, dep = 2, n = 1)

# smart php
meta_weight = c(a = 9, d = 7, n = 1, r = 2, k = 1)


# from parsed Metainfo
( doc_names	= sapply( Metainfos, function(meta){ meta.getDField(meta, npm_dfield_list, "n") } ) )


( doc_names	= sapply( Metainfos, function(meta){ meta.getDField(meta, php_dfield_list, "n") } ) )


## real extraction
# npm
t_vec = meta.list.extract(Metainfos, doc_names, npm_dfield_list, meta_weight)

# php
t_vec = meta.list.extract(Metainfos, doc_names, php_dfield_list, meta_weight)


# check
t_vec$t_vec
t_vec$doc_names



# add more stopwords by hand
more_stopwords = c("instal", "you", "your", "sudo", "will", "librari", "how", "run", "ansibl", "user", "our", "width", "each", "file",
					"youremailher", "emailexamplecom")

# system.time( A_list <- tm.create.models(t_vec, models = c("lsa")) )
# system.time( A_list <- tm.create.models(t_vec, stopwords_select = "cran", models = c("tt", "lsa")) )
system.time( A_list <- tm.create.models(t_vec, stopwords_select = "cran", add_stopwords = more_stopwords, models = c("tt", "lsa")) )
# take LSA standard (50% sv weight)
A = A_list$lsa



### V: TM applications : output to BitQueryS

## general for all data types

d = dist(A)


k = 16
# k = 24
# k = 40
# k = 48
# k = 50
# k = 64

# graphics.off()

# k-means
system.time( cl_r <- D3Visu_ClusterTopic_kmeans(A, k, 5, mds_plot = FALSE, topic_print = TRUE, d_mat = d) )


# pam
# system.time( cl_r <- D3Visu_ClusterTopic_pam(A, k, 5, mds_plot = F, topic_print = TRUE) )
system.time( cl_r <- D3Visu_ClusterTopic_pam(A, k, 5, mds_plot = F, topic_print = TRUE, d_mat = d) )


## JSON output (npm/PHP)

( gh_org = sr$gh_login[which(parser_res$ok_vec)] )
( full_link = sapply( sr$full_search[which(parser_res$ok_vec)], function(item){ item$repository$html_url } ) )

c(length(gh_org), length(full_link)) == length(Metainfos)


## npm
class(npm_dfield_list) = "npm"
export_l = createD3_PreJSON_NPM_PHP(Metainfos, npm_dfield_list, gh_org, full_link, cl_r$cl_topic_vec, cl_r$cluster)


## PHP
class(php_dfield_list) = "php"
export_l = createD3_PreJSON_NPM_PHP(Metainfos, php_dfield_list, gh_org, full_link, cl_r$cl_topic_vec, cl_r$cluster)


d_out = list(nodes = export_l, links = list(), search_query = search_query)

# message_str = "javascript_visualization"
# message_str = "minify_javascript"
# message_str = "laravel_php"
# message_str = "google_cloud"
# message_str = "api"
# message_str = "aws_sdk"


( fname = paste("BQsearch/output/npm_github_", tolower(message_str), "_", k, "cl.json", sep="") )


( fname = paste("BQsearch/output/php_github_", tolower(message_str), "_", k, "cl.json", sep="") )


save_JSON(d_out, fname, showPretty = TRUE)


# UTF-8
save_JSON(d_out, fname, showPretty = TRUE, utf8 = TRUE)

