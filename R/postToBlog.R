postToBlog <- function(file,
                     title='',
                     categories=c(),
                     tags=c(),
                     postID=NULL,
                     toc=TRUE,
                     type=c('new','edit'),
                     syntax.highlighter='Syntax Highlighter Evolved',
                     publish=FALSE){

  library(RWordPress)
  library(knitr)
  library(markdown)
  library(stringr)
  library(tools)

  # Determine what kind of action to take
  type <- match.arg(type)

  ## Determine file name of .html output file
  file.name <- basename(file)
  dir.name <- dirname(file)

  file.extension <- file_ext(file.name)

  # Should this be a stop()?
  if(file.extension!='Rmd'){
    warning("File extension is not .Rmd.  Check input file to make sure it is correct format.")
  }

  file.root <- str_split(basename(file),pattern = paste('.',file.extension,sep=''))[[1]][1]
  html.file <- paste(file.root,'.html',sep='')
  current.director <- getwd()
  setwd(dir.name)

  cat('Directory:',dir.name,'\n')
  cat('RMarkdown file:', file.name,'\n')
  cat('HTML file:', html.file,'\n')
  cat('Blog URL:', getOption('WordpressURL'),'\n')
  cat('Action Type:', type,'\n')
  cat('Syntax Highlighter:',syntax.highlighter,'\n')

  # in .Rprofile
  # options(WordpressLogin=('username'='password'),
  #         WordpressURL='http://www.yourblog.com/xmlrpc.php')

  # Table of Contents
  options(markdown.HTML.options =  c('use_xhtml','base64_images','mathjax','highlight_code',"toc","hard_wrap"))
  # options(markdown.HTML.options =  c(markdownHTMLOptions(default = T),"number_sections"))

  # Knit an HTML file
  knit2html(file.name)

  # If new category is used then create that category
  # Does not work
  if(length(categories)>=1 && FALSE){
    current.categories <- names(RWordPress::categories())
    new.categories <- !(categories %in% current.categories)
    sapply(categories[new.categories],function(x){
      newCategory(x)
    })
  }

  ## NEW POST
  if(type=='new'){
    newPost(
      list(
        description=knit2wordpress(html.file,syntax.highlighter=syntax.highlighter),
        title=title,
        categories=categories,
        mt_keywords=tags
      ),
      publish=publish)
    ## EDIT POST
  } else if(type=='edit') {
    posts <- RWordPress::getPosts()
    if(is.null(postID)){

      # Find Post ID
      postID <- posts[posts$title==title,'postid']
      # Check that there is a post to edit
      if(length(postID)==1 && !is.null(postID) && !is.na(postID)){
        print(paste('Editing post [',postID,']: ',title,'.',sep=''))
        editPost(postid = postID,
                 content=list(description=knit2wordpress(html.file,syntax.highlighter=syntax.highlighter),
                              title=title,
                              categories=categories,
                              mt_keyworkds=tags),
                 publish = publish
                  )
      }
      ## Iff there is exactly 1 post that matches ID given
    } else if(sum(posts$postid==postID) == 1){
      print(paste('Editing post [',postID,']: ',title,'.',sep=''))
      editPost(postid = postID,
               content=list(description=knit2wordpress(html.file,syntax.highlighter=syntax.highlighter),
                            title=title,
                            categories=categories,
                            mt_keyworkds=tags),
               publish = publish
      )
    }
  }
  setwd(current.director)
}

postToBlog.dev <- function(file,
                       title='',
                       categories=c(),
                       tags=c(),
                       postID=NULL,
                       toc=TRUE,
                       type=c('new','edit','createOrReplace'),
                       syntax.highlighter='Syntax Highlighter Evolved',
                       publish=FALSE){

  library(RWordPress)
  library(knitr)
  library(markdown)
  library(stringr)
  library(tools)

  # Determine what kind of action to take
  type <- match.arg(type)

  ## Determine file name of .html output file
  file.name <- basename(file)
  dir.name <- dirname(file)

  file.extension <- file_ext(file.name)

  # Should this be a stop()?
  if(file.extension!='Rmd'){
    warning("File extension is not .Rmd.  Check input file to make sure it is correct format.")
  }

  file.root <- str_split(basename(file),pattern = paste('.',file.extension,sep=''))[[1]][1]
  html.file <- paste(file.root,'.html',sep='')
  current.director <- getwd()
  setwd(dir.name)

  cat('Directory:',dir.name,'\n')
  cat('RMarkdown file:', file.name,'\n')
  cat('HTML file:', html.file,'\n')
  cat('Blog URL:', getOption('WordpressURL'),'\n')
  cat('Action Type:', type,'\n')
  cat('Syntax Highlighter:',syntax.highlighter,'\n')

  # in .Rprofile
  # options(WordpressLogin=('username'='password'),
  #         WordpressURL='http://www.yourblog.com/xmlrpc.php')

  # Table of Contents
  options(markdown.HTML.options =  c('use_xhtml','base64_images','mathjax','highlight_code',"toc","hard_wrap"))
  # options(markdown.HTML.options =  c(markdownHTMLOptions(default = T),"number_sections"))

  # Knit an HTML file
  knit2html(file.name)

  # Get Title From .Rmd
  if(is.null(title)){
    lines <- readLines(file)
    title.lines <- lines[grepl('title:',lines)]
    if(length(title.lines) == 1){
      title <- str_split(title.lines,'title:')[[1]][2]
      title <- str_replace_all(title,'\"','')
      title <- str_trim(title,side = 'left')
    } else if(length(title.lins) > 1){
      stop('Title Ambiguous.')
    }
  }


  # Get Post ID
  posts <- RWordPress::getPosts()
  # Find Post ID
  postID <- posts[posts$title==title,'postid']


  # New/Edit -- need title, post-id

  ## NEW POST
  if(type=='new'){
    newPost(
      list(
        description=knit2wordpress(html.file,syntax.highlighter=syntax.highlighter),
        title=title,
        categories=categories,
        mt_keywords=tags
      ),
      publish=publish)
    ## EDIT POST
  } else if(type=='edit') {
    posts <- RWordPress::getPosts()
    if(is.null(postID)){

      # Find Post ID
      postID <- posts[posts$title==title,'postid']
      # Check that there is a post to edit
      if(length(postID)==1 && !is.null(postID) && !is.na(postID)){
        print(paste('Editing post [',postID,']: ',title,'.',sep=''))
        editPost(postid = postID,
                 content=list(description=knit2wordpress(html.file,syntax.highlighter=syntax.highlighter),
                              title=title,
                              categories=categories,
                              mt_keyworkds=tags),
                 publish = publish
        )
      }
      ## Iff there is exactly 1 post that matches ID given
    } else if(sum(posts$postid==postID) == 1){
      print(paste('Editing post [',postID,']: ',title,'.',sep=''))
      editPost(postid = postID,
               content=list(description=knit2wordpress(html.file,syntax.highlighter=syntax.highlighter),
                            title=title,
                            categories=categories,
                            mt_keyworkds=tags),
               publish = publish
      )
    }
  }
  setwd(current.director)
}

