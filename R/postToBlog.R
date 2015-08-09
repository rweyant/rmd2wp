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
# Determine title from .Rmd file
determine_title <- function(file){
  lines <- readLines(file)
  title.lines <- lines[grepl('title:',lines)]
  if(length(title.lines) == 1){
    title <- str_split(title.lines,'title:')[[1]][2]
    title <- str_replace_all(title,'\"','')
    title <- str_trim(title,side = 'left')
  } else if(length(title.lins) > 1){
    stop('Title Ambiguous.')
  }
  return(title)
}

# Check if post with same title exists.  Returns that post's ID.
checkPostExistence <- function(title){
  posts <- RWordPress::getPosts()
  # Find Post ID
  postID <- as.numeric(as.character(posts[posts$title==title,'postid']))
  if(length(postID)>1) stop('Ambiguous Post Title. Multiple posts exist with same title.')
  return(ifelse(length(postID)==1,postID,NA))
}

# in .Rprofile
# options(WordpressLogin=('username'='password'),
#         WordpressURL='http://www.yourblog.com/xmlrpc.php')
postToBlog.dev <- function(file,
                       title='',
                       categories=c(),
                       tags=c(),
                       postID=NA,
                       toc=TRUE,
                       type=c('new','edit','CreateOrReplace'),
                       syntax.highlighter='Syntax Highlighter Evolved',
                       publish=FALSE){

  # Load dependencies
  library(RWordPress)
  library(knitr)
  library(markdown)
  library(stringr)
  library(tools)

  # Determine what kind of action to take
  type <- match.arg(type)

  # Determine file name of .html output file
  file.name <- basename(file)
  dir.name <- dirname(file)
  file.extension <- file_ext(file.name)
  current.directory <- getwd()
  setwd(dir.name)

  # Check that file passed was .Rmd
  if(file.extension!='Rmd'){
    stop("File extension is not .Rmd.  Check input file to make sure it is correct format.")
  }

  # Determine what the HTML file will be called
  file.root <- str_split(file.name,pattern = paste('.',file.extension,sep=''))[[1]][1]
  html.file <- paste(file.root,'.html',sep='')

  cat('Directory:',dir.name,'\n')
  cat('RMarkdown file:', file.name,'\n')
  cat('HTML file:', html.file,'\n')
  cat('Blog URL:', getOption('WordpressURL'),'\n')
  cat('Action Type:', type,'\n')
  cat('Syntax Highlighter:',syntax.highlighter,'\n')

  # Set some markdown options
  options(markdown.HTML.options =  c('use_xhtml','base64_images','mathjax','highlight_code',"toc","hard_wrap"))

  knit2html(file.name) # Knit an HTML file

  # Get Title From .Rmd
  if(is.null(title)){
    determine_title(file.name)
  }

  # Get Post ID
  if(is.na(postID)) postID <- checkPostExistence(title)

  # Create New Post
  if(is.na(postID) || type=='new') {
    print('Creating New Post')
    newPost(content=list(description=knit2wordpress(html.file,syntax.highlighter=syntax.highlighter),
                         title=title,
                         categories=categories,
                         mt_keywords=tags),
            publish=publish)
  }  else if (!is.na(postID)) {
    # Edit Existing Post
    print('Editing Old Post')
    editPost(postid = postID,
             content=list(description=knit2wordpress(html.file,syntax.highlighter=syntax.highlighter),
                          title=title,
                          categories=categories,
                          mt_keyworkds=tags),
             publish = publish)
  } else{
    warning('Not Sure what to do.  No posts created or updated.')
  }

  # Go back to directory where function was called
  setwd(current.directory)
}

