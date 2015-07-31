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

  # Determine what kind of action to take
  type <- match.arg(type)

  ## Determine file name of .html output file
  file.pieces <- str_split(file,'\\.')[[1]]
  num.pieces <- length(file.pieces)
  # Should this be a stop()?
  if(file.pieces[num.pieces]!='Rmd'){
    warning("File extension is not .Rmd.  Check input file to make sure it is correct format.")
  }
  file.root <- paste(file.pieces[-num.pieces],collapse='.')
  html.file <- paste(file.root,'.html',sep='')

  cat('RMarkdown file:', file,'\n')
  cat('HTML file:', html.file,'\n')
  cat('Blog URL:', getOption('WordpressURL'),'\n')
  cat('Action Type:', type,'\n')
  cat('Syntax Highlighter:',syntax.highlighter,'\n')

  # in .Rprofile
  # options(WordpressLogin=('username'='password'),
  #         WordpressURL='http://www.yourblog.com/xmlrpc.php')

  # Table of Contents
  options(markdown.HTML.options =  c(markdownHTMLOptions(default = T),"toc"))

  # Knit an HTML file
  knit2html(file)

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
}
