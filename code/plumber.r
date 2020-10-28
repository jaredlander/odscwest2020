library(plumber)

#* Echo my name
#* @get /myname
function()
{
    return(list(name='Jared'))
}

#* Echo back the input
#* @param msg
#* @get /echo
function(msg='default')
{
    list(msg=paste0("The message is: '", msg, "'"))
}

#* Add the two numbers
#* @param x:numeric First Number
#* @param y:numeric Second Number
#* @get /add
function(x, y)
{
    return(as.numeric(x) + as.numeric(y))
}
