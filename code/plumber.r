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

# the_mod <- readr::read_rds(here::here('code', 'mod0.rds'))
the_mod <- readr::read_rds('mod0.rds')

library(workflows)

#* Predict Status from individual data
#* @param Income:numeric hundreds
#* @param Seniority singles
#* @param Records "yes"/"no"
#* @param Amount thousands
#* @param Job "fixed"/"freelance"/"others"/"partime"
#* @get /predict
function(Income, Seniority, Records, Amount, Job)
{
    the_data <- data.frame(
        Income=as.numeric(Income)
        , Seniority=as.numeric(Seniority)
        , Records=Records
        , Amount=as.numeric(Amount)
        , Job=Job
    )
    
    return(
        list(
            predicted_status=predict(the_mod, new_data=the_data, type='prob')
        )
    )
}

#* Predict with data in body, expects to get a row of JSON
#* @get /predict2
function(req)
{
    return(
        list(
            predicted_status=predict(the_mod, new_data=req$body, type='prob')
        )
    )
}
