
var getGetREADME = function(onSuccess, onError)
{
  $.ajax(
    { url: '/getREADME'
    , success: onSuccess
    , error: onError
    , type: 'GET'
    });
}

var postInitialize = function(body, onSuccess, onError)
{
  $.ajax(
    { url: '/initialize'
    , success: onSuccess
    , data: JSON.stringify(body)
    , contentType: 'application/json'
    , error: onError
    , type: 'POST'
    });
}

var getGetGraph = function(onSuccess, onError)
{
  $.ajax(
    { url: '/getGraph'
    , success: onSuccess
    , error: onError
    , type: 'GET'
    });
}
