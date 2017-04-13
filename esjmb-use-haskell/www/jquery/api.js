
var getGetGraphFollowers = function(onSuccess, onError)
{
  $.ajax(
    { url: '/getGraphFollowers'
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

var getGetGraphFriends = function(onSuccess, onError)
{
  $.ajax(
    { url: '/getGraphFriends'
    , success: onSuccess
    , error: onError
    , type: 'GET'
    });
}

var getGetDegreeDistribution = function(onSuccess, onError)
{
  $.ajax(
    { url: '/getDegreeDistribution'
    , success: onSuccess
    , error: onError
    , type: 'GET'
    });
}
