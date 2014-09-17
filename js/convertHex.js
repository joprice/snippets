
function parseHexString(str) { 
  var result = [];
  while (str.length >= 8) { 
    result.push(parseInt(str.substring(0, 8), 16));
    str = str.substring(8, str.length);
  }
  return result;
}

function createHexString(arr) {
  var result = "", z;
  for (var i = 0; i < arr.length; i++) {
    var str = arr[i].toString(16);
    z = 8 - str.length + 1;
    str = Array(z).join("0") + str;
    result += str;
  }
  return result;
}

