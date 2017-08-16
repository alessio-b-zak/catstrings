exports._splitAt = function (just) {
  return function (nothing) {
    return function (i) {
      return function (s) {
        return i >= 0 && i < s.length ?
               just({ before: s.slice(0, i), after: s.slice(i) }) :
               nothing;
      };
    };
  };
};

exports.mapWithIndex = function (f) {
  return function (arr) {
    var l = arr.length;
    var result = new Array(l);
    for (var i = 0; i < l; i++) {
      result[i] = f(i)(arr[i]);
    }
    return result;
  };
};

exports.inputValue = function(event) {
  return function () {
    return event.target.value;
  };
};