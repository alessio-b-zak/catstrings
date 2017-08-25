exports._splitAt = function (just) {
  return function (nothing) {
    return function (i) {
      return function (s) {
        return i >= 0 && i <= s.length ?
               just({ before: s.slice(0, i), after: s.slice(i) }) :
               nothing;
      };
    };
  };
};

exports.inputValue = function(event) {
  return function () {
    return event.target.value;
  };
};

exports._eventKey = function (Just) {
  return function (Nothing) {
    return function (event) {
      return function () {
        if (event && event.charCode) return Just(event.charCode);
        else return Nothing;
      };
    };
  };
};

exports.infinity = Infinity;

exports.rangeExcl = function (start) {
  return function (end) {
    return Array(Math.max(0,end-start)).fill(start).map(function (x, y) {
      return x + y;
    });
  };
};