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