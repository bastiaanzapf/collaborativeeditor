
function jsNewHash ( hash ) {
   window[hash] = Array();
}

function jsStoreHash ( hash, key, value ) {
   window[hash][key] = value;
}

function jsReadHash ( hash, key ) {
   return window[hash][key];
}

function jsPush ( hash, value ) {
   window[hash].push( value );
}

function jsPop ( hash ) {
   return window[hash].pop();
}