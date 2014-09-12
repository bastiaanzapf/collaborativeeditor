
function jsNewHash ( hash ) {
   window[hash] = Array();
}

function jsStoreHash ( hash, key, value ) {
   window[hash][key] = value;
}

function jsReadHash ( hash, key ) {
   return window[hash][key];
}