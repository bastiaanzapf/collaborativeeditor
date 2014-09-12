
function jsNewHash ( hash ) {
   console.log("New Hash");
   console.log(hash);
   window[hash] = Array();
}

function jsStoreHash ( hash, key, value ) {
   console.log("Store in Hash");
   console.log(hash);
   window[hash][key] = value;
}

function jsReadHash ( hash, key ) {
   console.log("Read Hash");
   console.log(window[hash][key]);
   return window[hash][key];
}