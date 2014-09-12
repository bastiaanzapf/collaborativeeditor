
debug = 1;

function jsNewHash ( hash ) {	 
   if (debug)
      console.log('New Hash: ' + hash);
   window[hash] = Array();
}

function jsStoreHash ( hash, key, value ) {
   if (debug) { 
      console.log('Store hash: ' + hash + ' Key: ' + key);
   }
   window[hash][key] = value;
}

function jsReadHash ( hash, key ) {
   if (debug) { 
      console.log('Read hash: ' + hash + ' Key: ' + key);
   }
   return window[hash][key];
}

function jsPush ( hash, value ) {
   window[hash].push( value );
}

function jsPop ( hash ) {
   return window[hash].pop();
}