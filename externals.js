
function jsNewHash(name) {
  window[name]=new Array();
}

function jsStoreHash(name,key,value) {
  window[name][key]=value;
}

function jsReadHash(name,key) {
  return window[name][key];
}

function jsInitKeypress(elem, cb) {

  fun = function(x) {B(A(cb,[[0,66],0]));};
  console.log('init');

  elem.addEventListener('keypress', function(e) { fun(e) }, false);
  return true; 
}